#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/unixsupport.h>

#include "vec.c"
#include "pgl.h"

enum {V_IDX, N_IDX, UV_IDX, C_IDX, COUNT};

#define ALNN(n, i) (((i)+(n-1))&~(n-1))

#ifdef G4
#define DSTAL 32
#else
#define DSTAL 16
#endif

#ifdef USE_ALTIVEC
/* Altivec code derived from: */
/* http://www.freevec.org/category/simd/algorithms/algebra/matrix_operations */
#include <altivec.h>
#include <malloc.h>
#define simd_alloc(b, s) memalign (b, s)
#define A16 __attribute__ ((aligned (16)))
#define AL16(i) ALNN (16, i)
#define AL32(i) ALNN (32, i)
#define CM_ELEMS 16
#else
#define simd_alloc(b, s) malloc (s)
#define A16
#define AL16(i) (i)
#define AL32(i) (i)
#define CM_ELEMS 12
#endif

struct skin {
    float weights[3];
    int boneinfo;
} A16;

struct bone {
    float v[4];
    float q[4];

    float mv[4];
    float mq[4];

    float aq[4];
    float amq[4];
    float amv[4];

    int parent;
};

struct abone {
    float cm[CM_ELEMS];
} A16;

typedef struct state {
    int num_bones;
    int num_vertices;
    GLuint bufid[2];
    void *ptrs[2];
    void *bufs[COUNT];
    struct skin *skin;
    struct bone *bones;
    struct abone *abones;
    struct bone *rbone;
    struct abone *rabone;
    FILE *stlf;
} State;

static int use_vbo;

#define State_val(v) ((State *) Data_custom_val (v))

static void ml_state_finalize (value state_v)
{
    State *s = State_val (state_v);
    free (s->ptrs[0]);
    if (use_vbo) {
        glDeleteBuffers (2, s->bufid);
    }
    else {
        free (s->ptrs[1]);
        free (s->bufs[0]);
    }
}

static char state_ops_name[] = "dormin.0";
static struct custom_operations state_custom_ops = {
    state_ops_name,
    ml_state_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static void copy_vertices (float *p, int num_vertices, value a_v)
{
    int i, k;

    for (i = 0, k = 0; i < num_vertices; ++i, p += 3) {
        p[0] = Double_field (a_v, k++);
        p[1] = Double_field (a_v, k++);
        p[2] = Double_field (a_v, k++);
    }
}

static void set_geom (State *s, void **ptrs, value vertexa_v, value normala_v,
                      value uva_v, value skin_v, value colors_v)
{
    int i;
    float *p;
    int num_vertices;
    struct skin *skin;

    num_vertices = Wosize_val (vertexa_v) / (Double_wosize * 3);

    copy_vertices (ptrs[V_IDX], num_vertices, vertexa_v);
    copy_vertices (ptrs[N_IDX], num_vertices, normala_v);

    for (i = 0, p = ptrs[UV_IDX]; i < num_vertices * 2; ++i) {
        p[i] = Double_field (uva_v, i);
    }
    memcpy (ptrs[C_IDX], String_val (colors_v), num_vertices * 4);

    skin = s->skin;
    for (i = 0; i < num_vertices; ++i) {
        int j;
        value v;

        v = Field (skin_v, i);
        skin[i].boneinfo = Int_val (Field (v, 3));

        for (j = 0; j < Int_val (Field (v, 3)); ++j) {
            double val;
            int boneindex;
            const int shifts[] = {2,12,22};

            val = Double_val (Bp_val (Field (v, j)));

            boneindex = floor (val);
            skin[i].weights[j] = fabs (val - (int) val);
            skin[i].boneinfo |= (boneindex + 1) << shifts[j];
        }
    }
}

static void skin_init (State *s, value vertexa_v, value normala_v,
                       value uva_v, value skin_v, value colors_v)
{
    char *p;
    GLsizei sizevn, sizev, sizeu, sizec;
    void *ptrs[COUNT];

    s->num_vertices = Wosize_val (vertexa_v) / (Double_wosize * 3);

    sizev = AL32 (3 * sizeof (GLfloat) * s->num_vertices);
    sizeu = 2 * sizeof (GLfloat) * s->num_vertices;
    sizec = 4 * s->num_vertices;

    sizevn = sizev * 2;

    p = simd_alloc (16, AL16 (sizevn) + s->num_vertices * sizeof (struct skin));
    s->skin = (struct skin *) (p + AL16 (sizevn));
    s->ptrs[0] = ptrs[V_IDX] = p;
    ptrs[N_IDX] = p + sizev;

    p = stat_alloc (sizec + sizeu);
    s->ptrs[1] = ptrs[UV_IDX] = p;
    ptrs[C_IDX] = p + sizeu;

    set_geom (s, ptrs, vertexa_v, normala_v, uva_v, skin_v, colors_v);

    if (use_vbo) {
        glGenBuffers (2, s->bufid);

        glBindBuffer (GL_ARRAY_BUFFER, s->bufid[0]);
        glBufferData (GL_ARRAY_BUFFER, sizevn, s->ptrs[0], GL_DYNAMIC_DRAW);

        glBindBuffer (GL_ARRAY_BUFFER, s->bufid[1]);
        glBufferData (GL_ARRAY_BUFFER, sizeu+sizec, s->ptrs[1], GL_STATIC_DRAW);

        glBindBuffer (GL_ARRAY_BUFFER, 0);
        stat_free (s->ptrs[1]);

        p = NULL;
        s->bufs[V_IDX] = p;
        s->bufs[N_IDX] = p + sizev;
        s->bufs[UV_IDX] = p;
        s->bufs[C_IDX] = p + sizeu;
    }
    else {
        p = simd_alloc (DSTAL, sizevn);
        s->bufs[V_IDX] = p;
        s->bufs[N_IDX] = p + sizev;
        s->bufs[UV_IDX] = ptrs[UV_IDX];
        s->bufs[C_IDX] = ptrs[C_IDX];

        memcpy (p, s->ptrs[0], sizevn);
    }
    s->num_vertices /= 1;
}

CAMLprim value ml_skin_draw_begin (value state_v)
{
    CAMLparam1 (state_v);
    State *s = State_val (state_v);

    glEnableClientState (GL_VERTEX_ARRAY);
    glEnableClientState (GL_NORMAL_ARRAY);
    glEnableClientState (GL_TEXTURE_COORD_ARRAY);
    glEnableClientState (GL_COLOR_ARRAY);

    if (use_vbo) glBindBuffer (GL_ARRAY_BUFFER, s->bufid[0]);
    glVertexPointer (3, GL_FLOAT, 3 * sizeof (GLfloat), s->bufs[V_IDX]);
    glNormalPointer (GL_FLOAT, 3 * sizeof (GLfloat), s->bufs[N_IDX]);

    if (use_vbo) glBindBuffer (GL_ARRAY_BUFFER, s->bufid[1]);
    glTexCoordPointer (2, GL_FLOAT, 0, s->bufs[UV_IDX]);
    glColorPointer (4, GL_UNSIGNED_BYTE, 0, s->bufs[C_IDX]);

    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_draw_end (value state_v)
{
    CAMLparam1 (state_v);
    glDisableClientState (GL_VERTEX_ARRAY);
    glDisableClientState (GL_NORMAL_ARRAY);
    glDisableClientState (GL_TEXTURE_COORD_ARRAY);
    glDisableClientState (GL_COLOR_ARRAY);
    if (use_vbo) glBindBuffer (GL_ARRAY_BUFFER, 0);
    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_init (value name_v, value use_vbo_v, value geom_v)
{
    CAMLparam3 (name_v, use_vbo_v, geom_v);
    CAMLlocal5 (vertexa_v, normala_v, uva_v, skin_v, colors_v);
    value state_v;
    State *s;

    state_v = caml_alloc_custom (&state_custom_ops, sizeof (*s), 0, 1);
    s = State_val (state_v);
    memset (s, 0, sizeof (*s));

    use_vbo = Bool_val (use_vbo_v);
#ifdef _WIN32
    if (use_vbo) {
        GETPA (BindBuffer);
        GETPA (GenBuffers);
        GETPA (BufferData);
        GETPA (BufferSubData);
        GETPA (MapBuffer);
        GETPA (UnmapBuffer);
    }
#endif
    vertexa_v = Field (geom_v, 0);
    normala_v = Field (geom_v, 1);
    uva_v     = Field (geom_v, 2);
    skin_v    = Field (geom_v, 3);
    colors_v  = Field (geom_v, 4);

    skin_init (s, vertexa_v, normala_v, uva_v, skin_v, colors_v);
    CAMLreturn (state_v);
}

#ifdef TIMING
#include <err.h>
#include <sys/time.h>
static double now (void)
{
    struct timeval tv;

    if (gettimeofday (&tv, NULL)) err (1, "gettimeofday");
    return tv.tv_sec + tv.tv_usec * 1e-6;
}
#endif

#ifdef USE_ALTIVEC

#define DCB(o, b, i) __asm__ __volatile__ (#o " %0, %1" ::"b"(b),"r"(i))

static vector float appbones (State *s,
                              struct skin *skin,
                              vector float x,
                              vector float y,
                              vector float z,
                              vector float nx,
                              vector float ny,
                              vector float nz,
                              vector float *np)
{
    int j;
    int num_bones;
    int bone_index;
    struct abone *b;
    vector float vz = (vector float) vec_splat_u32 (0);
    vector float v, w, n;
    vector unsigned char S = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4<<3};

    v = n = vz;
    w = vec_ld (0, skin->weights);

    num_bones = skin->boneinfo & 3;
    bone_index = skin->boneinfo >> 2;
    for (j = 0; j < num_bones; ++j) {
        vector float t0, t1, t2, t3, t4, t5, r0, r1, r2, r3, vw;

        b = &s->abones[bone_index & 0x3ff];
        bone_index >>= 10;
        vw = vec_splat (w, 0);
        w = vec_slo (w, S);

        r0 = vec_ld ( 0, b->cm);
        r1 = vec_ld (16, b->cm);
        r2 = vec_ld (32, b->cm);
        r3 = vec_ld (48, b->cm);

        t0 = vec_madd (r0, x, r3);
        t1 = vec_madd (r1, y, t0);
        t2 = vec_madd (r2, z, t1);
        v = vec_madd (t2, vw, v);

        t3 = vec_madd (r0, nx, vz);
        t4 = vec_madd (r1, ny, t3);
        t5 = vec_madd (r2, nz, t4);
        n = vec_madd (t5, vw, n);
    }

    *np = n;
    return v;
}
#endif

static void translate (State *s, float *vdst, float *ndst)
{
    int i, j;
    struct abone *b;
    float *vsrc = s->ptrs[0];
    float *nsrc =
        (float *) ((char *) vsrc + AL32 (s->num_vertices * 3 * sizeof (GLfloat)));
    struct skin *skin = s->skin;

#ifdef TIMING
    double S = now (), E;
#endif

#ifdef USE_ALTIVEC
    vector unsigned char p0 =
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16, 17, 18, 19 };
    vector unsigned char p1 =
        { 4, 5, 6, 7, 8, 9, 10, 11, 16, 17, 18, 19, 20, 21, 22, 23 };
    vector unsigned char p2 =
        { 8, 9, 10, 11, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27 };

    for (i = 0, j = 0; i < s->num_vertices >> 2; ++i, j += 48) {
        vector float v0, v1, v2, n0, n1, n2;
        vector float vx, vy, vz, nx, ny, nz;
        vector float vr0, vr1, vr2, vr3;
        vector float nr0, nr1, nr2, nr3;

#ifdef G4
        if (!(i & 3)) {
            DCB (dcbz, vdst, j);
            DCB (dcbz, ndst, j);
        }

        DCB (dcbz, vdst, j + 32);
        DCB (dcbz, ndst, j + 32);
#endif

        DCB (dcbt, skin, 0);
        DCB (dcbt, skin + 1, 0);
        DCB (dcbt, skin + 2, 0);
        DCB (dcbt, skin + 3, 0);

        DCB (dcbt, vsrc, j + 64);
        DCB (dcbt, nsrc, j + 64);
        DCB (dcbt, vsrc, j + 96);
        DCB (dcbt, nsrc, j + 96);

        /* Load */
        v0 = vec_ld (j, vsrc);
        v1 = vec_ld (j + 16, vsrc);
        v2 = vec_ld (j + 32, vsrc);
        n0 = vec_ld (j, nsrc);
        n1 = vec_ld (j + 16, nsrc);
        n2 = vec_ld (j + 32, nsrc);

        /* First vertex/normal */
        vx = vec_splat (v0, 0);
        vy = vec_splat (v0, 1);
        vz = vec_splat (v0, 2);
        nx = vec_splat (n0, 0);
        ny = vec_splat (n0, 1);
        nz = vec_splat (n0, 2);

        vr0 = appbones (s, skin, vx, vy, vz, nx, ny, nz, &nr0);
        skin++;

        /* Second vertex/normal */
        vx = vec_splat (v0, 3);
        vy = vec_splat (v1, 0);
        vz = vec_splat (v1, 1);
        nx = vec_splat (n0, 3);
        ny = vec_splat (n1, 0);
        nz = vec_splat (n1, 1);

        vr1 = appbones (s, skin, vx, vy, vz, nx, ny, nz, &nr1);
        skin++;

        /* Third vertex/normal */
        vx = vec_splat (v1, 2);
        vy = vec_splat (v1, 3);
        vz = vec_splat (v2, 0);
        nx = vec_splat (n1, 2);
        ny = vec_splat (n1, 3);
        nz = vec_splat (n2, 0);

        vr2 = appbones (s, skin, vx, vy, vz, nx, ny, nz, &nr2);
        skin++;

        /* Fourth vertex/normal */
        vx = vec_splat (v2, 1);
        vy = vec_splat (v2, 2);
        vz = vec_splat (v2, 3);
        nx = vec_splat (n2, 1);
        ny = vec_splat (n2, 2);
        nz = vec_splat (n2, 3);

        vr3 = appbones (s, skin, vx, vy, vz, nx, ny, nz, &nr3);
        skin++;

        /* Assemble */
        v0 = vec_perm (vr0, vr1, p0);
        v1 = vec_perm (vr1, vr2, p1);
        v2 = vec_perm (vr2, vr3, p2);

        n0 = vec_perm (nr0, nr1, p0);
        n1 = vec_perm (nr1, nr2, p1);
        n2 = vec_perm (nr2, nr3, p2);

        /* Store */
        vec_st (v0, j, vdst);
        vec_st (v1, j + 16, vdst);
        vec_st (v2, j + 32, vdst);

        vec_st (n0, j, ndst);
        vec_st (n1, j + 16, ndst);
        vec_st (n2, j + 32, ndst);
    }

    i <<= 2;
    vsrc += i*3;
    nsrc += i*3;
    vdst += i*3;
    ndst += i*3;
#else
    i = 0;
#endif

    for (; i < s->num_vertices; ++i, vsrc += 3, nsrc += 3, vdst += 3, ndst += 3,
             ++skin)
    {
        int num_bones, bone_index;
        float v[3] = {0,0,0}, n[3] = {0,0,0}, v0[4], v1[4], w;

        num_bones = skin->boneinfo & 3;
        bone_index = skin->boneinfo >> 2;
        for (j = 0; j < num_bones; ++j) {
            w = skin->weights[j];
            b = &s->abones[bone_index & 0x3ff];
            bone_index >>= 10;

            mapply_to_point (v1, b->cm, vsrc);
            v1[0] *= w;
            v1[1] *= w;
            v1[2] *= w;

            mapply_to_vector (v0, b->cm, nsrc);
            v0[0] *= w;
            v0[1] *= w;
            v0[2] *= w;

            vaddto (v, v1);
            vaddto (n, v0);
        }

        vcopy (vdst, v);
        vcopy (ndst, n);
    }

#ifdef TIMING
    E = now ();
    printf ("took %f sec\n", E - S);
#endif
}

CAMLprim value ml_skin_set_skel (value state_v, value skel_v)
{
    int i;
    size_t size;
    struct bone *b;
    struct abone *ab;
    CAMLparam2 (state_v, skel_v);
    CAMLlocal2 (v, floats_v);
    State *s = State_val (state_v);

    s->num_bones = Wosize_val (skel_v);
    size = (s->num_bones + 1) * sizeof (*b);
    s->bones = b = simd_alloc (16, size);
    s->abones = ab = simd_alloc (16, (s->num_bones + 1) * sizeof (*ab));

    memset (b, 0, size);
    b->parent = -1;
    b->q[3] = 1.0;
    b->mq[3] = 1.0;
    b->aq[3] = 1.0;
    b->amq[3] = 1.0;
    b++;

    for (i = 0; i < s->num_bones; ++i, ++b) {
        v = Field (skel_v, i);
        floats_v = Field (v, 1);

        b->parent = Int_val (Field (v, 0)) + 1;

        b->v[0] = Double_field (floats_v, 1);
        b->v[1] = Double_field (floats_v, 2);
        b->v[2] = Double_field (floats_v, 3);

        b->q[0] = Double_field (floats_v, 5);
        b->q[1] = Double_field (floats_v, 6);
        b->q[2] = Double_field (floats_v, 7);
        b->q[3] = Double_field (floats_v, 8);
    }

    b = s->bones + 1;
    ab = s->abones + 1;
    for (i = 0; i < s->num_bones; ++i, ++b, ++ab) {
        float v[3];
        struct bone *parent = &s->bones[b->parent];

        qapply (v, parent->mq, b->v);
        qcompose (b->mq, b->q, parent->mq);
        vadd (b->mv, v, parent->mv);
    }

    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_set_anim (value state_v, value anim_v)
{
    int i;
    CAMLparam2 (state_v, anim_v);
    CAMLlocal1 (floats_v);
    State *s = State_val (state_v);
    struct bone *b = s->bones;
    struct abone *ab;

    if (s->rbone) {
        memcpy (b, s->rbone, sizeof (*b));
    }
    b++;

    for (i = 0; i < s->num_bones; ++i, ++b) {
        floats_v = Field (anim_v, i);
        b->aq[0] = Double_field (floats_v, 0);
        b->aq[1] = Double_field (floats_v, 1);
        b->aq[2] = Double_field (floats_v, 2);
        b->aq[3] = Double_field (floats_v, 3);
    }

    b = s->bones + 1;
    ab = s->abones + 1;

    for (i = 0; i < s->num_bones; ++i, ++b, ++ab) {
        float v[4], v1[4], q[4], q1[4];
        struct bone *parent = &s->bones[b->parent];

        qapply (v, parent->amq, b->v);
        qcompose (b->amq, b->aq, parent->amq);
        vadd (b->amv, v, parent->amv);

        qconjugate (q1, b->mq);
        qcompose (q, q1, b->amq);

        qapply (v, q, b->mv);
        vsub (v1, b->amv, v);
        q2matrixt (ab->cm, q, v1);
    }

    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_anim (value state_v)
{
    GLboolean ret;
    CAMLparam1 (state_v);
    float *vdst, *ndst;
    State *s = State_val (state_v);

    if (use_vbo) {
        glBindBuffer (GL_ARRAY_BUFFER, s->bufid[0]);
        vdst = ndst = glMapBuffer (GL_ARRAY_BUFFER, GL_WRITE_ONLY);
        if (!vdst) caml_failwith ("glMapBuffer failed");

        ndst += (float *) s->bufs[N_IDX] - (float *) s->bufs[V_IDX];
    }
    else {
        vdst = s->bufs[V_IDX];
        ndst = s->bufs[N_IDX];
    }

    if (s->rabone) {
        memcpy (s->abones, s->rabone, sizeof (*s->abones));
    }
    translate (s, vdst, ndst);

    if (use_vbo) {
        ret = glUnmapBuffer (GL_ARRAY_BUFFER);
        if (ret == GL_FALSE) caml_failwith ("glUnmapBuffer failed");
    }

    CAMLreturn (Val_unit);
}

#ifndef GL_GENERATE_MIPMAP
#define GL_GENERATE_MIPMAP                0x8191
#endif

CAMLprim value ml_set_generate_mipmaps (value unit_v)
{
    (void) unit_v;
    glTexParameteri (GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
    return Val_unit;
}

CAMLprim value ml_skin_set_parent (value skin_v, value root_skin_v,
                                   value root_index_v)
{
    CAMLparam3 (skin_v, root_skin_v, root_index_v);
    State *s, *root;
    int index = Int_val (root_index_v);

    s = State_val (skin_v);
    root = State_val (root_skin_v);
    s->rbone = &root->bones[index + 1];
    s->rabone = &root->abones[index + 1];

    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_stl_begin (value skin_v)
{
    CAMLparam1 (skin_v);
    State *s = State_val (skin_v);

    s->stlf = fopen ("dump.stl", "wb");
    if (!s->stlf) {
        value ue = unix_error_of_code (errno);
        caml_raise (ue);
    }
    fprintf (s->stlf, "solid\n");
    CAMLreturn (Val_unit);
}

#include <GL/glut.h>
CAMLprim value ml_skin_stl_end (value skin_v)
{
    CAMLparam1 (skin_v);
    State *s = State_val (skin_v);

    fprintf (s->stlf, "endsolid\n");
    fclose (s->stlf);
    s->stlf = NULL;
    glDrawBuffer (GL_BACK);
    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_stl (value skin_v, value index_v, value count_v)
{
    CAMLparam3 (skin_v, index_v, count_v);
    State *s = State_val (skin_v);
    int index = Int_val (index_v);
    int count = Int_val (count_v);
    int i;
    float *v;
    FILE *f;

    v = s->bufs[V_IDX] + (index*3*4);

    f = s->stlf;
    for (i = 0; i < count - 2; ++i ) {
        float *v0;
        float *v1;
        float *v2;
        float a[3], b[3], n1[3], n[3];

        if (i & 1) {
            v0 = v + i*3;
            v1 = v + i*3 + 3;
            v2 = v + i*3 + 6;
        }
        else  {
            v0 = v + i*3 + 3;
            v1 = v + i*3;
            v2 = v + i*3 + 6;
        }
        vsub (a, v1, v0);
        vsub (b, v2, v0);

        vcross (n1, b, a);
        vnorm (n, n1);

        fprintf (f, "facet normal %e %e %e\n", n[0], n[1], n[2]);
        fprintf (f, "outer loop\n");
        fprintf (f, "vertex %e %e %e\n", v0[0], v0[1], v0[2]);
        fprintf (f, "vertex %e %e %e\n", v1[0], v1[1], v1[2]);
        fprintf (f, "vertex %e %e %e\n", v2[0], v2[1], v2[2]);
        fprintf (f, "endloop\n");
        fprintf (f, "endfacet\n");
    }

    CAMLreturn (Val_unit);
}
