#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>

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
#else
#define simd_alloc(b, s) malloc (s)
#define A16
#define AL16(i) (i)
#define AL32(i) (i)
#endif

struct skin {
    float weights[3];
    int boneindices[3];
    int num_bones;
} A16;

struct bone {
    float v[4];
    float q[4];

    float mv[4];
    float mq[4];

    float aq[4];
    float amq[4];
    float amv[4];

    float cm[16];
    int parent;
} A16;

typedef struct {
    int num_bones;
    int num_vertices;
    GLuint bufid[2];
    void *ptrs[2];
    void *bufs[COUNT];
    struct skin *skin;
    struct bone *bones;
} State;

static State glob_state;
static int use_vbo;

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
        skin[i].num_bones = Int_val (Field (v, 3));

        for (j = 0; j < skin[i].num_bones; ++j) {
            double val, w;

            val = Double_val (Bp_val (Field (v, j)));

            skin[i].boneindices[j] = (int) val;
            w = val - skin[i].boneindices[j];
            skin[i].weights[j] = w;
            skin[i].boneindices[j] += 1;
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
}

CAMLprim value ml_skin_draw_begin (value unit_v)
{
    State *s = &glob_state;

    (void) unit_v;

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

    return Val_unit;
}

CAMLprim value ml_skin_draw_end (value unit_v)
{
    (void) unit_v;
    glDisableClientState (GL_VERTEX_ARRAY);
    glDisableClientState (GL_NORMAL_ARRAY);
    glDisableClientState (GL_TEXTURE_COORD_ARRAY);
    glDisableClientState (GL_COLOR_ARRAY);
    if (use_vbo) glBindBuffer (GL_ARRAY_BUFFER, 0);
    return Val_unit;
}

CAMLprim value ml_skin_init (value use_vbo_v, value geom_v)
{
    CAMLparam2 (use_vbo_v, geom_v);
    CAMLlocal5 (vertexa_v, normala_v, uva_v, skin_v, colors_v);
    State *s = &glob_state;

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
    CAMLreturn (Val_unit);
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
    struct bone *b;
    vector float vz = (vector float) vec_splat_u32 (0);
    vector float v, w, n;
    vector unsigned char S = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4<<3};

    v = n = vz;
    w = vec_ld (0, skin->weights);

    j = skin->num_bones;
    for (j = 0; j < skin->num_bones; ++j) {
        vector float t0, t1, t2, t3, t4, t5, r0, r1, r2, r3, vw;

        b = &s->bones[skin->boneindices[j]];
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
    struct bone *b;
    float *vsrc = s->ptrs[0];
    float *nsrc = vsrc + AL16 (s->num_vertices * 3);
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
        float v[3] = {0,0,0}, n[3] = {0,0,0}, v0[4], v1[4], w;

        for (j = 0; j < skin->num_bones; ++j) {
            w = skin->weights[j];
            b = &s->bones[skin->boneindices[j]];

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

CAMLprim value ml_skin_set_skel (value skel_v)
{
    int i;
    size_t size;
    struct bone *b;
    CAMLparam1 (skel_v);
    CAMLlocal2 (v, floats_v);
    State *s = &glob_state;

    s->num_bones = Wosize_val (skel_v);
    size = (s->num_bones + 1) * sizeof (struct bone);
    s->bones = b = simd_alloc (16, size);

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
    for (i = 0; i < s->num_bones; ++i, ++b) {
        float v[3];
        struct bone *parent = &s->bones[b->parent];

        qapply (v, parent->mq, b->v);
        qcompose (b->mq, b->q, parent->mq);
        vadd (b->mv, v, parent->mv);
#ifdef USE_ALTIVEC
        b->cm[3] = b->mv[0];
        b->cm[7] = b->mv[1];
        b->cm[11] = b->mv[2];
#endif
    }

    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_set_anim (value anim_v)
{
    int i;
    CAMLparam1 (anim_v);
    CAMLlocal1 (floats_v);
    State *s = &glob_state;
    struct bone *b = s->bones + 1;

    for (i = 0; i < s->num_bones; ++i, ++b) {
        floats_v = Field (anim_v, i);
        b->aq[0] = Double_field (floats_v, 0);
        b->aq[1] = Double_field (floats_v, 1);
        b->aq[2] = Double_field (floats_v, 2);
        b->aq[3] = Double_field (floats_v, 3);
    }

    b = s->bones + 1;
    for (i = 0; i < s->num_bones; ++i, ++b) {
        float v[4], v1[4], q[4], q1[4];
        struct bone *parent = &s->bones[b->parent];

        qapply (v, parent->amq, b->v);
        qcompose (b->amq, b->aq, parent->amq);
        vadd (b->amv, v, parent->amv);

        qconjugate (q1, b->mq);
        qcompose (q, q1, b->amq);

        qapply (v, q, b->mv);
        vsub (v1, b->amv, v);
        q2matrixt (b->cm, q, v1);
    }

    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_anim (value unit_v)
{
    GLboolean ret;
    CAMLparam1 (unit_v);
    float *vdst, *ndst;
    State *s = &glob_state;

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
