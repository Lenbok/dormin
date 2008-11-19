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

#ifdef USE_ALTIVEC
/* Altivec code derived from: */
/* http://www.freevec.org/category/simd/algorithms/algebra/matrix_operations */

#include <altivec.h>
#ifndef __APPLE__
#include <malloc.h>
#define simd_alloc(s) memalign (16, s)
#else
#define simd_alloc malloc
#endif
#define A16 __attribute__ ((aligned (16)))
#define STRIDE 16
#define V_ELEMS 4
#define AL16(i) (((i)+15)&~15)

#else

#define STRIDE 0
#define V_ELEMS 3
#define simd_alloc(s) malloc (s)
#define A16
#define AL16(i) (i)

#endif

const int usage[COUNT] = {GL_DYNAMIC_DRAW, GL_DYNAMIC_DRAW,
                          GL_STATIC_DRAW, GL_STATIC_DRAW};

struct skin {
    int boneindices[3];
#ifdef USE_ALTIVEC
    float weights[12] A16;
#else
    float weights[3];
#endif
    int num_bones;
} A16;

struct bone {
    float v[4] A16;
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
    GLuint bufid[COUNT];
    void *ptrs[COUNT];
    void *bufs[COUNT];
    struct skin *skin;
    struct bone *bones;
} State;

static State glob_state;
static int use_vbo;

static void copy_vertices (float *p, int num_vertices, value a_v)
{
    int i, j, k;

    for (i = 0, j = 0, k = 0; i < num_vertices; ++i) {
        p[j++] = Double_field (a_v, k++);
        p[j++] = Double_field (a_v, k++);
        p[j++] = Double_field (a_v, k++);
#ifdef USE_ALTIVEC
        p[j++] = 1.0;
#endif
    }
}

static void set_geom (State *s, value vertexa_v, value normala_v,
                      value uva_v, value skin_v, value colors_v)
{
    int i;
    float *p;
    int num_vertices;
    struct skin *skin;

    num_vertices = Wosize_val (vertexa_v) / (Double_wosize * 3);

    copy_vertices (s->ptrs[V_IDX], num_vertices, vertexa_v);
    copy_vertices (s->ptrs[N_IDX], num_vertices, normala_v);

    for (i = 0, p = s->ptrs[UV_IDX]; i < num_vertices * 2; ++i) {
        p[i] = Double_field (uva_v, i);
    }
    memcpy (s->ptrs[C_IDX], String_val (colors_v), num_vertices * 4);

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
#ifdef USE_ALTIVEC
            vector float vw = {w,w,w,w};

            vec_st (vw, j*16, skin[i].weights);
#else
            skin[i].weights[j] = w;
#endif
            skin[i].boneindices[j] += 1;
        }
    }
}

static void skin_init (State *s, value vertexa_v, value normala_v,
                       value uva_v, value skin_v, value colors_v)
{
    int i;
    char *p;
    GLsizei size;
    int sizes[COUNT], offsets[COUNT];

    s->num_vertices = Wosize_val (vertexa_v) / (Double_wosize * 3);

    sizes[V_IDX] = V_ELEMS * sizeof (GLfloat);
    sizes[N_IDX] = V_ELEMS * sizeof (GLfloat);
    sizes[UV_IDX] = 2 * sizeof (GLfloat);
    sizes[C_IDX] = 4;

    for (i = 0, size = 0; i < COUNT; ++i) {
        offsets[i] = size;
        sizes[i] *= s->num_vertices;
        size += sizes[i];
    }

    p = simd_alloc (AL16 (size) + s->num_vertices * sizeof (struct skin));
    s->skin = (struct skin *) (p + AL16 (size));

    for (i = 0; i < COUNT; ++i) s->ptrs[i] = p + offsets[i];

    set_geom (s, vertexa_v, normala_v, uva_v, skin_v, colors_v);

    if (use_vbo) {
        glGenBuffers (COUNT, s->bufid);

        for (i = 0; i < COUNT; ++i) {
            glBindBuffer (GL_ARRAY_BUFFER, s->bufid[i]);
            glBufferData (GL_ARRAY_BUFFER, sizes[i], NULL, usage[i]);
            glBufferSubData (GL_ARRAY_BUFFER, 0, sizes[i], s->ptrs[i]);
            s->bufs[i] = NULL;
        }
        glBindBuffer (GL_ARRAY_BUFFER, 0);
    }
    else {
        for (i = 0; i < COUNT; ++i) {
            if (usage[i] == GL_STATIC_DRAW) {
                s->bufs[i] = s->ptrs[i];
            }
            else {
                s->bufs[i] = simd_alloc (sizes[i]);
                memcpy (s->bufs[i], s->ptrs[i], sizes[i]);
            }
        }
    }
}

CAMLprim value ml_skin_draw_begin (value unit_v)
{
    State *s = &glob_state;

    (void) unit_v;
    glEnableClientState (GL_VERTEX_ARRAY);
    if (use_vbo) glBindBuffer (GL_ARRAY_BUFFER, s->bufid[V_IDX]);
    glVertexPointer (3, GL_FLOAT, STRIDE, s->bufs[V_IDX]);

    glEnableClientState (GL_NORMAL_ARRAY);
    if (use_vbo) glBindBuffer (GL_ARRAY_BUFFER, s->bufid[N_IDX]);
    glNormalPointer (GL_FLOAT, STRIDE, s->bufs[N_IDX]);

    glEnableClientState (GL_TEXTURE_COORD_ARRAY);
    if (use_vbo) glBindBuffer (GL_ARRAY_BUFFER, s->bufid[UV_IDX]);
    glTexCoordPointer (2, GL_FLOAT, 0, s->bufs[UV_IDX]);

    glEnableClientState (GL_COLOR_ARRAY);
    if (use_vbo) glBindBuffer (GL_ARRAY_BUFFER, s->bufid[C_IDX]);
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

static void translate (State *s, float *vdst, float *ndst)
{
    int i, j;
    struct bone *b;
    float *vsrc = s->ptrs[V_IDX];
    float *nsrc = s->ptrs[N_IDX];
    struct skin *skin = s->skin;

#ifdef TIMING
    double S = now (), E;
#endif

#ifdef USE_ALTIVEC
    for (i = 0; i < s->num_vertices; ++i, ++skin) {
        vector float v, n, vs, ns, vz;
        vector float r0, r1, r2, r3, nx, ny, nz;

        v = n = vz = (vector float) vec_splat_u32 (0);

        vs = vec_ld (i<<4, vsrc);
        ns = vec_ld (i<<4, nsrc);

        nx = vec_splat (ns, 0);
        ny = vec_splat (ns, 1);
        nz = vec_splat (ns, 2);

        for (j = 0; j < skin->num_bones; ++j) {
            vector float vw, x, y, z, t0, t1, t2;

            b = &s->bones[skin->boneindices[j]];

            vw = vec_ld (j<<4, skin->weights);

            r0 = vec_ld ( 0, b->cm);
            r1 = vec_ld (16, b->cm);
            r2 = vec_ld (32, b->cm);
            r3 = vec_ld (48, b->cm);

            x = vec_splat (vs, 0);
            y = vec_splat (vs, 1);
            z = vec_splat (vs, 2);

            t0 = vec_madd (r0, x, r3);
            t1 = vec_madd (r1, y, t0);
            t2 = vec_madd (r2, z, t1);
            v = vec_madd (t2, vw, v);

            t0 = vec_madd (r0, nx, vz);
            t1 = vec_madd (r1, ny, t0);
            t2 = vec_madd (r2, nz, t1);
            n = vec_madd (t2, vw, n);
        }
        vec_st (v, i<<4, vdst);
        vec_st (n, i<<4, ndst);
    }
#else
    for (i = 0; i < s->num_vertices; ++i,
             vsrc += 3, nsrc += 3, vdst += 3, ndst += 3, ++skin)
    {
        if (skin->num_bones == 1) {
            b = &s->bones[skin->boneindices[0]];

            mapply_to_point (vdst, b->cm, vsrc);
            mapply_to_vector (ndst, b->cm, nsrc);
        }
        else
        {
            int z = 0;
            float v[3] = {0,0,0}, n[3] = {0,0,0}, v0[4], v1[4], w;

            for (j = 0; j < skin->num_bones; ++j) {
                w = skin->weights[j];
                b = &s->bones[skin->boneindices[j]];

                if (w < 0.0) z = 1;

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

            /* hack hack */
            if (z) vcopy (vdst, vsrc);
            else vcopy (vdst, v);
            vcopy (ndst, n);
        }
    }
#endif

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
    s->bones = b = simd_alloc (size);

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
    float *vdst, *vsrc, *ndst, *nsrc;
    State *s = &glob_state;

    if (use_vbo) {
        glBindBuffer (GL_ARRAY_BUFFER, s->bufid[V_IDX]);
        vdst = glMapBuffer (GL_ARRAY_BUFFER, GL_WRITE_ONLY);
        if (!vdst) {
            fprintf (stderr, "glMapBuffer for vertices failed\n");
            exit (EXIT_FAILURE);
        }

        glBindBuffer (GL_ARRAY_BUFFER, s->bufid[N_IDX]);
        ndst = glMapBuffer (GL_ARRAY_BUFFER, GL_WRITE_ONLY);
        if (!ndst) {
            fprintf (stderr, "glMapBuffer for normals failed\n");
            exit (EXIT_FAILURE);
        }
    }
    else {
        vdst = s->bufs[V_IDX];
        ndst = s->bufs[N_IDX];
    }

    vsrc = s->ptrs[V_IDX];
    nsrc = s->ptrs[N_IDX];

    translate (s, vdst, ndst);

    if (use_vbo) {
        glBindBuffer (GL_ARRAY_BUFFER, s->bufid[V_IDX]);
        ret = glUnmapBuffer (GL_ARRAY_BUFFER);
        if (ret == GL_FALSE) {
            fprintf (stderr, "glUnmapBuffer for vertices failed\n");
            exit (EXIT_FAILURE);
        }

        glBindBuffer (GL_ARRAY_BUFFER, s->bufid[N_IDX]);
        ret = glUnmapBuffer (GL_ARRAY_BUFFER);
        if (ret == GL_FALSE) {
            fprintf (stderr, "glUnmapBuffer for normals failed\n");
            exit (EXIT_FAILURE);
        }
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
