#define GL_GLEXT_PROTOTYPES
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif
#ifdef __APPLE__
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif

#ifndef GL_VERSION_1_5
#define GL_ARRAY_BUFFER                   0x8892
#define GL_DYNAMIC_DRAW                   0x88E8
#define GL_STATIC_DRAW                    0x88E4
#define GL_WRITE_ONLY                     0x88B9
#ifndef APIENTRYP
#define APIENTRYP APIENTRY *
#endif
typedef ptrdiff_t GLintptr;
typedef ptrdiff_t GLsizeiptr;
typedef void (APIENTRYP PFNGLBINDBUFFERPROC) (GLenum target, GLuint buffer);
typedef void (APIENTRYP PFNGLGENBUFFERSPROC) (GLsizei n, GLuint *buffers);
typedef void (APIENTRYP PFNGLBUFFERDATAPROC) (GLenum target, GLsizeiptr size, const GLvoid *data, GLenum usage);
typedef GLvoid* (APIENTRYP PFNGLMAPBUFFERPROC) (GLenum target, GLenum access);
typedef GLboolean (APIENTRYP PFNGLUNMAPBUFFERPROC) (GLenum target);
static PFNGLBINDBUFFERPROC glBindBuffer;
static PFNGLGENBUFFERSPROC glGenBuffers;
static PFNGLBUFFERDATAPROC glBufferData;
static PFNGLMAPBUFFERPROC glMapBuffer;
static PFNGLUNMAPBUFFERPROC glUnmapBuffer;
#define GETPA(name) for (;;) {                                          \
    *(PROC *) &gl##name = wglGetProcAddress ("gl" # name);     \
    if (!gl##name) {                                                    \
        fprintf (stderr, "could not get address of gl"#name"\n");       \
        exit (EXIT_FAILURE);                                            \
    }                                                                   \
    break;                                                              \
}
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#include "vec.c"

enum {V_IDX, N_IDX, UV_IDX, C_IDX, COUNT};

struct skin {
    int boneindices[3];
    float weights[3];
    int num_bones;
};

struct bone {
    int parent;
    float v[3];
    float q[4];

    float mv[3];
    float mq[4];

    float aq[4];
    float amq[4];
    float amv[3];

    float am[12];
    float im[12];
};

typedef struct {
    int num_bones;
    int num_vertices;
    GLuint bufid[COUNT];
    float *ptrs[COUNT];
    struct skin *skin;
    struct bone *bones;
} State;

static State glob_state;

static void skin_init (State *s, value vertexa_v, value normala_v,
                       value uva_v, value skin_v, value colors_v)
{
    int i;
    GLsizei size;
    float *p;
    struct skin *skin;
    s->num_vertices = Wosize_val (vertexa_v) / (Double_wosize * 3);

    glGenBuffers (COUNT, s->bufid);

    size = s->num_vertices * sizeof (GLfloat) * 3;
    p = s->ptrs[V_IDX] = stat_alloc (size);
    for (i = 0; i < s->num_vertices * 3; ++i) {
        p[i] = Double_field (vertexa_v, i);
    }
    glBindBuffer (GL_ARRAY_BUFFER, s->bufid[V_IDX]);
    glBufferData (GL_ARRAY_BUFFER, size, p, GL_DYNAMIC_DRAW);

    p = s->ptrs[N_IDX] = stat_alloc (size);
    for (i = 0; i < s->num_vertices * 3; ++i) {
        p[i] = Double_field (normala_v, i);
    }
    glBindBuffer (GL_ARRAY_BUFFER, s->bufid[N_IDX]);
    glBufferData (GL_ARRAY_BUFFER, size, p, GL_DYNAMIC_DRAW);

    size = s->num_vertices * sizeof (GLfloat) * 2;
    p = s->ptrs[UV_IDX] = stat_alloc (size);
    for (i = 0; i < s->num_vertices * 2; ++i) {
        p[i] = Double_field (uva_v, i);
    }
    glBindBuffer (GL_ARRAY_BUFFER, s->bufid[UV_IDX]);
    glBufferData (GL_ARRAY_BUFFER, size, p, GL_STATIC_DRAW);
    stat_free (p);

    size = s->num_vertices * 4;
    glBindBuffer (GL_ARRAY_BUFFER, s->bufid[C_IDX]);
    glBufferData (GL_ARRAY_BUFFER, size, String_val (colors_v), GL_STATIC_DRAW);

    s->skin = skin = stat_alloc (s->num_vertices * sizeof (struct skin));
    for (i = 0; i < s->num_vertices; ++i) {
        int j;
        value v;

        v = Field (skin_v, i);
        skin[i].num_bones = Int_val (Field (v, 3));

        for (j = 0; j < skin[i].num_bones; ++j) {
            double val;

            val = Double_val (Bp_val (Field (v, j)));
            skin[i].boneindices[j] = (int) val;
            skin[i].weights[j] = val - skin[i].boneindices[j];
            skin[i].boneindices[j] += 1;
        }
    }
}

CAMLprim value ml_skin_draw_begin (value unit_v)
{
    State *s = &glob_state;

    glEnableClientState (GL_VERTEX_ARRAY);
    glBindBuffer (GL_ARRAY_BUFFER, s->bufid[V_IDX]);
    glVertexPointer (3, GL_FLOAT, 0, NULL);

    glEnableClientState (GL_NORMAL_ARRAY);
    glBindBuffer (GL_ARRAY_BUFFER, s->bufid[N_IDX]);
    glNormalPointer (GL_FLOAT, 0, NULL);

    glEnableClientState (GL_TEXTURE_COORD_ARRAY);
    glBindBuffer (GL_ARRAY_BUFFER, s->bufid[UV_IDX]);
    glTexCoordPointer (2, GL_FLOAT, 0, NULL);

    glEnableClientState (GL_COLOR_ARRAY);
    glBindBuffer (GL_ARRAY_BUFFER, s->bufid[C_IDX]);
    glColorPointer (4, GL_UNSIGNED_BYTE, 0, NULL);

    return Val_unit;
}

CAMLprim value ml_skin_draw_end (value uint_v)
{
    glDisableClientState (GL_VERTEX_ARRAY);
    glDisableClientState (GL_NORMAL_ARRAY);
    glDisableClientState (GL_TEXTURE_COORD_ARRAY);
    glDisableClientState (GL_COLOR_ARRAY);
    glBindBuffer (GL_ARRAY_BUFFER, 0);

#if 0
    glDisable (GL_TEXTURE_2D);
    glDisable (GL_LIGHTING);
    glColor3f (1.0, 0.0, 0.0);
    glDisable (GL_DEPTH_TEST);
    glLineWidth (4.0);

    glBegin (GL_LINES);
    {
        int i;
        State *s = &glob_state;
        struct bone *b = s->bones + 1;

        for (i = 0; i < s->num_bones; ++i, ++b) {
            struct bone *p = &s->bones[b->parent];

            glVertex3fv (p->amv);
            glVertex3fv (b->amv);
        }
    }
    glEnd ();
#endif
    return Val_unit;
}

CAMLprim value ml_skin_init (value geom_v)
{
    CAMLparam1 (geom_v);
    CAMLlocal5 (vertexa_v, normala_v, uva_v, skin_v, colors_v);
    State *s = &glob_state;

#ifdef _WIN32
    GETPA (BindBuffer);
    GETPA (GenBuffers);
    GETPA (BufferData);
    GETPA (MapBuffer);
    GETPA (UnmapBuffer);
#endif
    vertexa_v = Field (geom_v, 0);
    normala_v = Field (geom_v, 1);
    uva_v     = Field (geom_v, 2);
    skin_v    = Field (geom_v, 3);
    colors_v  = Field (geom_v, 4);

    skin_init (s, vertexa_v, normala_v, uva_v, skin_v, colors_v);
    CAMLreturn (Val_unit);
}

static void translate (State *s, float *dst)
{
    int i, j;
    struct bone *b;
    float *curvert = s->ptrs[V_IDX];
    struct skin *skin = s->skin;

    for (i = 0; i < s->num_vertices; ++i, curvert += 3, dst += 3, ++skin)
    {
        float v[3] = {0,0,0}, v0[3], v1[3], v2[3], w, m[12];

        for (j = 0; j < skin->num_bones; ++j) {
            w = skin->weights[j] + 0.000011;
            b = &s->bones[skin->boneindices[j]];

            vsub (v0, curvert, b->mv);
            mapply_to_vector (v1, b->im, v0);

            mscale (m, b->am, w);
            mapply_to_point (v2, m, v1);

            vaddto (v, v2);
        }
        vcopy (dst, v);
    }

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
    s->bones = b = stat_alloc (size);

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
        float v[3], q[4], q1[4], z[3] = {0,0,0};
        struct bone *parent = &s->bones[b->parent];

        qapply (v, parent->mq, b->v);
        qcompose (b->mq, b->q, parent->mq);
        vadd (b->mv, v, parent->mv);

        qconjugate (q, b->mq);
        qscale (q1, q, 1.0 / qmagnitude (q));
        q2matrix (b->im, q1, z);
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
        float v[3];
        struct bone *parent = &s->bones[b->parent];

        qapply (v, parent->amq, b->v);
        qcompose (b->amq, b->aq, parent->amq);
        vadd (b->amv, v, parent->amv);

        q2matrix (b->am, b->amq, b->amv);
    }

    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_anim (value unit_v)
{
    GLboolean ret;
    CAMLparam1 (unit_v);
    float *vdst, *vsrc, *ndst, *nsrc;
    State *s = &glob_state;

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

    vsrc = s->ptrs[V_IDX];
    nsrc = s->ptrs[N_IDX];

    translate (s, vdst);
#if 0
    {
        int i;
        for (i = 0; i < s->num_vertices; ++i) {
            struct skin *skin;
            struct bone *bone, *parent;
            float v[3], v1[3], *v2;
            float q[4];

            skin = &s->skin[i];
            bone = &s->bones[skin->boneindices[0]];
            parent = &s->bones[bone->parent];

            vdst += 3;
            vsrc += 3;

            *ndst++ = *nsrc++;
            *ndst++ = *nsrc++;
            *ndst++ = *nsrc++;
        }
    }
#endif

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

    CAMLreturn (Val_unit);
}
