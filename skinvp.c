#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#define USE_VP

#include "vec.c"
#include "pgl.h"

#ifdef _MSC_VER
#define snprintf _snprintf
#endif

enum {V_IDX, N_IDX, UV_IDX, C_IDX, A_IDX, COUNT};

struct skin {
    int boneindices[3];
    float weights[3];
    int num_bones;
};

struct bone {
    int parent;

    float v[4];
    float q[4];

    float mv[4];
    float mq[4];

    float aq[4];
    float amq[4];
    float amv[4];

    float cm[12];
};

struct adescr {
    int size;
    int norm;
    int index;
    int stride;
    int offset;
    int num_elems;
    GLenum type;
};

const struct adescr ad[] = {
    { 4, 0, 0, 12, 0, 3, GL_FLOAT },
    { 4, 0, 2, 12, 0, 3, GL_FLOAT },
    { 4, 0, 8,  8, 0, 2, GL_FLOAT },
    { 1, 1, 3,  4, 0, 4, GL_UNSIGNED_BYTE },
    { 4, 0, 6, 12, 0, 3, GL_FLOAT }
};

typedef struct {
    char *array;
    int num_bones;
    int num_vertices;
    GLuint bufid;
    struct adescr ad[COUNT];
    struct skin *skin;
    struct bone *bones;
} State;

static State glob_state;
static int use_vbo;

static void skin_init (State *s, value vertexa_v, value normala_v,
                       value uva_v, value skin_v, value colors_v,
                       int interleave)
{
    int i;
    char *p;
    char *map;
    GLsizei size;
    struct skin *skin;
    GLsizei offset = 0;
    GLsizei stride = 12 * sizeof (GLfloat);
    GLsizei strides[COUNT];

    s->num_vertices = Wosize_val (vertexa_v) / (Double_wosize * 3);

    for (i = 0, size = 0; i < COUNT; ++i) {
        s->ad[i].index = ad[i].index;
        if (interleave) {
            s->ad[i].offset = offset;
            s->ad[i].stride = stride;
            strides[i] = stride;
            offset += ad[i].stride;
        }
        else {
            strides[i] = ad[i].stride;
            s->ad[i].offset = size;
            s->ad[i].stride =
                ad[i].num_elems*ad[i].size == ad[i].stride
                ? 0
                : ad[i].stride
                ;
        }
        s->ad[i].norm = ad[i].norm;
        s->ad[i].type = ad[i].type;
        s->ad[i].num_elems = ad[i].num_elems;
        s->ad[i].size = ad[i].num_elems * ad[i].size * s->num_vertices;
        size += s->ad[i].size;
    }

    if (use_vbo) {
        glGenBuffers (1, &s->bufid);
        glBindBuffer (GL_ARRAY_BUFFER, s->bufid);
        glBufferData (GL_ARRAY_BUFFER, size, NULL, GL_STATIC_DRAW);
        map = glMapBuffer (GL_ARRAY_BUFFER, GL_WRITE_ONLY);
        if (!map) caml_failwith ("glMapBuffer failed");
    }
    else {
        map = stat_alloc (size);
    }

    p = map + s->ad[V_IDX].offset;
    for (i = 0; i < s->num_vertices; ++i) {
        ((float *) p)[0] = Double_field (vertexa_v, i*3 + 0);
        ((float *) p)[1] = Double_field (vertexa_v, i*3 + 1);
        ((float *) p)[2] = Double_field (vertexa_v, i*3 + 2);
        p += strides[V_IDX];
    }

    p = map + s->ad[N_IDX].offset;
    for (i = 0; i < s->num_vertices; ++i) {
        ((float *) p)[0] = Double_field (normala_v, i*3 + 0);
        ((float *) p)[1] = Double_field (normala_v, i*3 + 1);
        ((float *) p)[2] = Double_field (normala_v, i*3 + 2);
        p += strides[N_IDX];
    }

    p = map + s->ad[UV_IDX].offset;
    for (i = 0; i < s->num_vertices; ++i) {
        ((float *) p)[0] = Double_field (uva_v, i*2 + 0);
        ((float *) p)[1] = Double_field (uva_v, i*2 + 1);
        p += strides[UV_IDX];
    }

    p = map + s->ad[C_IDX].offset;

    for (i = 0; i < s->num_vertices; ++i) {
        memcpy (p, String_val (colors_v) + i*4, 4);
        p += strides[C_IDX];
    }

    s->skin = skin = stat_alloc (s->num_vertices * sizeof (struct skin));
    p = map + s->ad[A_IDX].offset;

    for (i = 0; i < s->num_vertices; ++i) {
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
            ((float *) p)[j] = skin[i].boneindices[j] * 3 + w;
        }

        for (j = skin[i].num_bones; j < 3; ++j) {
            ((float *) p)[j] = 0.0;
        }
        p += strides[A_IDX];
    }

    if (use_vbo) {
        if (glUnmapBuffer (GL_ARRAY_BUFFER) == GL_FALSE)
            caml_failwith ("glUnmapBuffer failed");
    }
    else {
        s->array = map;
    }
}

static void skin_anim (State *s)
{
    int i, j = 3;
    struct bone *b = s->bones + 1;

    for (i = 0; i < s->num_bones; ++i, ++b, j += 3) {
        glProgramLocalParameter4fvARB (GL_VERTEX_PROGRAM_ARB, j + 0, b->cm + 0);
        glProgramLocalParameter4fvARB (GL_VERTEX_PROGRAM_ARB, j + 1, b->cm + 4);
        glProgramLocalParameter4fvARB (GL_VERTEX_PROGRAM_ARB, j + 2, b->cm + 8);
    }
}

static void load_program (const char *text, GLsizei size, GLuint progid)
{
    glBindProgramARB (GL_VERTEX_PROGRAM_ARB, progid);

    glProgramStringARB (GL_VERTEX_PROGRAM_ARB, GL_PROGRAM_FORMAT_ASCII_ARB,
                        size, (const GLubyte *) text);
    if (glGetError () != GL_NO_ERROR) {
        GLint pos;
        char buf[1024];

        glGetIntegerv (GL_PROGRAM_ERROR_POSITION_ARB, &pos);
        snprintf (buf, 1024, "glProgramStringARB: error %s at %d",
                  (char *) glGetString (GL_PROGRAM_ERROR_STRING_ARB), pos);
        caml_failwith (buf);
    }
    {
        float r[4];

        memset (r, 0, sizeof (r));
        glProgramLocalParameter4fvARB (GL_VERTEX_PROGRAM_ARB, 0, r);
        glProgramLocalParameter4fvARB (GL_VERTEX_PROGRAM_ARB, 0, r);
        glProgramLocalParameter4fvARB (GL_VERTEX_PROGRAM_ARB, 0, r);
        glProgramLocalParameter4fvARB (GL_VERTEX_PROGRAM_ARB, 0, r);
    }
}

CAMLprim value ml_skin_draw_begin_vp (value unit_v)
{
    int i;
    State *s = &glob_state;

    (void) unit_v;

    glBindBuffer (GL_ARRAY_BUFFER, s->bufid);

    for (i = 0; i < COUNT; ++i) {
        struct adescr *ad = &s->ad[i];

        glEnableVertexAttribArrayARB (ad->index);
        glVertexAttribPointerARB (ad->index, ad->num_elems, ad->type,
                                  ad->norm, ad->stride, s->array + ad->offset);
    }

    glEnable (GL_VERTEX_PROGRAM_ARB);
    return Val_unit;
}

CAMLprim value ml_skin_draw_end_vp (value unit_v)
{
    int i;
    State *s = &glob_state;

    (void) unit_v;

    for (i = 0; i < COUNT; ++i)
        glDisableVertexAttribArrayARB (s->ad[i].index);

    glBindBuffer (GL_ARRAY_BUFFER, 0);
    glDisable (GL_VERTEX_PROGRAM_ARB);
    return Val_unit;
}

CAMLprim value ml_skin_init_vp (value text_v, value use_vbo_v, value geom_v)
{
    CAMLparam3 (use_vbo_v, text_v, geom_v);
    CAMLlocal5 (vertexa_v, normala_v, uva_v, skin_v, colors_v);
    State *s = &glob_state;

    use_vbo = Bool_val (use_vbo_v);
#ifdef _WIN32
    GETPA (BindBuffer);
    GETPA (GenBuffers);
    GETPA (BufferData);
    GETPA (MapBuffer);
    GETPA (UnmapBuffer);
    GETPA (BindProgramARB);
    GETPA (ProgramStringARB);
    GETPA (ProgramLocalParameter4fvARB);
    GETPA (EnableVertexAttribArrayARB);
    GETPA (DisableVertexAttribArrayARB);
    GETPA (VertexAttribPointerARB);
#endif

    vertexa_v = Field (geom_v, 0);
    normala_v = Field (geom_v, 1);
    uva_v     = Field (geom_v, 2);
    skin_v    = Field (geom_v, 3);
    colors_v  = Field (geom_v, 4);

    skin_init (s, vertexa_v, normala_v, uva_v, skin_v, colors_v, 1);
    load_program (String_val (text_v), caml_string_length (text_v), 1);
    skin_anim (s);

    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_set_skel_vp (value skel_v)
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
        float v[3];
        struct bone *parent = &s->bones[b->parent];

        qapply (v, parent->mq, b->v);
        qcompose (b->mq, b->q, parent->mq);
        vadd (b->mv, v, parent->mv);

        memset (b->cm, 0, sizeof (b->cm));
        b->cm[0] = 1.0;
        b->cm[5] = 1.0;
        b->cm[10] = 1.0;
    }

    CAMLreturn (Val_unit);
}

CAMLprim value ml_skin_set_anim_vp (value anim_v)
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

CAMLprim value ml_skin_anim_vp (value unit_v)
{
    CAMLparam1 (unit_v);
    State *s = &glob_state;

    skin_anim (s);
    CAMLreturn (Val_unit);
}
