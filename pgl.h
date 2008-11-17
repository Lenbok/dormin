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
#endif

#ifndef GL_ARB_vertex_program
#define GL_VERTEX_PROGRAM_ARB             0x8620
#define GL_PROGRAM_STRING_ARB             0x8628
#define GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB 0x8645
#define GL_PROGRAM_ERROR_POSITION_ARB     0x864B
#define GL_PROGRAM_ERROR_STRING_ARB       0x8874
#define GL_PROGRAM_FORMAT_ASCII_ARB       0x8875
#define GL_PROGRAM_FORMAT_ARB             0x8876
#define GL_PROGRAM_ATTRIBS_ARB            0x88AC
#define GL_MAX_PROGRAM_ATTRIBS_ARB        0x88AD
typedef void (APIENTRYP PFNGLPROGRAMSTRINGARBPROC) (GLenum target, GLenum format, GLsizei len, const GLvoid *string);
typedef void (APIENTRYP PFNGLBINDPROGRAMARBPROC) (GLenum target, GLuint program);
typedef void (APIENTRYP PFNGLPROGRAMLOCALPARAMETER4FVARBPROC) (GLenum target, GLuint index, const GLfloat *params);
typedef void (APIENTRYP PFNGLENABLEVERTEXATTRIBARRAYARBPROC) (GLuint index);
typedef void (APIENTRYP PFNGLDISABLEVERTEXATTRIBARRAYARBPROC) (GLuint index);
typedef void (APIENTRYP PFNGLVERTEXATTRIBPOINTERARBPROC) (GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid *pointer);
static PFNGLBINDPROGRAMARBPROC glBindProgramARB;
static PFNGLPROGRAMSTRINGARBPROC glProgramStringARB;
static PFNGLPROGRAMLOCALPARAMETER4FVARBPROC glProgramLocalParameter4fvARB;
static PFNGLENABLEVERTEXATTRIBARRAYARBPROC glEnableVertexAttribArrayARB;
static PFNGLENABLEVERTEXATTRIBARRAYARBPROC glDisableVertexAttribArrayARB;
static PFNGLVERTEXATTRIBPOINTERARBPROC glVertexAttribPointerARB;
#endif

#define GETPA(name) for (;;) {                                          \
    *(PROC *) &gl##name = wglGetProcAddress ("gl" # name);              \
    if (!gl##name) {                                                    \
        fprintf (stderr, "could not get address of gl"#name"\n");       \
        exit (EXIT_FAILURE);                                            \
    }                                                                   \
    break;                                                              \
}
