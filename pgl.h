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
    *(PROC *) &gl##name = wglGetProcAddress ("gl" # name);              \
    if (!gl##name) {                                                    \
        fprintf (stderr, "could not get address of gl"#name"\n");       \
        exit (EXIT_FAILURE);                                            \
    }                                                                   \
    break;                                                              \
}
#endif
