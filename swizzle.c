#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>

/* Following is just a rehash of the code in here:
   http://playstation2-linux.com/download/p2lsd/sparkys_swizzle_code.html
*/
static void unswizzle_32_to_8 (void *dst, unsigned char *src, void *pal, int w, int h)
{
    int x, y;
    int width = w;
    int height = h;
    uint32 *rgba = dst;
    uint32 *palD = pal;

    for (y = 0; y < height; ++y) {
        for (x = 0; x < w; ++x ) {
            int block_location = (y&(~0xf))*width + (x&(~0xf))*2;
            int swap_selector = (((y+2)>>2)&0x1)*4;
            int posY = (((y&(~3))>>1) + (y&1))&0x7;
            int column_location = posY*width*2 + ((x+swap_selector)&0x7)*4;
            int byte_num = ((y>>1)&1) + ((x>>2)&2);
            *rgba++ = palD[src[block_location + column_location + byte_num]];
        }
    }
}

static void unswizzle_32_to_4 (void *dst, unsigned char *src, void *pal, int w, int h)
{
    int x, y;
    int width = w;
    int height = h;
    uint32 *rgba = dst;
    uint32 *palD = pal;

    /* what follows is a gross hack
       on top of that it doesn't work all that good with (at the very least):
       wanda_face2_LV0_mip.nto at any mipmap level but 0
     */
    if (w != h) height <<= 1;

    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            int pageX = x & (~0x7f);
            int pageY = y & (~0x7f);

            int pages_horz = (width+127)/128;
            int pages_vert = (height+127)/128;

            int page_number = (pageY/128)*pages_horz + (pageX/128);

            int page32Y = (page_number/pages_vert)*32;
            int page32X = (page_number%pages_vert)*64;

            int page_location = page32Y*height*2 + page32X*4;

            int locX = x & 0x7f;
            int locY = y & 0x7f;

            int block_location = ((locX&(~0x1f))>>1)*height + (locY&(~0xf))*2;
            int swap_selector = (((y+2)>>2)&0x1)*4;
            int posY = (((y&(~3))>>1) + (y&1))&0x7;

            int column_location = posY*height*2 + ((x+swap_selector)&0x7)*4;

            int byte_num = (x>>3)&3; /* 0,1,2,3 */
            int bits_set = (y>>1)&1; /* 0,1            (lower/upper 4 bits) */

            unsigned char v =
                src[page_location + block_location + column_location + byte_num];

            /* bits_set = !bits_set; */
            v = (v >> (bits_set * 4)) & 0x0f;
            *rgba++ = palD[v];
        }
    }
}

CAMLprim value ml_to_rgba (value data_v, value positions_v,
                           value dim_v, value type_v)
{
    CAMLparam4 (data_v, positions_v, dim_v, type_v);
    CAMLlocal1 (rgba_v);
    uint32 *rgba;
    char *data = String_val (data_v);
    int pix_pos = Int_val (Field (positions_v, 0));
    int pal_pos = Int_val (Field (positions_v, 1));
    int w = Int_val (Field (dim_v, 0));
    int h = Int_val (Field (dim_v, 1));
    int size = w * h;
    int i;
    uint32 *pal = (uint32 *) (data + pal_pos);
    unsigned char *pix = (unsigned char *) (data + pix_pos);

    rgba_v = caml_alloc_string (size << 2);
    rgba = (uint32 *) String_val (rgba_v);

    switch (Int_val (type_v)) {
        case 0:
            for (i = 0; i < size; ++i)
                rgba[i] = pal[pix[i]];
            break;

        case 1:
            for (i = 0; i < (size >> 1); ++i) {
                unsigned char v = pix[i];
                rgba[i*2+1] = pal[v >> 4];
                rgba[i*2] = pal[v & 0x0f];
            }
            break;

        case 2:
            unswizzle_32_to_8 (rgba, pix, pal, w, h);
            break;

        case 3:
            unswizzle_32_to_4 (rgba, pix, pal, w, h);
            break;
    }

    CAMLreturn (rgba_v);
}
