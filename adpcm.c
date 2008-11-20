/* Based on:

   PSX VAG-Packer, hacked by bITmASTER@bigfoot.com

   v0.1
*/

/* Block interleave figured out by Antti Huovilainen. */

/* Usage:
   $ cc -o adpcm adpcm.c
   $ dd if=/path/to/sotc/dvd/XAD \
     | ./adpcm | sox -t raw -r 22050 -s -w -c 2 - -t ossdsp /dev/dsp
*/

#include <stdio.h>
#include <stdlib.h>

static const double f[5][2] = { { 0.0, 0.0 },
                                {  60.0 / 64.0,  0.0 },
                                {  115.0 / 64.0, -52.0 / 64.0 },
                                {  98.0 / 64.0, -55.0 / 64.0 },
                                {  122.0 / 64.0, -60.0 / 64.0 } };

int main (void)
{
    int predict_nr, shift_factor, flags;
    int i, block = 0;
    unsigned int d;
    int s;
    struct {
        double s_1;
        double s_2;
        double s_11;
        double s_21;
    } ctxs[2] = {0}, *ctx = ctxs;
    signed short int left[28*64], right[28*64];
    signed short int *chan = left;
    double samples[28];

    for (;;) {
        signed char data[16];

        if (fread (data, 16, 1, stdin) != 1) {
            perror ("fread");
            exit (EXIT_FAILURE);
        }

        if (data[1] == 7) break;

        predict_nr = data[0];
        shift_factor = predict_nr & 0xf;
        predict_nr >>= 4;

        for (i = 0; i < 28; i += 2) {
            d = data[i / 2 + 2];
            s = (d & 0xf) << 12;
            if (s & 0x8000)
                s |= 0xffff0000;
            samples[i] = (double) (s >> shift_factor);
            s = (d & 0xf0) << 8;
            if (s & 0x8000)
                s |= 0xffff0000;
            samples[i+1] = (double) (s >> shift_factor);
        }

        for (i = 0; i < 28; i++) {
            samples[i] += ctx->s_1 * f[predict_nr][0] + ctx->s_2 * f[predict_nr][1];
            ctx->s_2 = ctx->s_1;
            ctx->s_1 = samples[i];
            d = (int) (samples[i] + 0.5);
            chan[i + block*28] = d;
        }

        block++;
        if (block == 64) {
            if (chan == left) {
                chan = right;
                ctx = &ctxs[1];
            }
            else {
                chan = left;
                ctx = &ctxs[0];

                for (i = 0; i < 28*64; ++i) {
                    fputc (left[i] >> 8, stdout);
                    fputc (left[i], stdout);
                    fputc (right[i] >> 8, stdout);
                    fputc (right[i], stdout);
                }
            }
            block = 0;
        }
    }
    return 0;
}
