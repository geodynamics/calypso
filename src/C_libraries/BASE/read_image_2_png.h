
/* read_image_2_png.h */

#ifndef READ_IMAGE_2_PNG_
#define READ_IMAGE_2_PNG_

#include <stdio.h>
#include <stdlib.h>

#include "write_image_2_png.h"
#include "calypso_param_c.h"

/* prototypes */

void read_png_file_c(const char *fhead, int *num_x, int *num_y, int *iflag_rgba);

void copy_rgb_from_png_c(int *num_x, int *num_y, int *iflag_rgba, unsigned char *cimage);
void copy_rgba_from_png_c(int *num_x, int *num_y, int *iflag_rgba, unsigned char *cimage);
void copy_grayscale_from_png_c(int *num_x, int *num_y, int *iflag_rgba, unsigned char *cimage);
void copy_grayalpha_from_png_c(int *num_x, int *num_y, int *iflag_rgba, unsigned char *cimage);

#endif
