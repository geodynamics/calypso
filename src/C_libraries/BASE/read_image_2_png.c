
/* read_image_2_png.c */

#include "read_image_2_png.h"

#define PNG_BYTES_TO_CHECK (4)
unsigned char **bimage;

static void check_if_png(char *file_name, FILE **fp)
	{
	char    sig[PNG_BYTES_TO_CHECK];
	
	if ((*fp = fopen(file_name, "rb")) == NULL) exit(EXIT_FAILURE);
	if (fread(sig, 1, PNG_BYTES_TO_CHECK, *fp) != PNG_BYTES_TO_CHECK) {
		fclose(*fp);
		exit(EXIT_FAILURE);
	}
	if (png_sig_cmp((png_bytep) sig, IZERO, PNG_BYTES_TO_CHECK)) {
		fclose(*fp);
		exit(EXIT_FAILURE);
	}
}

static void read_png_info(FILE *fp, png_structp *png_ptr, png_infop *info_ptr)
{
	*png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (*png_ptr == NULL) {
		fclose(fp);
		exit(EXIT_FAILURE);
	}
	
	*info_ptr = png_create_info_struct(*png_ptr);
	if (*info_ptr == NULL) {
		png_destroy_read_struct(png_ptr, (png_infopp)NULL, (png_infopp)NULL);
		fclose(fp);
		exit(EXIT_FAILURE);
	}
	if (setjmp(png_jmpbuf(*png_ptr))) {
		png_destroy_read_struct(png_ptr, info_ptr, (png_infopp)NULL);
		fclose(fp);
		exit(EXIT_FAILURE);
	}
	png_init_io(*png_ptr, fp);
	png_set_sig_bytes(*png_ptr, PNG_BYTES_TO_CHECK);
	png_read_info(*png_ptr, *info_ptr);
}

static void read_png_raw_image(FILE *fp, png_structp png_ptr, png_infop info_ptr,
	png_bytepp *image, png_uint_32 *width, png_uint_32 *height, int *iflag_rgba)
{
	png_uint_32     i, j;
	
	*width = png_get_image_width(png_ptr, info_ptr);
	*height = png_get_image_height(png_ptr, info_ptr);
	
	if (png_get_color_type(png_ptr, info_ptr) == PNG_COLOR_TYPE_RGBA){
		*iflag_rgba = RGBA_COLOR;
	}
	else if (png_get_color_type(png_ptr, info_ptr) == PNG_COLOR_TYPE_RGB){
		*iflag_rgba = RGB_COLOR;
	}
    else if (png_get_color_type(png_ptr, info_ptr) == PNG_COLOR_TYPE_GRAY_ALPHA){
        *iflag_rgba = BW_ALPHA;
    }
    else if (png_get_color_type(png_ptr, info_ptr) == PNG_COLOR_TYPE_GRAY){
        *iflag_rgba = B_AND_W;
    }

	else exit(EXIT_FAILURE);
	
	
	if ((*image = (png_bytepp)malloc(*height * sizeof(png_bytep))) == NULL) {
		fclose(fp);
		exit(EXIT_FAILURE);
	}
	for (i = 0; i < *height; i++) {
		(*image)[i] = (png_bytep)malloc(png_get_rowbytes(png_ptr, info_ptr));
		if ((*image)[i] == NULL) {
			for (j = 0; j < i; j++) free((*image)[j]);
			free(*image);
			fclose(fp);
			exit(EXIT_FAILURE);
		}
	}
	png_read_image(png_ptr, *image);
}

static void read_png_image_w_gamma(FILE *fp, png_structp png_ptr, png_infop info_ptr,
			png_bytepp *image, png_uint_32 *width, png_uint_32 *height,
			int *iflag_rgba, double display_gamma)
{
	double          file_gamma;
	
	/*	display_gamma = 2.2;*/
	if (png_get_gAMA(png_ptr, info_ptr, &file_gamma)){
		png_set_gamma(png_ptr, display_gamma, file_gamma);
	} else {
		png_set_gamma(png_ptr, display_gamma, 0.50);
	};
	png_read_update_info(png_ptr, info_ptr);
	
	read_png_raw_image(fp, png_ptr, info_ptr, image, width, height, iflag_rgba);
}


void read_png_file_c(const char *fhead, int *num_x, int *num_y, int *iflag_rgba)
{
	char fname[LENGTHBUF];
	FILE            *fp;
	png_uint_32     i;
	png_uint_32 width;
	png_uint_32 height;
	double      file_gamma;
	png_structp png_ptr;
	png_infop   info_ptr;
	double	display_gamma = 1.0;
	
	
	sprintf(fname, "%s.png",fhead);
	check_if_png(fname, &fp);
	read_png_info(fp, &png_ptr, &info_ptr);
	read_png_image_w_gamma(fp, png_ptr, info_ptr, &bimage, &width, &height, 
			iflag_rgba, display_gamma);
	fclose(fp);
/*
	{
//		 Display gAMA Chunk 
		if (png_get_gAMA(png_ptr, info_ptr, &file_gamma))
		printf("gamma = %lf\n", file_gamma);
	}
*/
	{
		/* Display tEXT Chunk */
		png_textp       text_ptr;
		int             num_text;
		if (png_get_text(png_ptr, info_ptr, &text_ptr, &num_text))
		for (i = 0; i < num_text; i++)
		printf("%s = %s\n", text_ptr[i].key, text_ptr[i].text);
	}
	
	png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
	
	*num_x = (int) width;
	*num_y = (int) height;
	return;
}

void copy_rgb_from_png_c(const int num_x, const int num_y,
                         const int iflag_rgba, unsigned char *cimage)
{
	int k, j, l;
	
	if(iflag_rgba == RGBA_COLOR){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[3*k  ] = bimage[j][4*l  ];
				cimage[3*k+1] = bimage[j][4*l+1];
				cimage[3*k+2] = bimage[j][4*l+2];
			}
		};
	} else if(iflag_rgba == RGB_COLOR){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[3*k  ] = bimage[j][3*l  ];
				cimage[3*k+1] = bimage[j][3*l+1];
				cimage[3*k+2] = bimage[j][3*l+2];
			}
		};
	} else if(iflag_rgba == BW_ALPHA){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[3*k  ] = bimage[j][2*l];
				cimage[3*k+1] = bimage[j][2*l];
				cimage[3*k+2] = bimage[j][2*l];
			}
		};
	} else if(iflag_rgba == B_AND_W){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[3*k  ] = bimage[j][l];
				cimage[3*k+1] = bimage[j][l];
				cimage[3*k+2] = bimage[j][l];
			}
		};
	};
	
	free(bimage);
};

void copy_rgba_from_png_c(const int num_x, const int num_y,
                          const int iflag_rgba, unsigned char *cimage)
{
	int i, k, j, l;
	
	if(iflag_rgba == RGBA_COLOR){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[4*k  ] = bimage[j][4*l  ];
				cimage[4*k+1] = bimage[j][4*l+1];
				cimage[4*k+2] = bimage[j][4*l+2];
				cimage[4*k+3] = bimage[j][4*l+3];
			}
		};
	} else if(iflag_rgba == RGB_COLOR){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[4*k  ] = bimage[j][3*l  ];
				cimage[4*k+1] = bimage[j][3*l+1];
				cimage[4*k+2] = bimage[j][3*l+2];
				cimage[4*k+3] = (unsigned char) 255;
			}
		};
	} else if(iflag_rgba == BW_ALPHA){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[4*k  ] = bimage[j][2*l  ];
				cimage[4*k+1] = bimage[j][2*l  ];
				cimage[4*k+2] = bimage[j][2*l  ];
				cimage[4*k+3] = bimage[j][2*l+1];
			}
		};
	} else if(iflag_rgba == B_AND_W){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[4*k  ] = bimage[j][l];
				cimage[4*k+1] = bimage[j][l];
				cimage[4*k+2] = bimage[j][l];
				cimage[4*k+3] = (unsigned char) 255;
			}
		};
	};
	
	for (i = 0; i < num_y; i++) free(bimage[i]);
	free(bimage);
};

void copy_grayscale_from_png_c(const int num_x, const int num_y,
                               const int iflag_rgba, unsigned char *cimage)
{
	int k, j, l, mixed;
	
	if(iflag_rgba == RGBA_COLOR){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				mixed = ((int) bimage[j][4*l  ] + (int) bimage[j][4*l+1] + (int) bimage[j][4*l+2]) / 3;
				cimage[k  ] = (unsigned char) mixed;
			}
		};
	} else if(iflag_rgba == RGB_COLOR){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				mixed = ((int) bimage[j][3*l  ] + (int) bimage[j][3*l+1] + (int) bimage[j][3*l+2]) / 3;
				cimage[k  ] = (unsigned char) mixed;
			}
		};
	} else if(iflag_rgba == BW_ALPHA){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[k  ] = bimage[j][2*l  ];
			}
		};
	} else if(iflag_rgba == B_AND_W){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[k  ] = bimage[j][l];
			}
		};
	};
	
	free(bimage);
};

void copy_grayalpha_from_png_c(const int num_x, const int num_y,
                               const int iflag_rgba, unsigned char *cimage)
{
	int i, k, j, l, mixed;
	
	if(iflag_rgba == RGBA_COLOR){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				mixed = ((int) bimage[j][4*l  ] + (int) bimage[j][4*l+1] + (int) bimage[j][4*l+2]) / 3;
				cimage[2*k  ] = (unsigned char) mixed;
				cimage[2*k+1] = bimage[j][2*l+1];
			}
		};
	} else if(iflag_rgba == RGB_COLOR){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				mixed = ((int) bimage[j][3*l  ] + (int) bimage[j][3*l+1] + (int) bimage[j][3*l+2]) / 3;
				cimage[2*k  ] = (unsigned char) mixed;
				cimage[2*k+3] = (unsigned char) 255;
			}
		};
	} else if(iflag_rgba == BW_ALPHA){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[2*k  ] = bimage[j][2*l  ];
				cimage[2*k+1] = bimage[j][2*l+1];
			}
		};
	} else if(iflag_rgba == B_AND_W){
		for (l = 0; l < num_x; l++) {
			for (j = 0; j < num_y; j++) {
				k = (num_y-j-1) * num_x + l;
				cimage[2*k  ] = bimage[j][l];
				cimage[2*k+3] = (unsigned char) 255;
			}
		};
	};
	
	for (i = 0; i < num_y; i++) free(bimage[i]);
	free(bimage);
};
