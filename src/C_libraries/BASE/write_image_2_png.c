
/* write_image_2_png.c */

#include "write_image_2_png.h"


unsigned char ** alloc_img_buffer_2_png_rgba(int num_x, int num_y){
	static unsigned char **image;
	int j;
	/* allocate memory image[y_pixel#][4*x_pixel#]*/
	if((image = (png_bytepp)malloc(num_y * sizeof(png_bytep))) == NULL){
		printf("malloc error for Vertical PNG image buffer \n");
		exit(0);
	};
	for (j = 0; j < num_y; j++){
		if((image[j] = (png_bytep)malloc(4*num_x * sizeof(png_byte))) == NULL){
			printf("malloc error for Horizontal PNG image buffer %d \n", j);
			exit(0);
		};
	};
	return image;
};

unsigned char ** alloc_img_buffer_2_png_rgb(int num_x, int num_y){
	static unsigned char **image;
	int j;
	/* allocate memory image[y_pixel#][3*x_pixel#]*/
	if((image = (png_bytepp)malloc(num_y * sizeof(png_bytep))) == NULL){
		printf("malloc error for Vertical PNG image buffer \n");
		exit(0);
	};
	for (j = 0; j < num_y; j++){
		if((image[j] = (png_bytep)malloc(3*num_x * sizeof(png_byte))) == NULL){
			printf("malloc error for Horizontal PNG image buffer %d \n", j);
			exit(0);
		};
	};
	return image;
};

void dealloc_img_buffer_2_png(int num_y, unsigned char **image){
	int j;
	for (j=0; j<num_y; j++) free(image[j]);
	free(image);
};

static void write_row_callback(png_structp png_ptr, png_uint_32 row, int pass)
{
	/*
	printf("\r%3d%% saved", (row * 100) / png_ptr->height);
	 */
}

void write_png_rgba(const char *file_prefix, png_uint_32 num_x, png_uint_32 num_y,
                    png_bytepp image){
	FILE		*fp;
    char file_name[LENGTHBUF];
	png_structp	png_ptr;
	png_infop	info_ptr;
	
    sprintf(file_name, "%s.png", file_prefix);
    printf("PNG file ouput: %s",file_name);
    
	void write_row_callback(png_structp png_ptr, png_uint_32 row, int pass);
	
	if ((fp = fopen(file_name, "wb")) == NULL) return;
	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (png_ptr == NULL) {
		fclose(fp);
		return;
	}
	info_ptr = png_create_info_struct(png_ptr);
	if (info_ptr == NULL) {
		png_destroy_write_struct(&png_ptr,  (png_infopp)NULL);
		fclose(fp);
		return;
	}
	if (setjmp(png_jmpbuf(png_ptr))) {
		png_destroy_write_struct(&png_ptr,  &info_ptr);
		fclose(fp);
		return;
	}
	/* send file info */
	png_init_io(png_ptr, fp);
	png_set_write_status_fn(png_ptr, write_row_callback);
	png_set_filter(png_ptr, 0, PNG_ALL_FILTERS);
	png_set_compression_level(png_ptr, Z_BEST_COMPRESSION);
	/* set IHDR chunk */
	png_set_IHDR(png_ptr, info_ptr, num_x, num_y, 8, PNG_COLOR_TYPE_RGB_ALPHA,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
	png_set_gAMA(png_ptr, info_ptr, 1.0);

	{
		time_t		gmt;
		png_time	mod_time;
		png_text	text_ptr[1];

		time(&gmt);
		png_convert_from_time_t(&mod_time, gmt);
		png_set_tIME(png_ptr, info_ptr, &mod_time);
		
		text_ptr[0].key = "Software";
		text_ptr[0].text = "Kemo's viewer";
		text_ptr[0].compression = PNG_TEXT_COMPRESSION_NONE;
		png_set_text(png_ptr, info_ptr, text_ptr, 1);
	}
	
	/* Write header */
	png_write_info(png_ptr, info_ptr);
	/* Write image data*/
	png_write_image(png_ptr, image);
	png_write_end(png_ptr, info_ptr);
	/* Clear memory */
	png_destroy_write_struct(&png_ptr, &info_ptr);
	fclose(fp);
    
    printf(" ...end \n");
	return;
}

void write_png_rgb(const char *file_prefix, png_uint_32 num_x, png_uint_32 num_y,
			png_bytepp image)
{
	FILE		*fp;
    char file_name[LENGTHBUF];
	png_structp	png_ptr;
	png_infop	info_ptr;
	
    sprintf(file_name, "%s.png", file_prefix);
    printf("PNG file ouput: %s",file_name);
	
	void write_row_callback(png_structp png_ptr, png_uint_32 row, int pass);
	
	if ((fp = fopen(file_name, "wb")) == NULL) return;
	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (png_ptr == NULL) {
		fclose(fp);
		return;
	}
	info_ptr = png_create_info_struct(png_ptr);
	if (info_ptr == NULL) {
		png_destroy_write_struct(&png_ptr,  (png_infopp)NULL);
		fclose(fp);
		return;
	}
	if (setjmp(png_jmpbuf(png_ptr))) {
		png_destroy_write_struct(&png_ptr,  &info_ptr);
		fclose(fp);
		return;
	}
	/* send file info */
	png_init_io(png_ptr, fp);
	png_set_write_status_fn(png_ptr, write_row_callback);
	png_set_filter(png_ptr, 0, PNG_ALL_FILTERS);
	png_set_compression_level(png_ptr, Z_BEST_COMPRESSION);
	/* set IHDR chunk */
	png_set_IHDR(png_ptr, info_ptr, num_x, num_y, 8, PNG_COLOR_TYPE_RGB,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
    png_set_gamma(png_ptr, 2.2, 0.45455);

	{
		time_t		gmt;
		png_time	mod_time;
		png_text	text_ptr[1];

		time(&gmt);
		png_convert_from_time_t(&mod_time, gmt);
		png_set_tIME(png_ptr, info_ptr, &mod_time);
		
		text_ptr[0].key = "Software";
		text_ptr[0].text = "Kemo's viewer";
		text_ptr[0].compression = PNG_TEXT_COMPRESSION_NONE;
		png_set_text(png_ptr, info_ptr, text_ptr, 1);
	}
	
	/* Write header */
	png_write_info(png_ptr, info_ptr);
	/* Write image data*/
	png_write_image(png_ptr, image);
	png_write_end(png_ptr, info_ptr);
	/* Clear memory */
	png_destroy_write_struct(&png_ptr, &info_ptr);
	fclose(fp);
    
    printf(" ...end \n");
	return;
}

void write_png_rgba_c(const char *file_prefix, const int *num_x, const int *num_y,
					  const char *cimage){
	char fname[LENGTHBUF];
	unsigned char **image;
	png_uint_32 nx = (png_uint_32) *num_x;
	png_uint_32 ny = (png_uint_32) *num_y;
	int i, j, k;
	
	image = (png_bytepp)malloc(ny * sizeof(png_bytep));
	for (j = 0; j < ny; j++) image[j] = (png_bytep)malloc(4*nx * sizeof(png_byte));
	
	for (i = 0; i < nx; i++) {
		for (j = 0; j < ny; j++) {
			k = (ny-j-1)*nx + i;
			image[j][4*i  ] = (unsigned char) cimage[4*k];
			image[j][4*i+1] = (unsigned char) cimage[4*k+1];
			image[j][4*i+2] = (unsigned char) cimage[4*k+2];
			image[j][4*i+3] = (unsigned char) cimage[4*k+3];
		}
	}
	
	write_png_rgba(file_prefix, nx, ny, image);
	dealloc_img_buffer_2_png(ny, image);
	return;
}


void write_png_rgb_c(const char *file_prefix, const int *num_x, const int *num_y,
					 const unsigned char *cimage){
	char fname[LENGTHBUF];
	unsigned char **image;
	png_uint_32 nx = (png_uint_32) *num_x;
	png_uint_32 ny = (png_uint_32) *num_y;
	int i, j, k;
	
	image = (png_bytepp)malloc(ny * sizeof(png_bytep));
	for (j = 0; j < ny; j++) image[j] = (png_bytep)malloc(3*nx * sizeof(png_byte));
	
	for (i = 0; i < nx; i++) {
		for (j = 0; j < ny; j++) {
			k = (ny-j-1)*nx + i;
			image[j][3*i  ] = cimage[3*k];
			image[j][3*i+1] = cimage[3*k+1];
			image[j][3*i+2] = cimage[3*k+2];
		}
	}
	
	write_png_rgb(file_prefix, nx, ny, image);
	dealloc_img_buffer_2_png(ny, image);
	return;
}
