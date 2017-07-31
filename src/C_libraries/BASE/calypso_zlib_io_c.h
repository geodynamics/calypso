/*********************************************************************
    calypso_zlib_io_c.h
    fortran wrapper for zlib IO
*********************************************************************/

#ifndef KEMO_ZLIB_IO_C__
#define KEMO_ZLIB_IO_C__

#include <stdio.h>
#include <stdlib.h>

#include "zlib.h"               /* /usr(/local)/include/zlib.h */

#include "calypso_param_c.h"
#include "numbers_to_bin_c.h"


#define Z_DEFAULT_MEMLEVEL  8
#define GZ_WT_MODE "wb6f"
#define GZ_AD_MODE "ab6f"
#define GZ_RD_MODE "rb6f"

#define RAW_WT_MODE "wb0"
#define RAW_AD_MODE "ab0"
#define RAW_RD_MODE "rb0"

#define windowBits 15
#define GZIP_ENCODING 16
#define GZIP_AUTODETECT 16

/* prototypes */

void open_wt_rawfile(const char *file_name, int *ierr);
void open_ad_rawfile(const char *file_name, int *ierr);
void open_rd_rawfile(const char *file_name, int *ierr);
void close_rawfile();

void rawseek_go_fwd_f(int *ioffset, int *ierr);
void rawread_f(int *iflag_swap, int *ilength, char *textbuf, int *lenchara);
void rawwrite_f(int *ilength, char *textbuf, int *lenchara);

void open_wt_gzfile(const char *gz_file_name);
void open_ad_gzfile(const char *gz_file_name);
void open_rd_gzfile(const char *gz_file_name);
void close_gzfile();

int open_rd_gzfile_w_flag(const char *gz_file_name);
int check_gzfile_eof();

void write_compress_txt(int *num_buffer, char *input_txt);
void write_compress_txt_nolf(int *num_buffer, char *input_txt);

void gzseek_go_fwd_f(int *ioffset, int *ierr);
void gzread_f(int *iflag_swap, int *ilength, char *textbuf, int *ierr);
void gzwrite_f(int *ilength, char *textbuf, int *ierr);

void get_one_line_from_gz(int *num_buffer, int *num_word, int *nchara, char *line_buf);
int skip_comment_gz_c(int *num_buffer, char *buf);

void gzip_defleat_once(int *len_buf, const char *buf, int *len_gzipbuf, 
                       int *len_gzipped, char *gzipbuf);
void gzip_defleat_begin(int *len_buf, const char *buf, int *len_gzipbuf, 
                        int *len_gzipped, char *gzipbuf);
void gzip_defleat_cont(int *len_buf, const char *buf, int *len_gzipbuf, int *len_gzipped);
void gzip_defleat_last(int *len_buf, const char *buf, int *len_gzipbuf, int *len_gzipped);

void gzip_infleat_once(int *len_gzipbuf, const char *gzipbuf, int *len_buf, 
                       char *buf, int *len_gzipped);
void gzip_infleat_begin(int *len_gzipbuf, const char *gzipbuf, int *len_buf, 
                        char *buf, int *len_gzipped);
void gzip_infleat_cont(int *len_gzipbuf, int *len_buf, const char *buf, int *len_gzipped);
void gzip_infleat_last(int *len_gzipbuf, int *len_buf, const char *buf, int *len_gzipped);


void compress_file(const char *txt_file_name, const char *gz_file_name);
void decompress_file(const char *gz_file_name, const char *txt_file_name);
#endif
