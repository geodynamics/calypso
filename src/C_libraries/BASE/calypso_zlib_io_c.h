/*********************************************************************
    calypso_zlib_io_c.h
    fortran wrapper for zlib IO
*********************************************************************/

#ifndef KEMO_ZLIB_IO_C__
#define KEMO_ZLIB_IO_C__

#include <stdio.h>
#include <stdlib.h>

#ifndef DEPENDENCY_CHECK
  #include <zlib.h>               /* /usr(/local)/include/zlib.h */
#endif

#include "calypso_param_c.h"
#include "numbers_to_bin_c.h"


#define GZ_WT_MODE "wb6f"
#define GZ_AD_MODE "ab6f"
#define GZ_RD_MODE "rb6f"

/* prototypes */

void * open_wt_gzfile_c(const char *gz_file_name);
void * open_ad_gzfile_c(const char *gz_file_name);
void * open_rd_gzfile_c(const char *gz_file_name);
void close_gzfile_c(void *FP_gzip);

int open_rd_gzfile_w_flag_c(const char *gz_file_name);
int rewind_gzfile_c(void *FP_gzip);
int check_gzfile_eof_c(void *FP_gzip);

void write_compress_txt_c(void *FP_gzip, int *nchara, char *input_txt);
void write_compress_txt_nolf_c(void *FP_gzip, int *nchara, char *input_txt);

void gzseek_go_fwd_c(void *FP_gzip, int *ioffset, int *ierr);
int gztell_c(void *FP_gzip);
int gzoffset_c(void *FP_gzip);

void gzread_32bit_c(void *FP_gzip, const int *iflag_swap, int *ilength,
					char *textbuf, int *ierr);
void gzread_64bit_c(void *FP_gzip, const int *iflag_swap, int *ilength,
					char *textbuf, int *ierr);
void gzwrite_c(void *FP_gzip, int *ilength, void *buf, int *ierr);

void get_one_line_from_gz_c(void *FP_gzip, int *num_buffer, int *num_word,
							int *nchara, char *line_buf);
int skip_comment_gz_c(void *FP_gzip, int *num_buffer, char *buf);
#endif
