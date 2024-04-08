/*********************************************************************
    calypso_zlib_io_c.c
    fortran wrapper for zlib IO
*********************************************************************/

#include <string.h>
#include "calypso_zlib_io_c.h"

/* The following macro calls a zlib routine and checks the return
 value. If the return value ("status") is not OK, it prints an error
 message and exits the program. Zlib's error statuses are all less
 than zero. */

static gzFile malloc_gzFile(void){
	gzFile file_gz = (gzFile) malloc(sizeof(gzFile));
	if(file_gz == NULL){
		printf("malloc error for gzFile structure\n");
		exit(1);
	};
	return file_gz;
}

void * open_wt_gzfile_c(const char *gz_file_name){
	gzFile file_gz = malloc_gzFile();
	if ((file_gz = gzopen(gz_file_name, GZ_WT_MODE)) == NULL){
		fprintf(stderr, "failed to gzopen\n");
		exit(1);
	}
    return (void *) file_gz;
}

void * open_ad_gzfile_c(const char *gz_file_name){
	gzFile file_gz = malloc_gzFile();
	if ((file_gz = gzopen(gz_file_name, GZ_AD_MODE)) == NULL){
		fprintf(stderr, "failed to gzopen\n");
		exit(1);
	}
    return (void *) file_gz;
}
void * open_rd_gzfile_c(const char *gz_file_name){
	gzFile file_gz = malloc_gzFile();
	if ((file_gz = gzopen(gz_file_name, GZ_RD_MODE)) == NULL){
		fprintf(stderr, "failed to gzopen\n");
		exit(1);
	}
    return (void *) file_gz;
}

void close_gzfile_c(void *FP_gzip){
	int iret;
	if ((iret = gzclose((gzFile) FP_gzip)) != Z_OK){
		fprintf(stderr, "gzclose failed.\n");
		exit(1);
	}
	return;
}

int open_rd_gzfile_w_flag_c(const char *gz_file_name){
	gzFile file_gz = (gzFile) open_rd_gzfile_c(gz_file_name);
	file_gz = gzopen(gz_file_name, GZ_RD_MODE);
	if (file_gz == NULL){
		fprintf(stderr, "failed to gzopen\n");
		return 1;
	}
	return 0;
}

int rewind_gzfile_c(void *FP_gzip){
    return gzrewind((gzFile) FP_gzip);
}

int check_gzfile_eof_c(void *FP_gzip){
    return gzeof((gzFile) FP_gzip);
}

void write_compress_txt_c(void *FP_gzip, const int nchara, char *input_txt){
	int writelen, num_txt;
	
	num_txt = (int) strlen(input_txt);
	input_txt[num_txt] =   '\n';
	input_txt[num_txt+1] = '\0';
	num_txt = num_txt + 1;
	/*
	fprintf(stderr,"nchara: %d, num_txt %d,\n",
			nchara, num_txt);
	*/
	writelen = gzwrite((gzFile) FP_gzip, input_txt, num_txt);
	if (writelen != num_txt) {
		fprintf(stderr, "failed to gzwrite\n");
		exit(1);
	}
	
	memset(input_txt, '\0', nchara*sizeof(char));
	return;
}

void write_compress_txt_nolf_c(void *FP_gzip, const int nchara, char *input_txt){
	int writelen, num_txt;
	
	num_txt = (int) strlen(input_txt);
	input_txt[num_txt] = '\0';
	writelen = gzwrite((gzFile) FP_gzip, input_txt, num_txt);
	if (writelen != num_txt) {
		fprintf(stderr, "failed to gzwrite\n");
		exit(1);
	}
	
	memset(input_txt, '\0', nchara*sizeof(char));
	return;
}



static int count_linechara(int num_buffer, const char *line_buf){
	int nchara_l;
	int j;
	
	nchara_l = 0;
	for (j = 0; j < num_buffer; j++) {
		if(line_buf[j] == '\n') {
			nchara_l = j + 1;
			break;
		};
	};
	return nchara_l;
}

/*
static int find_nullpoint(int num_buffer, const char *line_buf){
	int nchara_l;
	int j;
	
	nchara_l = 0;
	for (j = 0; j < num_buffer; j++) {
		if(line_buf[j] == '\n') {
			nchara_l = j + 1;
			break;
		};
	};
	return nchara_l;
}
*/

static int count_words(int nchara_l, const char *line_buf){
	int num_word;
	int j;
	
	num_word = 0;
	if(line_buf[0] != ' ') num_word = 1;
	for (j = 1; j < nchara_l-1; j++) {
		if(line_buf[j-1] == ' ' && line_buf[j] != ' ') num_word = num_word + 1;
	};
	if(nchara_l == 0) num_word = -1;
	
	return num_word;
}


static void get_one_line_by_zlib(void *FP_gzip, const int num_buffer, int *num_word,
								 int *nchara, char *line_buf){
	*nchara = 0;
	gzgets(((gzFile) FP_gzip), line_buf, num_buffer);
	
	*nchara = count_linechara(num_buffer, line_buf);
	*num_word = count_words(*nchara, line_buf);
	
	/*
	fprintf(stderr,"num_buffer: %d, nchar_line %d, num_word %d\n",
			num_buffer, *nchara, *num_word);
	*/
	
	if(*num_word == -1){
		fprintf(stderr, "increase text buffer size!!\n");
		fprintf(stderr, "%s \n",line_buf);
	}
	return;
}

int gzseek_go_fwd_c(void *FP_gzip, const int ioffset){
    z_off_t ierr_z;
    ierr_z = gzseek((gzFile) FP_gzip, (z_off_t) ioffset, SEEK_CUR);
    return (int)ierr_z;
}

int gztell_c(void *FP_gzip){
    z_off_t ierr_z = gztell((gzFile) FP_gzip);
    return (int) ierr_z;
}

int gzoffset_c(void *FP_gzip){
    z_off_t ierr_z = gzoffset((gzFile) FP_gzip);
    return (int) ierr_z;
}




int gzread_32bit_c(void *FP_gzip, const int iflag_swap, const int ilength,
                   char *textbuf){
    int ierr = gzread((gzFile) FP_gzip, textbuf, (unsigned int) ilength);
    ierr = ierr - ilength;
    if(iflag_swap == IFLAG_ON) {byte_swap_4((unsigned long) ilength, textbuf);};
    return ierr;
}

int gzread_64bit_c(void *FP_gzip, const int iflag_swap, const int ilength,
                   char *textbuf){
    int ierr =  gzread((gzFile) FP_gzip, textbuf, (unsigned int) ilength);
    ierr = ierr - ilength;
    if(iflag_swap == IFLAG_ON) {byte_swap_8((unsigned long) ilength, textbuf);};
    return ierr;
}

int gzwrite_c(void *FP_gzip, const int ilength, void *buf){
    int ierr = gzwrite((gzFile) FP_gzip, buf, (unsigned int) ilength);
    return (ierr - ilength);
}

void get_one_line_from_gz_c(void *FP_gzip, const int num_buffer,
							int *num_word, int *nchara, char *line_buf){
	get_one_line_by_zlib(FP_gzip, num_buffer, num_word, nchara, line_buf);
	line_buf[*nchara-1] = ' ';
	line_buf[*nchara  ] = '\n';
	return;
}

int skip_comment_gz_c(void *FP_gzip, const int num_buffer, char *buf){
	int nchara = 0, num_word = 0, icou = 0;
	
    get_one_line_from_gz_c(FP_gzip, num_buffer, &num_word, &nchara, buf);
	while ((nchara <= 1) || (buf[0] == '!') || (buf[0] == '#') || (buf[0] == '\n')) {
        get_one_line_from_gz_c(FP_gzip, num_buffer, &num_word, &nchara, buf);
		icou = icou + 1;
	};
	return num_word;
};
