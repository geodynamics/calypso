/*
********************************************************************
    calypso_zlib_access_c.c
    fortran wrapper for zlib data compression and extraction
********************************************************************
*/

#include <string.h>
#include "calypso_zlib_access_c.h"

z_stream strm_gl;

/* The following macro calls a zlib routine and checks the return
 value. If the return value ("status") is not OK, it prints an error
 message and exits the program. Zlib's error statuses are all less
 than zero. */

#define CALL_ZLIB(x) {                       \
int status;                                  \
status = x;                                  \
if (status < 0) {                            \
fprintf (stderr,                             \
"%s:%d: %s returned a bad status of %d.\n",  \
__FILE__, __LINE__, #x, status);             \
exit (EXIT_FAILURE);                         \
    }                                        \
}

static void zlib_deflate_stream_init (z_stream *strm)
{
    strm->zalloc = Z_NULL;
    strm->zfree  = Z_NULL;
    strm->opaque = Z_NULL;
    CALL_ZLIB (deflateInit (strm, Z_DEFAULT_COMPRESSION));
    return;
}
static void zlib_inflate_stream_init (z_stream *strm)
{
    strm->zalloc = Z_NULL;
    strm->zfree  = Z_NULL;
    strm->opaque = Z_NULL;
    CALL_ZLIB (inflateInit (strm));
    return;
}

static void gzip_deflate_stream_init (z_stream *strm)
{
    strm->zalloc = Z_NULL;
    strm->zfree  = Z_NULL;
    strm->opaque = Z_NULL;
    CALL_ZLIB (deflateInit2 (strm, Z_DEFAULT_COMPRESSION, Z_DEFLATED,
                             windowBits | GZIP_ENCODING, 8,
                             Z_DEFAULT_STRATEGY));
    return;
}
static void gzip_inflate_stream_init (z_stream *strm)
{
    strm->zalloc = Z_NULL;
    strm->zfree  = Z_NULL;
    strm->opaque = Z_NULL;
    CALL_ZLIB (inflateInit2 (strm, windowBits|GZIP_ENCODING|GZIP_AUTODETECT));
    return;
}

void calypso_zlib_defleat_once(const int *len_buf, const void *buf,
							   const int *len_gzipbuf, int *len_gzipped,
							   char *gzipbuf)
{
    z_stream strm;

    zlib_deflate_stream_init (& strm);
    strm.next_in = (unsigned char *) buf;
    strm.avail_in =  (uInt) *len_buf;
    strm.avail_out = (uInt) *len_gzipbuf;
    strm.next_out = (unsigned char *) gzipbuf;
    CALL_ZLIB (deflate (& strm, Z_FINISH));
    *len_gzipped = *len_gzipbuf - strm.avail_out;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped);*/
    deflateEnd (& strm);
    return;
}

void calypso_zlib_defleat_begin(const int *len_buf, const void *buf,
								const int *len_gzipbuf, int *len_gzipped, 
								char *gzipbuf)
{
    
    gzip_deflate_stream_init (& strm_gl);
    strm_gl.next_in = (unsigned char *) buf;
    strm_gl.avail_in =  (uInt) *len_buf;
    strm_gl.avail_out = (uInt) *len_gzipbuf;
    strm_gl.next_out = (unsigned char *) gzipbuf;
    CALL_ZLIB (deflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_out;
    return;
}

void calypso_gzip_defleat_once(const int *len_buf, const void *buf,
							   const int *len_gzipbuf, int *len_gzipped, char *gzipbuf)
{
    z_stream strm;

    gzip_deflate_stream_init (& strm);
    strm.next_in = (unsigned char *) buf;
    strm.avail_in =  (uInt) *len_buf;
    strm.avail_out = (uInt) *len_gzipbuf;
    strm.next_out = (unsigned char *) gzipbuf;
    CALL_ZLIB (deflate (& strm, Z_FINISH));
    *len_gzipped = *len_gzipbuf - strm.avail_out;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped);*/
    deflateEnd (& strm);
    return;
}

void calypso_gzip_defleat_begin(const int *len_buf, const void *buf,
								const int *len_gzipbuf, int *len_gzipped,
								char *gzipbuf)
{
    
    gzip_deflate_stream_init (& strm_gl);
    strm_gl.next_in = (unsigned char *) buf;
    strm_gl.avail_in =  (uInt) *len_buf;
    strm_gl.avail_out = (uInt) *len_gzipbuf;
    strm_gl.next_out = (unsigned char *) gzipbuf;
    CALL_ZLIB (deflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_out;
    return;
}

void calypso_gzip_defleat_cont(const int *len_buf, const void *buf,
							   const int *len_gzipbuf, int *len_gzipped)
{
    uInt avail_out_current;
    
    avail_out_current = strm_gl.avail_out;
    
    strm_gl.next_in = (unsigned char *) buf;
    strm_gl.avail_in =  (uInt) *len_buf;
    CALL_ZLIB (deflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_out;
    return;
}

void calypso_gzip_defleat_last(const int *len_buf, const void *buf,
							   const int *len_gzipbuf, int *len_gzipped)
{
    uInt avail_out_current;
    
    avail_out_current = strm_gl.avail_out;
    
    strm_gl.next_in = (unsigned char *) buf;
    strm_gl.avail_in =  (uInt) *len_buf;
    CALL_ZLIB (deflate (& strm_gl, Z_FINISH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_out;
/*    printf("compressed size:%d %d %d \n",*len_buf, avail_out_current, *len_gzipped);*/
    deflateEnd (& strm_gl);
    return;
}

void calypso_zlib_infleat_once(const int *len_gzipbuf, const char *gzipbuf,
							   const int *len_buf, void *buf, int *len_gzipped)
{
    z_stream strm;
    
/*    printf("pointer:%p %p %d \n", gzipbuf, buf, *len_gzipped); */
    
    zlib_inflate_stream_init (& strm);
    strm.next_in = (unsigned char *) gzipbuf;
    strm.avail_in =  (uInt) *len_gzipbuf;
    strm.avail_out = (uInt) *len_buf;
    strm.next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (& strm, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm.avail_in;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped); */
    inflateEnd (& strm);
    return;
}

void calypso_zlib_infleat_begin(const int *len_gzipbuf, const char *gzipbuf,
								const int *len_buf, void *buf, int *len_gzipped)
{
    
    zlib_inflate_stream_init (& strm_gl);
    strm_gl.next_in = (unsigned char *) gzipbuf;
    strm_gl.avail_in =  (uInt) *len_gzipbuf;
    strm_gl.avail_out = (uInt) *len_buf;
    strm_gl.next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_in;
    return;
}

void calypso_gzip_infleat_once(const int *len_gzipbuf, const char *gzipbuf,
							   const int *len_buf, void *buf, int *len_gzipped)
{
    z_stream strm;
    
    gzip_inflate_stream_init (& strm);
    strm.next_in = (unsigned char *) gzipbuf;
    strm.avail_in =  (uInt) *len_gzipbuf;
    strm.avail_out = (uInt) *len_buf;
    strm.next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (& strm, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm.avail_in;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped); */
    inflateEnd (& strm);
    return;
}

void calypso_gzip_infleat_begin(const int *len_gzipbuf, const char *gzipbuf,
								const int *len_buf, void *buf, int *len_gzipped)
{
    
    gzip_inflate_stream_init (& strm_gl);
    strm_gl.next_in = (unsigned char *) gzipbuf;
    strm_gl.avail_in =  (uInt) *len_gzipbuf;
    strm_gl.avail_out = (uInt) *len_buf;
    strm_gl.next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_in;
    return;
}

void calypso_gzip_infleat_cont(const int *len_gzipbuf, const int *len_buf, 
							   void *buf, int *len_gzipped)
{
    uInt avail_in_current;
    
    avail_in_current = strm_gl.avail_in;
    
    strm_gl.next_out = (unsigned char *) buf;
    strm_gl.avail_out =  (uInt) *len_buf;
    CALL_ZLIB (inflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_in;
    /*    printf("compressed size:%d %d %d \n",*len_buf, avail_in_current, *len_gzipped);*/
    return;
}

void calypso_gzip_infleat_last(const int *len_gzipbuf, const int *len_buf,
							   void *buf, int *len_gzipped)
{
    uInt avail_in_current;
    
    avail_in_current = strm_gl.avail_in;
    
    strm_gl.next_out = (unsigned char *) buf;
    strm_gl.avail_out =  (uInt) *len_buf;
    CALL_ZLIB (inflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_in;
    /*    printf("compressed size:%d %d %d \n",*len_buf, avail_in_current, *len_gzipped);*/
    inflateEnd (& strm_gl);
    return;
}
