
/*  calypso_param_c.h */

#ifndef KEMOSRC_PARAM_C_
#define KEMOSRC_PARAM_C_

#define KCHARA_C 256

#ifdef FC_NAME_LOWER_USCORE
  #define open_wt_rawfile           open_wt_rawfile_
  #define open_ad_rawfile           open_ad_rawfile_
  #define open_rd_rawfile           open_rd_rawfile_
  #define close_rawfile             close_rawfile_
  #define rawread_f                 rawread_f_
  #define rawwrite_f                rawwrite_f_
  #define rawseek_go_fwd_f          rawseek_go_fwd_f_

  #define open_wt_gzfile            open_wt_gzfile_
  #define open_ad_gzfile            open_ad_gzfile_
  #define open_rd_gzfile            open_rd_gzfile_
  #define close_gzfile              close_gzfile_
  #define check_gzfile_eof          check_gzfile_eof_
  #define write_compress_txt        write_compress_txt_
  #define write_compress_txt_nolf   write_compress_txt_nolf_
  #define gzseek_go_fwd_f           gzseek_go_fwd_f_
  #define gzread_f                  gzread_f_
  #define gzwrite_f                 gzwrite_f_
  #define get_one_line_from_gz      get_one_line_from_gz_
  #define compress_file             compress_file_
  #define decompress_file           decompress_file_
  #define gzip_defleat_once         gzip_defleat_once_
  #define gzip_defleat_begin        gzip_defleat_begin_
  #define gzip_defleat_cont         gzip_defleat_cont_
  #define gzip_defleat_last         gzip_defleat_last_
  #define gzip_infleat_once         gzip_infleat_once_
  #define gzip_infleat_begin        gzip_infleat_begin_
  #define gzip_infleat_cont         gzip_infleat_cont_
  #define gzip_infleat_last         gzip_infleat_last_

  #define write_png_rgba_c          write_png_rgba_c_
  #define write_png_rgb_c           write_png_rgb_c_

  #define kemo_fftw_plan_dft_r2c_1d    kemo_fftw_plan_dft_r2c_1d_
  #define kemo_fftw_plan_dft_c2r_1d    kemo_fftw_plan_dft_c2r_1d_
  #define kemo_fftw_plan_many_dft_r2c  kemo_fftw_plan_many_dft_r2c_
  #define kemo_fftw_plan_many_dft_c2r  kemo_fftw_plan_many_dft_c2r_
  #define kemo_fftw_destroy_plan       kemo_fftw_destroy_plan_
  #define kemo_fftw_cleanup            kemo_fftw_cleanup_
  #define kemo_fftw_execute            kemo_fftw_execute_
  #define kemo_fftw_execute_dft_r2c    kemo_fftw_execute_dft_r2c_
  #define kemo_fftw_execute_dft_c2r    kemo_fftw_execute_dft_c2r_

#elif FC_NAME_UPPER
  #define open_wt_rawfile           OPEN_WT_RAWFILE
  #define open_ad_rawfile           OPEN_AD_RAWFILE
  #define open_rd_rawfile           OPEN_RD_RAWFILE
  #define close_rawfile             CLOSE_RAWFILE
  #define rawread_f                 RAWREAD_F
  #define rawwrite_f                RAWWRITE_F
  #define rawseek_go_fwd_f          RAWSEEK_GO_FWD_F

  #define open_wt_gzfile            OPEN_WT_GZFILE
  #define open_ad_gzfile            OPEN_AD_GZFILE
  #define open_rd_gzfile            OPEN_RD_GZFILE
  #define close_gzfile              CLOSE_GZFILE
  #define check_gzfile_eof          CHECK_GZFILE_EOF
  #define write_compress_txt        WRITE_COMPRESS_TXT
  #define write_compress_txt_nolf   WRITE_COMPRESS_TXT_NOLF
  #define gzseek_go_fwd_f           GZSEEK_GO_FWD_F
  #define gzread_f                  GZREAD_F
  #define gzwrite_f                 GZWRITE_F
  #define get_one_line_from_gz      GET_ONE_LINE_FROM_GZ
  #define compress_file             COMPRESS_FILE
  #define decompress_file           DECOMPRESS_FILE
  #define gzip_defleat_once         GZIP_DEFLEAT_ONCE
  #define gzip_defleat_begin        GZIP_DEFLEAT_BEGIN
  #define gzip_defleat_cont         GZIP_DEFLEAT_CONT
  #define gzip_defleat_last         GZIP_DEFLEAT_LAST
  #define gzip_infleat_once         GZIP_INFLEAT_ONCE
  #define gzip_infleat_begin        GZIP_INFLEAT_BEGIN
  #define gzip_infleat_cont         GZIP_INFLEAT_CONT
  #define gzip_infleat_last         GZIP_INFLEAT_LAST

  #define write_png_rgba_c          WRITE_PNG_RGBA_C
  #define write_png_rgb_c           WRITE_PNG_RGB_C

  #define kemo_fftw_plan_dft_r2c_1d    KEMO_FFTW_PLAN_DFT_R2C_1D
  #define kemo_fftw_plan_dft_c2r_1d    KEMO_FFTW_PLAN_DFT_C2R_1D
  #define kemo_fftw_plan_many_dft_r2c  KEMO_FFTW_PLAN_MANY_DFT_R2C
  #define kemo_fftw_plan_many_dft_c2r  KEMO_FFTW_PLAN_MANY_DFT_C2R
  #define kemo_fftw_destroy_plan       KEMO_FFTW_DESTROY_PLAN
  #define kemo_fftw_cleanup            KEMO_FFTW_CLEANUP
  #define kemo_fftw_execute            KEMO_FFTW_EXECUTE
  #define kemo_fftw_execute_dft_r2c    KEMO_FFTW_EXECUTE_DFT_R2C
  #define kemo_fftw_execute_dft_c2r    KEMO_FFTW_EXECUTE_DFT_C2R

#elif FC_NAME_UPPER_STDCALL
  #define open_wt_rawfile           OPEN_WT_RAWFILE
  #define open_ad_rawfile           OPEN_AD_RAWFILE
  #define open_rd_rawfile           OPEN_RD_RAWFILE
  #define close_rawfile             CLOSE_RAWFILE
  #define rawread_f                 RAWREAD_F
  #define rawwrite_f                RAWWRITE_F
  #define rawseek_go_fwd_f          RAWSEEK_GO_FWD_F

  #define open_wt_gzfile            OPEN_WT_GZFILE
  #define open_ad_gzfile            OPEN_AD_GZFILE
  #define open_rd_gzfile            OPEN_RD_GZFILE
  #define close_gzfile              CLOSE_GZFILE
  #define check_gzfile_eof          CHECK_GZFILE_EOF
  #define write_compress_txt        WRITE_COMPRESS_TXT
  #define write_compress_txt_nolf   WRITE_COMPRESS_TXT_NOLF
  #define gzseek_go_fwd_f           GZSEEK_GO_FWD_F
  #define gzread_f                  GZREAD_F
  #define gzwrite_f                 GZWRITE_F
  #define get_one_line_from_gz      GET_ONE_LINE_FROM_GZ
  #define compress_file             COMPRESS_FILE
  #define decompress_file           DECOMPRESS_FILE
  #define gzip_defleat_once         GZIP_DEFLEAT_ONCE
  #define gzip_defleat_begin        GZIP_DEFLEAT_BEGIN
  #define gzip_defleat_cont         GZIP_DEFLEAT_CONT
  #define gzip_defleat_last         GZIP_DEFLEAT_LAST
  #define gzip_infleat_once         GZIP_INFLEAT_ONCE
  #define gzip_infleat_begin        GZIP_INFLEAT_BEGIN
  #define gzip_infleat_cont         GZIP_INFLEAT_CONT
  #define gzip_infleat_last         GZIP_INFLEAT_LAST

  #define write_png_rgba_c          WRITE_PNG_RGBA_C
  #define write_png_rgb_c           WRITE_PNG_RGB_C

  #define kemo_fftw_plan_dft_r2c_1d    KEMO_FFTW_PLAN_DFT_R2C_1D
  #define kemo_fftw_plan_dft_c2r_1d    KEMO_FFTW_PLAN_DFT_C2R_1D
  #define kemo_fftw_plan_many_dft_r2c  KEMO_FFTW_PLAN_MANY_DFT_R2C
  #define kemo_fftw_plan_many_dft_c2r  KEMO_FFTW_PLAN_MANY_DFT_C2R
  #define kemo_fftw_destroy_plan       KEMO_FFTW_DESTROY_PLAN
  #define kemo_fftw_cleanup            KEMO_FFTW_CLEANUP
  #define kemo_fftw_execute            KEMO_FFTW_EXECUTE
  #define kemo_fftw_execute_dft_r2c    KEMO_FFTW_EXECUTE_DFT_R2C
  #define kemo_fftw_execute_dft_c2r    KEMO_FFTW_EXECUTE_DFT_C2R

#else
  #define open_wt_rawfile           open_wt_rawfile
  #define open_ad_rawfile           open_ad_rawfile
  #define open_rd_rawfile           open_rd_rawfile
  #define close_rawfile             close_rawfile
  #define rawread_f                 rawread_f
  #define rawwrite_f                rawwrite_f
  #define rawseek_go_fwd_f          rawseek_go_fwd_f

  #define open_wt_gzfile            open_wt_gzfile
  #define open_ad_gzfile            open_ad_gzfile
  #define open_rd_gzfile            open_rd_gzfile
  #define close_gzfile              close_gzfile
  #define check_gzfile_eof          check_gzfile_eof
  #define write_compress_txt        write_compress_txt
  #define write_compress_txt_nolf   write_compress_txt_nolf
  #define gzseek_go_fwd_f           gzseek_go_fwd_f
  #define gzread_f                  gzread_f
  #define gzwrite_f                 gzwrite_f
  #define get_one_line_from_gz      get_one_line_from_gz
  #define compress_file             compress_file
  #define decompress_file           decompress_file
  #define gzip_defleat_once         gzip_defleat_once
  #define gzip_defleat_begin        gzip_defleat_begin
  #define gzip_defleat_cont         gzip_defleat_cont
  #define gzip_defleat_last         gzip_defleat_last
  #define gzip_infleat_once         gzip_infleat_once
  #define gzip_infleat_begin        gzip_infleat_begin
  #define gzip_infleat_cont         gzip_infleat_cont
  #define gzip_infleat_last         gzip_infleat_last

  #define write_png_rgba_c          write_png_rgba_c
  #define write_png_rgb_c           write_png_rgb_c

  #define kemo_fftw_plan_dft_r2c_1d    kemo_fftw_plan_dft_r2c_1d
  #define kemo_fftw_plan_dft_c2r_1d    kemo_fftw_plan_dft_c2r_1d
  #define kemo_fftw_plan_many_dft_r2c  kemo_fftw_plan_many_dft_r2c
  #define kemo_fftw_plan_many_dft_c2r  kemo_fftw_plan_many_dft_c2r
  #define kemo_fftw_destroy_plan       kemo_fftw_destroy_plan
  #define kemo_fftw_cleanup            kemo_fftw_cleanup
  #define kemo_fftw_execute            kemo_fftw_execute
  #define kemo_fftw_execute_dft_r2c    kemo_fftw_execute_dft_r2c
  #define kemo_fftw_execute_dft_c2r    kemo_fftw_execute_dft_c2r
#endif

/* constants */

#define ZERO  0.0
#define ONE   1.0
#define TWO   2.0
#define THREE 3.0
#define FOUR  4.0
#define FIVE  5.0
#define NINE  9.0
#define TEN   10.0
#define TWENTY_FOUR   24.0
#define FOURTY        40.0
#define KILO        1000.0

#define QUATEN     2.5
#define HALF       0.5
#define DECI       0.1
#define TWO_DECI   0.2
#define THREE_DECI 0.3
#define FOUR_DECI  0.4
#define SIX_DECI   0.6
#define SEVEN_DECI 0.7
#define NINE_DECI  0.9
#define CENT       0.01
#define TWO_CENT   0.02
#define TWO_MILI   0.002

#define PERP_DEG   90.0
#define SIXTY_DEG  60.0
#define OPP_DEG   180.0

#define IZERO  0
#define IONE   1
#define ITWO   2
#define ITHREE 3
#define IFOUR  4
#define ISIX   6

#define LENGTHBUF 4096     /* length of text buffer */

#define INBUFSIZ   65536        /*  buffer size for original data (arbitraly) */
#define OUTBUFSIZ  65536         /* buffer size for output data（arbitraly） */
#define EPSILON  1.e-9

#define IFLAG_SWAP  1

#endif
