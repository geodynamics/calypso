!>@file   m_png_file_IO.f90
!!@brief  module m_png_file_IO
!
!>@brief FORTRAN routine to PNG image IO
!!@date Programmed by H. Matsui on Sep., 2021
!!
!!@verbatim
!!      subroutine write_PNG_header_f(id_png, date_time,                &
!!     &                              n_rgb, npix_x, npix_y)
!!        integer(kind = 4), intent(in) :: id_png
!!        integer(kind = 4), intent(in) :: date_time(8)
!!        integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
!!      subroutine read_PNG_header_f(id_png, flag_endian,               &
!!     &                             n_rgb, npix_x, npix_y)
!!        integer(kind = 4), intent(in) :: id_png
!!        logical, intent(inout) :: flag_endian
!!        integer(kind = 4), intent(inout) :: n_rgb, npix_x, npix_y
!!@endverbatim
!
      module m_png_file_IO
!
      use m_precision
!
      implicit none
!
!>        PNG header '\x89PNG\r\n\x1a\n'
      character(len=1), parameter, private :: PNG_HEADER(8)             &
     &                    = (/char(137), 'P', 'N', 'G',                 &
     &                        char(13), char(10), char(26), char(10)/)
!
      integer(kind = 4), parameter, private :: len_ihdr = 13
      character(len=4), parameter, private :: IHDR_HEADER = 'IHDR'
!
      character(len = 1), parameter :: BIT_DEPTH =           char(8)
!
      character(len = 1), parameter :: GRAY_TYPE =           char(0)
      character(len = 1), parameter :: RGB_TYPE =            char(2)
      character(len = 1), parameter :: GRAY_A_TYPE =         char(4)
      character(len = 1), parameter :: RGBA_TYPE =           char(6)
!
      character(len = 1), parameter :: COMPRESSION_METHOD =  char(0)
      character(len = 1), parameter :: FILTER_METHOD =       char(0)
      character(len = 1), parameter :: INTERLACE_METHOD =    char(0)
      private :: BIT_DEPTH, COMPRESSION_METHOD
      private :: GRAY_TYPE, RGB_TYPE, GRAY_A_TYPE, RGBA_TYPE
      private :: FILTER_METHOD, INTERLACE_METHOD
!
      integer(kind = 4), parameter :: len_gama = 4
      character(len=4), parameter :: GAMMA_HEADER = 'gAMA'
      real(kind = 8), parameter :: gamma = 1.0 / 2.2d0
      integer(kind = 4), parameter :: int_gamma = int(gamma * 100000)
      private :: len_gama, GAMMA_HEADER, gamma, int_gamma
!
      integer(kind = 4), parameter, private :: len_time = 7
      character(len=4), parameter, private :: TIME_HEADER = 'tIME'
!
      character(len=4), parameter, private :: TEXT_HEADER = 'tEXt'
      character(len = 22), parameter, private                           &
     &    :: SOFTWARE = "Software"//char(0)//"Calypso_viz"
!
      character(len=4), parameter :: iDAT_HEADER = 'IDAT'
      character(len = 4), parameter :: iEND_HEADER = "IEND"
!
!------------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------------
!
      subroutine write_PNG_header_f(id_png, date_time,                  &
     &                              n_rgb, npix_x, npix_y)
!
      use number_to_bit
!
      integer(kind = 4), intent(in) :: id_png
      integer(kind = 4), intent(in) :: date_time(8)
      integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
!
      integer(kind = 4) :: i_crc
      character(len = 1) :: COLOR_TYPE
!
!
      write(id_png) PNG_HEADER(1:8)
!
      i_crc = 0
      write(id_png) num2bit4_big(len_ihdr)
      write(id_png) IHDR_HEADER
      call crc32_4_png(len(IHDR_HEADER), IHDR_HEADER, i_crc)
      write(id_png) num2bit4_big(npix_x)
      call crc32_4_png(4, num2bit4_big(npix_x), i_crc)
      write(id_png) num2bit4_big(npix_y)
      call crc32_4_png(4, num2bit4_big(npix_y), i_crc)
      write(id_png) BIT_DEPTH
      call crc32_4_png(len(BIT_DEPTH), BIT_DEPTH, i_crc)
!
      if(n_rgb .eq. 4) then
        COLOR_TYPE = RGBA_TYPE
      else if(n_rgb .eq. 2) then
        COLOR_TYPE = GRAY_A_TYPE
      else if(n_rgb .eq. 1) then
        COLOR_TYPE = GRAY_TYPE
      else
        COLOR_TYPE = RGB_TYPE
      end if
!
      write(id_png) COLOR_TYPE
      call crc32_4_png(len(COLOR_TYPE), COLOR_TYPE, i_crc)
      write(id_png) COMPRESSION_METHOD
      call crc32_4_png(len(COMPRESSION_METHOD),                         &
     &                 COMPRESSION_METHOD, i_crc)
      write(id_png) FILTER_METHOD
      call crc32_4_png(len(FILTER_METHOD), FILTER_METHOD, i_crc)
      write(id_png) INTERLACE_METHOD
      call crc32_4_png(len(INTERLACE_METHOD), INTERLACE_METHOD, i_crc)
      write(id_png) num2bit4_big(i_crc)
!
      i_crc = 0
      write(id_png) num2bit4_big(len_gama)
      write(id_png) GAMMA_HEADER
      call crc32_4_png(len(GAMMA_HEADER), GAMMA_HEADER, i_crc)
      write(id_png) num2bit4_big(int_gamma)
      call crc32_4_png(len(num2bit4_big(int_gamma)),                    &
     &                 num2bit4_big(int_gamma), i_crc)
      write(id_png) num2bit4_big(i_crc)
!
!
      i_crc = 0
      write(id_png) num2bit4_big(len_time)
      write(id_png) TIME_HEADER
      call crc32_4_png(len(TIME_HEADER), TIME_HEADER, i_crc)
      write(id_png) num2bit2_big(date_time(1))
      call crc32_4_png(len(num2bit2_big(date_time(1))),                 &
     &    num2bit2_big(date_time(1)), i_crc)
      write(id_png) char(date_time(2))
      call crc32_4_png(1, char(date_time(2)), i_crc)
      write(id_png) char(date_time(3))
      call crc32_4_png(1, char(date_time(3)), i_crc)
      write(id_png) char(date_time(5))
      call crc32_4_png(1, char(date_time(5)), i_crc)
      write(id_png) char(date_time(6))
      call crc32_4_png(1, char(date_time(6)), i_crc)
      write(id_png) char(date_time(7))
      call crc32_4_png(1, char(date_time(7)), i_crc)
      write(id_png) num2bit4_big(i_crc)
!
      i_crc = 0
      write(id_png) num2bit4_big(len(SOFTWARE))
      write(id_png) TEXT_HEADER
      call crc32_4_png(len(TEXT_HEADER), TEXT_HEADER, i_crc)
      write(id_png) SOFTWARE
      call crc32_4_png(len(SOFTWARE), SOFTWARE, i_crc)
      write(id_png) num2bit4_big(i_crc)
!
      end subroutine write_PNG_header_f
!
!------------------------------------------------------------------------
!
      subroutine read_PNG_header_f(id_png, flag_endian,                 &
     &                             n_rgb, npix_x, npix_y)
!
      use number_to_bit
!
      use transfer_to_long_integers
      use byte_swap_f
!
      integer(kind = 4), intent(in) :: id_png
!
      logical, intent(inout) :: flag_endian
      integer(kind = 4), intent(inout) :: n_rgb, npix_x, npix_y
!
      character(len = 32767) :: readbuf
      character(len = 1) :: cmp_char
      integer(kind = 4) :: int_read(1)
      integer(kind = 4) :: i_crc
      integer(kind = 4) :: iy, ilength
      integer(kind = 4) :: i_year
!
!
      read(id_png) readbuf(1:8)
      do iy = 1, size(PNG_HEADER)
        cmp_char = readbuf(iy:iy)
        if(cmp_char .ne. PNG_HEADER(iy)) write(*,*) 'Fail header at ', iy
      end do
!
      i_crc = 0
      read(id_png) int_read
      if(int_read(1) .eq. len_ihdr) then
        flag_endian = .FALSE.
        write(*,*) 'Keep big endian flag correctly'
      else
        flag_endian = .TRUE.
        call byte_swap_int4_f(cast_long(1), int_read)
        if((int_read(1) .eq. len_ihdr) .and. flag_endian)               &
     &       write(*,*) 'Flip endian flag correctly'
      end if
!
      i_crc = 0
      read(id_png) readbuf(1:4)
      call crc32_4_png(len(readbuf(1:4)), readbuf(1:4), i_crc)
      do iy = 1, len(IHDR_HEADER)
        if(readbuf(iy:iy) .ne. IHDR_HEADER(iy:iy))                      &
     &                        write(*,*) 'Fail at ', iy
      end do
!
!       Read number of pixels
      read(id_png) int_read(1:1)
      if(flag_endian) call byte_swap_int4_f(cast_long(1), int_read)
      call crc32_4_png(len(num2bit4_big(int_read(1))),                  &
     &                 num2bit4_big(int_read(1)), i_crc)
      npix_x = int_read(1)
!
      read(id_png) int_read(1:1)
      if(flag_endian) call byte_swap_int4_f(cast_long(1), int_read)
      call crc32_4_png(len(num2bit4_big(int_read(1))),                  &
     &                 num2bit4_big(int_read(1)), i_crc)
      npix_y = int_read(1)
!
!       Read Image parameters
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      if(readbuf(1:1) .ne. BIT_DEPTH) write(*,*) 'Fail at BIT_DEPTH'
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
!
      if(readbuf(1:1) .eq. RGB_TYPE) then
        n_rgb = 3
      else if(readbuf(1:1) .eq. RGBA_TYPE) then
        n_rgb = 4
      else if(readbuf(1:1) .eq. GRAY_A_TYPE) then
        n_rgb = 2
      else if(readbuf(1:1) .eq. GRAY_TYPE) then
        n_rgb = 1
      else
        write(*,*) 'Fail at COLOR_TYPE'
      end if
!
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      if(readbuf(1:1) .ne. COMPRESSION_METHOD)                          &
     &                write(*,*) 'Fail at COMPRESSION_METHOD'
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      if(readbuf(1:1) .ne. FILTER_METHOD)                               &
     &                write(*,*) 'Fail at FILTER_METHOD'
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      if(readbuf(1:1) .ne. INTERLACE_METHOD)                            &
     &                write(*,*) 'Fail at INTERLACE_METHOD'
!
      read(id_png) int_read(1:1)
      if(flag_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
!      write(*,*) 'crc32', int_read(1), i_crc
!
!       Read Gamma
      i_crc = 0
      read(id_png) int_read(1:1)
      if(flag_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. len_gama) write(*,*) 'Fail at len_gama'
      read(id_png) readbuf(1:4)
      call crc32_4_png(len(readbuf(1:4)), readbuf(1:4), i_crc)
      do iy = 1, len(GAMMA_HEADER)
        if(readbuf(iy:iy) .ne. GAMMA_HEADER(iy:iy)) write(*,*)          &
     &                                            'Fail at ', iy
      end do
!
      read(id_png) int_read(1:1)
      if(flag_endian) call byte_swap_int4_f(cast_long(1), int_read(1))
      call crc32_4_png(len(num2bit4_big(int_read(1))),                  &
     &                 num2bit4_big(int_read(1)), i_crc)
      if(int_read(1) .ne. int_gamma) write(*,*) 'Fail at int_gamma'
      read(id_png) int_read(1:1)
      if(flag_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
!      write(*,*) 'crc32', int_read(1), i_crc
!
!
      i_crc = 0
      read(id_png) int_read(1:1)
      if(flag_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. len_time) write(*,*) 'Fail at len_time'
      read(id_png) readbuf(1:4)
      call crc32_4_png(len(readbuf(1:4)), readbuf(1:4), i_crc)
      do iy = 1, len(TIME_HEADER)
        if(readbuf(iy:iy) .ne. TIME_HEADER(iy:iy))                      &
     &                                       write(*,*) 'Fail at ', iy
      end do
!
      read(id_png) readbuf(1:2)
      call crc32_4_png(len(readbuf(1:2)), readbuf(1:2), i_crc)
      i_year = 256*iachar(readbuf(1:1)) + iachar(readbuf(2:2))
      write(*,*) 'Year:    ', i_year
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      write(*,*) 'Month:   ', iachar(readbuf(1:1))
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      write(*,*) 'Day:     ', iachar(readbuf(1:1))
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      write(*,*) 'Hour:    ', iachar(readbuf(1:1))
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      write(*,*) 'Minuts:  ', iachar(readbuf(1:1))
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      write(*,*) 'Seconds: ', iachar(readbuf(1:1))
      read(id_png) int_read(1:1)
      if(flag_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
!      write(*,*) 'crc32', int_read(1), i_crc
!
      i_crc = 0
      read(id_png) int_read(1:1)
      if(flag_endian) call byte_swap_int4_f(cast_long(1), int_read)
      ilength = int_read(1)
      read(id_png) readbuf(1:4)
      call crc32_4_png(len(readbuf(1:4)), readbuf(1:4), i_crc)
      do iy = 1, len(TEXT_HEADER)
        if(readbuf(iy:iy) .ne. TEXT_HEADER(iy:iy)) write(*,*)           &
     &                   'Fail at ', iy
      end do
      read(id_png) readbuf(1:ilength)
      call crc32_4_png(len(readbuf(1:ilength)), readbuf(1:ilength),     &
     &                 i_crc)
      write(*,*) 'Text ', readbuf(1:ilength)
      read(id_png) int_read(1:1)
      if(flag_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
!      write(*,*) 'crc32', int_read(1), i_crc
!
      end subroutine read_PNG_header_f
!
!------------------------------------------------------------------------
!
      end module m_png_file_IO
