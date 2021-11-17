!>@file   calypso_png_file_IO.f90
!!        module calypso_png_file_IO
!!
!! @author H. Matsui
!! @date   Programmed in Oct., 2021
!!
!!
!>@brief PNG image IO
!!
!!@verbatim
!!      subroutine calypso_write_png                                    &
!!     &         (file_prefix, n_rgb, npix_x, npix_y, rgb)
!!      subroutine calypso_write_nofilter_png                           &
!!     &         (file_prefix, n_rgb, npix_x, npix_y, rgb)
!!        character(len = kchara), intent(in) :: file_prefix
!!        integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
!!        character(len = 1), intent(in) :: rgb(n_rgb*npix_x*npix_y)
!!
!!      subroutine calypso_read_png_size(file_prefix,                   &
!!     &          flag_little_endian, n_rgb, npix_x, npix_y)
!!        character(len = kchara), intent(in) :: file_prefix
!!        integer(kind = 4), intent(in) :: n_rgb
!!        integer(kind = 4), intent(inout) :: npix_x, npix_y
!!        logical, intent(inout) :: flag_little_endian
!!      subroutine calypso_read_png_data                                &
!!     &         (flag_little_endian, n_rgb, npix_x, npix_y, rgb)
!!        logical, intent(in) :: flag_little_endian
!!        integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
!!        character(len = 1), intent(inout) :: rgb(n_rgb*npix_x*npix_y)
!!@endverbatim
!!
      module calypso_png_file_IO
!
      use m_precision
      use m_png_file_IO
      use t_buffer_4_gzip
!
      implicit none
!
      integer(kind = 4), parameter, private :: id_png = 16
!
      private :: add_png_extension
!
!-----------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------------
!
      subroutine calypso_write_png                                      &
     &         (file_prefix, n_rgb, npix_x, npix_y, rgb)
!
      use gzip_defleate
      use number_to_bit
      use filtering_rgba_png_line
!
      character(len = kchara), intent(in) :: file_prefix
      integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
      character(len = 1), intent(in) :: rgb(n_rgb*npix_x*npix_y)
!
      type(buffer_4_gzip) :: zbuf
      character(len = kchara) :: file_name
      character(len = 1), allocatable :: png_rgb(:)
      integer(kind = kint) :: ntot_png_img
      integer(kind = 4) :: i_crc
      integer(kind = 4) :: date_time(8)
      character(len=10) :: dt_c(3)
!
!
      call date_and_time(dt_c(1), dt_c(2), dt_c(3), date_time)
!
      ntot_png_img = 3*(npix_x+1)*npix_y
      zbuf%ilen_gz = int(real(ntot_png_img)*1.01+24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      allocate(png_rgb(ntot_png_img))
!
      call filter_png_image(3, npix_x, npix_y, rgb, png_rgb)
!
      zbuf%ilen_gzipped = 0
      call zlib_defleat_char_once(ntot_png_img, png_rgb,                &
     &    int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      deallocate(png_rgb)
!
      file_name = add_png_extension(file_prefix)
      write(*,*) 'Write PNG image: ', trim(file_name),                  &
     &          zbuf%ilen_gzipped, 'Byte image data'
      open(id_png, file=file_name, access='STREAM')
      call write_PNG_header_f(id_png, date_time, n_rgb, npix_x, npix_y)
!
      i_crc = 0
      write(id_png) num2bit4_big(int(zbuf%ilen_gzipped))
      write(id_png) iDAT_HEADER
      call crc32_4_png(len(iDAT_HEADER), iDAT_HEADER, i_crc)
      write(id_png) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
      call crc32_4_png(int(zbuf%ilen_gzipped), zbuf%gzip_buf(1), i_crc)
      write(id_png) num2bit4_big(i_crc)
      call dealloc_zip_buffer(zbuf)
!
      i_crc = 0
      write(id_png) num2bit4_big(len(iEND_HEADER))
      write(id_png) iEND_HEADER
      call crc32_4_png(len(iEND_HEADER), iEND_HEADER, i_crc)
      write(id_png) num2bit4_big(i_crc)
      close(id_png)
!
      end subroutine calypso_write_png
!
!------------------------------------------------------------------------
!
      subroutine calypso_write_nofilter_png                             &
     &         (file_prefix, n_rgb, npix_x, npix_y, rgb)
!
      use gzip_defleate
      use number_to_bit
      use filtering_rgba_png_line
!
      character(len = kchara), intent(in) :: file_prefix
      integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
      character(len = 1), intent(in) :: rgb(n_rgb*npix_x*npix_y)
!
      type(buffer_4_gzip) :: zbuf
      character(len = kchara) :: file_name
      character(len = 1), allocatable :: png_rgb(:)
      integer(kind = kint) :: ntot_png_img
      integer(kind = 4) :: i_crc
      integer(kind = 4) :: date_time(8)
      character(len=10) :: dt_c(3)
!
!
      call date_and_time(dt_c(1), dt_c(2), dt_c(3), date_time)
!
      ntot_png_img = 3*(npix_x+1)*npix_y
      zbuf%ilen_gz = int(real(ntot_png_img)*1.01+24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      allocate(png_rgb(ntot_png_img))
!
      call no_filter_png_image(3, npix_x, npix_y, rgb, png_rgb)
!
      zbuf%ilen_gzipped = 0
      call zlib_defleat_char_once(ntot_png_img, png_rgb,                &
     &     int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      deallocate(png_rgb)
!
      file_name = add_png_extension(file_prefix)
      write(*,*) 'Write PNG image: ', trim(file_name),                  &
     &          zbuf%ilen_gzipped, 'Byte image data'
      open(id_png, file=file_name, access='STREAM')
      call write_PNG_header_f(id_png, date_time, n_rgb, npix_x, npix_y)
!
      i_crc = 0
      write(id_png) num2bit4_big(int(zbuf%ilen_gzipped))
      write(id_png) iDAT_HEADER
      call crc32_4_png(len(iDAT_HEADER), iDAT_HEADER, i_crc)
      write(id_png) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
      call crc32_4_png(int(zbuf%ilen_gzipped), zbuf%gzip_buf(1), i_crc)
      write(id_png) num2bit4_big(i_crc)
      call dealloc_zip_buffer(zbuf)
!
      i_crc = 0
      write(id_png) num2bit4_big(len(iEND_HEADER))
      write(id_png) iEND_HEADER
      call crc32_4_png(len(iEND_HEADER), iEND_HEADER, i_crc)
      write(id_png) num2bit4_big(i_crc)
      close(id_png)
!
      end subroutine calypso_write_nofilter_png
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      subroutine calypso_read_png_size(file_prefix,                     &
     &          flag_little_endian, n_rgb, npix_x, npix_y)
!
      use byte_swap_f
      use number_to_bit
      use transfer_to_long_integers
!
      character(len = kchara), intent(in) :: file_prefix
      integer(kind = 4), intent(inout) :: n_rgb, npix_x, npix_y
      logical, intent(inout) :: flag_little_endian
!
      character(len = kchara) :: file_name
!
!
      file_name = add_png_extension(file_prefix)
      write(*,*) 'Read PNG image: ', trim(file_name)
      open(id_png, file=file_name, STATUS = 'old', access='STREAM')
      call read_PNG_header_f(id_png, flag_little_endian,                &
     &                       n_rgb, npix_x, npix_y)
!
      end subroutine calypso_read_png_size
!
!------------------------------------------------------------------------
!
      subroutine calypso_read_png_data                                  &
     &         (flag_little_endian, n_rgb, npix_x, npix_y, rgb)
!
      use gzip_infleate
      use byte_swap_f
      use number_to_bit
      use transfer_to_long_integers
      use filtering_rgba_png_line
!
      logical, intent(in) :: flag_little_endian
      integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
      character(len = 1), intent(inout) :: rgb(n_rgb*npix_x*npix_y)
!
      type(buffer_4_gzip) :: zbuf
      character(len = 1), allocatable :: gzipbuf(:)
      character(len = 1), allocatable :: gzipbuf_tmp(:)
      character(len = 1), allocatable :: png_rgb(:)
      character(len = 32767) :: readbuf
      integer(kind = 4) :: i_crc, ilength
      integer(kind = 4) :: ntot_lenbuf, norg_lenbuf
      integer(kind = 4) :: int_read(1)
      integer(kind = 4) :: iy, icou
!
!
      icou = 0
      ntot_lenbuf = 0
      allocate(gzipbuf(ntot_lenbuf))
      do
        icou = icou + 1
        i_crc = 0
        read(id_png) int_read(1:1)
        if(flag_little_endian) call byte_swap_int4_f(cast_long(1),      &
     &                                               int_read)
        ilength = int_read(1)
!
        read(id_png) readbuf(1:4)
        call crc32_4_png(len(readbuf(1:4)), readbuf(1:4), i_crc)
!
        if(readbuf(1:4) .eq. iEND_HEADER) then
!          write(*,*) icou, 'exit loop'
          exit
        else if(readbuf(1:4) .ne. iDAT_HEADER) then
          write(*,*) icou, 'Something wrong'
          exit
!        else 
!          write(*,*) icou, 'read data', ilength, ' byte'
        end if
!
        read(id_png) readbuf(1:ilength)
        call crc32_4_png(ilength, readbuf(1:ilength), i_crc)
!
        read(id_png) int_read(1:1)
        if(flag_little_endian) call byte_swap_int4_f(cast_long(1),      &
     &                                               int_read)
        if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
!        write(*,*) 'crc32', int_read(1), i_crc
!
        allocate(gzipbuf_tmp(ntot_lenbuf))
        if(ntot_lenbuf .gt. 0) gzipbuf_tmp(1:ntot_lenbuf)               &
     &                        = gzipbuf(1:ntot_lenbuf)
        deallocate(gzipbuf)
!
        norg_lenbuf = ntot_lenbuf
        ntot_lenbuf = ntot_lenbuf + ilength
        allocate(gzipbuf(ntot_lenbuf))
        gzipbuf(1:norg_lenbuf) = gzipbuf_tmp(1:norg_lenbuf)
        deallocate(gzipbuf_tmp)
!
        do iy = 1, ilength
          gzipbuf(iy+norg_lenbuf) = readbuf(iy:iy)
        end do
      end do
!
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1),        &
     &                                             int_read)
      if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
!        write(*,*) 'crc32', int_read(1), i_crc
      close (id_png)
!
      allocate(png_rgb(3*(npix_x+1)*npix_y))
      zbuf%ilen_gz = ntot_lenbuf
      call alloc_zip_buffer(zbuf)
      call zlib_infleat_char_once(ntot_lenbuf, gzipbuf,                 &
     &   (n_rgb*(npix_x+1)*npix_y), png_rgb, zbuf)
      call dealloc_zip_buffer(zbuf)
!
      call unfilter_png_image(n_rgb, npix_x, npix_y, png_rgb, rgb)
      deallocate(png_rgb)
!
      end subroutine calypso_read_png_data
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      character(len=kchara) function add_png_extension(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
       write(add_png_extension,1011) trim(file_header)
 1011 format (a,".png")
!
      end function add_png_extension
!
!-----------------------------------------------------------------------
!
      end module calypso_png_file_IO
