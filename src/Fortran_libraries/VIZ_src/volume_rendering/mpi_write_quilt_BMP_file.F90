!>@file   mpi_write_quilt_BMP_file.F90
!!@brief  module mpi_write_quilt_BMP_file
!!
!!@author H. Matsui
!!@date Programmed on May., 2021
!!
!>@brief Quilt format bitmap data IO USING MPI-IO
!!
!!@verbatim
!!      subroutine sel_write_pvr_image_files(quilt_d)
!!        type(MPI_quilt_bitmap_IO), intent(in) :: quilt_d
!!      subroutine mpi_write_quilt_BMP_file(file_prefix,                &
!!     &          n_column_row, num_image_lc, icou_each_pe,             &
!!     &          npixel_x, npixel_y, images)
!!      subroutine mpi_write_quilt_gz_BMP_file(file_prefix,             &
!!     &          n_column_row, num_image_lc, icou_each_pe,             &
!!     &          npixel_x, npixel_y, images)
!!        character(len=kchara), intent(in) :: file_prefix
!!        integer(kind = kint), intent(in) :: n_column_row(2)
!!        integer(kind = kint), intent(in) :: num_image_lc
!!        integer(kind = kint), intent(in) :: icou_each_pe(num_image_lc)
!!        integer(kind = kint), intent(in) :: npixel_x, npixel_y
!!        type(each_rgb_image), intent(in) :: images(num_image_lc)
!!@endverbatim
!
      module mpi_write_quilt_BMP_file
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_MPI_quilt_bitmap_IO
!
      implicit none
!
      character(len=1), allocatable, private :: bgr_line(:,:)
!
      private :: s_mpi_write_quilt_BMP_file
      private :: mpi_write_quilt_gz_BMP_file
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_write_pvr_image_files(quilt_d)
!
      use output_image_sel_4_png
!
      type(MPI_quilt_bitmap_IO), intent(in) :: quilt_d
!
!
      if(quilt_d%image_seq_format .eq. iflag_QUILT_BMP) then
        call s_mpi_write_quilt_BMP_file                                 &
     &     (quilt_d%image_seq_prefix, quilt_d%n_column_row,             &
     &      quilt_d%num_image_lc, quilt_d%icou_each_pe,                 &
     &      quilt_d%npixel_xy(1), quilt_d%npixel_xy(2), quilt_d%images)
      else if(quilt_d%image_seq_format .eq. iflag_QUILT_BMP_GZ) then
        call mpi_write_quilt_gz_BMP_file                                &
     &     (quilt_d%image_seq_prefix, quilt_d%n_column_row,             &
     &      quilt_d%num_image_lc, quilt_d%icou_each_pe,                 &
     &      quilt_d%npixel_xy(1), quilt_d%npixel_xy(2), quilt_d%images)
      else
        call sel_write_seq_image_files                                  &
     &     (quilt_d%num_image_lc, quilt_d%icou_each_pe, quilt_d%images)
      end if
!
      end subroutine sel_write_pvr_image_files
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_mpi_write_quilt_BMP_file(file_prefix,                &
     &          n_column_row, num_image_lc, icou_each_pe,               &
     &          npixel_x, npixel_y, images)
!
      use m_calypso_mpi_IO
      use MPI_ascii_data_IO
      use t_calypso_mpi_IO_param
      use write_bmp_image
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: n_column_row(2)
      integer(kind = kint), intent(in) :: num_image_lc
      integer(kind = kint), intent(in)  :: icou_each_pe(num_image_lc)
!
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
      type(each_rgb_image), intent(in) :: images(num_image_lc)
!
      type(calypso_MPI_IO_params), save :: IO_param
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
      integer(kind = kint) :: icou, ix, iy, ip, j
      character(len=kchara) :: file_name
      integer :: ntot_pixel_x, ntot_pixel_y
!
      ntot_pixel_x = int(n_column_row(1)*npixel_x)
      ntot_pixel_y = int(n_column_row(2)*npixel_y)
      allocate(bgr_line(3,npixel_x))
!
      file_name = add_bmp_suffix(file_prefix)
      if(my_rank .eq. 0) write(*,*) 'Write Quilt Bitmap: ',             &
     &                  trim(file_name)
      call open_write_mpi_file(file_name, IO_param)
      call mpi_write_charahead(IO_param, 54,                            &
     &    BMP_header(ntot_pixel_x, ntot_pixel_y))
!
      do icou = 1, num_image_lc
        ip = icou_each_pe(icou) - 1
        ix = mod(ip,n_column_row(1))
        iy = ip / n_column_row(1)
        ilength = 3*int(npixel_x)
        do j = 1, npixel_y
          bgr_line(1,1:npixel_x) = images(icou)%rgb(3,1:npixel_x,j)
          bgr_line(2,1:npixel_x) = images(icou)%rgb(2,1:npixel_x,j)
          bgr_line(3,1:npixel_x) = images(icou)%rgb(1,1:npixel_x,j)
!
          ioffset = IO_param%ioff_gl                                    &
     &       + ilength * (ix + n_column_row(1) * ((j-1) + iy*npixel_y))
          call mpi_write_one_chara_b                                    &
     &       (IO_param%id_file, ioffset, ilength, bgr_line(1,1))
        end do
      end do
      call close_mpi_file(IO_param)
      call calypso_MPI_barrier
!
      deallocate(bgr_line)
!
      end subroutine s_mpi_write_quilt_BMP_file
!
! ----------------------------------------------------------------------
!
      subroutine mpi_write_quilt_gz_BMP_file(file_prefix,               &
     &          n_column_row, num_image_lc, icou_each_pe,               &
     &          npixel_x, npixel_y, images)
!
      use m_calypso_mpi_IO
      use MPI_ascii_data_IO
      use t_calypso_mpi_IO_param
      use t_buffer_4_gzip
      use zlib_convert_text
      use data_convert_by_zlib
      use calypso_mpi_int8
      use transfer_to_long_integers
      use write_bmp_image
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: n_column_row(2)
      integer(kind = kint), intent(in) :: num_image_lc
      integer(kind = kint), intent(in) :: icou_each_pe(num_image_lc)
!
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
      type(each_rgb_image), intent(in) :: images(num_image_lc)
!
      type(buffer_4_gzip) :: zbuf_head
      type(buffer_4_gzip), allocatable :: zbuf(:,:)
      integer(kind = kint_gl), allocatable :: istack_zbuf(:)
      integer(kind = kint_gl), allocatable :: nlen_zbuf_gl(:)
!
      type(calypso_MPI_IO_params), save :: IO_param
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
      integer(kind = kint) :: icou, ix, iy, i_img, j, kk
      character(len=kchara) :: file_name, gz_name
      integer :: ntot_pixel_x, ntot_pixel_y
!
!
      ntot_pixel_x = int(n_column_row(1)*npixel_x)
      ntot_pixel_y = int(n_column_row(2)*npixel_y)
      allocate(bgr_line(3,npixel_x))
      allocate(zbuf(npixel_y,num_image_lc))
      allocate(istack_zbuf(0:npixel_y*n_column_row(1)*n_column_row(2)))
      allocate(nlen_zbuf_gl(npixel_y*n_column_row(1)*n_column_row(2)))
!
      istack_zbuf(0) = 0
!$omp parallel workshare
      istack_zbuf(1:npixel_y*n_column_row(1)*n_column_row(2)) =  0
      nlen_zbuf_gl(1:npixel_y*n_column_row(1)*n_column_row(2)) = 0
!$omp end parallel workshare
!
      file_name = add_bmp_suffix(file_prefix)
      gz_name = add_gzip_extension(file_name)
      if(my_rank .eq. 0) write(*,*) 'Write gzipped Quilt Bitmap: ',     &
     &                  trim(gz_name)
      call open_write_mpi_file(gz_name, IO_param)
!
      if(my_rank .eq. 0) then
        call defleate_characters                                        &
     &     (54, BMP_header(ntot_pixel_x, ntot_pixel_y), zbuf_head)
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_gz                                  &
     &     (IO_param%id_file, ioffset, zbuf_head)
        call dealloc_zip_buffer(zbuf_head)
      end if
      call calypso_mpi_bcast_one_int8(zbuf_head%ilen_gzipped, 0)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf_head%ilen_gzipped
!
!
      ilength = 3*int(npixel_x)
      do icou = 1, num_image_lc
        i_img = icou_each_pe(icou)
        ix = mod(i_img-1,n_column_row(1))
        iy = (i_img-1) / n_column_row(1)
        do j = 1, npixel_y
          kk = ix+1 + (j-1) * n_column_row(1)                           &
     &              + iy *    n_column_row(1) * npixel_y
          bgr_line(1,1:npixel_x) = images(icou)%rgb(3,1:npixel_x,j)
          bgr_line(2,1:npixel_x) = images(icou)%rgb(2,1:npixel_x,j)
          bgr_line(3,1:npixel_x) = images(icou)%rgb(1,1:npixel_x,j)
!
          call gzip_defleate_characters_b                               &
     &       (cast_long(ilength), bgr_line(1,1), zbuf(j,icou))
          istack_zbuf(kk) = zbuf(j,icou)%ilen_gzipped
        end do
      end do
!
      call calypso_mpi_allreduce_int8(istack_zbuf(1), nlen_zbuf_gl(1),  &
     &    cast_long(npixel_y*n_column_row(1)*n_column_row(2)), MPI_SUM)
      do kk = 1, npixel_y*n_column_row(1)*n_column_row(2)
        istack_zbuf(kk) = istack_zbuf(kk-1) + nlen_zbuf_gl(kk)
      end do
!
      do icou = 1, num_image_lc
        i_img = icou_each_pe(icou)
        ix = mod(i_img-1,n_column_row(1))
        iy = (i_img-1) / n_column_row(1)
        do j = 1, npixel_y
          kk = ix+1 + (j-1) * n_column_row(1)                           &
     &              + iy *    n_column_row(1) * npixel_y
          ioffset = IO_param%ioff_gl + istack_zbuf(kk-1)
          call calypso_mpi_seek_write_gz                                &
     &       (IO_param%id_file, ioffset, zbuf(j,icou))
        end do
      end do
      call close_mpi_file(IO_param)
!
!$omp parallel private(icou)
      do icou = 1, num_image_lc
!$omp do private(j)
        do j = 1, npixel_y
          call dealloc_zip_buffer(zbuf(j,icou))
        end do
!$omp end do nowait
      end do
!$omp end parallel
      call calypso_MPI_barrier
!
      deallocate(zbuf, nlen_zbuf_gl, istack_zbuf)
      deallocate(bgr_line)
!
      end subroutine mpi_write_quilt_gz_BMP_file
!
! ----------------------------------------------------------------------
!
      end module mpi_write_quilt_BMP_file
