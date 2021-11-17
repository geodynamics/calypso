!>@file   t_MPI_quilt_bitmap_IO.f90
!!@brief  module t_MPI_quilt_bitmap_IO
!!
!!@author H. Matsui
!!@date Programmed on May., 2021
!!
!>@brief Quilt format bitmap data IO with MPI-IO
!!
!!@verbatim
!!      subroutine init_quilt_rgb_images(file_prefix, iflag_gz,         &
!!     &          n_column_row, npixel_xy, quilt_d)
!!        character(len = kchara), intent(in) :: file_prefix
!!        integer(kind = kint), intent(in) :: iflag_gz
!!        integer(kind = kint), intent(in) :: n_column_row(2)
!!        integer(kind = kint), intent(in) :: npixel_xy(2)
!!        type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
!!
!!      subroutine alloc_quilt_rgb_images(npixel_xy, quilt_d)
!!      subroutine dealloc_quilt_rgb_images(quilt_d)
!!        integer(kind = kint), intent(in) :: npixel_xy(2)
!!        type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
!!
!!      subroutine alloc_each_rgb_image(npix_xy, image)
!!      subroutine dealloc_each_rgb_image(image)
!!        integer(kind = kint), intent(in) :: npix_xy(2)
!!        type(each_rgb_image), intent(inout) :: image
!!@endverbatim
!
      module t_MPI_quilt_bitmap_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
      type each_rgb_image
!>        Independent file prefix
        character(len = kchara) :: each_prefix
!>        File format flag (BMP or PNG)
        integer(kind = kint) :: image_format
!>        Horizontal number of pixel
        integer(kind = kint) :: npix_xy(2)
        character(len=1), allocatable :: rgb(:,:,:)
      end type each_rgb_image
!
      type MPI_quilt_bitmap_IO
!>        Sequence of file prefix
        character(len = kchara) :: image_seq_prefix
!>        File format flag (BMP, PNG, or Quilt BMP)
        integer(kind = kint) :: image_seq_format
!
!>        Number of images
        integer(kind = kint) :: n_image
!>        Number of row and columns of images
        integer(kind = kint) :: n_column_row(2)
!
!>        Number of images in each process
        integer(kind = kint) :: num_image_lc
!>        Number of pixel (Horizontal, Vertical)
        integer(kind = kint) :: npixel_xy(2)
!>        RGB images
        type(each_rgb_image), allocatable :: images(:)
!>        Image index in each process
        integer(kind = kint), allocatable :: icou_each_pe(:)
      end type MPI_quilt_bitmap_IO
!
     private :: count_local_image_pe_quilt, set_local_image_pe_quilt
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_quilt_rgb_images(file_prefix, iflag_gz,           &
     &          n_column_row, npixel_xy, quilt_d)
!
      use set_parallel_file_name
      use output_image_sel_4_png
!
      character(len = kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: iflag_gz
      integer(kind = kint), intent(in) :: n_column_row(2)
      integer(kind = kint), intent(in) :: npixel_xy(2)
!
      type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
!
      integer(kind = kint) :: i
!
!
      quilt_d%n_column_row(1:2) = n_column_row(1:2)
      quilt_d%n_image = n_column_row(1) * n_column_row(2)
      call count_local_image_pe_quilt                                   &
     &   (quilt_d%n_image, quilt_d%num_image_lc)
!
      quilt_d%image_seq_prefix = file_prefix

      if(iflag_gz .gt. 0) then
        quilt_d%image_seq_format = iflag_QUILT_BMP_GZ
      else
        quilt_d%image_seq_format = iflag_QUILT_BMP
      end if

      quilt_d%npixel_xy(1:2) = npixel_xy(1:2)
      call alloc_quilt_rgb_images(quilt_d)
      do i = 1, quilt_d%num_image_lc
        quilt_d%images(i)%image_format = quilt_d%image_seq_format
        quilt_d%images(i)%each_prefix                                   &
     &     = add_int_suffix(i, quilt_d%image_seq_prefix)
        call alloc_each_rgb_image(quilt_d%npixel_xy, quilt_d%images(i))
      end do
!
      call set_local_image_pe_quilt(quilt_d%n_image,                    &
     &          quilt_d%num_image_lc, quilt_d%icou_each_pe)
!
      end subroutine init_quilt_rgb_images
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_quilt_rgb_images(quilt_d)
!
      type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
      integer(kind = kint) :: i
!
      do i = 1, quilt_d%num_image_lc
        call dealloc_each_rgb_image(quilt_d%images(i))
      end do
      deallocate(quilt_d%images, quilt_d%icou_each_pe)
!
      end subroutine dealloc_quilt_rgb_images
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_quilt_rgb_images(quilt_d)
!
      type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
!
!
      allocate(quilt_d%icou_each_pe(quilt_d%num_image_lc))
      allocate(quilt_d%images(quilt_d%num_image_lc))
!
      quilt_d%icou_each_pe(1:quilt_d%num_image_lc) = -1
!
      end subroutine alloc_quilt_rgb_images
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_local_image_pe_quilt(num_image, num_image_lc)
!
      integer(kind = kint), intent(in) :: num_image
      integer(kind = kint), intent(inout) :: num_image_lc
!
      integer(kind = kint) :: icou, ip
!
      icou = 0
      do ip = 0, num_image-1
        if(mod(ip,nprocs) .eq. my_rank) icou = icou + 1
      end do
      num_image_lc = icou
!
      end subroutine count_local_image_pe_quilt
!
! ----------------------------------------------------------------------
!
      subroutine set_local_image_pe_quilt                               &
     &         (num_image, num_image_lc, icou_each_pe)
!
      integer(kind = kint), intent(in) :: num_image
      integer(kind = kint), intent(in) :: num_image_lc
      integer(kind = kint), intent(inout)                               &
     &                     :: icou_each_pe(num_image_lc)
!
      integer(kind = kint) :: icou, ip
!
      icou = 0
      do ip = 0, num_image-1
        if(mod(ip,nprocs) .eq. my_rank) then
          icou = icou + 1
          icou_each_pe(icou) = ip + 1
        end if
      end do
!
      end subroutine set_local_image_pe_quilt
!
! ----------------------------------------------------------------------
!
      subroutine sel_write_seq_image_files(num_image_lc, icou_each_pe,  &
     &                                     images)
!
      use set_parallel_file_name
      use output_image_sel_4_png
!
      integer(kind = kint), intent(in) :: num_image_lc
      integer(kind = kint), intent(in)  :: icou_each_pe(num_image_lc)
!
      type(each_rgb_image), intent(in) :: images(num_image_lc)
!
      integer(kind = kint) :: icou, ip
!
      do icou = 1, num_image_lc
        ip = icou_each_pe(icou)
        write(*,*) ip, '-th output file from process', my_rank
        call sel_output_image_file                                      &
     &     (images(icou)%image_format, images(icou)%each_prefix,        &
     &      images(icou)%npix_xy(1), images(icou)%npix_xy(2),           &
     &      images(icou)%rgb)
      end do
!
      end subroutine sel_write_seq_image_files
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_each_rgb_image(npix_xy, image)
!
      integer(kind = kint), intent(in) :: npix_xy(2)
      type(each_rgb_image), intent(inout) :: image
!
      image%npix_xy(1:2) = npix_xy(1:2)
      allocate(image%rgb(3,image%npix_xy(1),image%npix_xy(2)))
!
      end subroutine alloc_each_rgb_image
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_each_rgb_image(image)
!
      type(each_rgb_image), intent(inout) :: image
!
      if(allocated(image%rgb) .eqv. .FALSE.) return
      deallocate(image%rgb)
      image%npix_xy(1:2) = 0
      image%image_format = 0
!
      end subroutine dealloc_each_rgb_image
!
! ----------------------------------------------------------------------
!
      end module t_MPI_quilt_bitmap_IO
