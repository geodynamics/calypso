!>@file   t_stencil_buffer_work.f90
!!@brief  module t_stencil_buffer_work
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Work structure to make stencil buffer
!!
!!@verbatim
!!      subroutine const_stencil_buffer_work                            &
!!     &         (irank_image_file, npe_img_composit,                   &
!!     &          num_pixel_xy, pvr_start, stencil_wk)
!!      subroutine dealloc_stencil_buffer_work(stencil_wk)
!!        type(pvr_ray_start_type), intent(in) :: pvr_start
!!        type(stencil_buffer_work), intent(inout) :: stencil_wk
!!@endverbatim
!!
!
      module t_stencil_buffer_work
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      type stencil_buffer_work
        integer(kind = kint) :: ntot_recv_image
        integer(kind = kint), allocatable :: istack_recv_image(:)
        integer(kind = kint), allocatable :: irank_4_composit(:)
        integer(kind = kint), allocatable :: item_recv_image(:)
        integer(kind = kint), allocatable :: irev_recv_image(:)
      end type stencil_buffer_work
!
      private :: alloc_stencil_buffer_work
      private :: count_local_ray_4_each_pixel
      private :: set_global_stencil_buffer
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_stencil_buffer_work                              &
     &         (irank_image_file, npe_img_composit,                     &
     &          num_pixel_xy, pvr_start, stencil_wk)
!
      use calypso_mpi_int8
      use t_pvr_ray_startpoints
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: npe_img_composit
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_ray_start_type), intent(in) :: pvr_start
!
      type(stencil_buffer_work), intent(inout) :: stencil_wk
!
      integer(kind = kint_gl) :: num_pvr_ray_gl
      integer(kind = kint_gl) :: max_ray_start_lc, max_ray_start_gl
      integer(kind = kint_gl), allocatable :: num_ray_start_lc(:)
      integer(kind = kint_gl), allocatable :: num_ray_start_gl(:)
!
!
      allocate(num_ray_start_lc(num_pixel_xy))
      if(my_rank .eq. int(irank_image_file)) then
        allocate(num_ray_start_gl(num_pixel_xy))
      end if
!
      call calypso_mpi_reduce_one_int8                                  &
     &   (cast_long(pvr_start%num_pvr_ray), num_pvr_ray_gl,             &
     &    MPI_SUM, int(irank_image_file))
!
      call count_local_ray_4_each_pixel(num_pixel_xy,                   &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_start,              &
     &    num_ray_start_lc, max_ray_start_lc)
!
      call calypso_mpi_reduce_int8(num_ray_start_lc, num_ray_start_gl,  &
     &    cast_long(num_pixel_xy), MPI_SUM, int(irank_image_file))
      call calypso_mpi_reduce_one_int8                                  &
     &   (max_ray_start_lc, max_ray_start_gl,                           &
     &    MPI_SUM, int(irank_image_file))
!
      call alloc_stencil_buffer_work(num_pixel_xy, stencil_wk)
      call set_global_stencil_buffer                                    &
     &   (irank_image_file, npe_img_composit,                           &
     &    num_pixel_xy, num_pvr_ray_gl, num_ray_start_gl, stencil_wk)
!
      if(i_debug.gt.0 .and. my_rank .eq. irank_image_file) then
        write(*,*) 'Stencil buffer size, num. of segmented image: ',    &
     &            stencil_wk%ntot_recv_image, max_ray_start_gl
        write(*,*) 'Number of total ray trace: ', num_pvr_ray_gl
      end if
!
      deallocate(num_ray_start_lc)
      if(my_rank .eq. irank_image_file) deallocate(num_ray_start_gl)
!
      end subroutine const_stencil_buffer_work
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_stencil_buffer_work(stencil_wk)
!
      type(stencil_buffer_work), intent(inout) :: stencil_wk
!
!
      deallocate(stencil_wk%irev_recv_image)
      deallocate(stencil_wk%istack_recv_image)
      deallocate(stencil_wk%irank_4_composit)
      deallocate(stencil_wk%item_recv_image)
!
      end subroutine dealloc_stencil_buffer_work
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_stencil_buffer_work(num_pixel_xy, stencil_wk)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(stencil_buffer_work), intent(inout) :: stencil_wk
!
!
      allocate(stencil_wk%istack_recv_image(0:nprocs))
!
      stencil_wk%istack_recv_image(0:nprocs) = 0
!
      allocate(stencil_wk%irank_4_composit(num_pixel_xy))
      allocate(stencil_wk%irev_recv_image(num_pixel_xy))
      allocate(stencil_wk%item_recv_image(num_pixel_xy))
!
      if(num_pixel_xy .gt. 0) then
!$omp parallel workshare
        stencil_wk%irank_4_composit(1:num_pixel_xy) = -1
        stencil_wk%irev_recv_image(1:num_pixel_xy) = 0
        stencil_wk%item_recv_image(1:num_pixel_xy) = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_stencil_buffer_work
!
!  ---------------------------------------------------------------------
!
      subroutine count_local_ray_4_each_pixel                           &
     &         (num_pixel_xy, num_pvr_ray, id_pixel_start,              &
     &          num_ray_start_lc, max_ray_start_lc)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
!
      integer(kind = kint_gl), intent(inout)                            &
     &            :: num_ray_start_lc(num_pixel_xy)
      integer(kind = kint_gl), intent(inout) :: max_ray_start_lc
!
      integer(kind = kint) :: inum, ipix
!
!
!$omp parallel workshare
      num_ray_start_lc(1:num_pixel_xy) = 0
!$omp end parallel workshare
!
      do inum = 1, num_pvr_ray
        ipix = id_pixel_start(inum)
        num_ray_start_lc(ipix) = num_ray_start_lc(ipix) + 1
      end do
      max_ray_start_lc = MAXVAL(num_ray_start_lc)
!
      end subroutine count_local_ray_4_each_pixel
!
!  ---------------------------------------------------------------------
!
      subroutine set_global_stencil_buffer                              &
     &         (irank_image_file, npe_img_composit,                     &
     &          num_pixel_xy, num_pvr_ray_gl, num_ray_start_gl,         &
     &          stencil_wk)
!
      use calypso_mpi_int
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: npe_img_composit
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint_gl), intent(in) :: num_pvr_ray_gl
      integer(kind = kint_gl), intent(in)                               &
     &            :: num_ray_start_gl(num_pixel_xy)
!
      type(stencil_buffer_work), intent(inout) :: stencil_wk
!
      integer(kind = kint_gl), allocatable :: istack_ray_start_gl(:)
!
      integer(kind = kint) :: icou, ipix, ip, i_rank
!
!
      if(my_rank .eq. irank_image_file) then
        allocate(istack_ray_start_gl(0:num_pixel_xy))
!
!$omp parallel workshare
        stencil_wk%irank_4_composit(1:num_pixel_xy) = -1
!$omp end parallel workshare

!$omp parallel workshare
        stencil_wk%istack_recv_image(0:nprocs) = 0
!$omp end parallel workshare
        istack_ray_start_gl(0) = 0
        icou = 0
        do ipix = 1, num_pixel_xy
          istack_ray_start_gl(ipix) = istack_ray_start_gl(ipix-1)       &
     &                               + num_ray_start_gl(ipix)
!
          if(num_ray_start_gl(ipix) .gt. 0) then
            icou = icou + 1
            ip = int((istack_ray_start_gl(ipix) - 1)                    &
     &              * npe_img_composit / num_pvr_ray_gl + 1)
            i_rank = int(mod(irank_image_file+npe_img_composit-ip,      &
     &              nprocs))
            stencil_wk%irank_4_composit(ipix) = i_rank
            stencil_wk%istack_recv_image(ip) = icou
            stencil_wk%irev_recv_image(ipix) = icou
            stencil_wk%item_recv_image(icou) = ipix
          end if
        end do
        do ip = npe_img_composit+1, nprocs
          stencil_wk%istack_recv_image(ip)                              &
     &                   = stencil_wk%istack_recv_image(ip-1)
        end do
        stencil_wk%ntot_recv_image                                      &
     &         = stencil_wk%istack_recv_image(nprocs)
!
!        write(50+my_rank,*) 'ipix, stencil_wk%irank_4_composit'
!        do ipix = 1, num_pixel_xy
!          write(50+my_rank,*) ipix, stencil_wk%irank_4_composit(ipix)
!        end do
!
        deallocate(istack_ray_start_gl)
      end if
!
      call calypso_mpi_bcast_int                                        &
     &   (stencil_wk%istack_recv_image, cast_long(nprocs+1),            &
     &    irank_image_file)
!
      call calypso_mpi_bcast_int                                        &
     &   (stencil_wk%irank_4_composit, cast_long(num_pixel_xy),         &
     &    irank_image_file)
      call calypso_mpi_bcast_int                                        &
     &   (stencil_wk%irev_recv_image, cast_long(num_pixel_xy),          &
     &    irank_image_file)
!
      call calypso_mpi_bcast_one_int(stencil_wk%ntot_recv_image,        &
     &                               irank_image_file)
      call calypso_mpi_bcast_int(stencil_wk%item_recv_image(1),         &
     &    cast_long(stencil_wk%ntot_recv_image), irank_image_file)
!
      end subroutine set_global_stencil_buffer
!
!  ---------------------------------------------------------------------
!
      end module t_stencil_buffer_work
