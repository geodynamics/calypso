!>@file   t_pvr_image_stack_table.f90
!!@brief  module t_pvr_image_stack_table
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Work structure to make stencil buffer
!!
!!@verbatim
!!      subroutine alloc_pvr_image_stack_table(img_stack)
!!      subroutine alloc_depth_pixel_composit                           &
!!     &         (num_pvr_ray, ntot_recv_composit, img_stack)
!!      subroutine alloc_pvr_ipixel_4_composit(num_pixel_xy, img_stack)
!!        type(pvr_image_stack_table), intent(inout) :: img_stack
!!
!!      subroutine dealloc_pvr_image_stack_table(img_stack)
!!      subroutine dealloc_depth_pixel_composit(img_stack)
!!      subroutine dealloc_pvr_ipixel_4_composit(img_stack)
!!        type(pvr_image_stack_table), intent(inout) :: img_stack
!!
!!      subroutine composit_rendering_image                             &
!!     &         (img_stack, npixel_recved, rgba_subdomain,             &
!!     &          npixel_stacked, rgba_composit)
!!      subroutine check_rendering_image(id_file, iref,                 &
!!     &          img_stack, npixel_recved, rgba_subdomain,             &
!!     &          npixel_stacked, rgba_composit)
!!        type(pvr_image_stack_table), intent(in) :: img_stack
!!
!!      subroutine set_global_pixel_4_composit                          &
!!     &         (stencil_wk, npixel_4_composit, num_pixel_xy,          &
!!     &          ipixel_4_composit, item_4_composit)
!!@endverbatim
!!
      module t_pvr_image_stack_table
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_calypso_comm_table
      use t_stencil_buffer_work
!
      implicit  none
!
      type pvr_image_stack_table
        integer(kind = kint) :: npixel_4_composit
        integer(kind = kint), allocatable :: istack_composition(:)
!
        integer(kind = kint), allocatable :: ipix_4_composit(:)
        real(kind = kreal), allocatable :: depth_pixel_composit(:)
        real(kind = kreal), allocatable :: depth_pvr_ray_start(:)
!
        integer(kind = kint), allocatable :: ipixel_4_composit(:)
        integer(kind = kint), allocatable :: item_4_composit(:)
      end type pvr_image_stack_table
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_image_stack_table(img_stack)
!
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
      integer(kind = kint) :: n_image
!
      n_image = img_stack%npixel_4_composit
      allocate(img_stack%istack_composition(0:n_image))
!
      img_stack%istack_composition(0:n_image) = 0
!
      end subroutine alloc_pvr_image_stack_table
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_depth_pixel_composit                             &
     &         (num_pvr_ray, ntot_recv_composit, img_stack)
!
      integer(kind = kint), intent(in) :: ntot_recv_composit
      integer(kind = kint), intent(in) :: num_pvr_ray
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
!
      allocate(img_stack%ipix_4_composit(ntot_recv_composit))
      allocate(img_stack%depth_pixel_composit(ntot_recv_composit))
      allocate(img_stack%depth_pvr_ray_start(num_pvr_ray))
!
      if(ntot_recv_composit .gt. 0) then
!$omp parallel workshare
        img_stack%ipix_4_composit(1:ntot_recv_composit) = 0
        img_stack%depth_pixel_composit(1:ntot_recv_composit) = 0.0d0
!$omp end parallel workshare
      end if
!
      if(num_pvr_ray .gt. 0) then
!$omp parallel workshare
        img_stack%depth_pvr_ray_start(1:num_pvr_ray) = 0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_depth_pixel_composit
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_ipixel_4_composit(num_pixel_xy, img_stack)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
      integer(kind = kint) :: n_composit
!
      n_composit = img_stack%npixel_4_composit
      allocate(img_stack%ipixel_4_composit(n_composit))
      allocate(img_stack%item_4_composit(num_pixel_xy))
!
      if(n_composit .gt. 0) then
!$omp parallel workshare
        img_stack%ipixel_4_composit(1:n_composit) = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_pvr_ipixel_4_composit
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_image_stack_table(img_stack)
!
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
!
      deallocate(img_stack%istack_composition)
!
      end subroutine dealloc_pvr_image_stack_table
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_depth_pixel_composit(img_stack)
!
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
!
      deallocate(img_stack%ipix_4_composit)
      deallocate(img_stack%depth_pixel_composit)
      deallocate(img_stack%depth_pvr_ray_start)
!
      end subroutine dealloc_depth_pixel_composit
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_ipixel_4_composit(img_stack)
!
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
      deallocate(img_stack%ipixel_4_composit)
      deallocate(img_stack%item_4_composit)
!
      end subroutine dealloc_pvr_ipixel_4_composit
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine composit_rendering_image                               &
     &         (img_stack, npixel_recved, rgba_subdomain,               &
     &          npixel_stacked, rgba_composit)
!
      use set_rgba_4_each_pixel
!
      type(pvr_image_stack_table), intent(in) :: img_stack
      integer(kind = kint), intent(in) :: npixel_recved, npixel_stacked
      real(kind = kreal), intent(in) :: rgba_subdomain(4,npixel_recved)
!
      real(kind = kreal), intent(inout)                                 &
     &      :: rgba_composit(4,npixel_stacked)
!
      integer(kind = kint) :: inum, ipix, ist, ied
!
!
!$omp parallel do private(ipix,ist,ied,inum)
      do ipix = 1, img_stack%npixel_4_composit
        ist = img_stack%istack_composition(ipix-1)
        ied = img_stack%istack_composition(ipix)
        do inum = ist+1, ied
          call composite_alpha_blending(rgba_subdomain(1,inum),         &
     &        rgba_composit(1,ipix))
        end do
      end do
!$omp end parallel do
!
      end subroutine composit_rendering_image
!
!  ---------------------------------------------------------------------
!
      subroutine check_rendering_image(id_file, iref,                   &
     &          img_stack, npixel_recved, rgba_subdomain,               &
     &          npixel_stacked, rgba_composit)
!
      use set_rgba_4_each_pixel
!
      integer(kind = kint), intent(in) :: id_file, iref
      type(pvr_image_stack_table), intent(in) :: img_stack
      integer(kind = kint), intent(in) :: npixel_recved, npixel_stacked
      real(kind = kreal), intent(in) :: rgba_subdomain(4,npixel_recved)
!
      real(kind = kreal), intent(in) :: rgba_composit(4,npixel_stacked)
!
      integer(kind = kint) :: inum, ipix, ist, ied
      real(kind = kreal) :: rgb_test(4)
      integer :: i
!
      rgb_test(1:4) = 0.0d0
!
      do ipix = 1, img_stack%npixel_4_composit
        ist = img_stack%istack_composition(ipix-1)
        ied = img_stack%istack_composition(ipix)
        do inum = ist+1, ied
          if(img_stack%ipixel_4_composit(ipix) .eq. iref) then
            write(id_file,*) 'rgba_subdomain', iref, inum,              &
     &              rgba_subdomain(1:4,inum),                           &
     &              img_stack%depth_pixel_composit(inum)
!
            call composite_alpha_blending(rgba_subdomain(1,inum),       &
     &          rgb_test(1))
          end if
        end do 
      end do
!
      do i = 1, npixel_stacked
        if(img_stack%ipixel_4_composit(i) .eq. iref) then
          write(id_file,*) 'rgb_test', iref, i, rgb_test(1:4)
          write(id_file,*) 'rgba_composit', iref, i, rgba_composit(1:4,i)
        end if
      end do
!
      end subroutine check_rendering_image
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_image_stack_table
