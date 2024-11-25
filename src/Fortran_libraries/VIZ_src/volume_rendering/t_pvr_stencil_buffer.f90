!>@file   t_pvr_stencil_buffer.f90
!!@brief  module t_pvr_stencil_buffer
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Work structure to make stencil buffer
!!
!!@verbatim
!!      subroutine const_pvr_stencil_buffer(elps_PVR, pvr_rgb,          &
!!     &          pvr_start, pvr_stencil, SR_sig, SR_r, SR_i)
!!      subroutine collect_rendering_image(pvr_start, num_pixel_actual, &
!!     &          rgba_real_gl, pvr_stencil, SR_sig, SR_r)
!!      subroutine dealloc_pvr_stencil_buffer(pvr_stencil)
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        type(pvr_ray_start_type), intent(in) :: pvr_start
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!@endverbatim
!!
      module t_pvr_stencil_buffer
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_calypso_comm_table
      use t_pvr_ray_startpoints
      use t_pvr_image_stack_table
      use t_stencil_buffer_work
      use t_pvr_image_array
!
      implicit  none
!
      type pvr_stencil_buffer
        type(pvr_image_stack_table) :: img_stack
        type(calypso_comm_table) :: img_output_tbl
        type(calypso_comm_table) :: img_composit_tbl
!
        integer(kind = kint) :: num_pixel_recv
        integer(kind = kint) :: npixel_recved
        real(kind = kreal), allocatable :: rgba_subdomain(:,:)
        integer(kind = kint) :: npixel_stacked
        real(kind = kreal), allocatable :: rgba_composit(:,:)
      end type pvr_stencil_buffer
!
      private :: alloc_pvr_stencil_buffer, reset_color_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_pvr_stencil_buffer(elps_PVR, pvr_rgb,            &
     &          pvr_start, pvr_stencil, SR_sig, SR_r, SR_i)
!
      use m_work_time
      use set_pvr_stencil_buffer
!
      type(elapsed_lables), intent(in) :: elps_PVR
      type(pvr_image_type), intent(in) :: pvr_rgb
      type(pvr_ray_start_type), intent(in) :: pvr_start
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      type(stencil_buffer_work) :: stencil_wk
!
!
      call const_stencil_buffer_work                                    &
     &   (pvr_rgb%irank_image_file, pvr_rgb%npe_img_composit,           &
     &    pvr_rgb%num_pixel_xy, pvr_start, stencil_wk)
!
      call s_set_pvr_stencil_buffer                                     &
     &   (pvr_rgb%irank_image_file, pvr_rgb%irank_end_composit,         &
     &    pvr_rgb%num_pixel_xy, elps_PVR, pvr_start, stencil_wk,        &
     &    pvr_stencil%num_pixel_recv, pvr_stencil%img_output_tbl,       &
     &    pvr_stencil%img_composit_tbl, pvr_stencil%img_stack,          &
     &    SR_sig, SR_r, SR_i)
!
      if(i_debug .eq. 0) then
        call dealloc_pvr_ipixel_4_composit(pvr_stencil%img_stack)
        call dealloc_depth_pixel_composit(pvr_stencil%img_stack)
      end if
      call dealloc_stencil_buffer_work(stencil_wk)
!
      call alloc_pvr_stencil_buffer(pvr_stencil)
!
      end subroutine const_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine collect_rendering_image(pvr_start, num_pixel_actual,   &
     &          rgba_real_gl, pvr_stencil, SR_sig, SR_r)
!
      use t_solver_SR
      use calypso_SR_type
      use select_copy_from_recv
!
      type(pvr_ray_start_type), intent(in) :: pvr_start
      integer(kind = kint), intent(in) :: num_pixel_actual
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
      real(kind = kreal), intent(inout)                                 &
     &                    :: rgba_real_gl(4,num_pixel_actual)
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call reset_color_data                                             &
     &   (pvr_stencil%npixel_recved, pvr_stencil%rgba_subdomain)
      call calypso_SR_type_N                                            &
     &   (iflag_import_mod, ifour, pvr_stencil%img_composit_tbl,        &
     &    pvr_start%num_pvr_ray, pvr_stencil%npixel_recved,             &
     &    pvr_start%rgba_ray(1,1), pvr_stencil%rgba_subdomain(1,1),     &
     &    SR_sig, SR_r)
!
      call reset_color_data                                             &
     &   (pvr_stencil%npixel_stacked, pvr_stencil%rgba_composit)
      call composit_rendering_image(pvr_stencil%img_stack,              &
     &    pvr_stencil%npixel_recved, pvr_stencil%rgba_subdomain,        &
     &    pvr_stencil%npixel_stacked, pvr_stencil%rgba_composit)
!
!
      call reset_color_data(num_pixel_actual, rgba_real_gl)
      call calypso_SR_type_N                                            &
     &   (iflag_import_mod, ifour, pvr_stencil%img_output_tbl,          &
     &    pvr_stencil%npixel_stacked, num_pixel_actual,                 &
     &    pvr_stencil%rgba_composit(1,1), rgba_real_gl(1,1),            &
     &    SR_sig, SR_r)
!
      end subroutine collect_rendering_image
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_stencil_buffer(pvr_stencil)
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!
!
      deallocate(pvr_stencil%rgba_subdomain)
      deallocate(pvr_stencil%rgba_composit)
!
      if(i_debug .gt. 0) then
        call dealloc_pvr_ipixel_4_composit(pvr_stencil%img_stack)
        call dealloc_depth_pixel_composit(pvr_stencil%img_stack)
      end if
!
      call dealloc_pvr_image_stack_table(pvr_stencil%img_stack)
!
      call dealloc_calypso_comm_table(pvr_stencil%img_output_tbl)
      call dealloc_calypso_comm_table(pvr_stencil%img_composit_tbl)
!
      end subroutine dealloc_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_stencil_buffer(pvr_stencil)
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!
!
      pvr_stencil%npixel_recved                                         &
     &      = pvr_stencil%img_composit_tbl%ntot_import
      allocate(pvr_stencil%rgba_subdomain(4,pvr_stencil%npixel_recved))
!
      pvr_stencil%npixel_stacked                                        &
     &      = pvr_stencil%img_stack%npixel_4_composit
      allocate(pvr_stencil%rgba_composit(4,pvr_stencil%npixel_stacked))
!
      end subroutine alloc_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_color_data(num, rgba)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: rgba(4*num)
!
      integer(kind = kint) :: i
!
!
!$omp parallel do
      do i = 1, num
        rgba(4*i-3) = 0.0d0
        rgba(4*i-2) = 0.0d0
        rgba(4*i-1) = 0.0d0
        rgba(4*i  ) = 0.0d0
      end do
!$omp end parallel do
!
      end subroutine reset_color_data
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_stencil_buffer
