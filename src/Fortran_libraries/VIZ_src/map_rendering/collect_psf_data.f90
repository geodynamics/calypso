!>@file   collect_psf_data.f90
!!@brief  module collect_psf_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine collect_psf_scalar(irank_draw, ifld_img, node, field,&
!!     &                              d_img, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        integer(kind = kint) , intent(in) :: ifld_img
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(in) :: field
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: d_img(node%istack_internod(nprocs))
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine collect_psf_node(irank_draw, node, xx_out, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        type(node_data), intent(in) :: node
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: xx_out(node%istack_internod(nprocs),n_vector)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine collect_psf_element(irank_draw, ele, ie_out, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        type(element_data), intent(in) :: ele
!!        integer(kind = kinds), intent(inout)                          &
!!     &            :: ie_out(node%istack_internod(nprocs),num_triangle)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
      module collect_psf_data
!
      use calypso_mpi
      use m_precision
!
      use t_geometry_data
      use t_phys_data
      use t_solver_SR
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
      use t_map_rendering_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_map_mesh(num_map, view_param,                   &
     &          psf_mesh, psf_dat, map_data, pvr_rgb, SR_sig)
!
      use m_elapsed_labels_4_VIZ
      use t_psf_patch_data
      use t_psf_results
      use t_pvr_image_array
!
      use collect_psf_mesh_field
!
      integer(kind= kint), intent(in) :: num_map
      type(psf_local_data), intent(in) :: psf_mesh(num_map)
      type(pvr_view_parameter), intent(in):: view_param(num_map)
!
      type(psf_results), intent(inout) :: psf_dat(num_map)
      type(map_rendering_data), intent(inout) :: map_data(num_map)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_map)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_map
!
!
      do i_map = 1, num_map
        pvr_rgb(i_map)%irank_image_file = mod(i_map,nprocs)
        call alloc_pvr_image_array(view_param(i_map)%n_pvr_pixel,       &
     &                             pvr_rgb(i_map))
        call init_map_rendering_data(view_param(i_map),                 &
     &                               pvr_rgb(i_map), map_data(i_map))
        call alloc_scalar_on_map(pvr_rgb(i_map)%num_pixel_xy,           &
     &                           map_data(i_map))
      end do
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+1)
      do i_map = 1, num_map
        call init_merge_psf_mesh                                        &
     &     (pvr_rgb(i_map)%irank_image_file, psf_mesh(i_map),           &
     &      psf_dat(i_map)%psf_nod, psf_dat(i_map)%psf_ele,             &
     &      psf_dat(i_map)%psf_phys, SR_sig)
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+1)
!
      end subroutine output_map_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine output_map_file(num_map, istep_psf, time_d,            &
     &          psf_mesh, color_param, cbar_param,          &
     &          psf_dat, map_data, pvr_rgb, SR_sig)
!
      use m_elapsed_labels_4_VIZ
      use t_psf_patch_data
      use t_psf_results
      use t_pvr_image_array
!
      use collect_psf_mesh_field
      use write_PVR_image
!
      integer(kind= kint), intent(in) :: num_map
      integer(kind= kint), intent(in) ::  istep_psf
      type(time_data), intent(in) :: time_d
      type(psf_local_data), intent(in) :: psf_mesh(num_map)
      type(pvr_colormap_parameter), intent(in) :: color_param(num_map)
      type(pvr_colorbar_parameter), intent(in) :: cbar_param(num_map)
!
      type(psf_results), intent(inout) :: psf_dat(num_map)
      type(map_rendering_data), intent(inout) :: map_data(num_map)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_map)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_map
!
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+1)
      do i_map = 1, num_map
        call collect_psf_scalar(pvr_rgb(i_map)%irank_image_file, ione,  &
     &      psf_mesh(i_map)%node, psf_mesh(i_map)%field,                &
     &      psf_dat(i_map)%psf_phys%d_fld(1,1), SR_sig)
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+1)
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+2)
      do i_map = 1, num_map
        call cal_map_rendering_data                                     &
     &     (time_d, psf_dat(i_map)%psf_nod, psf_dat(i_map)%psf_ele,     &
     &      psf_dat(i_map)%psf_phys, color_param(i_map),                &
     &      cbar_param(i_map), map_data(i_map), pvr_rgb(i_map))
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+2)
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+3)
      do i_map = 1, num_map
        call sel_write_pvr_image_file(istep_psf, -1, pvr_rgb(i_map))
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+3)
!
      end subroutine output_map_file
!
!  ---------------------------------------------------------------------
!
      end module collect_psf_data
