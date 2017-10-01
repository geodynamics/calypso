!>@file   set_controls_4_sph_shell.f90
!!@brief  module set_controls_4_sph_shell
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Constants for spectr data
!!
!!@verbatim
!!      subroutine set_FEM_mesh_mode_4_SPH(spctl, sph_params)
!!        type(sphere_data_control), intent(in) :: spctl
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!@endverbatim
!
      module set_controls_4_sph_shell
!
      use m_precision
      use m_machine_parameter
!
      use t_ctl_data_4_sphere_model
      use t_spheric_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_FEM_mesh_mode_4_SPH(spctl, sph_params)
!
      use m_spheric_constants
      use skip_comment_f
!
      type(sphere_data_control), intent(in) :: spctl
      type(sph_shell_parameters), intent(inout) :: sph_params
!
!
      if(spctl%sph_grid_type_ctl%iflag .gt. 0) then
        if(cmp_no_case(spctl%sph_grid_type_ctl%charavalue,              &
     &                 label_MESH_same))                                &
     &          sph_params%iflag_shell_mode = iflag_MESH_same
        if(cmp_no_case(spctl%sph_grid_type_ctl%charavalue,              &
     &                 label_MESH_pole))                                &
     &          sph_params%iflag_shell_mode = iflag_MESH_w_pole
        if(cmp_no_case(spctl%sph_grid_type_ctl%charavalue,              &
     &                 label_MESH_ctr))                                 &
     &          sph_params%iflag_shell_mode = iflag_MESH_w_center
        if(cmp_no_case(spctl%sph_grid_type_ctl%charavalue,              &
     &                 label_no_FEMMESH))                               &
     &          sph_params%iflag_shell_mode = iflag_no_FEMMESH
      else
        sph_params%iflag_shell_mode = iflag_no_FEMMESH
      end if
      if(iflag_debug .gt. 0) then
        write(*,*) 'iflag_shell_mode', sph_params%iflag_shell_mode
      end if
!
      end subroutine set_FEM_mesh_mode_4_SPH
!
! -----------------------------------------------------------------------
!
      end module set_controls_4_sph_shell
