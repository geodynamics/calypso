!>@file   bcast_4_sphere_ctl.f90
!!@brief  module bcast_4_sphere_ctl
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
!!      subroutine bcast_ctl_4_shell_define(spctl)
!!        type(sphere_data_control), intent(inout) :: spctl
!!      subroutine bcast_ctl_ndomain_4_shell(sdctl)
!!        type(sphere_domain_control), intent(inout) :: sdctl
!!@endverbatim
!
      module bcast_4_sphere_ctl
!
      use m_precision
      use t_ctl_data_4_sphere_model
      use t_ctl_data_4_divide_sphere
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_ctl_4_shell_define(spctl)
!
      use bcast_control_arrays
!
      type(sphere_data_control), intent(inout) :: spctl
!
!
      call bcast_ctl_array_ir(spctl%radius_ctl)
!
      call bcast_ctl_array_ci(spctl%radial_grp_ctl)
!
!
      call bcast_ctl_type_c1(spctl%sph_coef_type_ctl)
      call bcast_ctl_type_c1(spctl%sph_grid_type_ctl)
      call bcast_ctl_type_c1(spctl%radial_grid_type_ctl)
!
      call bcast_ctl_type_i1(spctl%phi_symmetry_ctl)
      call bcast_ctl_type_i1(spctl%ltr_ctl)
      call bcast_ctl_type_i1(spctl%ngrid_elevation_ctl)
      call bcast_ctl_type_i1(spctl%ngrid_azimuth_ctl)
!
      call bcast_ctl_type_i1(spctl%num_fluid_grid_ctl)
!
!
      call bcast_ctl_type_r1(spctl%Min_radius_ctl)
      call bcast_ctl_type_r1(spctl%ICB_radius_ctl)
      call bcast_ctl_type_r1(spctl%CMB_radius_ctl)
      call bcast_ctl_type_r1(spctl%Max_radius_ctl)
!
      call bcast_ctl_type_r1(spctl%fluid_core_size_ctl)
      call bcast_ctl_type_r1(spctl%ICB_to_CMB_ratio_ctl)
!
      call bcast_ctl_type_i1(spctl%num_radial_layer_ctl)
      call bcast_ctl_type_i1(spctl%num_med_layer_ctl)
!
      call bcast_ctl_array_i2(spctl%radial_layer_list_ctl)
      call bcast_ctl_array_i2(spctl%med_layer_list_ctl)
!
      end subroutine bcast_ctl_4_shell_define
!
!   --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_ctl_ndomain_4_shell(sdctl)
!
      use bcast_control_arrays
!
      type(sphere_domain_control), intent(inout) :: sdctl
!
!
      call bcast_ctl_type_c1(sdctl%inner_decomp_ctl)
!
      call bcast_ctl_type_i1(sdctl%num_radial_domain_ctl)
      call bcast_ctl_type_i1(sdctl%num_horiz_domain_ctl)
!
      call bcast_ctl_array_ci(sdctl%ndomain_sph_grid_ctl)
      call bcast_ctl_array_ci(sdctl%ndomain_legendre_ctl)
      call bcast_ctl_array_ci(sdctl%ndomain_spectr_ctl)
!
      end subroutine bcast_ctl_ndomain_4_shell
!
!  ---------------------------------------------------------------------
!
      end module bcast_4_sphere_ctl
