!>@file   bcast_4_sphere_ctl.f90
!!@brief  module bcast_4_sphere_ctl
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
!!      subroutine bcast_parallel_shell_ctl(psph_ctl)
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!
!!      subroutine bcast_ctl_4_shell_define(spctl)
!!        type(sphere_data_control), intent(inout) :: spctl
!!      subroutine bcast_ctl_ndomain_4_shell(sdctl)
!!        type(sphere_domain_control), intent(inout) :: sdctl
!!@endverbatim
!
      module bcast_4_sphere_ctl
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_ctl_4_shell_define
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_parallel_shell_ctl(psph_ctl)
!
      use t_ctl_data_gen_sph_shell
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_4_platform_ctl
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!
!
      call calypso_mpi_bcast_character(psph_ctl%block_name,             &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(psph_ctl%iflag_sph_shell, 0)
!
      if(psph_ctl%iflag_sph_shell .eq. 0) return
!
      call bcast_FEM_mesh_control(psph_ctl%Fmesh_ctl)
      call bcast_ctl_4_shell_define(psph_ctl%spctl)
      call bcast_ctl_ndomain_4_shell(psph_ctl%sdctl)
!
      end subroutine bcast_parallel_shell_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_4_shell_define(spctl)
!
      use t_ctl_data_4_sphere_model
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(sphere_data_control), intent(inout) :: spctl
!
!
      call bcast_ctl_array_ir(spctl%radius_ctl)
      call bcast_ctl_array_ci(spctl%radial_grp_ctl)
      call bcast_ctl_array_r1(spctl%add_ext_layer_ctl)
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
      call bcast_ctl_type_i1(spctl%increment_cheby_ctl)
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
      call calypso_mpi_bcast_character(spctl%block_name,                &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(spctl%i_shell_def, 0)
!
      end subroutine bcast_ctl_4_shell_define
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_ctl_ndomain_4_shell(sdctl)
!
      use t_ctl_data_4_divide_sphere
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(sphere_domain_control), intent(inout) :: sdctl
!
!
      call bcast_ctl_type_c1(sdctl%inner_decomp_ctl)
!
      call bcast_ctl_type_c1(sdctl%indices_ordering_set)
!
      call bcast_ctl_type_c1(sdctl%rj_inner_loop_ctl)
      call bcast_ctl_type_c1(sdctl%rlm_inner_loop_ctl)
      call bcast_ctl_type_c1(sdctl%rtm_inner_loop_ctl)
      call bcast_ctl_type_c1(sdctl%rtp_inner_loop_ctl)
!
      call bcast_ctl_type_c1(sdctl%rlm_distibution_ctl)
      call bcast_ctl_type_c1(sdctl%simple_r_decomp_ctl)
!
      call bcast_ctl_type_i1(sdctl%num_radial_domain_ctl)
      call bcast_ctl_type_i1(sdctl%num_horiz_domain_ctl)
!
      call bcast_ctl_array_ci(sdctl%ndomain_sph_grid_ctl)
      call bcast_ctl_array_ci(sdctl%ndomain_legendre_ctl)
      call bcast_ctl_array_ci(sdctl%ndomain_spectr_ctl)
!
      call calypso_mpi_bcast_character(sdctl%block_name,                &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(sdctl%i_domains_sph, 0)
!
      end subroutine bcast_ctl_ndomain_4_shell
!
!  ---------------------------------------------------------------------
!
      end module bcast_4_sphere_ctl
