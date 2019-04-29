!>@file   bcast_4_platform_ctl.f90
!!@brief  module bcast_4_platform_ctl
!!
!!@author H. Matsui
!!@date Programmed in June, 2016
!
!> @brief Broardcast input routine for data file headers
!!
!!@verbatim
!!      subroutine bcast_ctl_data_4_platform(plt)
!!        type(platform_data_control), intent(inout) :: plt
!!      subroutine bcast_FEM_mesh_control(Fmesh_ctl)
!!        type(FEM_mesh_control), intent(inout) :: Fmesh_ctl
!!@endverbatim
!
      module bcast_4_platform_ctl
!
      use m_precision
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_platform(plt)
!
      use t_ctl_data_4_platforms
      use bcast_control_arrays
!
      type(platform_data_control), intent(inout) :: plt
!
!
      call bcast_ctl_type_i1(plt%ndomain_ctl)
      call bcast_ctl_type_i1(plt%num_smp_ctl)
!
      call bcast_ctl_type_c1(plt%mesh_file_prefix)
!
      call bcast_ctl_type_c1(plt%field_file_prefix)
      call bcast_ctl_type_c1(plt%restart_file_prefix)
      call bcast_ctl_type_c1(plt%spectr_field_file_prefix)
!
      call bcast_ctl_type_c1(plt%sph_file_prefix)
!
      call bcast_ctl_type_c1(plt%coriolis_int_file_name)
      call bcast_ctl_type_c1(plt%bc_data_file_name_ctl)
!
      call bcast_ctl_type_c1(plt%interpolate_sph_to_fem_ctl)
      call bcast_ctl_type_c1(plt%interpolate_fem_to_sph_ctl)
!
      call bcast_ctl_type_c1(plt%mesh_file_fmt_ctl)
      call bcast_ctl_type_c1(plt%restart_file_fmt_ctl)
      call bcast_ctl_type_c1(plt%field_file_fmt_ctl)
      call bcast_ctl_type_c1(plt%sph_file_fmt_ctl)
      call bcast_ctl_type_c1(plt%itp_file_fmt_ctl)
      call bcast_ctl_type_c1(plt%spectr_field_fmt_ctl)
      call bcast_ctl_type_c1(plt%coriolis_file_fmt_ctl)
!
      call bcast_ctl_type_c1(plt%debug_flag_ctl)
!
      call bcast_ctl_type_c1(plt%del_org_data_ctl)
!
      end subroutine bcast_ctl_data_4_platform
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_FEM_mesh_control(Fmesh_ctl)
!
      use t_ctl_data_4_FEM_mesh
      use bcast_control_arrays
!
      type(FEM_mesh_control), intent(inout) :: Fmesh_ctl
!
!
      call bcast_ctl_type_c1(Fmesh_ctl%memory_conservation_ctl)
      call bcast_ctl_type_c1(Fmesh_ctl%FEM_mesh_output_switch)
      call bcast_ctl_type_c1(Fmesh_ctl%FEM_surface_output_switch)
      call bcast_ctl_type_c1(Fmesh_ctl%FEM_viewer_output_switch)
!
      call bcast_ctl_type_i1(Fmesh_ctl%FEM_sleeve_level_ctl)
!
      end subroutine bcast_FEM_mesh_control
!
!  ---------------------------------------------------------------------
!
      end module  bcast_4_platform_ctl
