!> @file  set_control_platform_data.F90
!!      module set_control_platform_data
!!
!!@author  H. Matsui
!!@date Programmed in Sep., 2021
!
!> @brief Stop program if zlib is not loaded
!!
!!@verbatim
!!      subroutine set_minimum_fem_platform(plt, Fmesh_ctl, mesh_file,  &
!!     &                                    iflag_output_SURF)
!!      subroutine set_control_parallel_mesh(plt, mesh_file)
!!        type(platform_data_control), intent(in) :: plt
!!        type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        integer(kind = kint), intent(inout) :: iflag_output_SURF
!!      subroutine set_control_restart_file_def(plt, file_IO)
!!        type(platform_data_control), intent(in) :: plt
!!        type(field_IO_params), intent(inout) :: file_IO
!!
!!      subroutine set_ctl_parallel_file_w_def(default_prefix,          &
!!     &          file_prefix_ctl, file_format_ctl, file_param)
!!        character(len=kchara), intent(in) :: default_prefix
!!        type(read_character_item), intent(in) :: file_prefix_ctl
!!        type(read_character_item), intent(in) :: file_format_ctl
!!        type(field_IO_params), intent(inout) :: file_param
!!
!!      subroutine set_control_sph_mesh(plt, Fmesh_ctl,                 &
!!     &         sph_file_param, mesh_file, sph_file_IO, FEM_mesh_flags)
!!        type(platform_data_control), intent(in) :: plt
!!        type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!!        type(field_IO_params), intent(inout) :: sph_file_param
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(field_IO_params), intent(inout) :: sph_file_IO
!!        type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!!@endverbatim
!
      module set_control_platform_data
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_ctl_data_4_platforms
      use t_file_IO_parameter
!
      implicit none
!
      character(len=kchara), parameter :: default_sph_prefix = 'in'
      character(len=kchara), parameter :: default_rst_prefix = 'rst'
      private :: default_sph_prefix
      private :: default_rst_prefix
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_minimum_fem_platform(plt, Fmesh_ctl, mesh_file,    &
     &                                    iflag_output_SURF)
!
      use t_ctl_data_4_FEM_mesh
      use m_file_format_switch
      use set_control_platform_item
!
      type(platform_data_control), intent(in) :: plt
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!
      type(field_IO_params), intent(inout) :: mesh_file
      integer(kind = kint), intent(inout) :: iflag_output_SURF
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_parallel_mesh(plt, mesh_file)
!
      call set_FEM_surface_output_flag(Fmesh_ctl, iflag_output_SURF)
      if(iflag_debug.gt.0) write(*,*)                                   &
     &   'mesh_file%file_prefix:  ', trim(mesh_file%file_prefix)
!
      end subroutine set_minimum_fem_platform
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_parallel_mesh(plt, mesh_file)
!
      use m_default_file_prefix
!
      type(platform_data_control), intent(in) :: plt
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_ctl_parallel_file_w_def(def_mesh_file_head,              &
     &    plt%mesh_file_prefix, plt%mesh_file_fmt_ctl, mesh_file)
!
      end subroutine set_control_parallel_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_control_restart_file_def(plt, file_IO)
!
      use m_file_format_switch
      use set_control_platform_item
!
      type(platform_data_control), intent(in) :: plt
      type(field_IO_params), intent(inout) :: file_IO
!
!
      call set_ctl_parallel_file_w_def(default_rst_prefix,              &
     &    plt%restart_file_prefix, plt%restart_file_fmt_ctl, file_IO)
!
      end subroutine set_control_restart_file_def
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ctl_parallel_file_w_def(default_prefix,            &
     &          file_prefix_ctl, file_format_ctl, file_param)
!
      use m_default_file_prefix
      use m_file_format_switch
      use set_control_platform_item
      use mpi_abort_by_missing_zlib
!
      character(len=kchara), intent(in) :: default_prefix
      type(read_character_item), intent(in) :: file_prefix_ctl
      type(read_character_item), intent(in) :: file_format_ctl
      type(field_IO_params), intent(inout) :: file_param
!
!
      call set_parallel_file_ctl_params(default_prefix,                 &
     &    file_prefix_ctl, file_format_ctl, file_param)
      call s_mpi_abort_by_missing_zlib(file_param%file_prefix,          &
     &                                 file_param%iflag_format)
!
      end subroutine set_ctl_parallel_file_w_def
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_sph_mesh(plt, Fmesh_ctl,                   &
     &         sph_file_param, mesh_file, sph_file_IO, FEM_mesh_flags)
!
      use m_file_format_switch
      use sph_file_IO_select
      use set_control_platform_item
      use mpi_abort_by_missing_zlib
!
      type(platform_data_control), intent(in) :: plt
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
      type(field_IO_params), intent(inout) :: sph_file_param
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: sph_file_IO
      type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!
!
      call set_control_parallel_mesh(plt, mesh_file)
!
!   set data format
!
      sph_file_param%iflag_format                                       &
     &     = choose_para_file_format(plt%sph_file_fmt_ctl)
      sph_file_IO%iflag_format                                          &
     &     = choose_para_file_format(plt%spectr_field_fmt_ctl)
!
!   set file header at once
!
      if(plt%sph_file_prefix%iflag .gt. 0) then
        sph_file_param%file_prefix =  plt%sph_file_prefix%charavalue
        call copy_mesh_format_and_prefix                                &
     &     (plt%sph_file_prefix%charavalue,                             &
     &      sph_file_param%iflag_format, mesh_file)
      else
        sph_file_param%file_prefix = default_sph_prefix
        mesh_file%file_prefix = default_sph_prefix
      end if
      call s_mpi_abort_by_missing_zlib(sph_file_param%file_prefix,      &
     &                                 sph_file_param%iflag_format)
!
      sph_file_IO%iflag_IO = plt%spectr_field_file_prefix%iflag
      if(sph_file_IO%iflag_IO .gt. 0) then
        sph_file_IO%file_prefix                                         &
     &         = plt%spectr_field_file_prefix%charavalue
      end if
      call s_mpi_abort_by_missing_zlib(sph_file_IO%file_prefix,         &
     &                                 sph_file_IO%iflag_format)
!
      call set_FEM_mesh_switch_4_SPH                                    &
     &    (Fmesh_ctl, FEM_mesh_flags%iflag_access_FEM)
      call set_FEM_surface_output_flag                                  &
     &    (Fmesh_ctl, FEM_mesh_flags%iflag_output_SURF)
      call set_FEM_viewer_output_flag                                   &
     &    (Fmesh_ctl, FEM_mesh_flags%iflag_output_VMESH)
!
      end subroutine set_control_sph_mesh
!
! ----------------------------------------------------------------------
!
      end module set_control_platform_data
