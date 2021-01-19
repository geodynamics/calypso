!>@file   set_control_platform_data.F90
!!        module set_control_platform_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!!
!>@brief    Load file definitions from control structures
!!
!!@verbatim
!!      subroutine set_control_mesh_def(plt, mesh_file)
!!      subroutine set_control_sph_mesh(plt, Fmesh_ctl,                 &
!!     &         sph_file_param, mesh_file, sph_file_IO, FEM_mesh_flags)
!!        type(platform_data_control), intent(in) :: plt
!!        type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!!        type(field_IO_params), intent(inout) :: sph_file_param
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(field_IO_params), intent(inout) :: sph_file_IO
!!        type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!!
!!      subroutine set_control_restart_file_def(plt, file_IO)
!!        type(platform_data_control), intent(in) :: plt
!!        type(field_IO_params), intent(inout) :: file_IO
!!      subroutine set_control_mesh_file_def                            &
!!     &         (default_prefix, plt_ctl, mesh_file)
!!        type(platform_data_control), intent(in) :: plt_ctl
!!        type(field_IO_params), intent(inout) :: mesh_file
!!@endverbatim
!!
!!@param id_rank  preocess ID
!
      module set_control_platform_data
!
      use m_precision
!
      use m_constants
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_file_IO_parameter
!
      implicit  none
!
      character(len=kchara), parameter :: default_sph_prefix = 'in'
      character(len=kchara), parameter :: default_rst_prefix = 'rst'
      private :: default_sph_prefix, default_rst_prefix
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_minimum_fem_platform_def(id_rank, plt, Fmesh_ctl,  &
     &          mesh_file, iflag_output_SURF)
!
      use m_file_format_switch
      use set_control_platform_item
!
      integer, intent(in) :: id_rank
      type(platform_data_control), intent(in) :: plt
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!
      type(field_IO_params), intent(inout) :: mesh_file
      integer(kind = kint), intent(inout) :: iflag_output_SURF
!
!
      call turn_off_debug_flag_by_ctl(id_rank, plt)
      call set_control_smp_def(id_rank, plt)
      call set_control_mesh_def(plt, mesh_file)
      call set_FEM_surface_output_flag(Fmesh_ctl, iflag_output_SURF)
      if(iflag_debug.gt.0) write(*,*)                                   &
     &   'mesh_file_head:  ', trim(mesh_file%file_prefix)
!
      end subroutine set_minimum_fem_platform_def
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_mesh_def(plt, mesh_file)
!
      use m_default_file_prefix
      use m_file_format_switch
      use set_control_platform_item
!
      type(platform_data_control), intent(in) :: plt
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_parallel_file_ctl_params(def_mesh_file_head,             &
     &    plt%mesh_file_prefix, plt%mesh_file_fmt_ctl, mesh_file)
!
      end subroutine set_control_mesh_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_sph_mesh(plt, Fmesh_ctl,                   &
     &         sph_file_param, mesh_file, sph_file_IO, FEM_mesh_flags)
!
      use m_file_format_switch
      use sph_file_IO_select
      use set_control_platform_item
!
      type(platform_data_control), intent(in) :: plt
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
      type(field_IO_params), intent(inout) :: sph_file_param
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: sph_file_IO
      type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!
!
      call set_control_mesh_def(plt, mesh_file)
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
!
      sph_file_IO%iflag_IO = plt%spectr_field_file_prefix%iflag
      if(sph_file_IO%iflag_IO .gt. 0) then
        sph_file_IO%file_prefix                                         &
     &         = plt%spectr_field_file_prefix%charavalue
      end if
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
! -----------------------------------------------------------------------
!
      subroutine set_control_restart_file_def(plt, file_IO)
!
      use t_file_IO_parameter
      use m_file_format_switch
      use set_control_platform_item
!
      type(platform_data_control), intent(in) :: plt
      type(field_IO_params), intent(inout) :: file_IO
!
!
      call set_parallel_file_ctl_params(default_rst_prefix,             &
     &    plt%restart_file_prefix, plt%restart_file_fmt_ctl,            &
     &    file_IO)
!
      end subroutine set_control_restart_file_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_mesh_file_def                              &
     &         (default_prefix, plt_ctl, mesh_file)
!
      use set_control_platform_item
!
      character(len=kchara), intent(in) :: default_prefix
      type(platform_data_control), intent(in) :: plt_ctl
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_file_control_params(default_prefix,                      &
     &    plt_ctl%mesh_file_prefix, plt_ctl%mesh_file_fmt_ctl,          &
     &    mesh_file)
!
      end subroutine set_control_mesh_file_def
!
! -----------------------------------------------------------------------
!
      end module set_control_platform_data
