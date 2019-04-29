!>@file   set_control_platform_data.f90
!!@brief  module set_control_platform_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  Set file headers and number of processor and threds
!!        from control data
!!
!!@verbatim
!!      subroutine turn_off_debug_flag_by_ctl(id_rank, plt)
!!      subroutine set_control_smp_def(id_rank, plt)
!!      subroutine set_control_mesh_def(plt, mesh_file)
!!      subroutine set_control_sph_mesh(plt, Fmesh_ctl,                 &
!!     &          mesh_file, sph_file_param, FEM_mesh_flags)
!!        type(platform_data_control), intent(in) :: plt
!!        type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(field_IO_params), intent(inout) :: sph_file_param
!!        type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!!      subroutine set_FEM_mesh_switch_4_SPH(Fmesh_ctl, iflag_access_FEM)
!!      subroutine set_FEM_surface_output_flag                          &
!!     &         (Fmesh_ctl, iflag_output_SURF)
!!        type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!!      subroutine set_FEM_viewer_output_flag(plt, iflag_output_VMESH)
!!      subroutine set_control_restart_file_def(plt, file_IO)
!!        type(platform_data_control), intent(in) :: plt
!!        type(field_IO_params), intent(inout) :: file_IO
!!
!!      subroutine set_control_mesh_file_def                            &
!!     &         (default_prefix, plt_ctl, mesh_file)
!!      subroutine set_file_control_params(default_prefix,              &
!!     &          file_prefix_ctl, file_format_ctl,  file_params)
!!      subroutine set_parallel_file_ctl_params(default_prefix,         &
!!     &          file_prefix_ctl, file_format_ctl,  file_params)
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
      character(len=kchara), parameter :: default_rst_prefix = 'rst'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine turn_off_debug_flag_by_ctl(id_rank, plt)
!
      use m_machine_parameter
      use skip_comment_f
!
      integer, intent(in) :: id_rank
      type(platform_data_control), intent(in) :: plt
!
!
      i_debug = 0
      if(plt%debug_flag_ctl%iflag .gt. 0) then
        if     (no_flag(plt%debug_flag_ctl%charavalue)) then
          i_debug =     iflag_minimum_msg
        else if(yes_flag(plt%debug_flag_ctl%charavalue)) then
          i_debug =     iflag_routine_msg
        else if(cmp_no_case(plt%debug_flag_ctl%charavalue,'Full')       &
     &     .or. cmp_no_case(plt%debug_flag_ctl%charavalue,'2')   ) then
          i_debug =     iflag_full_msg
        end if
      end if
!
      if(id_rank .eq. 0) iflag_debug = i_debug
!
      end subroutine turn_off_debug_flag_by_ctl
!
! ----------------------------------------------------------------------
!
      subroutine set_control_smp_def(id_rank, plt)
!
      use m_machine_parameter
!
      integer, intent(in) :: id_rank
      type(platform_data_control), intent(in) :: plt
!
      integer, external :: omp_get_max_threads
      integer :: np_smp4
!
!
      np_smp = 1
      if(plt%num_smp_ctl%iflag .gt. 0) then
        np_smp = plt%num_smp_ctl%intvalue
      end if
!
#ifdef _OPENMP
      np_smp4 = int(np_smp)
      if(np_smp4 .lt. omp_get_max_threads()) then
        if(id_rank .eq. 0) write(*,*)                                   &
     &               'Number of SMP threads is chenged to', np_smp
        call omp_set_num_threads(np_smp4)
      end if
#endif
!
      end subroutine set_control_smp_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_mesh_def(plt, mesh_file)
!
      use m_default_file_prefix
      use m_file_format_switch
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
     &          mesh_file, sph_file_param, FEM_mesh_flags)
!
      use m_file_format_switch
      use sph_file_IO_select
!
      type(platform_data_control), intent(in) :: plt
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: sph_file_param
      type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!
!
      call set_control_mesh_def(plt, mesh_file)
!
!   set data format
!
      iflag_sph_file_fmt                                                &
     &     = choose_para_file_format(plt%sph_file_fmt_ctl)
      sph_file_param%iflag_format                                       &
     &     = choose_para_file_format(plt%spectr_field_fmt_ctl)
!
!   set file header at once
!
      if(plt%sph_file_prefix%iflag .gt. 0) then
        sph_file_head =  plt%sph_file_prefix%charavalue
        call copy_mesh_format_and_prefix                                &
     &     (plt%sph_file_prefix%charavalue,                             &
     &      iflag_sph_file_fmt, mesh_file)
      end if
!
      sph_file_param%iflag_IO = plt%spectr_field_file_prefix%iflag
      if(sph_file_param%iflag_IO .gt. 0) then
        sph_file_param%file_prefix                                      &
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
!
      subroutine set_FEM_mesh_switch_4_SPH(Fmesh_ctl, iflag_access_FEM)
!
      use skip_comment_f
!
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
      integer(kind = kint), intent(inout) :: iflag_access_FEM
!
!
      iflag_access_FEM = 0
      if(Fmesh_ctl%FEM_mesh_output_switch%iflag .gt. 0) then
        if(yes_flag(Fmesh_ctl%FEM_mesh_output_switch%charavalue)) then
          iflag_access_FEM = 1
        end if
      end if
!
      end subroutine set_FEM_mesh_switch_4_SPH
!
! ----------------------------------------------------------------------
!
      subroutine set_FEM_surface_output_flag                            &
     &         (Fmesh_ctl, iflag_output_SURF)
!
      use skip_comment_f
!
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
      integer(kind = kint), intent(inout) :: iflag_output_SURF
!
!
      iflag_output_SURF = 0
!
      if(Fmesh_ctl%FEM_surface_output_switch%iflag .eq. 0) return
      if(yes_flag(Fmesh_ctl%FEM_surface_output_switch%charavalue)) then
        iflag_output_SURF = 1
      end if
 
      end subroutine set_FEM_surface_output_flag
!
! ----------------------------------------------------------------------
!
      subroutine set_FEM_viewer_output_flag                             &
     &         (Fmesh_ctl, iflag_output_VMESH)
!
      use skip_comment_f
!
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
      integer(kind = kint), intent(inout) :: iflag_output_VMESH
!
!
      iflag_output_VMESH = 0
!
      if(Fmesh_ctl%FEM_viewer_output_switch%iflag .eq. 0) return
      if(yes_flag(Fmesh_ctl%FEM_viewer_output_switch%charavalue)) then
        iflag_output_VMESH = 1
      end if
 
      end subroutine set_FEM_viewer_output_flag
!
! ----------------------------------------------------------------------
!
      subroutine set_control_restart_file_def(plt, file_IO)
!
      use t_file_IO_parameter
      use m_file_format_switch
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
! -----------------------------------------------------------------------
!
      subroutine set_control_mesh_file_def                              &
     &         (default_prefix, plt_ctl, mesh_file)
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
      subroutine set_file_control_params(default_prefix,                &
     &          file_prefix_ctl, file_format_ctl,  file_params)
!
      use t_control_elements
      use m_file_format_switch
!
      character(len = kchara), intent(in) :: default_prefix
      type(read_character_item), intent(in) :: file_prefix_ctl
      type(read_character_item), intent(in) :: file_format_ctl
!
      type(field_IO_params), intent(inout) :: file_params
!
!
      file_params%iflag_IO = file_prefix_ctl%iflag
      if(file_params%iflag_IO .gt. 0) then
        file_params%file_prefix = file_prefix_ctl%charavalue
      else
        file_params%file_prefix = default_prefix
      end if
!
      file_params%iflag_format = choose_file_format(file_format_ctl)
!
      end subroutine set_file_control_params
!
! ----------------------------------------------------------------------
!
      subroutine set_parallel_file_ctl_params(default_prefix,           &
     &          file_prefix_ctl, file_format_ctl, file_params)
!
      use t_control_elements
      use m_file_format_switch
!
      character(len = kchara), intent(in) :: default_prefix
      type(read_character_item), intent(in) :: file_prefix_ctl
      type(read_character_item), intent(in) :: file_format_ctl
!
      type(field_IO_params), intent(inout) :: file_params
!
!
      file_params%iflag_IO = file_prefix_ctl%iflag
      if(file_params%iflag_IO .gt. 0) then
        file_params%file_prefix = file_prefix_ctl%charavalue
      else
        file_params%file_prefix = default_prefix
      end if
!
      file_params%iflag_format                                          &
     &     = choose_para_file_format(file_format_ctl)
!
      end subroutine set_parallel_file_ctl_params
!
! ----------------------------------------------------------------------
!
      end module set_control_platform_data
