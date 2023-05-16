!>@file   set_control_platform_item.F90
!!        module set_control_platform_item
!!
!!@author H. Matsui
!!@date Programmed in 2009
!!
!>@brief  Set file headers and number of processor and threads
!!        from control data
!!
!!@verbatim
!!      subroutine turn_off_debug_flag_by_ctl(id_rank, plt)
!!      subroutine set_control_smp_def(id_rank, plt)
!!        type(platform_data_control), intent(in) :: plt
!!        type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!!        type(field_IO_params), intent(inout) :: sph_file_param
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(field_IO_params), intent(inout) :: sph_file_IO
!!        type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!!
!!      subroutine set_control_file_def(plt, file_param)
!!        type(platform_data_control), intent(in) :: plt
!!        type(field_IO_params), intent(inout) :: file_param
!!
!!      subroutine set_parallel_file_ctl_params(default_prefix,         &
!!     &          file_prefix_ctl, file_format_ctl,  file_params)
!!      subroutine set_file_control_params(default_prefix,              &
!!     &          file_prefix_ctl, file_format_ctl,  file_params)
!!        type(read_character_item), intent(in) :: file_prefix_ctl
!!        type(read_character_item), intent(in) :: file_format_ctl
!!        type(field_IO_params), intent(inout) :: file_params
!!
!!      logical function FEM_mesh_switch_4_SPH(Fmesh_ctl)
!!      logical function FEM_surface_output_switch(Fmesh_ctl)
!!        type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!!      logical function FEM_viewer_output_flag(plt)
!!        type(platform_data_control), intent(in) :: plt
!!
!!      logical function yes_no_flag_from_ctl(yes_no_ctl)
!!        type(read_character_item) :: yes_no_ctl
!!        type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!!@endverbatim
!!
!!@param id_rank  preocess ID
!
      module set_control_platform_item
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
#ifdef _OPENMP
      integer, external :: omp_get_max_threads
      integer :: np_smp4
#endif
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
! -----------------------------------------------------------------------
!
      subroutine set_control_file_def(default_prefix,                   &
     &          file_prefix_ctl, file_format_ctl, file_param)
!
      use m_default_file_prefix
      use m_file_format_switch
      use stop_by_missing_zlib
!
      character(len=kchara), intent(in) :: default_prefix
      type(read_character_item), intent(in) :: file_prefix_ctl
      type(read_character_item), intent(in) :: file_format_ctl
      type(field_IO_params), intent(inout) :: file_param
!
!
      call set_file_control_params(default_prefix,                      &
     &    file_prefix_ctl, file_format_ctl, file_param)
      call s_stop_by_missing_zlib(file_param%file_prefix,               &
     &                            file_param%iflag_format)
!
      end subroutine set_control_file_def
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_parallel_file_ctl_params(default_prefix,           &
     &          file_prefix_ctl, file_format_ctl, file_params)
!
      use t_control_array_character
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
      subroutine set_file_control_params(default_prefix,                &
     &          file_prefix_ctl, file_format_ctl,  file_params)
!
      use t_control_array_character
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
! ----------------------------------------------------------------------
!
      logical function FEM_mesh_switch_4_SPH(Fmesh_ctl)
!
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!
      FEM_mesh_switch_4_SPH                                             &
     &      = yes_no_flag_from_ctl(Fmesh_ctl%FEM_mesh_output_switch)
!
      end function FEM_mesh_switch_4_SPH
!
! ----------------------------------------------------------------------
!
      logical function FEM_surface_output_switch(Fmesh_ctl)
!
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!
      FEM_surface_output_switch                                         &
     &      = yes_no_flag_from_ctl(Fmesh_ctl%FEM_surface_output_switch)
!
      end function FEM_surface_output_switch
!
! ----------------------------------------------------------------------
!
      logical function FEM_viewer_output_flag(Fmesh_ctl)
!
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!
      FEM_viewer_output_flag                                            &
     &      = yes_no_flag_from_ctl(Fmesh_ctl%FEM_viewer_output_switch)
!
      end function FEM_viewer_output_flag
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      logical function yes_no_flag_from_ctl(yes_no_ctl)
!
      use t_control_array_character
      use skip_comment_f
!
      type(read_character_item), intent(in) :: yes_no_ctl
!
!
      yes_no_flag_from_ctl = .FALSE.
!
      if(yes_no_ctl%iflag .eq. 0) return
      if(yes_flag(yes_no_ctl%charavalue)) yes_no_flag_from_ctl = .TRUE.
 
      end function yes_no_flag_from_ctl
!
! ----------------------------------------------------------------------
!
      end module set_control_platform_item
