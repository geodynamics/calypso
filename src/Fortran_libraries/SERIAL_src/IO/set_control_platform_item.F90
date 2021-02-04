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
!!      subroutine set_parallel_file_ctl_params(default_prefix,         &
!!     &          file_prefix_ctl, file_format_ctl,  file_params)
!!      subroutine set_file_control_params(default_prefix,              &
!!     &          file_prefix_ctl, file_format_ctl,  file_params)
!!        type(read_character_item), intent(in) :: file_prefix_ctl
!!        type(read_character_item), intent(in) :: file_format_ctl
!!        type(field_IO_params), intent(inout) :: file_params
!!
!!      subroutine set_FEM_mesh_switch_4_SPH(Fmesh_ctl, iflag_access_FEM)
!!      subroutine set_FEM_surface_output_flag                          &
!!     &         (Fmesh_ctl, iflag_output_SURF)
!!        type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!!      subroutine set_FEM_viewer_output_flag(plt, iflag_output_VMESH)
!!        type(platform_data_control), intent(in) :: plt
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
      end module set_control_platform_item
