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
!!      subroutine turn_off_debug_flag_by_ctl(my_rank)
!!      subroutine set_control_smp_def(my_rank)
!!      subroutine set_control_mesh_def
!!      subroutine set_control_sph_mesh
!!      subroutine set_control_restart_file_def(fld_IO)
!!@endverbatim
!!
!!@param my_rank  preocess ID
!
      module set_control_platform_data
!
      use m_precision
!
      use m_constants
      use m_ctl_data_4_platforms
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine turn_off_debug_flag_by_ctl(my_rank)
!
      use m_machine_parameter
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      i_debug = 0
      if(debug_flag_ctl%iflag .gt. 0) then
        if     (no_flag(debug_flag_ctl%charavalue)) then
          i_debug =     iflag_minimum_msg
        else if(yes_flag(debug_flag_ctl%charavalue)) then
          i_debug =     iflag_routine_msg
        else if(cmp_no_case(debug_flag_ctl%charavalue,'Full')           &
     &     .or. cmp_no_case(debug_flag_ctl%charavalue,'2')   ) then
          i_debug =     iflag_full_msg
        end if
      end if
!
      if(my_rank .eq. 0) iflag_debug = i_debug
!
      end subroutine turn_off_debug_flag_by_ctl
!
! ----------------------------------------------------------------------
!
      subroutine set_control_smp_def(my_rank)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer, external :: omp_get_max_threads
      integer :: np_smp4
!
!
      np_smp = 1
      if(num_smp_ctl%iflag .gt. 0) np_smp = num_smp_ctl%intvalue
!
#ifdef _OPENMP
      if (int(np_smp) .lt. omp_get_max_threads()) then
        if(my_rank .eq. 0) write(*,*)                                   &
     &               'Number of SMP threads is chenged to', np_smp
        np_smp4 = int(np_smp)
        call omp_set_num_threads(np_smp4)
      end if
#endif
!
      end subroutine set_control_smp_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_mesh_def
!
      use m_read_mesh_data
      use m_file_format_switch
      use skip_comment_f
!
!
      if(mesh_extension_ctl%iflag .gt. 0) then
        if     (cmp_no_case(mesh_extension_ctl%charavalue,'Off')        &
     &     .or. cmp_no_case(mesh_extension_ctl%charavalue,'0') ) then
          iflag_mesh_file_ext = 0
        end if
      end if
!
      if (mesh_file_prefix%iflag .gt. 0) then
        mesh_file_head = mesh_file_prefix%charavalue
      else
        mesh_file_head = def_mesh_file_head
      end if
!
!   set data format
!
      call choose_file_format(mesh_file_fmt_ctl, iflag_mesh_file_fmt)
!
      end subroutine set_control_mesh_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_sph_mesh
!
      use m_read_mesh_data
      use m_control_params_sph_data
      use m_node_id_spherical_IO
      use m_file_format_switch
!
!   set data format
!
      call choose_file_format(sph_file_fmt_ctl, iflag_sph_file_fmt)
      call choose_file_format                                           &
     &   (spectr_file_fmt_ctl, iflag_sph_spectr_fmt)
!
!   set file header at once
!
      if(sph_file_prefix%iflag .gt. 0) then
        sph_file_head =  sph_file_prefix%charavalue
        mesh_file_head = sph_file_prefix%charavalue
        iflag_mesh_file_ext = 1
        iflag_mesh_file_fmt = iflag_sph_file_fmt
      end if
!
      iflag_sph_spec_output = spectr_file_head_ctl%iflag
      if(iflag_sph_spec_output .gt. 0) then
        spectr_file_head = spectr_file_head_ctl%charavalue
      end if
!
      end subroutine set_control_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine set_control_restart_file_def(fld_IO)
!
      use t_field_data_IO
      use m_file_format_switch
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      if (restart_file_prefix%iflag .gt. 0) then
        fld_IO%file_prefix = restart_file_prefix%charavalue
      end if
!
      call choose_para_file_format                                      &
     &   (restart_file_fmt_ctl, fld_IO%iflag_file_fmt)
!
      end subroutine set_control_restart_file_def
!
! -----------------------------------------------------------------------
!
      end module set_control_platform_data
