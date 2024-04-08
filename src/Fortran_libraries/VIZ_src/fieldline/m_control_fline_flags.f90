!>@file   m_control_fline_flags.f90
!!@brief  module m_control_fline_flags
!!
!!@date  Programmed by H.Matsui in Aug. 2011
!
!>@brief control parameters for each field line
!!
!!@verbatim
!!      subroutine fline_start_label_array(array_c)
!!      subroutine fline_direction_label_array(array_c)
!!      subroutine fline_seeds_label_array(array_c)
!!!        type(ctl_array_chara), intent(inout) :: array_c
!!@endverbatim
!
      module m_control_fline_flags
!
      use m_precision
!
      implicit  none
!
      character(len = kchara), parameter                                &
     &               :: cflag_surface_group = 'surface_group'
      character(len = kchara), parameter                                &
     &               :: cflag_surface_list =  'surface_list'
      character(len = kchara), parameter                                &
     &               :: cflag_position_list = 'position_list'
      character(len = kchara), parameter                                &
     &               :: cflag_spray_in_domain = 'spray_in_domain'
!
      character(len = kchara), parameter                                &
     &               :: cflag_forward_trace =  'forward'
      character(len = kchara), parameter                                &
     &               :: cflag_backward_trace = 'backward'
      character(len = kchara), parameter                                &
     &               :: cflag_both_trace =     'both'
!
      character(len = kchara), parameter                                &
     &               :: cflag_random_by_amp =  'amplitude'
      character(len = kchara), parameter                                &
     &               :: cflag_random_by_area = 'area_size'
      character(len = kchara), parameter                                &
     &               :: cflag_no_random =      'no_random'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine fline_start_label_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(cflag_surface_group,   array_c)
      call append_c_to_ctl_array(cflag_surface_list,    array_c)
      call append_c_to_ctl_array(cflag_position_list,   array_c)
      call append_c_to_ctl_array(cflag_spray_in_domain, array_c)
!
      end subroutine fline_start_label_array
!
! ----------------------------------------------------------------------
!
      subroutine fline_direction_label_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(cflag_forward_trace,  array_c)
      call append_c_to_ctl_array(cflag_backward_trace, array_c)
      call append_c_to_ctl_array(cflag_both_trace,     array_c)
!
      end subroutine fline_direction_label_array
!
! ----------------------------------------------------------------------
!
      subroutine fline_seeds_label_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(cflag_random_by_amp,  array_c)
      call append_c_to_ctl_array(cflag_random_by_area, array_c)
      call append_c_to_ctl_array(cflag_no_random,      array_c)
!
      end subroutine fline_seeds_label_array
!
! ----------------------------------------------------------------------
!
      end module m_control_fline_flags
