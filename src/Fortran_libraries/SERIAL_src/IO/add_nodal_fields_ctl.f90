!>@file   add_nodal_fields_ctl.f90
!!@brief  module add_nodal_fields_ctl
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Add require field name
!!
!!@verbatim
!!      logical function check_field_list_ctl(field, field_ctl)
!!      subroutine add_phys_name_ctl(field, field_ctl)
!!      subroutine add_viz_name_ctl(my_rank, field_name, field_ctl)
!!        type(field_def), intent(in) :: field
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!      logical function check_vis_control_flag(visualize_ctl)
!!      logical function check_monitor_control_flag(monitor_ctl)
!!
!!      character(len = kchara) function set_vis_control_flag(iflag_viz)
!!      character(len = kchara) function set_monitor_control_flag       &
!!     &                               (iflag_fld_monitor)
!!
!!      integer(kind = kint) function num_ctl_flag_visualize_field()
!!      integer(kind = kint) function num_ctl_flag_monitored_field()
!!      subroutine set_ctl_flag_visualize_field(names)
!!      subroutine set_ctl_flag_monitored_field(names)
!!@endverbatim
!
      module add_nodal_fields_ctl
!
      use m_precision
      use m_machine_parameter
      use t_field_labels
      use t_control_array_character3
!
      implicit  none
!
      integer(kind = kint), parameter :: n_label_viz_flag = 2
      character(len = kchara), parameter :: cflag_viz_on =  'Viz_On'
      character(len = kchara), parameter :: cflag_viz_off = 'Viz_Off'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_monitor_on =  'Monitor_On'
      character(len = kchara), parameter                                &
     &                        :: cflag_monitor_off = 'Monitor_Off'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      logical function check_field_list_ctl(field, field_ctl)
!
      type(field_def), intent(in) :: field
      type(ctl_array_c3), intent(in) :: field_ctl
!
      integer(kind = kint) :: i
!
!
      check_field_list_ctl = .TRUE.
      do i = 1, field_ctl%num
        if (field_ctl%c1_tbl(i) .eq. field%name) return
      end do
      check_field_list_ctl = .FALSE.
!
!      if(iflag_debug .gt. 0) then
!        write(*,*) trim(field%name), ' is missing in the field list.'
!      end if
!
      end function check_field_list_ctl
!
! -----------------------------------------------------------------------
!
      subroutine add_phys_name_ctl(field, field_ctl)
!
      type(field_def), intent(in) :: field
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      type(read_chara3_item) :: field_tmp_ctl
!
!
      if(check_field_list_ctl(field, field_ctl)) return
!
      field_tmp_ctl%iflag = 1
      field_tmp_ctl%charavalue(1) = trim(field%name)
      field_tmp_ctl%charavalue(2) = cflag_viz_off
      field_tmp_ctl%charavalue(3) = cflag_monitor_off
!
      call append_control_array_c3(field_tmp_ctl, field_ctl)
!
      if(iflag_debug .gt. 0) then
        write(*,*) trim(field_ctl%c1_tbl(field_ctl%num) ),              &
     &            ' is added at field ID ',   field_ctl%num
      end if
!
      end subroutine add_phys_name_ctl
!
! -----------------------------------------------------------------------
!
      subroutine add_viz_name_ctl(my_rank, field_name, field_ctl)
!
      integer, intent(in) :: my_rank
      character(len=kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      type(read_chara3_item) :: field_tmp_ctl
!
      integer(kind = kint) :: i
!
!
      do i = 1, field_ctl%num
        if(field_ctl%c1_tbl(i) .eq. field_name) then
          if(field_ctl%c2_tbl(i) .ne. cflag_viz_on) then
            field_ctl%c2_tbl(i) = cflag_viz_on
            if(my_rank .gt. 0) then
              write(*,*) 'Visualization flag is turned on for ',        &
     &                   trim(field_ctl%c1_tbl(i)), ' (', i, ')'
            end if
          end if
          return
        end if
      end do
!
      field_tmp_ctl%iflag = 1
      field_tmp_ctl%charavalue(1) = trim(field_name)
      field_tmp_ctl%charavalue(2) = cflag_viz_on
      field_tmp_ctl%charavalue(3) = cflag_monitor_off
!
      call append_control_array_c3(field_tmp_ctl, field_ctl)
!
      if(iflag_debug .gt. 0) then
        write(*,*) trim(field_ctl%c1_tbl(field_ctl%num) ),              &
     &            ' is added for viz at field ID ',   field_ctl%num
      end if
!
      end subroutine add_viz_name_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function check_vis_control_flag(visualize_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: visualize_ctl
!
      check_vis_control_flag = cmp_no_case(visualize_ctl, cflag_viz_on)
!
      end function check_vis_control_flag
!
! -----------------------------------------------------------------------
!
      logical function check_monitor_control_flag(monitor_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: monitor_ctl
!
      check_monitor_control_flag                                        &
     &      = cmp_no_case(monitor_ctl, cflag_monitor_on)
!
      end function check_monitor_control_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      character(len = kchara) function set_vis_control_flag(iflag_viz)
!
      integer (kind = kint), intent(in) :: iflag_viz
!
      if(iflag_viz .gt. 0) then
        set_vis_control_flag = cflag_viz_on
      else
        set_vis_control_flag = cflag_viz_off
      end if
!
      end function set_vis_control_flag
!
! -----------------------------------------------------------------------
!
      character(len = kchara) function set_monitor_control_flag         &
     &                               (iflag_fld_monitor)
!
      integer (kind = kint), intent(in) :: iflag_fld_monitor
!
      if(iflag_fld_monitor .gt. 0) then
        set_monitor_control_flag = cflag_monitor_on
      else
        set_monitor_control_flag = cflag_monitor_off
      end if
!
      end function set_monitor_control_flag
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_ctl_flag_visualize_field()
      num_ctl_flag_visualize_field = n_label_viz_flag
      return
      end function num_ctl_flag_visualize_field
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_ctl_flag_monitored_field()
      num_ctl_flag_monitored_field = n_label_viz_flag
      return
      end function num_ctl_flag_monitored_field
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_flag_visualize_field(names)
!
      use t_read_control_elements
!
      character(len = kchara), intent(inout) :: names(n_label_viz_flag)
!
!
      call set_control_labels(cflag_viz_on,  names( 1))
      call set_control_labels(cflag_viz_off, names( 2))
!
      end subroutine set_ctl_flag_visualize_field
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_flag_monitored_field(names)
!
      use t_read_control_elements
!
      character(len = kchara), intent(inout) :: names(n_label_viz_flag)
!
!
      call set_control_labels(cflag_monitor_on,  names( 1))
      call set_control_labels(cflag_monitor_off, names( 2))
!
      end subroutine set_ctl_flag_monitored_field
!
! ----------------------------------------------------------------------
!
      end module add_nodal_fields_ctl
