!>@file   t_mid_equator_control.f90
!!        module t_mid_equator_control
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!
!> @brief control date for volume averaged spectr data
!!
!!@verbatim
!!      subroutine reset_mid_equator_control(meq_ctl)
!!        type(mid_equator_control), intent(inout) :: meq_ctl
!!      subroutine read_mid_eq_monitor_ctl                              &
!!     &         (id_control, hd_block, iflag, meq_ctl, c_buf)
!!        type(mid_equator_control), intent(inout) :: meq_ctl
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin mid_equator_monitor_ctl
!!    pick_circle_coord_ctl         spherical
!!    nphi_mid_eq_ctl               500
!!    pick_cylindrical_radius_ctl   0.75
!!    pick_vertical_position_ctl    0.6
!!  end mid_equator_monitor_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_mid_equator_control
!
      use m_precision
!
      use t_read_control_elements
      use t_control_elements
      use t_control_array_integer
      use skip_comment_f
!
      implicit  none
!
      type mid_equator_control
!>        Structure for coordiniate system for circled data
        type(read_character_item) :: pick_circle_coord_ctl
!
!>        Structure for Number of zonal points for benchamek check
        type(read_integer_item) :: nphi_mid_eq_ctl
!
!>        Structure for position for s
        type(read_real_item) :: pick_s_ctl
!
!>        Structure for position for z
        type(read_real_item) :: pick_z_ctl
      end type mid_equator_control
!
!
!   labels for item
!
      character(len=kchara), parameter                                  &
     &            :: hd_nphi_mid_eq = 'nphi_mid_eq_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_s_ctl = 'pick_cylindrical_radius_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_z_ctl =  'pick_vertical_position_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_circle_coord = 'pick_circle_coord_ctl'
!
      private :: hd_nphi_mid_eq, hd_pick_s_ctl, hd_pick_z_ctl
      private :: hd_circle_coord
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine reset_mid_equator_control(meq_ctl)
!
      type(mid_equator_control), intent(inout) :: meq_ctl
!
      meq_ctl%pick_circle_coord_ctl%iflag = 0
      meq_ctl%nphi_mid_eq_ctl%iflag = 0
      meq_ctl%pick_s_ctl%iflag =    0
      meq_ctl%pick_z_ctl%iflag =    0
!
      end subroutine reset_mid_equator_control
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_mid_eq_monitor_ctl                                &
     &         (id_control, hd_block, iflag, meq_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(mid_equator_control), intent(inout) :: meq_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (iflag .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pick_s_ctl, meq_ctl%pick_s_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pick_z_ctl, meq_ctl%pick_z_ctl)
!
        call read_integer_ctl_type(c_buf, hd_nphi_mid_eq,               &
     &      meq_ctl%nphi_mid_eq_ctl)
!
        call read_chara_ctl_type(c_buf, hd_circle_coord,                &
     &      meq_ctl%pick_circle_coord_ctl)
      end do
      iflag = 1
!
      end subroutine read_mid_eq_monitor_ctl
!
! -----------------------------------------------------------------------
!
      end module t_mid_equator_control
