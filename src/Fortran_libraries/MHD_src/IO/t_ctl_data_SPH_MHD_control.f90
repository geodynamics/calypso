!>@file   t_ctl_data_SPH_MHD_control.f90
!!@brief  module t_ctl_data_SPH_MHD_control
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_sph_mhd_control(hd_block, iflag, smctl_ctl)
!!      subroutine bcast_sph_mhd_control(smctl_ctl)
!!        type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!!@endverbatim
!
      module t_ctl_data_SPH_MHD_control
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_time_steps
      use t_ctl_data_mhd_evo_scheme
!
      use skip_comment_f
!
      implicit none
!
      type sph_mhd_control_control
!>        Structure for time stepping control
        type(time_data_control) :: tctl
!>        Structure for restart flag
        type(mhd_restart_control) :: mrst_ctl
!>        Structures for time integration controls
        type(mhd_evo_scheme_control) :: mevo_ctl
      end type sph_mhd_control_control
!
!    label for entry of group
!
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_restart_file =   'restart_file_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_time_loop =      'time_loop_ctl'
!
      integer (kind=kint) :: i_tstep =      0
      integer (kind=kint) :: i_restart_file =   0
      integer (kind=kint) :: i_time_loop =      0
!
!
      private :: hd_time_step, i_tstep
      private :: hd_restart_file, i_restart_file
      private :: hd_time_loop, i_time_loop
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_control(hd_block, iflag, smctl_ctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_block, iflag)
        if(iflag .gt. 0) exit
!
!
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, smctl_ctl%tctl)
        call read_restart_ctl                                           &
     &     (hd_restart_file, i_restart_file, smctl_ctl%mrst_ctl)
!
        call read_time_loop_ctl                                         &
     &     (hd_time_loop, i_time_loop, smctl_ctl%mevo_ctl)
      end do
!
      end subroutine read_sph_mhd_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_control(smctl_ctl)
!
      use bcast_4_time_step_ctl
!
      type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!
!
      call bcast_restart_ctl(smctl_ctl%mrst_ctl)
      call bcast_time_loop_ctl(smctl_ctl%mevo_ctl)
      call bcast_ctl_data_4_time_step(smctl_ctl%tctl)
!
      end subroutine bcast_sph_mhd_control
!
!   --------------------------------------------------------------------
!
!
      end module t_ctl_data_SPH_MHD_control
