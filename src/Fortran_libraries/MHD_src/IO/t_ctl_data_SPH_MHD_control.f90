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
!!      subroutine init_sph_mhd_control_label(hd_block, smctl_ctl)
!!      subroutine read_sph_mhd_control                                 &
!!     &         (id_control, hd_block, smctl_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sph_mhd_control(id_control, smctl_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_mhd_control_control), intent(in) :: smctl_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine reset_sph_mhd_control(smctl_ctl)
!!        type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!!@endverbatim
!
      module t_ctl_data_SPH_MHD_control
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_time_steps
      use t_ctl_data_mhd_evo_scheme
      use t_ctl_data_mhd_restart
!
      use skip_comment_f
!
      implicit none
!
      type sph_mhd_control_control
!>        Block name
        character(len=kchara) :: block_name = 'control'
!>        Structure for time stepping control
        type(time_data_control) :: tctl
!>        Structure for restart flag
        type(mhd_restart_control) :: mrst_ctl
!>        Structures for time integration controls
        type(mhd_evo_scheme_control) :: mevo_ctl
!
        integer (kind=kint) :: i_control =      0
      end type sph_mhd_control_control
!
!    label for entry of group
      character(len=kchara), parameter                                  &
     &      :: hd_time_step =      'time_step_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_restart_file =   'restart_file_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_time_loop =      'time_loop_ctl'
!
      private :: hd_time_step, hd_restart_file, hd_time_loop
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_control                                   &
     &         (id_control, hd_block, smctl_ctl, c_buf)
!
      use ctl_data_4_time_steps_IO
      use ctl_data_mhd_evo_scheme_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_mhd_control_control), intent(inout) :: smctl_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(smctl_ctl%i_control .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, smctl_ctl%tctl, c_buf)
        call read_restart_ctl                                           &
     &     (id_control, hd_restart_file, smctl_ctl%mrst_ctl, c_buf)
!
        call read_time_loop_ctl                                         &
     &     (id_control, hd_time_loop, smctl_ctl%mevo_ctl, c_buf)
      end do
      smctl_ctl%i_control = 1
!
      end subroutine read_sph_mhd_control
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_mhd_control(id_control, smctl_ctl, level)
!
      use ctl_data_mhd_evo_scheme_IO
      use ctl_data_4_time_steps_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(sph_mhd_control_control), intent(in) :: smctl_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(smctl_ctl%i_control .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 smctl_ctl%block_name)
      call write_control_time_step_data                                 &
     &   (id_control, smctl_ctl%tctl, level)
      call write_restart_ctl(id_control, smctl_ctl%mrst_ctl, level)
!
      call write_time_loop_ctl(id_control, smctl_ctl%mevo_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                smctl_ctl%block_name)
!
      end subroutine write_sph_mhd_control
!
!   --------------------------------------------------------------------
!
      subroutine init_sph_mhd_control_label(hd_block, smctl_ctl)
!
      use ctl_data_4_time_steps_IO
      use ctl_data_mhd_evo_scheme_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!
!
      smctl_ctl%block_name = hd_block
      call init_ctl_time_step_label(hd_time_step, smctl_ctl%tctl)
      call init_time_loop_ctl_label(hd_time_loop, smctl_ctl%mevo_ctl)
      call init_restart_ctl_label(hd_restart_file, smctl_ctl%mrst_ctl)
!
      end subroutine init_sph_mhd_control_label
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine reset_sph_mhd_control(smctl_ctl)
!
      type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!
!
      call reset_restart_ctl(smctl_ctl%mrst_ctl)
      call reset_time_loop_ctl(smctl_ctl%mevo_ctl)
      call reset_ctl_data_4_time_step(smctl_ctl%tctl)
!
      smctl_ctl%i_control = 0
!
      end subroutine reset_sph_mhd_control
!
!   --------------------------------------------------------------------
!
!
      end module t_ctl_data_SPH_MHD_control
