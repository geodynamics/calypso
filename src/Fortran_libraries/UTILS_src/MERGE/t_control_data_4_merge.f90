!>@file   t_control_data_4_merge.f90
!!@brief  module t_control_data_4_merge
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2007
!
!>@brief  Control data for merge program
!!
!!@verbatim
!!       subroutine read_control_4_merge(mgd_ctl)
!!       subroutine read_control_assemble_sph(mgd_ctl)
!!        type(control_data_4_merge), intent(inout) :: mgd_ctl
!!@endverbatim
!
!
      module t_control_data_4_merge
!
      use m_precision
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_control_elements
      use skip_comment_f
!
      implicit none
!
      integer (kind = kint), parameter :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name =     'control_merge'
      character (len = kchara), parameter                               &
     &         :: ctl_assemble_sph_name = 'control_assemble_sph'
!
!
!>        Structure for merged program control
      type control_data_4_merge
!>        Structure for file information for original data
        type(platform_data_control) :: source_plt
!>        Structure for file information for assembled data
        type(platform_data_control) :: assemble_plt
!
!>        Structure for field information control
        type(field_control) :: fld_mge_ctl
!>        Structure for time stepping control
        type(time_data_control) :: t_mge_ctl
!>        Structure for new time stepping control
        type(time_data_control) :: t2_mge_ctl
!
!>        Re-normalization flag for magnetic field
        type(read_real_item) :: magnetic_ratio_ctl
      end type control_data_4_merge
!
!   Top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_assemble = 'assemble_control'
      integer (kind=kint) :: i_assemble = 0
!
!   2nd level for assemble_control
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
      character(len=kchara), parameter                                  &
     &                    :: hd_newrst_magne = 'newrst_magne_ctl'
!
      integer (kind=kint) :: i_platform =      0
      integer (kind=kint) :: i_new_data =      0
      integer (kind=kint) :: i_model =         0
      integer (kind=kint) :: i_control =       0
      integer (kind=kint) :: i_newrst_magne =  0
!
!>      label for block
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
!>      Number of field
      integer (kind=kint) :: i_phys_values =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      integer (kind=kint) :: i_tstep =      0
!
      character(len=kchara), parameter                                  &
     &      :: hd_new_time_step = 'new_time_step_ctl'
      integer (kind=kint) :: i_nstep =      0
!
!   newrst_magne_ratio data
!
      character(len=kchara), parameter                                  &
     &      :: hd_magnetic_field_ratio =  'magnetic_field_ratio_ctl'
!
      private :: control_file_code
      private :: ctl_assemble_sph_name, control_file_name
      private :: hd_assemble, i_assemble
      private :: hd_platform, i_platform
      private :: hd_new_data, i_new_data, i_newrst_magne
      private :: hd_model, hd_control, i_model, i_control
      private :: hd_phys_values, i_phys_values
      private :: hd_time_step, i_tstep, hd_new_time_step, i_nstep
      private :: hd_newrst_magne, hd_magnetic_field_ratio
!
      private :: read_merge_control_data
      private :: read_newrst_control
      private :: read_merge_field_data, read_merge_step_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine read_control_4_merge(mgd_ctl)
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_merge_control_data(mgd_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_merge
!
! -----------------------------------------------------------------------
!
       subroutine read_control_assemble_sph(mgd_ctl)
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = ctl_assemble_sph_name)
!
      call load_ctl_label_and_line
      call read_merge_control_data(mgd_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_assemble_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_merge_control_data(mgd_ctl)
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      if(right_begin_flag(hd_assemble) .eq. 0) return
      if (i_assemble .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_assemble = find_control_end_flag(hd_assemble)
        if(i_assemble .gt. 0) exit
!
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, mgd_ctl%source_plt)
        call read_control_platforms                                     &
     &     (hd_new_data, i_new_data, mgd_ctl%assemble_plt)
!
        call read_merge_field_data(mgd_ctl)
        call read_merge_step_data(mgd_ctl)
        call read_newrst_control(mgd_ctl)
      end do
!
      end subroutine read_merge_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_merge_field_data(mgd_ctl)
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      if(right_begin_flag(hd_model) .eq. 0) return
      if (i_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_model = find_control_end_flag(hd_model)
        if(i_model .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (hd_phys_values, i_phys_values, mgd_ctl%fld_mge_ctl)
      end do
!
      end subroutine read_merge_field_data
!
! -----------------------------------------------------------------------
!
       subroutine read_merge_step_data(mgd_ctl)
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_control = find_control_end_flag(hd_control)
        if(i_control .gt. 0) exit
!
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, mgd_ctl%t_mge_ctl)
        call read_control_time_step_data                                &
     &     (hd_new_time_step, i_nstep, mgd_ctl%t2_mge_ctl)
      end do
!
      end subroutine read_merge_step_data
!
! -----------------------------------------------------------------------
!
      subroutine read_newrst_control(mgd_ctl)
!
      use m_machine_parameter
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      if(right_begin_flag(hd_newrst_magne) .eq. 0) return
      if (i_newrst_magne .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_newrst_magne = find_control_end_flag(hd_newrst_magne)
        if(i_newrst_magne .gt. 0) exit
!
        call read_real_ctl_type(hd_magnetic_field_ratio,                &
     &      mgd_ctl%magnetic_ratio_ctl)
      end do
!
      end subroutine read_newrst_control
!
! -----------------------------------------------------------------------
!
      end module t_control_data_4_merge
