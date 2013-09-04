!
!      module m_control_data_4_merge
!
!      Written by H. Matsui
!
!       subroutine read_control_4_merge
!       subroutine read_control_assemble_sph
!
      module m_control_data_4_merge
!
      use m_precision
      use m_machine_parameter
      use m_read_control_elements
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
      real(kind=kreal) :: magnetic_field_ratio_ctl
!
!   Top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_assemble = 'assemble_control'
      integer (kind=kint) :: i_assemble = 0
!
!   2nd level for assemble_control
!
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
      character(len=kchara), parameter                                  &
     &                    :: hd_newrst_magne = 'newrst_magne_ctl'
!
      integer (kind=kint) :: i_model =         0
      integer (kind=kint) :: i_control =       0
      integer (kind=kint) :: i_newrst_magne =  0
!
!   newrst_magne_ratio data
!
      character(len=kchara), parameter                                  &
     &      :: hd_magnetic_field_ratio =  'magnetic_field_ratio_ctl'
      integer (kind=kint) :: i_magnetic_field_ratio =   0
!
      private :: hd_assemble, i_assemble
      private :: hd_model, hd_control, i_model, i_control
      private :: hd_newrst_magne
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
       subroutine read_control_4_merge
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_merge_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_merge
!
! -----------------------------------------------------------------------
!
       subroutine read_control_assemble_sph
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = ctl_assemble_sph_name)
!
      call load_ctl_label_and_line
      call read_merge_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_assemble_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_merge_control_data
!
      use m_machine_parameter
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
!
!   2 begin phys_values_ctl
!
      if(right_begin_flag(hd_assemble) .eq. 0) return
      if (i_assemble .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_assemble, i_assemble)
        if(i_assemble .gt. 0) exit
!
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_new_data
!
        call read_merge_field_data
        call read_merge_step_data
        call read_newrst_control
      end do
!
      end subroutine read_merge_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_merge_field_data
!
       use m_ctl_data_4_fields
!
!   2 begin phys_values_ctl
!
      if(right_begin_flag(hd_model) .eq. 0) return
      if (i_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_model, i_model)
        if(i_model .gt. 0) exit
!
        call read_phys_values
      end do
!
      end subroutine read_merge_field_data
!
! -----------------------------------------------------------------------
!
       subroutine read_merge_step_data
!
       use m_ctl_data_4_time_steps
!
!   2 begin time_step_ctl
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_control, i_control)
        if(i_control .gt. 0) exit
!
        call read_time_step_ctl
      end do
!
      end subroutine read_merge_step_data
!
! -----------------------------------------------------------------------
!
      subroutine  read_newrst_control
!
      use m_machine_parameter
!
!
      if(right_begin_flag(hd_newrst_magne) .eq. 0) return
      if (i_newrst_magne .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_newrst_magne, i_newrst_magne)
        if(i_newrst_magne .gt. 0) exit
!
        call read_real_ctl_item(hd_magnetic_field_ratio,                &
     &        i_magnetic_field_ratio, magnetic_field_ratio_ctl)
      end do
!
      end subroutine  read_newrst_control
!
! -----------------------------------------------------------------------
!
      end module m_control_data_4_merge
