!
!      module m_read_ctl_gen_sph_shell
!
!      Written by H. Matsui on July, 2007
!
!       subroutine read_control_4_gen_shell_grids
!
      module m_read_ctl_gen_sph_shell
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
      integer (kind = kint) :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name = 'control_sph_shell'
!
      character (len = kchara) :: tmp_character

!
!   Top level
!
      character(len=kchara), parameter                                  &
     &      :: hd_sph_shell = 'spherical_shell_ctl'
      integer (kind=kint) :: i_sph_shell = 0
!
      private :: control_file_code, control_file_name
      private :: hd_sph_shell, i_sph_shell
      private :: read_control_data_4_shell
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_gen_shell_grids
!
      use m_read_control_elements
      use skip_comment_f
!
!
      ctl_file_code = control_file_code
!
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_control_data_4_shell
!
      close(ctl_file_code)
!
      end subroutine read_control_4_gen_shell_grids
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_4_shell
!
      use m_read_control_elements
      use m_ctl_data_4_platforms
      use m_ctl_data_4_sphere_model
      use m_ctl_data_4_divide_sphere
      use skip_comment_f
!
!
      if(right_begin_flag(hd_sph_shell) .eq. 0) return
      if(i_sph_shell .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sph_shell, i_sph_shell)
        if(i_sph_shell .gt. 0) exit
!
!
        call read_ctl_data_4_platform
        call read_ctl_4_shell_define
!
        call read_ctl_ndomain_4_shell
      end do
!
      end subroutine read_control_data_4_shell
!
!   --------------------------------------------------------------------
!
      end module m_read_ctl_gen_sph_shell
