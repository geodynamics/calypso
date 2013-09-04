!m_ctl_data_termal_norm.f90
!      module m_ctl_data_termal_norm
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine deallocate_coef_4_termal_ctl
!      subroutine deallocate_coef_4_t_diffuse_ctl
!
!      subroutine read_thermal_ctl
!
!   --------------------------------------------------------------------
! example
!
!      begin thermal
!        array coef_4_termal_ctl     1
!          coef_4_termal_ctl            One                   1.0  end
!        end array
!        array coef_4_t_diffuse_ctl  1
!          coef_4_t_diffuse_ctl         Prandtl_number       -1.0  end
!        end array
!      end  thermal
!   --------------------------------------------------------------------
!
      module m_ctl_data_termal_norm
!
      use m_precision
!
      implicit  none
!
!
      integer(kind=kint) :: num_coef_4_t_diffuse_ctl = 0
      character(len=kchara),allocatable :: coef_4_t_diffuse_name_ctl(:)
      real (kind = kreal), allocatable :: coef_4_t_diffuse_power_ctl(:)
!
      integer(kind=kint) :: num_coef_4_termal_ctl =    0
      character(len=kchara),allocatable :: coef_4_termal_name_ctl(:)
      real (kind = kreal), allocatable :: coef_4_termal_power_ctl(:)
!
!   entry label
!
      character(len=kchara), parameter :: hd_thermal = 'thermal'
      integer (kind=kint) :: i_thermal = 0
!
!   5th level for coefs for thermal
!
      character(len=kchara) :: hd_n_thermal = 'coef_4_termal_ctl'
      character(len=kchara) :: hd_n_t_diff =  'coef_4_t_diffuse_ctl'
      integer (kind=kint) :: i_n_thermal = 0
      integer (kind=kint) :: i_n_t_diff = 0
!
      private :: hd_thermal, i_thermal
      private :: hd_n_thermal, hd_n_t_diff
!
      private :: allocate_coef_4_termal_ctl
      private :: allocate_coef_4_t_diffuse_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
       subroutine allocate_coef_4_termal_ctl
!
        allocate(coef_4_termal_name_ctl(num_coef_4_termal_ctl))
        allocate(coef_4_termal_power_ctl(num_coef_4_termal_ctl))
        coef_4_termal_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_termal_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_t_diffuse_ctl
!
        allocate(coef_4_t_diffuse_name_ctl(num_coef_4_t_diffuse_ctl))
        allocate(coef_4_t_diffuse_power_ctl(num_coef_4_t_diffuse_ctl))
        coef_4_t_diffuse_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_t_diffuse_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_termal_ctl
!
        deallocate(coef_4_termal_name_ctl, coef_4_termal_power_ctl)
!
       end subroutine deallocate_coef_4_termal_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_t_diffuse_ctl
!
        deallocate(coef_4_t_diffuse_name_ctl)
        deallocate(coef_4_t_diffuse_power_ctl)
!
       end subroutine deallocate_coef_4_t_diffuse_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_thermal_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_thermal) .eq. 0) return
      if (i_thermal .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_thermal, i_thermal)
        if(i_thermal .gt. 0) exit
!
!
        call find_control_array_flag(hd_n_thermal,                      &
     &      num_coef_4_termal_ctl)
        if(num_coef_4_termal_ctl.gt.0 .and. i_n_thermal.eq.0) then
          call allocate_coef_4_termal_ctl
          call read_control_array_vect_list(hd_n_thermal,               &
     &        num_coef_4_termal_ctl, i_n_thermal,                       &
     &        coef_4_termal_name_ctl, coef_4_termal_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_t_diff,                       &
     &      num_coef_4_t_diffuse_ctl)
        if(num_coef_4_t_diffuse_ctl.gt.0 .and. i_n_t_diff.eq.0) then
          call allocate_coef_4_t_diffuse_ctl
          call read_control_array_vect_list(hd_n_t_diff,                &
     &        num_coef_4_t_diffuse_ctl, i_n_t_diff,                     &
     &        coef_4_t_diffuse_name_ctl, coef_4_t_diffuse_power_ctl)
        end if
      end do
!
      end subroutine read_thermal_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_termal_norm
