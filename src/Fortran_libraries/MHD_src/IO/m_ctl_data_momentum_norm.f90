!m_ctl_data_momentum_norm.f90
!      module m_ctl_data_momentum_norm
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine deallocate_coef_4_velocity_ctl
!      subroutine deallocate_coef_4_press_ctl
!      subroutine deallocate_coef_4_v_diffuse_ctl
!      subroutine deallocate_coef_4_buoyancy_ctl
!      subroutine deallocate_coef_4_comp_buo_ctl
!      subroutine deallocate_coef_4_coriolis_ctl
!      subroutine deallocate_coef_4_lorentz_ctl
!
!      subroutine read_momentum_ctl
!
!!   --------------------------------------------------------------------
!! example of control block
!!
!!  begin momentum
!!    array coef_4_velocity_ctl            1
!!      coef_4_velocity_ctl          One                        1.0
!!    end array
!!    array coef_4_press_ctl               1
!!      coef_4_press_ctl             Ekman_number              -1.0
!!    end array
!!    array coef_4_v_diffuse_ctl           1
!!      coef_4_v_diffuse_ctl         One                        1.0
!!    end array
!!    array coef_4_buoyancy_ctl            3
!!      coef_4_buoyancy_ctl          Radial_parameter           1.0
!!      coef_4_buoyancy_ctl          modified_Rayleigh_number   1.0
!!      coef_4_buoyancy_ctl          Ekman_number              -1.0
!!    end array
!!    array coef_4_Coriolis_ctl            2
!!      coef_4_Coriolis_ctl          Two                        1.0
!!      coef_4_Coriolis_ctl          Ekman_number              -1.0
!!    end array
!!    array coef_4_Lorentz_ctl             2
!!      coef_4_Lorentz_ctl           magnetic_Prandtl_number   -1.0
!!      coef_4_Lorentz_ctl           Ekman_number              -1.0
!!    end array
!!    array coef_4_composit_buoyancy_ctl   3
!!      coef_4_composit_buoyancy_ctl  Radial_parameter           1.0
!!      coef_4_composit_buoyancy_ctl  Composite_Rayleigh_number  1.0
!!      coef_4_composit_buoyancy_ctl  Ekman_number              -1.0
!!    end array
!!  end  momentum
!!   --------------------------------------------------------------------
!
!
      module m_ctl_data_momentum_norm
!
      use m_precision
!
      implicit  none
!
!
      integer(kind=kint) :: num_coef_4_v_diffuse_ctl = 0
      character(len=kchara),allocatable :: coef_4_v_diffuse_name_ctl(:)
      real (kind = kreal), allocatable :: coef_4_v_diffuse_power_ctl(:)
!
      integer(kind=kint) :: num_coef_4_velocity_ctl =  0
      integer(kind=kint) :: num_coef_4_press_ctl =     0
      character(len=kchara),allocatable :: coef_4_velocity_name_ctl(:)
      character(len=kchara),allocatable :: coef_4_press_name_ctl(:)
      real (kind = kreal), allocatable :: coef_4_velocity_power_ctl(:)
      real (kind = kreal), allocatable :: coef_4_press_power_ctl(:)
!
      integer(kind=kint) :: num_coef_4_buoyancy_ctl =  0
      integer(kind=kint) :: num_coef_4_comp_buo_ctl =  0
      integer(kind=kint) :: num_coef_4_Coriolis_ctl =  0
      integer(kind=kint) :: num_coef_4_Lorentz_ctl =   0
      character(len=kchara),allocatable :: coef_4_buoyancy_name_ctl(:)
      character(len=kchara),allocatable :: coef_4_comp_buo_name_ctl(:)
      character(len=kchara),allocatable :: coef_4_Coriolis_name_ctl(:)
      character(len=kchara),allocatable :: coef_4_Lorentz_name_ctl(:)
      real(kind = kreal), allocatable :: coef_4_buoyancy_power_ctl(:)
      real(kind = kreal), allocatable :: coef_4_comp_buo_power_ctl(:)
      real(kind = kreal), allocatable :: coef_4_Coriolis_power_ctl(:)
      real(kind = kreal), allocatable :: coef_4_Lorentz_power_ctl(:)
!
!   entry label
!
      character(len=kchara), parameter :: hd_momentum = 'momentum'
      integer (kind=kint) :: i_momentum = 0
!
!   5th level for coefs for momentum
!
      character(len=kchara) :: hd_n_mom =    'coef_4_velocity_ctl'
      character(len=kchara) :: hd_n_press =  'coef_4_press_ctl'
      character(len=kchara) :: hd_n_v_diff = 'coef_4_v_diffuse_ctl'
      character(len=kchara) :: hd_n_buo =    'coef_4_buoyancy_ctl'
      character(len=kchara) :: hd_n_c_buo                               &
     &                      = 'coef_4_composit_buoyancy_ctl'
      character(len=kchara) :: hd_n_cor =    'coef_4_Coriolis_ctl'
      character(len=kchara) :: hd_n_lor =    'coef_4_Lorentz_ctl'
!
      integer (kind=kint) :: i_n_mom =    0
      integer (kind=kint) :: i_n_press =  0
      integer (kind=kint) :: i_n_v_diff = 0
      integer (kind=kint) :: i_n_buo =    0
      integer (kind=kint) :: i_n_c_buo =  0
      integer (kind=kint) :: i_n_cor =    0
      integer (kind=kint) :: i_n_lor =    0
!
!
      private :: hd_momentum, i_momentum
      private :: hd_n_mom, hd_n_press, hd_n_v_diff
      private :: hd_n_buo, hd_n_c_buo, hd_n_cor, hd_n_lor
!
      private :: allocate_coef_4_velocity_ctl
      private :: allocate_coef_4_v_diffuse_ctl
      private :: allocate_coef_4_buoyancy_ctl
      private :: allocate_coef_4_comp_buo_ctl
      private :: allocate_coef_4_coriolis_ctl
      private :: allocate_coef_4_lorentz_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_velocity_ctl
!
        allocate(coef_4_velocity_name_ctl(num_coef_4_velocity_ctl))
        allocate(coef_4_velocity_power_ctl(num_coef_4_velocity_ctl))
        coef_4_velocity_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_velocity_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_press_ctl
!
        allocate(coef_4_press_name_ctl(num_coef_4_press_ctl))
        allocate(coef_4_press_power_ctl(num_coef_4_press_ctl))
        coef_4_press_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_press_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_v_diffuse_ctl
!
        allocate(coef_4_v_diffuse_name_ctl(num_coef_4_v_diffuse_ctl))
        allocate(coef_4_v_diffuse_power_ctl(num_coef_4_v_diffuse_ctl))
        coef_4_v_diffuse_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_v_diffuse_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_buoyancy_ctl
!
        allocate(coef_4_buoyancy_name_ctl(num_coef_4_buoyancy_ctl))
        allocate(coef_4_buoyancy_power_ctl(num_coef_4_buoyancy_ctl))
        coef_4_buoyancy_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_buoyancy_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_comp_buo_ctl
!
        allocate(coef_4_comp_buo_name_ctl(num_coef_4_comp_buo_ctl))
        allocate(coef_4_comp_buo_power_ctl(num_coef_4_comp_buo_ctl))
        coef_4_comp_buo_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_comp_buo_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_coriolis_ctl
!
        allocate(coef_4_Coriolis_name_ctl(num_coef_4_Coriolis_ctl))
        allocate(coef_4_Coriolis_power_ctl(num_coef_4_Coriolis_ctl))
        coef_4_Coriolis_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_coriolis_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_lorentz_ctl
!
        allocate(coef_4_Lorentz_name_ctl(num_coef_4_Lorentz_ctl))
        allocate(coef_4_Lorentz_power_ctl(num_coef_4_Lorentz_ctl))
        coef_4_Lorentz_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_lorentz_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_velocity_ctl
!
        deallocate(coef_4_velocity_name_ctl, coef_4_velocity_power_ctl)
!
       end subroutine deallocate_coef_4_velocity_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_press_ctl
!
        deallocate(coef_4_press_name_ctl, coef_4_press_power_ctl)
!
       end subroutine deallocate_coef_4_press_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_v_diffuse_ctl
!
        deallocate(coef_4_v_diffuse_name_ctl)
        deallocate(coef_4_v_diffuse_power_ctl)
!
       end subroutine deallocate_coef_4_v_diffuse_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_buoyancy_ctl
!
        deallocate(coef_4_buoyancy_name_ctl, coef_4_buoyancy_power_ctl)
!
       end subroutine deallocate_coef_4_buoyancy_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_comp_buo_ctl
!
        deallocate(coef_4_comp_buo_name_ctl, coef_4_comp_buo_power_ctl)
!
       end subroutine deallocate_coef_4_comp_buo_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_coriolis_ctl
!
        deallocate(coef_4_Coriolis_name_ctl, coef_4_Coriolis_power_ctl)
!
       end subroutine deallocate_coef_4_coriolis_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_lorentz_ctl
!
        deallocate(coef_4_Lorentz_name_ctl, coef_4_Lorentz_power_ctl)
!
       end subroutine deallocate_coef_4_lorentz_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_momentum_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_momentum) .eq. 0) return
      if (i_momentum .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_momentum, i_momentum)
        if(i_momentum .gt. 0) exit
!
!
        call find_control_array_flag(hd_n_mom, num_coef_4_velocity_ctl)
        if(num_coef_4_velocity_ctl.gt.0 .and. i_n_mom.eq.0) then
          call allocate_coef_4_velocity_ctl
          call read_control_array_vect_list(hd_n_mom,                   &
     &        num_coef_4_velocity_ctl, i_n_mom,                         &
     &        coef_4_velocity_name_ctl, coef_4_velocity_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_press, num_coef_4_press_ctl)
        if(num_coef_4_press_ctl.gt.0 .and. i_n_press.eq.0) then
          call allocate_coef_4_press_ctl
          call read_control_array_vect_list(hd_n_press,                 &
     &        num_coef_4_press_ctl, i_n_press,                          &
     &        coef_4_press_name_ctl, coef_4_press_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_v_diff,                       &
     &      num_coef_4_v_diffuse_ctl)
        if(num_coef_4_v_diffuse_ctl.gt.0 .and. i_n_v_diff.eq.0) then
          call allocate_coef_4_v_diffuse_ctl
          call read_control_array_vect_list(hd_n_v_diff,                &
     &        num_coef_4_v_diffuse_ctl, i_n_v_diff,                     &
     &        coef_4_v_diffuse_name_ctl, coef_4_v_diffuse_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_buo, num_coef_4_buoyancy_ctl)
        if(num_coef_4_buoyancy_ctl.gt.0 .and. i_n_buo.eq.0) then
          call allocate_coef_4_buoyancy_ctl
          call read_control_array_vect_list(hd_n_buo,                   &
     &        num_coef_4_buoyancy_ctl, i_n_buo,                         &
     &        coef_4_buoyancy_name_ctl, coef_4_buoyancy_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_c_buo,                        &
     &      num_coef_4_comp_buo_ctl)
        if(num_coef_4_comp_buo_ctl.gt.0 .and. i_n_c_buo.eq.0) then
          call allocate_coef_4_comp_buo_ctl
          call read_control_array_vect_list(hd_n_c_buo,                 &
     &        num_coef_4_comp_buo_ctl, i_n_c_buo,                       &
     &        coef_4_comp_buo_name_ctl, coef_4_comp_buo_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_cor, num_coef_4_Coriolis_ctl)
        if(num_coef_4_Coriolis_ctl.gt.0 .and. i_n_cor.eq.0) then
          call allocate_coef_4_coriolis_ctl
          call read_control_array_vect_list(hd_n_cor,                   &
     &        num_coef_4_Coriolis_ctl, i_n_cor,                         &
     &        coef_4_Coriolis_name_ctl, coef_4_Coriolis_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_lor, num_coef_4_Lorentz_ctl)
        if(num_coef_4_Lorentz_ctl.gt.0 .and. i_n_lor.eq.0) then
          call allocate_coef_4_lorentz_ctl
          call read_control_array_vect_list(hd_n_lor,                   &
     &        num_coef_4_Lorentz_ctl, i_n_lor,                          &
     &        coef_4_Lorentz_name_ctl, coef_4_Lorentz_power_ctl)
        end if
      end do
!
      end subroutine read_momentum_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_momentum_norm
