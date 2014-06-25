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
      use t_read_control_arrays
!
      implicit  none
!
!
!>      Structure for number and power to construct viscousity term
!!@n      coef_4_viscous_ctl%c_tbl:  Name of number 
!!@n      coef_4_viscous_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_viscous_ctl
!
!>      Structure for number and power to construct intertia term
!!@n      coef_4_intertia_ctl%c_tbl:  Name of number 
!!@n      coef_4_intertia_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_intertia_ctl
!>      Structure for number and power to construct pressure gradient
!!@n      coef_4_grad_p_ctl%c_tbl:  Name of number 
!!@n      coef_4_grad_p_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_grad_p_ctl
!
!
!>      Structure for number and power to construct termal buoyancy
!!@n      coef_4_termal_buo_ctl%c_tbl:  Name of number 
!!@n      coef_4_termal_buo_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_termal_buo_ctl
!>      Structure for number and power to construct compositional buoyancy
!!@n      coef_4_comp_buo_ctl%c_tbl:  Name of number 
!!@n      coef_4_comp_buo_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_comp_buo_ctl
!>      Structure for number and power to construct Coriolis force
!!@n      coef_4_Coriolis_ctl%c_tbl:  Name of number 
!!@n      coef_4_Coriolis_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_Coriolis_ctl
!>      Structure for number and power to construct Lorentz force
!!@n      coef_4_Loreantz_ctl%c_tbl:  Name of number 
!!@n      coef_4_Loreantz_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_Loreantz_ctl
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
!
      private :: hd_momentum, i_momentum
      private :: hd_n_mom, hd_n_press, hd_n_v_diff
      private :: hd_n_buo, hd_n_c_buo, hd_n_cor, hd_n_lor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_velocity_ctl
!
      call dealloc_control_array_c_r(coef_4_intertia_ctl)
!
      end subroutine deallocate_coef_4_velocity_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_press_ctl
!
      call dealloc_control_array_c_r(coef_4_grad_p_ctl)
!
      end subroutine deallocate_coef_4_press_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_v_diffuse_ctl
!
      call dealloc_control_array_c_r(coef_4_viscous_ctl)
!
      end subroutine deallocate_coef_4_v_diffuse_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_buoyancy_ctl
!
      call dealloc_control_array_c_r(coef_4_termal_buo_ctl)
!
      end subroutine deallocate_coef_4_buoyancy_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_comp_buo_ctl
!
      call dealloc_control_array_c_r(coef_4_comp_buo_ctl)
!
      end subroutine deallocate_coef_4_comp_buo_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_coriolis_ctl
!
      call dealloc_control_array_c_r(coef_4_Coriolis_ctl)
!
      end subroutine deallocate_coef_4_coriolis_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_lorentz_ctl
!
      call dealloc_control_array_c_r(coef_4_Loreantz_ctl)
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
        call read_control_array_c_r(hd_n_mom, coef_4_intertia_ctl)
        call read_control_array_c_r(hd_n_press, coef_4_grad_p_ctl)
        call read_control_array_c_r(hd_n_v_diff, coef_4_viscous_ctl)
!
        call read_control_array_c_r(hd_n_buo, coef_4_termal_buo_ctl)
        call read_control_array_c_r(hd_n_c_buo, coef_4_comp_buo_ctl)
        call read_control_array_c_r(hd_n_cor, coef_4_Coriolis_ctl)
        call read_control_array_c_r(hd_n_lor, coef_4_Loreantz_ctl)
      end do
!
      end subroutine read_momentum_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_momentum_norm
