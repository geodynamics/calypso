!t_ctl_data_momentum_norm.f90
!      module t_ctl_data_momentum_norm
!
!        programmed by H.Matsui on March. 2006
!
!!      subroutine read_momentum_ctl(hd_block, iflag, mom_ctl)
!!      subroutine bcast_momentum_ctl(mom_ctl)
!!        type(momentum_equation_control), intent(inout) :: mom_ctl
!!
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
      module t_ctl_data_momentum_norm
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
!
!>      Structure for coefficients of momentum equation
      type momentum_equation_control
!>        Structure for number and power to construct viscousity term
!!@n        coef_4_viscous%c_tbl:  Name of number 
!!@n        coef_4_viscous%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_viscous
!
!>        Structure for number and power to construct intertia term
!!@n        coef_4_intertia%c_tbl:  Name of number 
!!@n        coef_4_intertia%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_intertia
!>        Structure for number and power to construct pressure gradient
!!@n        coef_4_grad_p%c_tbl:  Name of number 
!!@n        coef_4_grad_p%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_grad_p
!
!
!>        Structure for number and power to construct termal buoyancy
!!@n        coef_4_termal_buo%c_tbl:  Name of number 
!!@n        coef_4_termal_buo%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_termal_buo
!>        Structure for number and power to construct compositional buoyancy
!!@n        coef_4_comp_buo%c_tbl:  Name of number 
!!@n        coef_4_comp_buo%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_comp_buo
!>        Structure for number and power to construct Coriolis force
!!@n        coef_4_Coriolis%c_tbl:  Name of number 
!!@n        coef_4_Coriolis%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_Coriolis
!>        Structure for number and power to construct Lorentz force
!!@n        coef_4_Lorentz%c_tbl:  Name of number 
!!@n        coef_4_Lorentz%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_Lorentz
      end type momentum_equation_control
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
      private :: hd_n_mom, hd_n_press, hd_n_v_diff
      private :: hd_n_buo, hd_n_c_buo, hd_n_cor, hd_n_lor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_momentum_ctl(hd_block, iflag, mom_ctl)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(momentum_equation_control), intent(inout) :: mom_ctl
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
        call read_control_array_c_r(hd_n_mom, mom_ctl%coef_4_intertia)
        call read_control_array_c_r(hd_n_press, mom_ctl%coef_4_grad_p)
        call read_control_array_c_r                                     &
       &   (hd_n_v_diff, mom_ctl%coef_4_viscous)
!
        call read_control_array_c_r                                     &
       &   (hd_n_buo, mom_ctl%coef_4_termal_buo)
        call read_control_array_c_r                                     &
       &   (hd_n_c_buo, mom_ctl%coef_4_comp_buo)
        call read_control_array_c_r(hd_n_cor, mom_ctl%coef_4_Coriolis)
        call read_control_array_c_r(hd_n_lor, mom_ctl%coef_4_Lorentz)
      end do
!
      end subroutine read_momentum_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_momentum_ctl(mom_ctl)
!
      use bcast_control_arrays
!
      type(momentum_equation_control), intent(inout) :: mom_ctl
!
!
      call bcast_ctl_array_cr(mom_ctl%coef_4_intertia)
      call bcast_ctl_array_cr(mom_ctl%coef_4_grad_p)
      call bcast_ctl_array_cr(mom_ctl%coef_4_viscous)
!
      call bcast_ctl_array_cr(mom_ctl%coef_4_termal_buo)
      call bcast_ctl_array_cr(mom_ctl%coef_4_comp_buo)
      call bcast_ctl_array_cr(mom_ctl%coef_4_Coriolis)
      call bcast_ctl_array_cr(mom_ctl%coef_4_Lorentz)
!
      end subroutine bcast_momentum_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_momentum_norm
