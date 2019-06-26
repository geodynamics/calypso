!>@file   t_ctl_data_momentum_norm.f90
!!@brief  module t_ctl_data_momentum_norm
!!
!!@author H. Matsui
!>@brief   Control for momentum equation
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on Merch, 2006
!!
!!@verbatim
!!      subroutine read_momentum_ctl                                    &
!!     &         (id_control, hd_block, mom_ctl, c_buf)
!!      subroutine bcast_momentum_ctl(mom_ctl)
!!      subroutine dealloc_momentum_ctl(mom_ctl)
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
!!@endverbatim
!
!
      module t_ctl_data_momentum_norm
!
      use m_precision
      use m_machine_parameter
      use t_control_array_charareal
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
!
        integer (kind=kint) :: i_momentum = 0
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
      subroutine read_momentum_ctl                                      &
     &         (id_control, hd_block, mom_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(momentum_equation_control), intent(inout) :: mom_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(mom_ctl%i_momentum .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r(id_control,                         &
     &      hd_n_mom, mom_ctl%coef_4_intertia, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_n_press, mom_ctl%coef_4_grad_p, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_n_v_diff, mom_ctl%coef_4_viscous, c_buf)
!
        call read_control_array_c_r(id_control,                         &
     &      hd_n_buo, mom_ctl%coef_4_termal_buo, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_n_c_buo, mom_ctl%coef_4_comp_buo, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_n_cor, mom_ctl%coef_4_Coriolis, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_n_lor, mom_ctl%coef_4_Lorentz, c_buf)
      end do
      mom_ctl%i_momentum = 1
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
      call MPI_BCAST(mom_ctl%i_momentum, 1,                             &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_momentum_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_momentum_ctl(mom_ctl)
!
      type(momentum_equation_control), intent(inout) :: mom_ctl
!
!
      call dealloc_control_array_c_r(mom_ctl%coef_4_intertia)
      call dealloc_control_array_c_r(mom_ctl%coef_4_grad_p)
      call dealloc_control_array_c_r(mom_ctl%coef_4_viscous)
!
      call dealloc_control_array_c_r(mom_ctl%coef_4_termal_buo)
      call dealloc_control_array_c_r(mom_ctl%coef_4_comp_buo)
      call dealloc_control_array_c_r(mom_ctl%coef_4_Coriolis)
      call dealloc_control_array_c_r(mom_ctl%coef_4_Lorentz)
      mom_ctl%i_momentum = 0
!
      end subroutine dealloc_momentum_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_momentum_norm
