!>@file   t_ctl_data_mhd_normalize.f90
!!@brief  module t_ctl_data_mhd_normalize
!!
!!@author H. Matsui
!>@brief   Control for normalizations of MHD dynamo
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on Merch, 2006
!!
!!@verbatim
!!      subroutine init_coef_term_ctl_label(hd_block, eqs_ctl)
!!      subroutine read_coef_term_ctl                                   &
!!     &         (id_control, hd_block, eqs_ctl, c_buf)
!!      subroutine write_coef_term_ctl(id_control, eqs_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(equations_control), intent(in) :: eqs_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_coef_term_ctl(eqs_ctl)
!!        type(equations_control), intent(in) :: eqs_ctl
!!
!!   --------------------------------------------------------------------
!!    example
!!
!!!!!!  dimensionless numbers !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  available numbers
!!     Prandtl_number, magnetic_Prandtl_number
!!     Rayleigh_number, modified_Rayleigh_number
!!     Composit_Rayleigh_number
!!     Reynords_number
!!     Taylor_number, Ekman_number
!!     Elsasser_number
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin dimensionless_ctl
!!      array dimless_ctl 6
!!        dimless_ctl  Prandtl_number                   1.0e-0
!!        dimless_ctl  modified_Rayleigh_number         1.0E+2
!!        dimless_ctl  Ekman_number                     1.0e-3
!!        dimless_ctl  magnetic_Prandtl_number          5.0e+0
!!        dimless_ctl  Composite_Rayleigh_number        1.0E+2
!!        dimless_ctl  Composite_Prandtl_number         1.0E+2
!!      end array dimless_ctl
!!    end  dimensionless_ctl
!!
!!!!!! Normalization settings  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    define
!!     coef_4_termal_ctl:      time integration and advection of temp
!!     coef_4_velocity_ctl:    time integration and advection of temperature
!!     coef_4_magnetic_ctl:    time integration and advection of magnetic f.
!!     coef_4_composition_ctl: time integration and advection of composition
!!
!!     coef_4_press_ctl:   coefficients for pressure gradient
!!     coef_4_mag_p_ctl:   coefficients for potential electric field
!!
!!     coef_4_t_diffuse_ctl:   coefficients for thermal diffusion
!!     coef_4_v_diffuse_ctl:   coefficients for viscous diffusion
!!     coef_4_m_diffuse_ctl:   coefficients for magnetic diffusion
!!     coef_4_c_diffuse_ctl:   coefficients for compositional diffusion
!!
!!     coef_4_buoyancy_ctl:   coefficients for buoyancy
!!     coef_4_Coriolis_ctl:   coefficients for Coriolis force
!!     coef_4_Lorentz_ctl:    coefficients for Lorantz force
!!     coef_4_composit_buoyancy_ctl: coefficients for compositional buoyancy
!!
!!     One:  1, Zero (Ignore), Two:  2,   Radial_parameter: (1-ri/ro)
!!     Radial_35: (1-0.35)
!!
!!     Real number.... Power of each numbers
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    begin coefficients_ctl
!!      begin thermal
!!        ...
!!      end  thermal
!!
!!      begin momentum
!!        ...
!!      end  momentum
!!
!!      begin induction
!!        ...
!!      end  induction
!!
!!      begin composition
!!        ...
!!      end  composition
!!
!!    end  coefficients_ctl
!!
!!   --------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_mhd_normalize
!
      use m_precision
!
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_control_array_charareal
      use t_ctl_data_momentum_norm
      use t_ctl_data_induct_norm
      use t_ctl_data_termal_norm
!
      implicit  none
!
!
!>      Structure for coefficients of governing equations
      type equations_control
!>        Block name
        character(len=kchara) :: block_name = 'coefficients_ctl'
!>        Structure for coefficients of momentum equation
        type(momentum_equation_control) :: mom_ctl
!>        Structure for coefficients of magnetic induction equation
        type(induction_equation_control) :: induct_ctl
!>        Structure for coefficients of heat and composition equation
        type(heat_equation_control) :: heat_ctl
!>        Structure for coefficients of heat and composition equation
        type(heat_equation_control) :: comp_ctl
!
        integer(kind = kint) :: i_coef_term_ctl = 0
      end type equations_control
!
!   4th level for coefficients
!
      character(len=kchara), parameter, private                         &
     &        :: hd_momentum = 'momentum'
      character(len=kchara), parameter, private                         &
     &        :: hd_thermal = 'thermal'
      character(len=kchara), parameter, private                         &
     &        :: hd_dsc_diff_adv = 'composition'
      character(len=kchara), parameter, private                         &
     &        :: hd_induction = 'induction'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_coef_term_ctl                                     &
     &         (id_control, hd_block, eqs_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(equations_control), intent(inout) :: eqs_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(eqs_ctl%i_coef_term_ctl .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_thermal_ctl                                           &
     &     (id_control, hd_thermal, eqs_ctl%heat_ctl, c_buf)
        call read_momentum_ctl                                          &
     &     (id_control, hd_momentum, eqs_ctl%mom_ctl, c_buf)
        call read_induction_ctl                                         &
     &     (id_control, hd_induction, eqs_ctl%induct_ctl, c_buf)
        call read_composition_eq_ctl                                    &
     &     (id_control, hd_dsc_diff_adv, eqs_ctl%comp_ctl, c_buf)
      end do
      eqs_ctl%i_coef_term_ctl = 1
!
      end subroutine read_coef_term_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_coef_term_ctl(id_control, eqs_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(equations_control), intent(in) :: eqs_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(eqs_ctl%i_coef_term_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 eqs_ctl%block_name)
      call write_thermal_ctl(id_control, eqs_ctl%heat_ctl, level)
      call write_momentum_ctl(id_control, eqs_ctl%mom_ctl, level)
      call write_induction_ctl(id_control, eqs_ctl%induct_ctl, level)
      call write_composition_eq_ctl                                     &
     &   (id_control, eqs_ctl%comp_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                eqs_ctl%block_name)
!
      end subroutine write_coef_term_ctl
!
!   --------------------------------------------------------------------
!
      subroutine init_coef_term_ctl_label(hd_block, eqs_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(equations_control), intent(inout) :: eqs_ctl
!
      eqs_ctl%block_name = trim(hd_block)
      call init_momentum_ctl_label(hd_momentum, eqs_ctl%mom_ctl)
      call init_induction_ctl_label(hd_induction, eqs_ctl%induct_ctl)
      call init_thermal_ctl_label(hd_thermal, eqs_ctl%heat_ctl)
      call init_composition_eq_ctl_label(hd_dsc_diff_adv,               &
     &                                   eqs_ctl%comp_ctl)
!
      end subroutine init_coef_term_ctl_label
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_coef_term_ctl(eqs_ctl)
!
      type(equations_control), intent(inout) :: eqs_ctl
!
!
      call dealloc_thermal_ctl(eqs_ctl%heat_ctl)
      call dealloc_momentum_ctl(eqs_ctl%mom_ctl)
      call dealloc_induction_ctl(eqs_ctl%induct_ctl)
      call dealloc_thermal_ctl(eqs_ctl%comp_ctl)
!
      eqs_ctl%i_coef_term_ctl = 0
!
      end subroutine dealloc_coef_term_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_mhd_normalize
