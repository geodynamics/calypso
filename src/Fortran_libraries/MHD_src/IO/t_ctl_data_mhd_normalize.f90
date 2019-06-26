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
!!      subroutine read_dimless_ctl                                     &
!!     &         (id_control, hd_block, dless_ctl, c_buf)
!!      subroutine read_coef_term_ctl                                   &
!!     &         (id_control, hd_block, eqs_ctl, c_buf)
!!
!!      subroutine bcast_dimless_ctl(dless_ctl)
!!      subroutine bcast_coef_term_ctl(eqs_ctl)
!!
!!      subroutine dealloc_dimless_ctl(dless_ctl)
!!      subroutine dealloc_coef_term_ctl(eqs_ctl)
!!        type(dimless_control), intent(inout) :: dless_ctl
!!        type(equations_control), intent(inout) :: eqs_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
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
!!      end array
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
      use calypso_mpi
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
!>        Structure for list of dimensionless numbers
      type dimless_control
!>        Structure for list of dimensionless numbers
!!@n        dimless%c_tbl:  Name of each number 
!!@n        dimless%vect:   valus of each number
        type(ctl_array_cr) :: dimless
!
        integer (kind=kint) :: i_dimless_ctl =   0
      end type dimless_control
!
!>      Structure for coefficients of governing equations
      type equations_control
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
!   4th level for dimensionless numbers
!
      character(len=kchara), parameter :: hd_dimless =  'dimless_ctl'
!
!   4th level for coefficients
!
      character(len=kchara), parameter :: hd_momentum = 'momentum'
      character(len=kchara), parameter :: hd_thermal = 'thermal'
      character(len=kchara), parameter                                  &
     &        :: hd_dsc_diff_adv = 'composition'
      character(len=kchara), parameter :: hd_induction = 'induction'
!
      private :: hd_dimless
      private :: hd_momentum, hd_induction
      private :: hd_thermal, hd_dsc_diff_adv
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_dimless_ctl                                       &
     &         (id_control, hd_block, dless_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(dimless_control), intent(inout) :: dless_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(dless_ctl%i_dimless_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r(id_control,                         &
     &      hd_dimless, dless_ctl%dimless, c_buf)
      end do
      dless_ctl%i_dimless_ctl = 1
!
      end subroutine read_dimless_ctl
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
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(eqs_ctl%i_coef_term_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
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
!   --------------------------------------------------------------------
!
      subroutine bcast_dimless_ctl(dless_ctl)
!
      use bcast_control_arrays
!
      type(dimless_control), intent(inout) :: dless_ctl
!
!
      call bcast_ctl_array_cr(dless_ctl%dimless)
!
      call MPI_BCAST(dless_ctl%i_dimless_ctl, 1,                        &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_dimless_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_coef_term_ctl(eqs_ctl)
!
      type(equations_control), intent(inout) :: eqs_ctl
!
!
      call bcast_thermal_ctl(eqs_ctl%heat_ctl)
      call bcast_momentum_ctl(eqs_ctl%mom_ctl)
      call bcast_induction_ctl(eqs_ctl%induct_ctl)
      call bcast_thermal_ctl(eqs_ctl%comp_ctl)
!
      call MPI_BCAST(eqs_ctl%i_coef_term_ctl, 1,                        &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_coef_term_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_dimless_ctl(dless_ctl)
!
      type(dimless_control), intent(inout) :: dless_ctl
!
!
      call dealloc_control_array_c_r(dless_ctl%dimless)
      dless_ctl%i_dimless_ctl = 0
!
      end subroutine dealloc_dimless_ctl
!
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
