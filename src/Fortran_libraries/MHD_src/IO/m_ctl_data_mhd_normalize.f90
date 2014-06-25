!m_ctl_data_mhd_normalize.f90
!      module m_ctl_data_mhd_normalize
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine deallocate_dimless_ctl
!
!      subroutine read_dimless_ctl
!      subroutine read_coef_term_ctl
!
!   --------------------------------------------------------------------
!    example
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
!    begin coefficients_ctl
!      begin thermal
!        ...
!      end  thermal
!!
!      begin momentum
!        ...
!      end  momentum
!!
!      begin induction
!        ...
!      end  induction
!!
!      begin composition
!        ...
!      end  composition
!!
!    end  coefficients_ctl
!!
!!   --------------------------------------------------------------------
!
      module m_ctl_data_mhd_normalize
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
      use t_read_control_arrays
!
      implicit  none
!
!
!>      Structure for list of dimensionless numbers
!!@n      coef_4_dimless_ctl%c_tbl:  Name of each number 
!!@n      coef_4_dimless_ctl%vect:   valus of each number
      type(ctl_array_cr), save :: coef_4_dimless_ctl
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_dimless_ctl =  'dimensionless_ctl'
      integer (kind=kint) :: i_dimless_ctl =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_coef_term_ctl ='coefficients_ctl'
      integer (kind=kint) :: i_coef_term_ctl = 0
!
!   4th level for dimensionless numbers
!
      character(len=kchara), parameter :: hd_dimless =  'dimless_ctl'
!
      private :: hd_dimless_ctl, hd_coef_term_ctl
      private :: i_dimless_ctl,  i_coef_term_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_dimless_ctl
!
      call dealloc_control_array_c_r(coef_4_dimless_ctl)
!
      end subroutine deallocate_dimless_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_dimless_ctl
!
!
      if(right_begin_flag(hd_dimless_ctl) .eq. 0) return
      if (i_dimless_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_dimless_ctl, i_dimless_ctl)
        if(i_dimless_ctl .gt. 0) exit
!
        call read_control_array_c_r(hd_dimless, coef_4_dimless_ctl)
      end do
!
      end subroutine read_dimless_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_coef_term_ctl
!
      use m_ctl_data_momentum_norm
      use m_ctl_data_termal_norm
      use m_ctl_data_induct_norm
      use m_ctl_data_composite_norm
!
!
      if(right_begin_flag(hd_coef_term_ctl) .eq. 0) return
      if (i_coef_term_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_coef_term_ctl, i_coef_term_ctl)
        if(i_coef_term_ctl .gt. 0) exit
!
        call read_thermal_ctl
        call read_momentum_ctl
        call read_induction_ctl
        call read_composition_eq_ctl
      end do
!
      end subroutine read_coef_term_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_mhd_normalize
