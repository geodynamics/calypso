!>@file   t_normalize_parameter.f90
!!@brief  module t_normalize_parameter
!!
!!@author H. Matsui
!!@date Programmed in 2005
!!@date Modified in Jan., 2007
!
!>@brief  dimensionless number list for each term
!!
!!@verbatim
!!      subroutine alloc_dimless_list(dimless_list)
!!      subroutine dealloc_dimless_list(dimless_list)
!!        type(list_of_dimless), intent(inout) :: dimless_list
!!      subroutine alloc_coef_power_list(coefs_list)
!!      subroutine dealloc_coef_power_list(coefs_list)
!!        type(powers_4_coefficients), intent(inout) :: coefs_list
!!@endverbatim
!!
      module t_normalize_parameter
!
      use m_precision
!
      implicit  none
!
!
!>      Structure of dimensionless numbers
      type list_of_dimless
!>        Number of parameters
        integer(kind=kint) :: num
!>        List of dimensionless number's name
        character(len=kchara), allocatable :: name(:)
!>        List of values
        real (kind = kreal), allocatable :: value(:)
      end type list_of_dimless
!
!
!>      Structure of powers for coefficients
      type powers_4_coefficients
!>        Number of parameters
        integer(kind=kint) :: num
!>        List of parameter name
        character(len=kchara), allocatable :: name(:)
!>        List of power
        real (kind = kreal), allocatable :: power(:)
      end type powers_4_coefficients
!
!
!>       List of parameters to construct coefficients
      type coef_parameters_list
!>        Dimensionless numbers for heat flux
        type(list_of_dimless) :: dimless_list
!
!>        Dimensionless numbers for heat flux
        type(powers_4_coefficients) :: coefs_termal
!>        Dimensionless numbers for momentum flux
        type(powers_4_coefficients) :: coefs_momentum
!>        Dimensionless numbers for pressure gradient
        type(powers_4_coefficients) :: coefs_pressure
!>        Dimensionless numbers for heat evolution of magnetic field
        type(powers_4_coefficients) :: coefs_magnetic
!>        Dimensionless numbers for heat electric potential
        type(powers_4_coefficients) :: coefs_magne_p
!>        Dimensionless numbers for heat composition flux
        type(powers_4_coefficients) :: coefs_composition
!
!>        Dimensionless numbers for heat thermal diffusion
        type(powers_4_coefficients) :: coefs_t_diffuse
!>        Dimensionless numbers for heat viscous diffusion
        type(powers_4_coefficients) :: coefs_v_diffuse
!>        Dimensionless numbers for heat magnetic diffusion
        type(powers_4_coefficients) :: coefs_m_diffuse
!>        Dimensionless numbers for heat compositional diffusion
        type(powers_4_coefficients) :: coefs_c_diffuse
!
!>        Dimensionless numbers for heat thermal buoyancy flux
        type(powers_4_coefficients) :: coefs_buoyancy
!>        Dimensionless numbers for heat compositional buoyancy flux
        type(powers_4_coefficients) :: coefs_comp_buo
!>        Dimensionless numbers for heat Coriolis force
        type(powers_4_coefficients) :: coefs_Coriolis
!>        Dimensionless numbers for heat Lorengtz force
        type(powers_4_coefficients) :: coefs_Lorentz
!>        Dimensionless numbers for heat magnetic induction
        type(powers_4_coefficients) :: coefs_induction
!>        Dimensionless numbers for heat heat source
        type(powers_4_coefficients) :: coefs_h_source
!>        Dimensionless numbers for heat compositional source
        type(powers_4_coefficients) :: coefs_c_source
      end type coef_parameters_list
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dimless_list(dimless_list)
!
      type(list_of_dimless), intent(inout) :: dimless_list
!
!
      allocate(dimless_list%name(dimless_list%num))
      allocate(dimless_list%value(dimless_list%num))
      if(dimless_list%num .gt. 0) dimless_list%value = 0.0d0
!
      end subroutine alloc_dimless_list
!
! -----------------------------------------------------------------------
!
      subroutine alloc_coef_power_list(coefs_list)
!
      type(powers_4_coefficients), intent(inout) :: coefs_list
!
!
      allocate(coefs_list%name(coefs_list%num))
      allocate(coefs_list%power(coefs_list%num))
      if(coefs_list%num .gt. 0) coefs_list%power = 0.0d0
!
      end subroutine alloc_coef_power_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_dimless_list(dimless_list)
!
      type(list_of_dimless), intent(inout) :: dimless_list
!
!
      deallocate(dimless_list%name, dimless_list%value)
!
      end subroutine dealloc_dimless_list
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_coef_power_list(coefs_list)
!
      type(powers_4_coefficients), intent(inout) :: coefs_list
!
!
      deallocate(coefs_list%name, coefs_list%power)
!
      end subroutine dealloc_coef_power_list
!
! -----------------------------------------------------------------------
!
      end module t_normalize_parameter
