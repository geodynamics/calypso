!>@file   m_normalize_parameter.f90
!!@brief  module m_normalize_parameter
!!
!!@author H. Matsui
!!@date Programmed in 2005
!!@date Modified in Jan., 2007
!
!>@brief  dimensionless number list for each term
!!
!!@verbatim
!!      subroutine allocate_coef_4_termal
!!      subroutine allocate_coef_4_velocity
!!      subroutine allocate_coef_4_press
!!      subroutine allocate_coef_4_magne
!!      subroutine allocate_coef_4_mag_p
!!
!!      subroutine allocate_coef_4_t_diffuse
!!      subroutine allocate_coef_4_v_diffuse
!!      subroutine allocate_coef_4_m_diffuse
!!
!!      subroutine allocate_coef_4_buoyancy
!!      subroutine allocate_coef_4_comp_buo
!!      subroutine allocate_coef_4_coriolis
!!      subroutine allocate_coef_4_lorentz
!!      subroutine allocate_coef_4_induction
!!
!!      subroutine allocate_coef_4_composition
!!      subroutine allocate_coef_4_c_diffuse
!!
!!      subroutine allocate_coef_4_h_source
!!      subroutine allocate_coef_4_c_source
!!
!!
!!      subroutine deallocate_coef_4_termal
!!      subroutine deallocate_coef_4_velocity
!!      subroutine deallocate_coef_4_press
!!      subroutine deallocate_coef_4_magne
!!      subroutine deallocate_coef_4_mag_p
!!
!!      subroutine deallocate_coef_4_t_diffuse
!!      subroutine deallocate_coef_4_v_diffuse
!!      subroutine deallocate_coef_4_m_diffuse
!!
!!      subroutine deallocate_coef_4_buoyancy
!!      subroutine deallocate_coef_4_comp_buo
!!      subroutine deallocate_coef_4_coriolis
!!      subroutine deallocate_coef_4_lorentz
!!      subroutine deallocate_coef_4_induction
!!
!!      subroutine deallocate_coef_4_composition
!!      subroutine deallocate_coef_4_c_diffuse
!!
!!      subroutine deallocate_coef_4_h_source
!!      subroutine deallocate_coef_4_c_source
!!@endverbatim
!!
      module m_normalize_parameter
!
      use m_precision
!
      implicit  none
!
!
!>      Number of dimensionless numbers for heat flux
      integer(kind=kint) :: num_coef_4_termal
!>      Number of dimensionless numbers for momentum flux
      integer(kind=kint) :: num_coef_4_velocity
!>      Number of dimensionless numbers for pressure gradient
      integer(kind=kint) :: num_coef_4_press
!>      Number of dimensionless numbers for volution of magnetic field
      integer(kind=kint) :: num_coef_4_magnetic
!>      Number of dimensionless numbers for electric potential
      integer(kind=kint) :: num_coef_4_mag_p
!>      Number of dimensionless numbers for composition flux
      integer(kind=kint) :: num_coef_4_composition
!>      Number of dimensionless numbers for thermal diffusion
      integer(kind=kint) :: num_coef_4_t_diffuse
!>      Number of dimensionless numbers for viscous diffusion
      integer(kind=kint) :: num_coef_4_v_diffuse
!>      Number of dimensionless numbers for magnetic diffusion
      integer(kind=kint) :: num_coef_4_m_diffuse
!>      Number of dimensionless numbers for compositional diffusion
      integer(kind=kint) :: num_coef_4_c_diffuse
!>      Number of dimensionless numbers for thermal buoyancy
      integer(kind=kint) :: num_coef_4_buoyancy
!>      Number of dimensionless numbers for compositional buoyancy
      integer(kind=kint) :: num_coef_4_comp_buo
!>      Number of dimensionless numbers for Coriolis force
      integer(kind=kint) :: num_coef_4_Coriolis
!>      Number of dimensionless numbers for Lorengtz force
      integer(kind=kint) :: num_coef_4_Lorentz
!>      Number of dimensionless numbers for magnetic induction
      integer(kind=kint) :: num_coef_4_induction
!>      Number of dimensionless numbers for heat source
      integer(kind=kint) :: num_coef_4_h_source
!>      Number of dimensionless numbers for compositional source
      integer(kind=kint) :: num_coef_4_c_source
!
!>      Name of dimensionless numbers for heat flux
      character (len=kchara), allocatable :: coef_4_termal_name(:)
!>      Name of dimensionless numbers for momentum flux
      character (len=kchara), allocatable :: coef_4_velocity_name(:)
!>      Name of dimensionless numbers for pressure gradient
      character (len=kchara), allocatable :: coef_4_press_name(:)
!>      Name of dimensionless numbers for evolution of magnetic field
      character (len=kchara), allocatable :: coef_4_magnetic_name(:)
!>      Name of dimensionless numbers for electric potential
      character (len=kchara), allocatable :: coef_4_mag_p_name(:)
!>      Name of dimensionless numbers for composition flux
      character (len=kchara), allocatable :: coef_4_composit_name(:)
!>      Name of dimensionless numbers for thermal diffusion
      character (len=kchara), allocatable :: coef_4_t_diffuse_name(:)
!>      Name of dimensionless numbers for viscous diffusion
      character (len=kchara), allocatable :: coef_4_v_diffuse_name(:)
!>      Name of dimensionless numbers for magnetic diffusion
      character (len=kchara), allocatable :: coef_4_m_diffuse_name(:)
!>      Name of dimensionless numbers for compositional diffusion
      character (len=kchara), allocatable :: coef_4_c_diffuse_name(:)
!>      Name of dimensionless numbers for thermal buoyancy
      character (len=kchara), allocatable :: coef_4_buoyancy_name(:)
!>      Name of dimensionless numbers for compositional buoyancy
      character (len=kchara), allocatable :: coef_4_comp_buo_name(:)
!>      Name of dimensionless numbers for Coriolis force
      character (len=kchara), allocatable :: coef_4_Coriolis_name(:)
!>      Name of dimensionless numbers for Lorengtz force
      character (len=kchara), allocatable :: coef_4_Lorentz_name(:)
!>      Name of dimensionless numbers for magnetic induction
      character (len=kchara), allocatable :: coef_4_induction_name(:)
!>      Name of dimensionless numbers for heat source
      character (len=kchara), allocatable :: coef_4_h_source_name(:)
!>      Name of dimensionless numbers for light element source
      character (len=kchara), allocatable :: coef_4_c_source_name(:)
!
!>      Power of dimensionless numbers for heat flux
      real (kind = kreal), allocatable :: coef_4_termal_power(:)
!>      Power of dimensionless numbers for momentum flux
      real (kind = kreal), allocatable :: coef_4_velocity_power(:)
!>      Power of dimensionless numbers for pressure gradient
      real (kind = kreal), allocatable :: coef_4_press_power(:)
!>      Power of dimensionless numbers for evolution of magnetic field
      real (kind = kreal), allocatable :: coef_4_magnetic_power(:)
!>      Power of dimensionless numbers for electric potential
      real (kind = kreal), allocatable :: coef_4_mag_p_power(:)
!>      Power of dimensionless numbers for composition flux
      real (kind = kreal), allocatable :: coef_4_composit_power(:)
!>      Power of dimensionless numbers for thermal diffusion
      real (kind = kreal), allocatable :: coef_4_t_diffuse_power(:)
!>      Power of dimensionless numbers for viscous diffusion
      real (kind = kreal), allocatable :: coef_4_v_diffuse_power(:)
!>      Power of dimensionless numbers for magnetic diffusion
      real (kind = kreal), allocatable :: coef_4_m_diffuse_power(:)
!>      Power of dimensionless numbers for compositional diffusion
      real (kind = kreal), allocatable :: coef_4_c_diffuse_power(:)
!>      Power of dimensionless numbers for thermal buoyancy
      real (kind = kreal), allocatable :: coef_4_buoyancy_power(:)
!>      Power of dimensionless numbers for compositional buoyancy
      real (kind = kreal), allocatable :: coef_4_comp_buo_power(:)
!>      Power of dimensionless numbers for Coriolis force
      real (kind = kreal), allocatable :: coef_4_Coriolis_power(:)
!>      Power of dimensionless numbers for Lorengtz force
      real (kind = kreal), allocatable :: coef_4_Lorentz_power(:)
!>      Power of dimensionless numbers for magnetic induction
      real (kind = kreal), allocatable :: coef_4_induction_power(:)
!>      Power of dimensionless numbers for heat source
      real (kind = kreal), allocatable :: coef_4_h_source_power(:)
!>      Power of dimensionless numbers for light element source
      real (kind = kreal), allocatable :: coef_4_c_source_power(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_coef_4_termal
!
      allocate(coef_4_termal_name(num_coef_4_termal))
      allocate(coef_4_termal_power(num_coef_4_termal))
      if(num_coef_4_termal .gt. 0) coef_4_termal_power = 0.0d0
!
      end subroutine allocate_coef_4_termal
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_velocity
!
      allocate(coef_4_velocity_name(num_coef_4_velocity))
      allocate(coef_4_velocity_power(num_coef_4_velocity))
      if(num_coef_4_velocity .gt. 0) coef_4_velocity_power = 0.0d0
!
      end subroutine allocate_coef_4_velocity
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_press
!
      allocate(coef_4_press_name(num_coef_4_press))
      allocate(coef_4_press_power(num_coef_4_press))
      if(num_coef_4_press .gt. 0) coef_4_press_power = 0.0d0
!
      end subroutine allocate_coef_4_press
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_magne
!
      allocate(coef_4_magnetic_name(num_coef_4_magnetic))
      allocate(coef_4_magnetic_power(num_coef_4_magnetic))
      if(num_coef_4_magnetic .gt. 0) coef_4_magnetic_power = 0.0d0
!
      end subroutine allocate_coef_4_magne
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_mag_p
!
      allocate(coef_4_mag_p_name(num_coef_4_mag_p))
      allocate(coef_4_mag_p_power(num_coef_4_mag_p))
      if(num_coef_4_mag_p .gt. 0) coef_4_mag_p_power = 0.0d0
!
      end subroutine allocate_coef_4_mag_p
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_t_diffuse
!
      allocate(coef_4_t_diffuse_name(num_coef_4_t_diffuse))
      allocate(coef_4_t_diffuse_power(num_coef_4_t_diffuse))
      if(num_coef_4_t_diffuse .gt. 0) coef_4_t_diffuse_power = 0.0d0
!
      end subroutine allocate_coef_4_t_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_v_diffuse
!
      allocate(coef_4_v_diffuse_name(num_coef_4_v_diffuse))
      allocate(coef_4_v_diffuse_power(num_coef_4_v_diffuse))
      if(num_coef_4_v_diffuse .gt. 0) coef_4_v_diffuse_power = 0.0d0
!
      end subroutine allocate_coef_4_v_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_m_diffuse
!
      allocate(coef_4_m_diffuse_name(num_coef_4_m_diffuse))
      allocate(coef_4_m_diffuse_power(num_coef_4_m_diffuse))
      if(num_coef_4_m_diffuse .gt. 0) coef_4_m_diffuse_power = 0.0d0
!
      end subroutine allocate_coef_4_m_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_buoyancy
!
      allocate(coef_4_buoyancy_name(num_coef_4_buoyancy))
      allocate(coef_4_buoyancy_power(num_coef_4_buoyancy))
      if(num_coef_4_buoyancy .gt. 0) coef_4_buoyancy_power = 0.0d0
!
      end subroutine allocate_coef_4_buoyancy
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_comp_buo
!
      allocate(coef_4_comp_buo_name(num_coef_4_comp_buo))
      allocate(coef_4_comp_buo_power(num_coef_4_comp_buo))
      if(num_coef_4_comp_buo .gt. 0) coef_4_comp_buo_power = 0.0d0
!
      end subroutine allocate_coef_4_comp_buo
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_coriolis
!
      allocate(coef_4_Coriolis_name(num_coef_4_Coriolis))
      allocate(coef_4_Coriolis_power(num_coef_4_Coriolis))
      if(num_coef_4_Coriolis .gt. 0) coef_4_Coriolis_power = 0.0d0
!
      end subroutine allocate_coef_4_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_lorentz
!
      allocate(coef_4_Lorentz_name(num_coef_4_Lorentz))
      allocate(coef_4_Lorentz_power(num_coef_4_Lorentz))
      if(num_coef_4_Lorentz .gt. 0) coef_4_Lorentz_power = 0.0d0
!
      end subroutine allocate_coef_4_lorentz
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_induction
!
      allocate(coef_4_induction_name(num_coef_4_induction))
      allocate(coef_4_induction_power(num_coef_4_induction))
      if(num_coef_4_induction .gt. 0) coef_4_induction_power = 0.0d0
!
      end subroutine allocate_coef_4_induction
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_composition
!
      allocate(coef_4_composit_name(num_coef_4_composition))
      allocate(coef_4_composit_power(num_coef_4_composition))
      if(num_coef_4_composition .gt. 0) coef_4_composit_power = 0.0d0
!
      end subroutine allocate_coef_4_composition
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_c_diffuse
!
      allocate(coef_4_c_diffuse_name(num_coef_4_c_diffuse))
      allocate(coef_4_c_diffuse_power(num_coef_4_c_diffuse))
      if(num_coef_4_c_diffuse .gt. 0) coef_4_c_diffuse_power = 0.0d0
!
      end subroutine allocate_coef_4_c_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_h_source
!
      allocate(coef_4_h_source_name(num_coef_4_h_source))
      allocate(coef_4_h_source_power(num_coef_4_h_source))
      if(num_coef_4_h_source .gt. 0) coef_4_h_source_power = 0.0d0
!
      end subroutine allocate_coef_4_h_source
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_c_source
!
      allocate(coef_4_c_source_name(num_coef_4_c_source))
      allocate(coef_4_c_source_power(num_coef_4_c_source))
      if(num_coef_4_c_source .gt. 0) coef_4_c_source_power = 0.0d0
!
      end subroutine allocate_coef_4_c_source
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_coef_4_termal
!
      deallocate(coef_4_termal_name, coef_4_termal_power)
!
      end subroutine deallocate_coef_4_termal
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_velocity
!
      deallocate(coef_4_velocity_name, coef_4_velocity_power)
!
      end subroutine deallocate_coef_4_velocity
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_press
!
      deallocate(coef_4_press_name, coef_4_press_power)
!
      end subroutine deallocate_coef_4_press
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_magne
!
      deallocate(coef_4_magnetic_name, coef_4_magnetic_power)
!
      end subroutine deallocate_coef_4_magne
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_mag_p
!
      deallocate(coef_4_mag_p_name, coef_4_mag_p_power)
!
      end subroutine deallocate_coef_4_mag_p
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_t_diffuse
!
      deallocate(coef_4_t_diffuse_name, coef_4_t_diffuse_power)
!
      end subroutine deallocate_coef_4_t_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_v_diffuse
!
      deallocate(coef_4_v_diffuse_name, coef_4_v_diffuse_power)
!
      end subroutine deallocate_coef_4_v_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_m_diffuse
!
      deallocate(coef_4_m_diffuse_name, coef_4_m_diffuse_power)
!
      end subroutine deallocate_coef_4_m_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_buoyancy
!
      deallocate(coef_4_buoyancy_name, coef_4_buoyancy_power)
!
      end subroutine deallocate_coef_4_buoyancy
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_comp_buo
!
      deallocate(coef_4_comp_buo_name, coef_4_comp_buo_power)
!
      end subroutine deallocate_coef_4_comp_buo
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_coriolis
!
      deallocate(coef_4_Coriolis_name, coef_4_Coriolis_power)
!
      end subroutine deallocate_coef_4_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_lorentz
!
      deallocate(coef_4_Lorentz_name, coef_4_Lorentz_power)
!
      end subroutine deallocate_coef_4_lorentz
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_induction
!
      deallocate(coef_4_induction_name, coef_4_induction_power)
!
      end subroutine deallocate_coef_4_induction
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_composition
!
      deallocate(coef_4_composit_name, coef_4_composit_power)
!
      end subroutine deallocate_coef_4_composition
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_c_diffuse
!
      deallocate(coef_4_c_diffuse_name, coef_4_c_diffuse_power)
!
      end subroutine deallocate_coef_4_c_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_h_source
!
      deallocate(coef_4_h_source_name, coef_4_h_source_power)
!
      end subroutine deallocate_coef_4_h_source
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_c_source
!
      deallocate(coef_4_c_source_name, coef_4_c_source_power)
!
      end subroutine deallocate_coef_4_c_source
!
! -----------------------------------------------------------------------
!
      end module m_normalize_parameter
