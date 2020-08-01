!>@file   t_base_field_labels.f90
!!        module t_base_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Addresses for basic fields
!!
!!@verbatim
!!      integer(kind = kint) function missing_field                     &
!!     &                   (iphys_tgt, target_name, iphys_ref, ref_name)
!!
!!      subroutine set_base_vector_addresses                            &
!!     &         (i_phys, field_name, base_fld, flag)
!!      subroutine set_base_scalar_addresses                            &
!!     &         (i_phys, field_name, base_fld, flag)
!!        type(base_field_address), intent(inout) :: base_fld
!!
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   velocity        [i_velo]:     velocity    u
!!   pressure        [i_press]:     pressure    P
!!   vorticity       [i_vort]:     vorticity   \omega = \nabra \times v
!!   system_Rotation [i_omega]:    System rotation   \Omega
!!
!!   vector_potential [i_vecp] :   vector potential \nabla \times A = B
!!   magnetic_field [i_magne]:     magnetic field   B
!!   current_density [i_current]:    current density  J = \nabla \times B
!!   background_B [i_back_B]: background magnetic field 
!!                                                  B_{0}
!!   magnetic_potential [i_mag_p]:   potential       \phi
!!   scalar_potential [i_scalar_p]:  scalar potential   \phi
!!
!!   temperature [i_temp]:  temperature T
!!   composition [i_light]:  Composition anormally C
!!   density [i_density]:      density     \rho
!!   entropy [i_entropy]:      Entropy               S
!!
!!   reference_temperature [i_ref_t]:   T_0
!!   reference_composition [i_ref_c]:   C_0
!!   reference_density [i_ref_density]:       \rho_0
!!   reference_entropy [i_ref_entropy]:       S_0
!!
!!   perturbation_temp [i_per_temp]:         \Theta = T - T_0
!!   perturbation_composition [i_per_light]:  C - C_0
!!   perturbation_density [i_per_density]:      \rho - \rho_0
!!   perturbation_entropy [i_per_entropy]:      S - S_0
!!
!!   heat_source [i_heat_source]:            heat source          q_{T}
!!   composition_source [i_light_source]:     compositoin source  q_{C}
!!   entropy_source [i_entropy_source]:         entropy source    q_{S}
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_base_field_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!
!>       Structure for start address for base fields
      type base_field_address
!>        Start address for velocity
!!         \f$ u_{i} \f$
        integer (kind=kint) :: i_velo  =           izero
!>        Start address for vorticity
!!         @f$ \omega_{i} = e_{ijk} \partial_{j} u_{k} @f$
        integer (kind=kint) :: i_vort  =           izero
!>        Start address for pressure
!!         @f$ p @f$
        integer (kind=kint) :: i_press =           izero
!!
!>        start address for rotation of ststem @f$ Omega @f$
!!          i_omega:   poloidal component
!!          i_omega+1: radial derivative of poloidal component
!!          i_omega+2: 2nd radial derivative of poloidal component
        integer (kind=kint) :: i_omega = izero
!
!>        start address for magnetic field
!!         @f$ B_{i} @f$
        integer (kind=kint) :: i_magne =           izero
!>        start address for magnetic vector potential
!!         @f$ B_{i} = e_{ijk} \partial_{j} A_{k} @f$
        integer (kind=kint) :: i_vecp =            izero
!>        start address for current density
!!         @f$ J_{i} = e_{ijk} \partial_{j} B_{k} @f$
        integer (kind=kint) :: i_current =         izero
!>        start address for magnetic potential
!!         @f$ W @f$
        integer (kind=kint) :: i_mag_p =           izero
!>        start address for electric potential
!!         @f$ \varphi @f$
        integer (kind=kint) :: i_scalar_p =        izero
!
!>        start address for background magnetic field @f$ B_{0} @f$
!!          i_back_B:   poloidal component
!!          i_back_B+1: radial derivative of poloidal component
!!          i_back_B+2: 2nd radial derivative of poloidal component
        integer (kind=kint) :: i_back_B = izero
!
!>        start address for temperature
!!         @f$ T @f$
        integer (kind=kint) :: i_temp  =           izero
!>        start address for perturbation of temperature
!!         @f$ \Theta = T - T_{0} @f$
        integer (kind=kint) :: i_per_temp =        izero
!>        start address for reference temperature
!!         @f$  T_{0} @f$
        integer (kind=kint) :: i_ref_t =           izero
!
!>        start address for compostiion variation
!!         @f$ C @f$
        integer (kind=kint) :: i_light =           izero
!>        start address for perturbation of composition
!!         @f$  C - C_{0} @f$
        integer (kind=kint) :: i_per_light =       izero
!>        start address for reference temperature
!!         @f$  C_{0} @f$
        integer (kind=kint) :: i_ref_c =           izero
!
!>        start address for density
!!         @f$ \rho @f$
        integer (kind=kint) :: i_density =         izero
!>        start address for perturbation of density
!!         @f$  \rho - \rho_{0} @f$
        integer (kind=kint) :: i_per_density =     izero
!>        start address for reference density
!!         @f$  \rho_{0} @f$
        integer (kind=kint) :: i_ref_density =     izero
!
!>        start address for entropy
!!         @f$ S @f$
        integer (kind=kint) :: i_entropy =         izero
!>        start address for perturbation of entropy
!!         @f$  S - S_{0} @f$
        integer (kind=kint) :: i_per_entropy =     izero
!>        start address for reference entropy
!!         @f$  S_{0} @f$
        integer (kind=kint) :: i_ref_entropy =     izero
!
!>        start address for heat source
!!         @f$ q_{T} @f$
        integer (kind=kint) :: i_heat_source =     izero
!>        start address for composion source
!!         @f$ q_{C} @f$
        integer (kind=kint) :: i_light_source =    izero
!>        start address for entropysource
!!         @f$ q_{S} @f$
        integer (kind=kint) :: i_entropy_source =  izero
      end type base_field_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function missing_field                       &
     &                   (iphys_tgt, target_name, iphys_ref, ref_name)
!
      integer(kind = kint), intent(in) :: iphys_tgt, iphys_ref
      character(len = kchara), intent(in) :: target_name, ref_name
!
      missing_field = 0
      if(iphys_ref .gt. 0) return
      write(*,*) iphys_tgt, ': Following fields are required for ',     &
     &          trim(target_name), ': ', trim(ref_name)
      missing_field = 1
      return
!
      end function missing_field
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_base_vector_addresses                              &
     &         (i_phys, field_name, base_fld, flag)
!
      use m_base_field_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: base_fld
      logical, intent(inout) :: flag
!
!
      flag = check_base_vector(field_name)
      if(flag) then
        if (field_name .eq. velocity%name) then
          base_fld%i_velo = i_phys
        else if (field_name .eq. vorticity%name) then
          base_fld%i_vort = i_phys
!
        else if (field_name .eq. magnetic_field%name) then
          base_fld%i_magne =    i_phys
        else if (field_name .eq. vector_potential%name) then
          base_fld%i_vecp =     i_phys
        else if (field_name .eq. current_density%name) then
          base_fld%i_current =  i_phys
!
        else if (field_name .eq. system_Rotation%name) then
          base_fld%i_omega =    i_phys
        else if (field_name .eq. background_B%name) then
          base_fld%i_back_B =   i_phys
        end if
      end if
!
      end subroutine set_base_vector_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_base_scalar_addresses                              &
     &         (i_phys, field_name, base_fld, flag)
!
      use m_base_field_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: base_fld
      logical, intent(inout) :: flag
!
!
      flag = check_base_scalar(field_name)
      if(flag) then
        if (field_name .eq. pressure%name) then
          base_fld%i_press = i_phys
        else if (field_name .eq. magnetic_potential%name) then
          base_fld%i_mag_p =    i_phys
        else if (field_name .eq. scalar_potential%name) then
          base_fld%i_scalar_p = i_phys
!
        else if (field_name .eq. temperature%name) then
          base_fld%i_temp =            i_phys
        else if (field_name .eq. perturbation_temp%name) then
          base_fld%i_per_temp =        i_phys
        else if (field_name .eq. reference_temperature%name) then
          base_fld%i_ref_t =           i_phys
!
        else if (field_name .eq. composition%name) then
          base_fld%i_light =          i_phys
        else if (field_name .eq. perturbation_composition%name) then
          base_fld%i_per_light =      i_phys
        else if (field_name .eq. reference_composition%name) then
          base_fld%i_ref_c =          i_phys
!
        else if (field_name .eq. density%name) then
          base_fld%i_density =        i_phys
        else if (field_name .eq. perturbation_density%name) then
          base_fld%i_per_density =    i_phys
        else if (field_name .eq. reference_density%name) then
          base_fld%i_ref_density =    i_phys
!
        else if (field_name .eq. entropy%name) then
          base_fld%i_entropy =        i_phys
        else if (field_name .eq. perturbation_entropy%name) then
          base_fld%i_per_entropy =    i_phys
        else if (field_name .eq. reference_entropy%name) then
          base_fld%i_ref_entropy =    i_phys
!
        else if (field_name .eq. heat_source%name) then
          base_fld%i_heat_source =    i_phys
        else if (field_name .eq. composition_source%name) then
          base_fld%i_light_source =   i_phys
        else if (field_name .eq. entropy_source%name) then
          base_fld%i_entropy_source = i_phys
        end if
      end if  
!
      end subroutine set_base_scalar_addresses
!
! ----------------------------------------------------------------------
!
      end module t_base_field_labels
