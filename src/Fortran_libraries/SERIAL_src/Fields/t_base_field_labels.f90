!>@file   t_base_field_labels.f90
!!        module t_base_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      integer(kind = kint) function missing_field                     &
!!     &                   (iphys_tgt, target_name, iphys_ref, ref_name)
!!
!!      logical function check_base_vector(field_name)
!!      logical function check_base_scalar(field_name)
!!      subroutine set_base_vector_addresses                            &
!!     &         (i_phys, field_name, base_fld, flag)
!!      subroutine set_base_scalar_addresses                            &
!!     &         (i_phys, field_name, base_fld, flag)
!!        type(base_field_address), intent(inout) :: base_fld
!!
!!      integer(kind = kint) function num_base_fields()
!!      subroutine set_base_scalar_addresses                            &
!!     &         (i_phys, field_name, base_fld, flag)
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
      integer(kind = kint), parameter, private :: nfld_base = 25
!
!>        Field label for velocity
!!         @f$ u_{i} @f$
      type(field_def), parameter :: velocity                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'velocity',                                &
     &                math = '$ u_{i} $')
!>        Field label for vorticity
!!         @f$ \omega_{i} = e_{ijk} \partial_{j} u_{k} @f$
      type(field_def), parameter :: vorticity                           &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vorticity',                               &
     &                math = '$ \omega_{i}'                             &
     &                    // ' = e_{ijk} \partial_{j} u_{k} $')
!>        Field label for pressure @f$ p @f$
      type(field_def), parameter :: pressure                            &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pressure',                                &
     &                math = '$ u_{i} $')
!!
!>        System rotation @f$ \Omega_{i} @f$
      type(field_def), parameter :: system_Rotation                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'system_Rotation',                         &
     &                math = '$ \Omega_{i} $')
!
!>        Field label for magnetic field
!!         @f$ B_{i} @f$
      type(field_def), parameter :: magnetic_field                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_field',                          &
     &                math = '$ B_{i} $')
!>        Field label for magnetic vector potential
!!         @f$ B_{i} = e_{ijk} \partial_{j} A_{k} @f$
      type(field_def), parameter :: vector_potential                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vector_potential',                        &
     &                math = '$ B_{i} = e_{ijk} \partial_{j} A_{k} $')
!>        Field label for current density
!!         @f$ J_{i} = e_{ijk} \partial_{j} B_{k} @f$
      type(field_def), parameter :: current_density                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'current_density',                         &
     &                math = '$ J_{i} = e_{ijk} \partial_{j} B_{k} $')
!>        Field label of backgroupnd  magnetic field
!!         @f$ B_{0} @f$
      type(field_def), parameter :: background_B                        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'background_B',                            &
     &                math = '$ B_{0} $')
!
!>        Field label for magnetic potential
!!         @f$ W @f$
      type(field_def), parameter :: magnetic_potential                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magnetic_potential',                      &
     &                math = '$ W $')
!>        Field label for electric potential
!!         @f$ \varphi @f$
      type(field_def), parameter :: scalar_potential                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'scalar_potential',                        &
     &                math = '$ \varphi $')
!!
!>        Field label for density
!!         @f$ \rho @f$
      type(field_def), parameter :: density                             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'density',                                 &
     &                math = '$ \rho $')
!>        Field label for perturbation of density
!!         @f$  \Thera_{\rho} = \rho - \rho_{0} @f$
      type(field_def), parameter :: perturbation_density                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'perturbation_density',                    &
     &                math = '$ \Thera_{\rho} = \rho - \rho_{0} $')
!>        Field label for reference density
!!         @f$  \rho_{0} @f$
      type(field_def), parameter :: reference_density                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'reference_density',                       &
     &                math = '$ \rho_{0} $')
!
!>        Field label for temperature
!!         @f$ T @f$
      type(field_def), parameter :: temperature                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'temperature',                             &
     &                math = '$ T $')
!>        Field label for perturbation of temperature
!!         @f$ \Theta = T - T_{0} @f$
      type(field_def), parameter :: perturbation_temp                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'perturbation_temp',                       &
     &                math = '$ \Theta = T - T_{0} $')
!>        Field label for reference temperature
!!         @f$  T_{0} @f$
      type(field_def), parameter :: reference_temperature               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'reference_temperature',                   &
     &                math = '$ T_{0} $')
!
!>        Field label for compostiion variation
!!         @f$ C @f$
      type(field_def), parameter :: composition                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'composition',                             &
     &                math = '$ C $')
!>        Field label for perturbation of composition
!!         @f$  C - C_{0} @f$
      type(field_def), parameter :: perturbation_composition            &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'perturbation_composition',                &
     &                math = '$ \Thera_{C} = C - C_{0} $')
!>        Field label for reference composition
!!         @f$  C_{0} @f$
      type(field_def), parameter :: reference_composition               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'reference_composition',                   &
     &                math = '$ C_{0} $')
!
!>        Field label for entropy
!!         @f$ S @f$
      type(field_def), parameter :: entropy                             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'entropy',                                 &
     &                math = '$ S $')
!>        Field label for perturbation of entropy
!!         @f$  S - S_{0} @f$
      type(field_def), parameter :: perturbation_entropy                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'perturbation_entropy',                    &
     &                math = '$ \Thera_{S} = S - S_{0} $')
!>        Field label for reference entropy
!!         @f$  S_{0} @f$
      type(field_def), parameter :: reference_entropy                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'reference_entropy',                       &
     &                math = '$ S_{0} $')
!
!>        Field label for heat source
!!         @f$ q_{T} @f$
      type(field_def), parameter :: heat_source                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'heat_source',                             &
     &                math = '$ q_{T} $')
!>        Field label for composion source
!!         @f$ q_{C} @f$
      type(field_def), parameter :: composition_source                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'composition_source',                      &
     &                math = '$ q_{C} $')
!>        Field label for entropysource
!!         @f$ q_{S} @f$
      type(field_def), parameter :: entropy_source                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'entropy_source',                          &
     &                math = '$ q_{S} $')
!>        Field label for entropysource
!!
!!
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
! -----------------------------------------------------------------------
!
      logical function check_base_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_vector                                                 &
     &   =    (field_name .eq. velocity%name)                           &
     &   .or. (field_name .eq. vorticity%name)                          &
     &   .or. (field_name .eq. magnetic_field%name)                     &
     &   .or. (field_name .eq. vector_potential%name)                   &
     &   .or. (field_name .eq. current_density%name)                    &
     &   .or. (field_name .eq. system_Rotation%name)                    &
     &   .or. (field_name .eq. background_B%name)
!
      end function check_base_vector
!
! ----------------------------------------------------------------------
!
      logical function check_base_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_scalar                                                 &
     &   =    (field_name .eq. pressure%name)                           &
     &   .or. (field_name .eq. magnetic_potential%name)                 &
     &   .or. (field_name .eq. scalar_potential%name)                   &
!
     &   .or. (field_name .eq. density%name)                            &
     &   .or. (field_name .eq. perturbation_density%name)               &
     &   .or. (field_name .eq. reference_density%name)                  &
!
     &   .or. (field_name .eq. temperature%name)                        &
     &   .or. (field_name .eq. perturbation_temp%name)                  &
     &   .or. (field_name .eq. reference_temperature%name)              &
!
     &   .or. (field_name .eq. composition%name)                        &
     &   .or. (field_name .eq. perturbation_composition%name)           &
     &   .or. (field_name .eq. reference_composition%name)              &
!
     &   .or. (field_name .eq. entropy%name)                            &
     &   .or. (field_name .eq. perturbation_entropy%name)               &
     &   .or. (field_name .eq. reference_entropy%name)                  &
!
     &   .or. (field_name .eq. heat_source%name)                        &
     &   .or. (field_name .eq. composition_source%name)                 &
     &   .or. (field_name .eq. entropy_source%name)
!
      end function check_base_scalar
!
! ----------------------------------------------------------------------
!
      subroutine set_base_vector_addresses                              &
     &         (i_phys, field_name, base_fld, flag)
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
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_base_fields()
      num_base_fields = nfld_base
      return
      end function num_base_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_base_field_names(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(nfld_base)
      character(len = kchara), intent(inout) :: names(nfld_base)
      character(len = kchara), intent(inout) :: maths(nfld_base)
!
!
      call set_field_labels(velocity,                                   &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(vorticity,                                  &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(pressure,                                   &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(system_Rotation,                            &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(magnetic_field,                             &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(vector_potential,                           &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(current_density,                            &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(background_B,                               &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(magnetic_potential,                         &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(scalar_potential,                           &
     &    n_comps(10), names(10), maths(10))
!
      call set_field_labels(temperature,                                &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(perturbation_temp,                          &
     &    n_comps(12), names(12), maths(12))
      call set_field_labels(reference_temperature,                      &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(heat_source,                                &
     &    n_comps(14), names(14), maths(14))
!
      call set_field_labels(composition,                                &
     &    n_comps(15), names(15), maths(15))
      call set_field_labels(perturbation_composition,                   &
     &    n_comps(16), names(16), maths(16))
      call set_field_labels(reference_composition,                      &
     &    n_comps(17), names(17), maths(17))
      call set_field_labels(composition_source,                         &
     &    n_comps(18), names(18), maths(18))
!
      call set_field_labels(entropy,                                    &
     &    n_comps(19), names(19), maths(19))
      call set_field_labels(perturbation_entropy,                       &
     &    n_comps(20), names(20), maths(20))
      call set_field_labels(reference_entropy,                          &
     &    n_comps(21), names(21), maths(21))
      call set_field_labels(entropy_source,                             &
     &    n_comps(22), names(22), maths(22))
!
      call set_field_labels(density,                                    &
     &    n_comps(23), names(23), maths(23))
      call set_field_labels(perturbation_density,                       &
     &    n_comps(24), names(24), maths(24))
      call set_field_labels(reference_density,                          &
     &    n_comps(25), names(25), maths(25))
!
      end subroutine set_base_field_names
!
! ----------------------------------------------------------------------
!
      end module t_base_field_labels
