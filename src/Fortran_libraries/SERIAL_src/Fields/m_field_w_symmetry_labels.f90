!>@file   m_field_w_symmetry_labels.f90
!!        module m_field_w_symmetry_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields with equatorial symmetry
!!
!!@verbatim
!!      logical function check_base_vector_symmetry(field_name)
!!      logical function check_base_scalar_w_symmetry(field_name)
!!
!!      integer(kind = kint) function num_fields_w_symmetry()
!!      subroutine set_field_w_symmetry_labels(n_comps, names, maths)
!!
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   sym_velocity,           asym_velocity:           velocity    u
!!   sym_temperature,        asym_temperature:        temperature T
!!   sym_pressure,           asym_pressure:           pressure    P
!!   sym_density,            asym_density:            density     \rho
!!   sym_vorticity,          asym_vorticity:          vorticity   \omega
!!   sym_vector_potential,   asym_vector_potential:   vector potential A
!!   sym_magnetic_field,     asym_magnetic_field:     magnetic field B
!!   sym_current_density,    asym_current_density:    current density  J
!!   sym_magnetic_potential, asym_magnetic_potential: potential \phi
!!   sym_composition,        asym_composition:        Composition  C
!!   sym_entropy,            asym_entropy:            Entropy S
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_field_w_symmetry_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nfld_w_sym = 2 * 16
!
!>        Field label for symmetric velocity @f$ u_{sym} @f$
      type(field_def), parameter :: sym_velocity                        &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'sym_velocity',                          &
     &                  math = '$ u_{sym} $')
!>        Field label for asymmetric velocity @f$ u_{asym} @f$
      type(field_def), parameter :: asym_velocity                       &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'asym_velocity',                         &
     &                  math = '$ u_{asym} $')
!
!>        Field label for symmetric vorticity
!!         @f$ \omega_{sym} = e_{ijk} \partial_{j} u_{asym} @f$
      type(field_def), parameter :: sym_vorticity                       &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'sym_vorticity',                         &
     &                  math = '$ \omega_{sym} $')
!>        Field label for asymmetric vorticity
!!         @f$ \omega_{asym} = e_{ijk} \partial_{j} u_{sym} @f$
      type(field_def), parameter :: asym_vorticity                      &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'asym_vorticity',                        &
     &                  math = '$ \omega_{asym} $')
!
!>        Field label for symmetric pressure @f$ p_{sym} @f$
      type(field_def), parameter :: sym_pressure                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_pressure',                          &
     &                  math = '$ p_{sym} $')
!>        Field label for asymmetric pressure @f$ p_{asym} @f$
      type(field_def), parameter :: asym_pressure                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_pressure',                         &
     &                  math = '$ p_{asym} $')
!
!>        Field label for symmetric magnetic field @f$ B_{sym} @f$
      type(field_def), parameter :: sym_magnetic_field                  &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'sym_magnetic_field',                    &
     &                  math = '$ B_{sym} $')
!>        Field label for asymmetric magnetic field @f$ B_{asym} @f$
      type(field_def), parameter :: asym_magnetic_field                 &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'asym_magnetic_field',                   &
     &                  math = '$ B_{asym} $')
!
!>        Field label for symmetric  magnetic vector potential
!!         @f$ B_{asym} = e_{ijk} \partial_{j} A_{sym} @f$
      type(field_def), parameter :: sym_vector_potential                &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'sym_vector_potential',                  &
     &                  math = '$ A_{sym} $')
!>        Field label for asymmetric magnetic vector potential
!!         @f$ B_{sym} = e_{ijk} \partial_{j} A_{asym} @f$
      type(field_def), parameter :: asym_vector_potential               &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'asym_vector_potential',                 &
     &                  math = '$ A_{asym} $')
!
!>        Field label for symmetric current density
!!         @f$ J_{sym} = e_{ijk} \partial_{j} B_{k} @f$
      type(field_def), parameter :: sym_current_density                 &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'sym_current_density',                   &
     &                  math = '$ J_{sym} $')
!>        Field label for asymmetric current density
!!         @f$ J_{asym} = e_{ijk} \partial_{j} B_{k} @f$
      type(field_def), parameter :: asym_current_density                &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'asym_current_density',                  &
     &                  math = '$ J_{asym} $')
!
!>        Field label for symmetric  magnetic potential @f$ W_{sym} @f$
      type(field_def), parameter :: sym_magnetic_potential              &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_magnetic_potential',                &
     &                  math = '$ W_{sym} $')
!>        Field label for asymmetric magnetic potential @f$ W_{asym} @f$
      type(field_def), parameter :: asym_magnetic_potential             &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_magnetic_potential',               &
     &                  math = '$ W_{asym} $')
!
!>        Field label for symmetric  electric potential @f$ \varphi @f$
      type(field_def), parameter :: sym_scalar_potential                &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_scalar_potential',                  &
     &                  math = '$ \varphi_{sym} $')
!>        Field label for asymmetric electric potential @f$ \varphi @f$
      type(field_def), parameter :: asym_scalar_potential               &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_scalar_potential',                 &
     &                  math = '$ \varphi_{asym} $')
!
!
!>        Field label for symmetric density @f$ \rho_{sym} @f$
      type(field_def), parameter :: sym_density                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_density',                           &
     &                  math = '$ \rho_{sym} $')
!>        Field label for asymmetric density @f$ \rho_{asym} @f$
      type(field_def), parameter :: asym_density                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_density',                          &
     &                  math = '$ \rho_{asym} $')
!
!>        Field label for symmetric temperature @f$ T_{sym} @f$
      type(field_def), parameter :: sym_temperature                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_temperature',                       &
     &                  math = '$ T_{sym} $')
!>        Field label for asymmetric temperature @f$ T_{asym} @f$
      type(field_def), parameter :: asym_temperature                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_temperature',                      &
     &                  math = '$ T_{asym} $')
!
!>        Field label for symmetric compostiion variation               &
!!        @f$ C_{sym} @f$
      type(field_def), parameter :: sym_composition                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_composition',                       &
     &                  math = '$ C_{sym} $')
!>        Field label for asymmetric compostiion variation              &
!!        @f$ C_{asym} @f$
      type(field_def), parameter :: asym_composition                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_composition',                      &
     &                  math = '$ C_{asym} $')
!
!>        Field label for symmetric entropy @f$ S_{sym} @f$
      type(field_def), parameter :: sym_entropy                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_entropy',                           &
     &                  math = '$ S_{sym} $')
!>        Field label for asymmetric entropy @f$ S_{asym} @f$
      type(field_def), parameter :: asym_entropy                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_entropy',                          &
     &                  math = '$ S_{asym} $')
!
!>        Field label for symmetric perturbation of density
!!         @f$  \rho_{sym} - \rho_{0} @f$
      type(field_def), parameter :: sym_perturbation_density            &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_perturbation_density',              &
     &                  math = '$ \rho_{sym} - \rho_{0} $')
!>        Field label for asymmetric perturbation of density
!!         @f$  \rho_{asym} - \rho_{0} @f$
      type(field_def), parameter :: asym_perturbation_density           &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_perturbation_density',             &
     &                  math = '$ \rho_{asym} - \rho_{0} $')
!
!>        Field label for symmetric perturbation of temperature
!!         @f$ \Theta_{ssym} = T_{sym} - T_{0} @f$
      type(field_def), parameter :: sym_perturbation_temp               &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_perturbation_temp',                 &
     &                  math = '$ \Theta_{sym} $')
!>        Field label for asymmetric perturbation of temperature
!!         @f$ \Theta_{asym} = T_{asym} - T_{0} @f$
      type(field_def), parameter :: asym_perturbation_temp              &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_perturbation_temp',                &
     &                  math = '$ \Theta_{asym} $')
!
!>        Field label for symmetric perturbation of composition
!!         @f$  C_{sym} - C_{0} @f$
      type(field_def), parameter :: sym_perturbation_composition        &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_perturbation_composition',          &
     &                  math = '$ C_{sym} - C_{0} $')
!>        Field label for asymmetric perturbation of composition
!!         @f$  C_{asym} - C_{0} @f$
      type(field_def), parameter :: asym_perturbation_composition       &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_perturbation_composition',         &
     &                  math = '$ C_{asym} - C_{0} $')
!
!>        Field label for symmetric perturbation of entropy
!!         @f$  S_{sym} - S_{0} @f$
      type(field_def), parameter :: sym_perturbation_entropy            &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'sym_perturbation_entropy',              &
     &                  math = '$ S_{sym} - S_{0} $')
!>        Field label for asymmetric perturbation of entropy
!!         @f$  S_{asym} - S_{0} @f$
      type(field_def), parameter :: asym_perturbation_entropy           &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'asym_perturbation_entropy',             &
     &                  math = '$ S_{asym} - S_{0} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_base_vector_symmetry(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_vector_symmetry = .FALSE.
      if (    (field_name .eq. sym_velocity%name)                       &
     &   .or. (field_name .eq. sym_vorticity%name)                      &
     &   .or. (field_name .eq. sym_magnetic_field%name)                 &
     &   .or. (field_name .eq. sym_vector_potential%name)               &
     &   .or. (field_name .eq. sym_current_density%name)                &
     &   .or. (field_name .eq. asym_velocity%name)                      &
     &   .or. (field_name .eq. asym_vorticity%name)                     &
     &   .or. (field_name .eq. asym_magnetic_field%name)                &
     &   .or. (field_name .eq. asym_vector_potential%name)              &
     &   .or. (field_name .eq. asym_current_density%name)               &
     &       )   check_base_vector_symmetry = .TRUE.
!
      end function check_base_vector_symmetry
!
! ----------------------------------------------------------------------
!
      logical function check_base_scalar_w_symmetry(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_scalar_w_symmetry = .FALSE.
      if (    (field_name .eq. sym_pressure%name)                       &
     &   .or. (field_name .eq. sym_magnetic_potential%name)             &
     &   .or. (field_name .eq. sym_scalar_potential%name)               &
     &   .or. (field_name .eq. sym_density%name)                        &
     &   .or. (field_name .eq. sym_temperature%name)                    &
     &   .or. (field_name .eq. sym_composition%name)                    &
     &   .or. (field_name .eq. sym_entropy%name)                        &
     &   .or. (field_name .eq. sym_perturbation_density%name)           &
     &   .or. (field_name .eq. sym_perturbation_temp%name)              &
     &   .or. (field_name .eq. sym_perturbation_composition%name)       &
     &   .or. (field_name .eq. sym_perturbation_entropy%name)           &
     &   .or. (field_name .eq. asym_pressure%name)                      &
     &   .or. (field_name .eq. asym_magnetic_potential%name)            &
     &   .or. (field_name .eq. asym_scalar_potential%name)              &
     &   .or. (field_name .eq. asym_density%name)                       &
     &   .or. (field_name .eq. asym_temperature%name)                   &
     &   .or. (field_name .eq. asym_composition%name)                   &
     &   .or. (field_name .eq. asym_entropy%name)                       &
     &   .or. (field_name .eq. asym_perturbation_density%name)          &
     &   .or. (field_name .eq. asym_perturbation_temp%name)             &
     &   .or. (field_name .eq. asym_perturbation_composition%name)      &
     &   .or. (field_name .eq. asym_perturbation_entropy%name)          &
     &      )   check_base_scalar_w_symmetry = .TRUE.
!
      end function check_base_scalar_w_symmetry
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_fields_w_symmetry()
      num_fields_w_symmetry = nfld_w_sym
      return
      end function num_fields_w_symmetry
!
! ----------------------------------------------------------------------
!
      subroutine set_field_w_symmetry_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nfld_w_sym)
      character(len = kchara), intent(inout) :: names(nfld_w_sym)
      character(len = kchara), intent(inout) :: maths(nfld_w_sym)
!
!
      call set_field_labels(sym_velocity,                               &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(asym_velocity,                              &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(sym_vorticity,                              &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(asym_vorticity,                             &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(sym_pressure,                               &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(asym_pressure,                              &
     &    n_comps( 6), names( 6), maths( 6))
!
      call set_field_labels(sym_magnetic_field,                         &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(asym_magnetic_field,                        &
     &    n_comps( 8), names( 8), maths( 8))
!
      call set_field_labels(sym_vector_potential,                       &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(asym_vector_potential,                      &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(sym_current_density,                        &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(asym_current_density,                       &
     &    n_comps(12), names(12), maths(12))
!
      call set_field_labels(sym_magnetic_potential,                     &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(asym_magnetic_potential,                    &
     &    n_comps(14), names(14), maths(14))
      call set_field_labels(sym_scalar_potential,                       &
     &    n_comps(15), names(15), maths(15))
      call set_field_labels(asym_scalar_potential,                      &
     &    n_comps(16), names(16), maths(16))
!
      call set_field_labels(sym_density,                                &
     &    n_comps(17), names(17), maths(17))
      call set_field_labels(asym_density,                               &
     &    n_comps(18), names(18), maths(18))
      call set_field_labels(sym_temperature,                            &
     &    n_comps(19), names(19), maths(19))
      call set_field_labels(asym_temperature,                           &
     &    n_comps(20), names(20), maths(20))
      call set_field_labels(sym_composition,                            &
     &    n_comps(21), names(21), maths(21))
      call set_field_labels(asym_composition,                           &
     &    n_comps(22), names(22), maths(22))
      call set_field_labels(sym_entropy,                                &
     &    n_comps(23), names(23), maths(23))
      call set_field_labels(asym_entropy,                               &
     &    n_comps(24), names(24), maths(24))
!
      call set_field_labels(sym_perturbation_density,                   &
     &    n_comps(25), names(25), maths(25))
      call set_field_labels(asym_perturbation_density,                  &
     &    n_comps(26), names(26), maths(26))
      call set_field_labels(sym_perturbation_temp,                      &
     &    n_comps(27), names(27), maths(27))
      call set_field_labels(asym_perturbation_temp,                     &
     &    n_comps(28), names(28), maths(28))
      call set_field_labels(sym_perturbation_composition,               &
     &    n_comps(29), names(29), maths(29))
      call set_field_labels(asym_perturbation_composition,              &
     &    n_comps(30), names(30), maths(30))
      call set_field_labels(sym_perturbation_entropy,                   &
     &    n_comps(31), names(31), maths(31))
      call set_field_labels(asym_perturbation_entropy,                  &
     &    n_comps(32), names(32), maths(32))
!
      end subroutine set_field_w_symmetry_labels
!
! ----------------------------------------------------------------------
!
      end module m_field_w_symmetry_labels
