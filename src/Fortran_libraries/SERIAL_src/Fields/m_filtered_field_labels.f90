!>@file   m_filtered_field_labels.f90
!!        module m_filtered_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      logical function check_filter_vector(field_name)
!!      logical function check_filter_scalar(field_name)
!!
!!      integer(kind = kint) function num_filter_fields()
!!      subroutine set_filter_field_labels(n_comps, names, maths)
!!
!! !!!!!  Filtered field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names (Single filtered, wide filtered, double filtered)
!!
!!   filter_velocity    [filter_fld%i_velo]:     filtered velocity    u
!!   filter_vorticity   [filter_fld%i_vort]: 
!!            filtered vorticity   \omega = \nabra \times v
!!
!!   filter_vector_potential  [filter_fld%i_vecp]: 
!!            filtered vector potential \nabla \times A = B
!!   filter_magne      [filter_fld%i_magne]: filtered magnetic field   B
!!   filter_current    [filter_fld%i_current]: 
!!            filtered current density  J = \nabla \times B
!!
!!   filter_temperature  [filter_fld%i_temp]:  filtered temperature              T
!!   filter_composition  [filter_fld%i_light]:
!!                              filtered Composition anormally   C
!!   filter_density      [filter_fld%i_density]: filtered density  \rho
!!   filter_entropy      [filter_fld%i_entropy]: filtered Entropy  S
!!
!!   filter_pert_temperature   [filter_fld%i_per_temp]: \Theta = T - T_0
!!   filter_pert_composition   [filter_fld%i_per_light]:    C - C_0
!!   filter_pert_density       [filter_fld%i_per_density]: \rho - \rho_0
!!   filter_pert_entropy       [filter_fld%i_per_entropy]:  S - S_0
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_filtered_field_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nfld_filter = 13
!
!  filtered field
!
!>        Field label for filtered velocity
!!         @f$ \tilde{u}_{i} @f$
      type(field_def), parameter :: filter_velocity                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filter_velocity',                         &
     &                math = '$ \tilde{u}_{i} $')
!>        Field label for filtered velocity
!!         @f$ \tilde{\omega}_{i} @f$
      type(field_def), parameter :: filter_vorticity                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filter_vorticity',                        &
     &                math = '$ \tilde{\omega}_{i} $')
!!
!>        Field label for filtered magnetic field
!!         @f$ \tilde{B}_{i} @f$
      type(field_def), parameter :: filter_magne                        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filter_magne',                            &
     &                math = '$ \tilde{B}_{i} $')
!>        Field label for filtered current density
!!         @f$ \tilde{J}_{i} @f$
      type(field_def), parameter :: filter_current                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filter_current',                          &
     &                math = '$ \tilde{J}_{i} $')
!>        Field label for filtered vetor potential
!!         @f$ \tilde{A}_{i} @f$
      type(field_def), parameter :: filter_vector_potential             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filter_vector_potential',                 &
     &                math = '$ \tilde{A}_{i} $')
!
!>        Field label for filtered temperature
!!         @f$ \tilde{T} @f$
      type(field_def), parameter :: filter_temperature                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_temperature',                      &
     &                math = '$ \tilde{T} $')
!>        Field label for filtered perturbation of temperature
!!         @f$ \tilde{\Theta} @f$
      type(field_def), parameter :: filter_pert_temperature             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_pert_temperature',                 &
     &                math = '$ \tilde{\Theta} $')
!
!>        Field label for filtered conposition
!!         @f$ \tilde{C} @f$
      type(field_def), parameter :: filter_composition                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_composition',                      &
     &                math = '$ \tilde{C} $')
!>        Field label for filtered conposition
!!         @f$ \tilde{C} - C_{0} @f$
      type(field_def), parameter :: filter_pert_composition             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_pert_composition',                 &
     &                math = '$ \tilde{\Theta}_{C} $')
!
!>        Field label for filtered density
!!         @f$ \tilde{\rho} @f$
      type(field_def), parameter :: filter_density                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_density',                          &
     &                math = '$ \tilde{\rho} $')
!>        Field label for filtered perturbation of density
!!         @f$ \tilde{\rho} - rho_{0} @f$
      type(field_def), parameter :: filter_pert_density                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_pert_density',                     &
     &                math = '$ \tilde{\Theta}_{\rho} $')
!
!>        Field label for filtered entropy
!!         @f$ \tilde{S} @f$
      type(field_def), parameter :: filter_entropy                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_entropy',                          &
     &                math = '$ \tilde{S} $')
!>        Field label for filtered perturbation of entropy
!!         @f$ \tilde{S} - S_{0} @f$
      type(field_def), parameter :: filter_pert_entropy                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_pert_entropy',                     &
     &                math = '$ \tilde{\Theta}_{S} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_filter_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filter_vector = .FALSE.
      if (    (field_name .eq. filter_velocity%name)                    &
     &   .or. (field_name .eq. filter_vorticity%name)                   &
     &   .or. (field_name .eq. filter_magne%name)                       &
     &   .or. (field_name .eq. filter_vector_potential%name)            &
     &   .or. (field_name .eq. filter_current%name)                     &
     &      )   check_filter_vector = .TRUE.
!
      end function check_filter_vector
!
! ----------------------------------------------------------------------
!
      logical function check_filter_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filter_scalar                                               &
     &   =    (field_name .eq. filter_temperature%name)                 &
     &   .or. (field_name .eq. filter_composition%name)                 &
     &   .or. (field_name .eq. filter_density%name)                     &
     &   .or. (field_name .eq. filter_entropy%name)                     &
!
     &   .or. (field_name .eq. filter_pert_temperature%name)            &
     &   .or. (field_name .eq. filter_pert_composition%name)            &
     &   .or. (field_name .eq. filter_pert_density%name)                &
     &   .or. (field_name .eq. filter_pert_entropy%name)
!
      end function check_filter_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_filter_fields()
      num_filter_fields = nfld_filter
      return
      end function num_filter_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_filter_field_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nfld_filter)
      character(len = kchara), intent(inout) :: names(nfld_filter)
      character(len = kchara), intent(inout) :: maths(nfld_filter)
!
!
      call set_field_labels(filter_velocity,                            &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(filter_vorticity,                           &
     &    n_comps( 2), names( 2), maths( 2))
!
      call set_field_labels(filter_magne,                               &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(filter_current,                             &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(filter_vector_potential,                    &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(filter_temperature,                         &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(filter_pert_temperature,                    &
     &    n_comps( 7), names( 7), maths( 7))
!
      call set_field_labels(filter_composition,                         &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(filter_pert_composition,                    &
     &    n_comps( 9), names( 9), maths( 9))
!
      call set_field_labels(filter_density,                             &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(filter_pert_density,                        &
     &    n_comps(11), names(11), maths(11))
!
      call set_field_labels(filter_entropy,                             &
     &    n_comps(12), names(12), maths(12))
      call set_field_labels(filter_pert_entropy,                        &
     &    n_comps(13), names(13), maths(13))
!
      end subroutine set_filter_field_labels
!
! ----------------------------------------------------------------------
!
      end module m_filtered_field_labels
