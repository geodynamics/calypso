!>@file   m_explicit_term_labels.f90
!!        module m_explicit_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      logical function check_vector_work_field(field_name)
!!      logical function check_scalar_work_field(field_name)
!!      logical function check_work_4_poisson(field_name)
!!      logical function check_vector_check_field(field_name)
!!      logical function check_scalar_check_field(field_name)
!!
!!      subroutine set_explicit_work_names(array_c2i)
!!      subroutine set_check_fields_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  force include SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!      Explicit terms at previous time step
!!    sum_forces       [exp_work%i_forces]:     Total force
!!    rot_sum_forces   [exp_work%i_rot_forces]: Rotation of toatl force
!!    div_sum_forces   [exp_work%i_div_forces]: Divergence of total force
!!
!!      Explicit terms at previous time step
!!    previous_momentum    [exp_work%i_pre_mom]
!!    previous_induction   [exp_work%i_pre_uxb]
!!    previous_heat        [exp_work%i_pre_heat]
!!    previous_composition [exp_work%i_pre_composit]
!!    previous_pressure    [exp_work%i_pre_press]
!!    previous_potential   [exp_work%i_pre_phi]
!!
!!
!!    pressure_work     [i_p_phi]: work for pressure Poisson equation
!!    m_potential_work  [i_m_phi]: work for potential Poisson equation
!!
!!      Check of explicit terms
!!    check_momentum       [check_fld1%i_pre_mom]
!!    check_induction      [check_fld1%i_pre_uxb]
!!    check_heat           [check_fld1%i_pre_heat]
!!    check_composition    [check_fld1%i_pre_composit]
!!    check_pressure       [check_fld1%i_pre_press]
!!    check_potential      [check_fld1%i_pre_phi]
!!
!!      Second check of explicit terms
!!    check_momentum_2     [check_fld2%i_pre_mom]
!!    check_induction_2    [check_fld2%i_pre_uxb]
!!    check_heat_2         [check_fld2%i_pre_heat]
!!    check_composition_2  [check_fld2%i_pre_composit]
!!    check_pressure_2     [check_fld2%i_pre_press]
!!    check_potential_2    [check_fld2%i_pre_phi]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_explicit_term_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!  arrays for current forces
!
!>        Field label for total forces
      type(field_def), parameter :: sum_forces                          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'sum_forces',                              &
     &                math = '$ F_{i} $')
!>        Field label for curl of total forces
      type(field_def), parameter :: rot_sum_forces                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_sum_forces',                          &
     &                math = '$ e_{ijk} \partial_{j} F_{k} $')
!>        Field label for divergence of total forces
      type(field_def), parameter :: div_sum_forces                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_sum_forces',                          &
     &                math = '$ \partial_{i} F_{i} $')
!
!  arrays for previous evolution
!
!>        Field label for explicit term for momentum at previous step
      type(field_def), parameter :: previous_momentum                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'previous_momentum',                       &
     &                math = '$ F_{u}^{N-1} $')
!>        Field label for explicit term for induction at previous step
      type(field_def), parameter :: previous_induction                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'previous_induction',                      &
     &                math = '$ F_{B}^{N-1} $')
!>        Field label for explicit term for heat at previous step
      type(field_def), parameter :: previous_heat                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'previous_heat',                           &
     &                math = '$ F_{T}^{N-1} $')
!>        Field label for explicit term for composition
!!        at previous step
      type(field_def), parameter :: previous_composition                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'previous_composition',                    &
     &                math = '$ F_{C}^{N-1} $')
!>        Field label for explicit term for pressure at previous step
      type(field_def), parameter :: previous_pressure                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'previous_pressure',                       &
     &                math = '$ P^{N-1} $')
!>        Field label for explicit term for potential at previous step
      type(field_def), parameter :: previous_potential                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'previous_potential',                      &
     &                math = '$ \varphi^{N-1} $')
!
!     Work area for Poisson equation
!
!>        Field label of work area for pressure
!!         @f$ \varphi @f$
      type(field_def), parameter :: pressure_work                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pressure_work',                           &
     &                math = '$ \varphi $')
!>        Field label of work area for scalar potential
!!         @f$ \varphi @f$
      type(field_def), parameter :: m_potential_work                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'm_potential_work',                        &
     &                math = '$ \varphi $')
!
!  arrays for evolution check
!
!>        Field label for explicit term for momentum for check
      type(field_def), parameter :: check_momentum                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'check_momentum',                          &
     &                math = '$ u_{i}^{N-1} $')
!>        Field label for explicit term for induction for check
      type(field_def), parameter :: check_induction                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'check_induction',                         &
     &                math = '$ B_{i}^{N-1} $')
!>        Field label for explicit term for heat for check
      type(field_def), parameter :: check_heat                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_heat',                              &
     &                math = '$ T^{N-1} $')
!>        Field label for explicit term for composition
!!        for check
      type(field_def), parameter :: check_composition                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_composition',                       &
     &                math = '$ C^{N-1} $')
!>        Field label for explicit term for pressure for check
      type(field_def), parameter :: check_pressure                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_pressure',                          &
     &                math = '$ P^{N-1} $')
!>        Field label for explicit term for electric potential
!!        for check
      type(field_def), parameter :: check_potential                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_potential',                         &
     &                math = '$ \varphi^{N-1} $')
!
!>        Field label for explicit term for momentum for 2nd check
      type(field_def), parameter :: check_momentum_2                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'check_momentum_2',                        &
     &                math = '$ u_{i}^{N-2} $')
!>        Field label for explicit term for induction for 2nd check
      type(field_def), parameter :: check_induction_2                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'check_induction_2',                       &
     &                math = '$ B_{i}^{N-2} $')
!>        Field label for explicit term for heat for 2nd check
      type(field_def), parameter :: check_heat_2                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_heat_2',                            &
     &                math = '$ T^{N-2} $')
!>        Field label for explicit term for composition
!!        for 2nd check
      type(field_def), parameter :: check_composition_2                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_composition_2',                     &
     &                math = '$ C^{N-2} $')
!>        Field label for explicit term for pressure for 2nd check
      type(field_def), parameter :: check_pressure_2                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_pressure_2',                        &
     &                math = '$ P^{N-2} $')
!>        Field label for explicit term for electric potential
!!        for 2nd check
      type(field_def), parameter :: check_potential_2                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_potential_2',                       &
     &                math = '$ \varphi^{N-2} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_vector_work_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_vector_work_field                                           &
     &   =    (field_name .eq. sum_forces%name)                         &
     &   .or. (field_name .eq. rot_sum_forces%name)                     &
!
     &   .or. (field_name .eq. previous_momentum%name)                  &
     &   .or. (field_name .eq. previous_induction%name)
!
      end function check_vector_work_field
!
! ----------------------------------------------------------------------
!
      logical function check_scalar_work_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_scalar_work_field                                           &
     &   =    (field_name .eq. div_sum_forces%name)                     &
!
     &   .or. (field_name .eq. previous_heat%name)                      &
     &   .or. (field_name .eq. previous_composition%name)               &
     &   .or. (field_name .eq. previous_pressure%name)                  &
     &   .or. (field_name .eq. previous_potential%name)
!
      end function check_scalar_work_field
!
! ----------------------------------------------------------------------
!
      logical function check_work_4_poisson(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_work_4_poisson = .FALSE.
      if (    (field_name .eq. pressure_work%name)                      &
     &   .or. (field_name .eq. m_potential_work%name)                   &
     &      )   check_work_4_poisson = .TRUE.
!
      end function check_work_4_poisson
!
! ----------------------------------------------------------------------
!
      logical function check_vector_check_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_vector_check_field                                          &
     &   =    (field_name .eq. check_momentum%name)                     &
     &   .or. (field_name .eq. check_induction%name)                    &
!
     &   .or. (field_name .eq. check_momentum_2%name)                   &
     &   .or. (field_name .eq. check_induction_2%name)
!
      end function check_vector_check_field
!
! ----------------------------------------------------------------------
!
      logical function check_scalar_check_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_scalar_check_field                                          &
     &   =    (field_name .eq. check_heat%name)                         &
     &   .or. (field_name .eq. check_composition%name)                  &
     &   .or. (field_name .eq. check_pressure%name)                     &
     &   .or. (field_name .eq. check_potential%name)                    &
!
     &   .or. (field_name .eq. check_heat_2%name)                       &
     &   .or. (field_name .eq. check_composition_2%name)                &
     &   .or. (field_name .eq. check_pressure_2%name)                   &
     &   .or. (field_name .eq. check_potential_2%name)
!
      end function check_scalar_check_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_explicit_work_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(sum_forces,           array_c2i)
      call set_field_label_to_ctl(rot_sum_forces,       array_c2i)
      call set_field_label_to_ctl(div_sum_forces,       array_c2i)
      call set_field_label_to_ctl(previous_momentum,    array_c2i)
      call set_field_label_to_ctl(previous_induction,   array_c2i)
      call set_field_label_to_ctl(previous_heat,        array_c2i)
      call set_field_label_to_ctl(previous_composition, array_c2i)
      call set_field_label_to_ctl(previous_pressure,    array_c2i)
      call set_field_label_to_ctl(previous_potential,   array_c2i)
      call set_field_label_to_ctl(pressure_work,        array_c2i)
      call set_field_label_to_ctl(m_potential_work,     array_c2i)
!
      end subroutine set_explicit_work_names
!
! ----------------------------------------------------------------------
!
      subroutine set_check_fields_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(check_momentum,      array_c2i)
      call set_field_label_to_ctl(check_induction,     array_c2i)
      call set_field_label_to_ctl(check_heat,          array_c2i)
      call set_field_label_to_ctl(check_composition,   array_c2i)
      call set_field_label_to_ctl(check_pressure,      array_c2i)
      call set_field_label_to_ctl(check_potential,     array_c2i)
      call set_field_label_to_ctl(check_momentum_2,    array_c2i)
      call set_field_label_to_ctl(check_induction_2,   array_c2i)
      call set_field_label_to_ctl(check_heat_2,        array_c2i)
      call set_field_label_to_ctl(check_composition_2, array_c2i)
      call set_field_label_to_ctl(check_pressure_2,    array_c2i)
      call set_field_label_to_ctl(check_potential_2,   array_c2i)
!
      end subroutine set_check_fields_names
!
! ----------------------------------------------------------------------
!
      end module m_explicit_term_labels
