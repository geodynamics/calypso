!>@file   m_filtered_force_labels.f90
!!        module m_filtered_force_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for forces by filtered field
!!
!!@verbatim
!!      logical function check_filtered_force(field_name)
!!      logical function check_filtered_flux_tensor(field_name)
!!      logical function check_filtered_scalar_flux(field_name)
!!
!!      integer(kind = kint) function num_filter_force()
!!        subroutine set_filter_force_labels(n_comps, names, maths)
!!
!! !!!!!  divergence of forces by filtered field !!!!!!!!!!!!!!!!!!
!!
!!      Field label  [Address]
!!
!!   inertia_by_filtered             [force_by_filter%i_m_advect]
!!   Lorentz_force_by_filtered       [force_by_filter%i_lorentz]
!!   magnetic_tension_by_filtered    [force_by_filter%i_m_tension]
!!
!!   filtered_buoyancy               [force_by_filter%i_buoyancy]
!!   filtered_comp_buoyancy          [force_by_filter%i_comp_buo]
!!
!!   vecp_induction_by_filtered      [force_by_filter%i_vp_induct]
!!   magnetic_induction_by_filtered  [force_by_filter%i_induction]
!!   magnetic_stretch_by_filtered    [force_by_filter%i_mag_stretch]
!!
!!   heat_advect_by_filtered         [force_by_filter%i_h_advect]
!!   pert_h_advect_by_filtered       [force_by_filter%i_ph_advect]
!!   comp_advect_by_filtered         [force_by_filter%i_c_advect]
!!   pert_c_advect_by_filtered       [force_by_filter%i_pc_advect]
!!
!!   momentum_flux_by_filtered       [force_by_filter%i_m_flux]
!!   maxwell_tensor_by_filtered      [force_by_filter%i_maxwell]
!!   induction_tensor_by_filtered    [force_by_filter%i_induct_t]
!!
!!   heat_flux_by_filtered           [force_by_filter%i_h_flux]
!!   pert_h_flux_by_filtered         [force_by_filter%i_ph_flux]
!!   composite_flux_by_filtered      [force_by_filter%i_c_flux]
!!   pert_c_flux_by_filtered         [force_by_filter%i_pc_flux]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_filtered_force_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
      integer(kind = kint), parameter, private :: nforce_filter = 19
!
!>        Field label for advection for momentum by filtered field
!!         @f$ -e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k} @f$
      type(field_def), parameter :: inertia_by_filtered                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'inertia_by_filtered',                     &
     &           math = '$ -e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}$')
!>        Field label for momentum flux by filtered field
!!         @f$ \tilde{u}_{i} \tilde{u}_{j} @f$
      type(field_def), parameter :: momentum_flux_by_filtered           &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'momentum_flux_by_filtered',               &
     &                math = '$ \tilde{u}_{i} \tilde{u}_{j} $')
!>        Field label for Lorentz force
!!         @f$ e_{ijk} \tilde{J}_{j} \tilde{B}_{k} @f$
      type(field_def), parameter :: Lorentz_force_by_filtered           &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'Lorentz_force_by_filtered',               &
     &                math = '$ e_{ijk} \tilde{J}_{j} \tilde{B}_{k}$')
!>        start address for magnetic tension
!!         @f$ \tilde{B}_{j} \partial_{j} \tilde{B}_{i} @f$
      type(field_def), parameter :: magnetic_tension_by_filtered        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_tension_by_filtered',            &
     &          math = '$ \tilde{B}_{j} \partial_{j} \tilde{B}_{i}$ ')
!>        Field label for Maxwell tensor
!!         @f$ \tilde{B}_{i} \tilde{B}_{j} @f$
      type(field_def), parameter :: maxwell_tensor_by_filtered          &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'maxwell_tensor_by_filtered',              &
     &                math = '$ \tilde{B}_{i} \tilde{B}_{j} $')
!
!>        Field label for filtered buoyancy
!!         @f$ -\alpha_{T} g_{i} \tilde{T} @f$
      type(field_def), parameter :: filtered_buoyancy                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filtered_buoyancy',                       &
     &                math = '$ -\alpha_{T} g_{i} \tilde{T} $')
!>        Field label for compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} \tilde{C} @f$
      type(field_def), parameter :: filtered_comp_buoyancy              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filtered_comp_buoyancy',                  &
     &                math = '$ -\alpha_{C} g_{i} \tilde{C} $')
!!
!>        Field label for induction for vector potential
!!         @f$ e_{ijk} \tilde{u}_{j} \tilde{B}_{k} @f$
      type(field_def), parameter :: vecp_induction_by_filtered          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vecp_induction_by_filtered',              &
     &                math = '$ e_{ijk} \tilde{u}_{j} \tilde{B}_{k} $')
!>        Field label for magnetic induction
!!        @f$ e_{ijk} \partial_{j}
!!            (e_{klm} \tilde{u}_{l} \tilde{B}_{m}) @f$
      type(field_def), parameter :: magnetic_induction_by_filtered      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_induction_by_filtered',          &
     &                math = '$ e_{ijk} \partial_{j} '                  &
     &                    // '(e_{klm} \tilde{u}_{l} \tilde{B}_{m}) $')
!>        Field label for magnetic stretch term
!!         @f$ (\tilde{B}_{j} \partial_{j} \tilde{u}_{i}) @f$
      type(field_def), parameter :: magnetic_stretch_by_filtered        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_stretch_by_filtered',            &
     &                math = '$ (\tilde{B}_{j} \partial_{j})'           &
     &                      //' \tilde{u}_{i} $')
!>        Field label for Tensor for magnetic induction
!!         @f$ \tilde{u}_{i} \tilde{B}_{j}
!!            - \tilde{B}_{i} \tilde{u}_{J} @f$
      type(field_def), parameter :: induction_tensor_by_filtered        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'induction_tensor_by_filtered',            &
     &                math = '$ (\tilde{B}_{i} \partial_{i})'           &
     &                      //' \tilde{u}_{k} $')
!
!>        Field label for advection for temperature
!!         @f$ \tilde{u}_{i} \partial_{i} \tilde{T} @f$
      type(field_def), parameter :: heat_advect_by_filtered             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'heat_advect_by_filtered',                 &
     &              math = '$ \tilde{u}_{i} \partial_{i} \tilde{T} $')
!>        Field label for advection for perturbation of temperature
!!         @f$ \tilde{u}_{i} \partial_{i} \tilde{\Theta} @f$
      type(field_def), parameter :: pert_h_advect_by_filtered           &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pert_h_advect_by_filtered',               &
     &          math = '$ \tilde{u}_{i} \partial_{i} \tilde{\Theta} $')
!>        Field label for heat flux
!!         @f$ \tilde{u}_{i} \tilde{T} @f$
      type(field_def), parameter :: heat_flux_by_filtered               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'heat_flux_by_filtered',                   &
     &                math = '$ \tilde{u}_{i} \tilde{T} $')
!>        Field label for perturbation of heat flux
!!         @f$ \tilde{u}_{i} \tilde{\Theta} @f$
      type(field_def), parameter :: pert_h_flux_by_filtered             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'pert_h_flux_by_filtered',                 &
     &                math = '$ \tilde{u}_{i} \tilde{\Theta} $')
!
!>        Field label for advection for composition
!!         @f$ \tilde{u}_{i} \partial_{i} \tilde{C} @f$
      type(field_def), parameter :: comp_advect_by_filtered             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'comp_advect_by_filtered',                 &
     &              math = '$ \tilde{u}_{i} \partial_{i} \tilde{C} $')
!>        Field label for advection for perturbation of composition
!!         @f$ \tilde{u}_{i} \partial_{i} \tilde{\Theta}_{C} @f$
      type(field_def), parameter :: pert_c_advect_by_filtered           &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pert_c_advect_by_filtered',               &
     &      math = '$ \tilde{u}_{i} \partial_{i} \tilde{\Theta}_{C} $')
!>        Field label for compositinoal flux
!!         @f$ \tilde{u}_{i} \tilde{C} @f$
      type(field_def), parameter :: composite_flux_by_filtered          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'composite_flux_by_filtered',              &
     &                math = '$ \tilde{u}_{i} \tilde{C} $')
!>        Field label for perturbation of composition flux
!!         @f$ \tilde{u}_{i} \tilde{\Theta}_{C} @f$
      type(field_def), parameter :: pert_c_flux_by_filtered             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'pert_c_flux_by_filtered',                 &
     &                math = '$ \tilde{u}_{i} \tilde{\Theta}_{C} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_filtered_force(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filtered_force                                              &
     &   =    (field_name .eq. inertia_by_filtered%name)                &
     &   .or. (field_name .eq. Lorentz_force_by_filtered%name)          &
     &   .or. (field_name .eq. magnetic_tension_by_filtered%name)       &
!
     &   .or. (field_name .eq. filtered_buoyancy%name)                  &
     &   .or. (field_name .eq. filtered_comp_buoyancy%name)             &
!
     &   .or. (field_name .eq. vecp_induction_by_filtered%name)         &
     &   .or. (field_name .eq. magnetic_induction_by_filtered%name)     &
     &   .or. (field_name .eq. magnetic_stretch_by_filtered%name)       &
     &   .or. (field_name .eq. induction_tensor_by_filtered%name)       &
!
     &   .or. (field_name .eq. heat_flux_by_filtered%name)              &
     &   .or. (field_name .eq. pert_h_flux_by_filtered%name)            &
     &   .or. (field_name .eq. composite_flux_by_filtered%name)         &
     &   .or. (field_name .eq. pert_c_flux_by_filtered%name)
!
      end function check_filtered_force
!
! ----------------------------------------------------------------------
!
      logical function check_filtered_flux_tensor(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filtered_flux_tensor                                        &
     &   =    (field_name .eq. momentum_flux_by_filtered%name)          &
     &   .or. (field_name .eq. maxwell_tensor_by_filtered%name)
!
      end function check_filtered_flux_tensor
!
! ----------------------------------------------------------------------
!
      logical function check_filtered_scalar_flux(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filtered_scalar_flux                                        &
     &   =    (field_name .eq. heat_advect_by_filtered%name)            &
     &   .or. (field_name .eq. pert_h_advect_by_filtered%name)          &
     &   .or. (field_name .eq. comp_advect_by_filtered%name)            &
     &   .or. (field_name .eq. pert_c_advect_by_filtered%name)
!
      end function check_filtered_scalar_flux
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! 
      integer(kind = kint) function num_filter_force()
      num_filter_force = nforce_filter
      return
      end function num_filter_force
!
! ----------------------------------------------------------------------
!
      subroutine set_filter_force_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nforce_filter)
      character(len = kchara), intent(inout) :: names(nforce_filter)
      character(len = kchara), intent(inout) :: maths(nforce_filter)
!
!
      call set_field_labels(inertia_by_filtered,                        &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(Lorentz_force_by_filtered,                  &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(magnetic_tension_by_filtered,               &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(filtered_buoyancy,                          &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(filtered_comp_buoyancy,                     &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(vecp_induction_by_filtered,                 &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(magnetic_induction_by_filtered,             &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(magnetic_stretch_by_filtered,               &
     &    n_comps( 8), names( 8), maths( 8))
!
      call set_field_labels(heat_advect_by_filtered,                    &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(pert_h_advect_by_filtered,                  &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(comp_advect_by_filtered,                    &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(pert_c_advect_by_filtered,                  &
     &    n_comps(12), names(12), maths(12))
!
      call set_field_labels(momentum_flux_by_filtered,                  &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(maxwell_tensor_by_filtered,                 &
     &    n_comps(14), names(14), maths(14))
      call set_field_labels(induction_tensor_by_filtered,               &
     &    n_comps(15), names(15), maths(15))
!
      call set_field_labels(heat_flux_by_filtered,                      &
     &    n_comps(16), names(16), maths(16))
      call set_field_labels(pert_h_flux_by_filtered,                    &
     &    n_comps(17), names(17), maths(17))
      call set_field_labels(composite_flux_by_filtered,                 &
     &    n_comps(18), names(18), maths(18))
      call set_field_labels(pert_c_flux_by_filtered,                    &
     &    n_comps(19), names(19), maths(19))
!
      end subroutine set_filter_force_labels
!
! ----------------------------------------------------------------------
!
      end module m_filtered_force_labels
