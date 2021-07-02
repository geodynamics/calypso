!>@file   m_filtered_ene_flux_labels.f90
!!        module m_filtered_ene_flux_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for energy fluxes by filtered field
!!
!!@verbatim
!!      logical function check_filter_enegy_fluxes(field_name)
!!
!!      integer(kind = kint) function num_filtered_ene_fluxes()
!!      subroutine set_filtered_ene_flax_labels(n_comps, names, maths)
!!
!! !!!!!  List of energy flux by SGS terms  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   inertia_work_by_filtered          [eflux_by_filter%i_m_advect_work]
!!   wk_against_Lorentz_by_filtered    [eflux_by_filter%i_nega_ujb]
!!   Lorentz_work_by_filtered          [eflux_by_filter%i_ujb]
!!   mag_tension_work_by_filtered      [eflux_by_filter%i_m_tension_wk]
!!
!!   filtered_buoyancy_flux            [eflux_by_filter%i_buo_gen]
!!   filtered_comp_buoyancy_flux       [eflux_by_filter%i_c_buo_gen]
!!
!!   mag_ene_generation_by_filtered    [eflux_by_filter%i_me_gen]
!!   mag_stretch_flux_by_filtered
!!                              [eflux_by_filter%i_mag_stretch_flux]
!!
!!   temp_generation_by_filtered       [eflux_by_filter%i_temp_gen]
!!   part_temp_gen_by_filtered         [eflux_by_filter%i_par_t_gen]
!!   comp_generation_by_filtered       [eflux_by_filter%i_comp_gen]
!!   part_comp_gen_by_filtered         [eflux_by_filter%i_par_c_gen]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_filtered_ene_flux_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
      integer(kind = kint), parameter, private :: neflux_by_filter = 12
!
!>        Field label of work of inertia
!!         @f$ u_{i} (\tilde{u}_{j} \partial_{j} \tilde{u}_{i}) @f$,
!!         @f$ u_{i} (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}) @f$
      type(field_def), parameter :: inertia_work_by_filtered            &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'inertia_work_by_filtered',                &
     &                math = '$ u_{i} (e_{ijk}'                         &
     &                     //  ' \tilde{\omega}_{j} \tilde{u}_{k})$')
!
!>        Field label of work against Lorentz force
!!         @f$ - u_{i} (e_{ijk} \tilde{J}_{j} \tilde{B}_{k}) @f$
      type(field_def), parameter :: wk_against_Lorentz_by_filtered      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'wk_against_Lorentz_by_filtered',          &
     &         math = '$ u_{i} (e_{ijk} \tilde{J}_{j} \tilde{B}_{k})$')
!>        Field label of work of Lorentz force
!!         @f$ u_{i} (e_{ijk} \tilde{J}_{j} \tilde{B}_{k}) @f$
      type(field_def), parameter :: Lorentz_work_by_filtered            &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Lorentz_work_by_filtered',                &
     &         math = '$ u_{i} (e_{ijk} \tilde{J}_{j} \tilde{B}_{k})$')
!>        Field address of work of magnetic tension
!!         @f$ u_{i} (\tilde{B}_{j} \partial_{j}) \tilde{B}_{i} @f$
      type(field_def), parameter :: mag_tension_work_by_filtered        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'mag_tension_work_by_filtered',            &
     &                math = '$ u_{i} (\tilde{B}_{j} \partial_{j}) '    &
     &                    // ' \tilde{B}_{i} $')
!
!>        Field label for filtered buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} \tilde{T} @f$
      type(field_def), parameter :: filtered_buoyancy_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filtered_buoyancy_flux',                  &
     &                math = '$ -u_{i} \alpha_{T} g_{i} \tilde{T} $')
!>        Field label of compositional buoyancy flux
!!         @f$ -u_{i} \alpha_{C} g_{i} \tilde{C} @f$
      type(field_def), parameter :: filtered_comp_buoyancy_flux         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filtered_comp_buoyancy_flux',             &
     &                math = '$ -u_{i} \alpha_{C} g_{i} \tilde{C} $')
!
!>        Field label of magnetic energy flux
!!         @f$ B_{i} e_{ijk} \partial_{j}
!!            (e_{klm} \tilde{u}_{l} \tilde{B}_{m}) @f$
      type(field_def), parameter :: mag_ene_generation_by_filtered      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'mag_ene_generation_by_filtered',          &
     &                math = '$ B_{i} e_{ijk} \partial_{j} '            &
     &                   // ' (e_{klm} \tilde{u}_{l} \tilde{B}_{m}) $')
!>        Field label of energy flux of magnetic stretch term
!!          @f$ B_{i} (\tilde{B}_{j} \partial_{j} \tilde{u}_{i}) @f$
      type(field_def), parameter :: mag_stretch_flux_by_filtered        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'mag_stretch_flux_by_filtered',            &
     &                math = '$ B_{i} '                                 &
     &              // ' (\tilde{B}_{i} \partial_{k} \tilde{u}_{k}) $')
!
!>        Field label of temperature flux
!!         @f$ T (\tilde{u}_{i} \partial_{i}) \tilde{T} @f$
      type(field_def), parameter :: temp_generation_by_filtered         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'temp_generation_by_filtered',             &
     &          math = '$ T (\tilde{u}_{i} \partial_{i}) \tilde{T} $')
!>        Field label of perturbation temperature flux
!!         @f$ \Theta (\tilde{u}_{i} \partial_{i}) \tilde{\Theta} @f$
      type(field_def), parameter :: part_temp_gen_by_filtered           &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'part_temp_gen_by_filtered',               &
     &                math = '$ \Theta (\tilde{u}_{i} \partial_{i})'    &
     &                    // ' \tilde{\Theta} $')
!>        Field label of composition flux
!!         @f$ C (\tilde{u}_{i} \partial_{i}) \tilde{C} @f$
      type(field_def), parameter :: comp_generation_by_filtered         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'comp_generation_by_filtered',             &
     &           math = '$ C (\tilde{u}_{i} \partial_{i}) \tilde{C} $')
!>        Field label of perturbation composition flux
!!         @f$ \Theta_{C} (\tilde{u}_{i} \partial_{i})
!!            \tilde{\Theta}_{C} @f$
      type(field_def), parameter :: part_comp_gen_by_filtered           &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'part_comp_gen_by_filtered',               &
     &                math = '$ \Theta_{C}(\tilde{u}_{i} \partial_{i})' &
     &                    // ' \tilde{\Theta}_{C} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_filter_enegy_fluxes(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filter_enegy_fluxes                                         &
     &   =    (field_name .eq. inertia_work_by_filtered%name)           &
     &   .or. (field_name .eq. wk_against_Lorentz_by_filtered%name)     &
     &   .or. (field_name .eq. Lorentz_work_by_filtered%name)           &
     &   .or. (field_name .eq. mag_tension_work_by_filtered%name)       &
     &   .or. (field_name .eq. filtered_buoyancy_flux%name)             &
     &   .or. (field_name .eq. filtered_comp_buoyancy_flux%name)        &
     &   .or. (field_name .eq. mag_ene_generation_by_filtered%name)     &
     &   .or. (field_name .eq. mag_stretch_flux_by_filtered%name)       &
     &   .or. (field_name .eq. temp_generation_by_filtered%name)        &
     &   .or. (field_name .eq. part_temp_gen_by_filtered%name)          &
     &   .or. (field_name .eq. comp_generation_by_filtered%name)        &
     &   .or. (field_name .eq. part_comp_gen_by_filtered%name)
!
      end function check_filter_enegy_fluxes
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! 
      integer(kind = kint) function num_filtered_ene_fluxes()
      num_filtered_ene_fluxes = neflux_by_filter
      return
      end function num_filtered_ene_fluxes
!
! ----------------------------------------------------------------------
!
      subroutine set_filtered_ene_flax_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout)                            &
     &                        :: n_comps(neflux_by_filter)
      character(len = kchara), intent(inout) :: names(neflux_by_filter)
      character(len = kchara), intent(inout) :: maths(neflux_by_filter)
!
!
      call set_field_labels(inertia_work_by_filtered,                   &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(wk_against_Lorentz_by_filtered,             &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(Lorentz_work_by_filtered,                   &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(mag_tension_work_by_filtered,               &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(filtered_buoyancy_flux,                     &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(filtered_comp_buoyancy_flux,                &
     &    n_comps( 6), names( 6), maths( 6))
!
      call set_field_labels(mag_ene_generation_by_filtered,             &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(mag_stretch_flux_by_filtered,               &
     &    n_comps( 8), names( 8), maths( 8))
!
      call set_field_labels(temp_generation_by_filtered,                &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(part_temp_gen_by_filtered,                  &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(comp_generation_by_filtered,                &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(part_comp_gen_by_filtered,                  &
     &    n_comps(12), names(12), maths(12))
!
      end subroutine set_filtered_ene_flax_labels
!
! ----------------------------------------------------------------------
!
      end module m_filtered_ene_flux_labels
