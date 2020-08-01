!>@file   m_energy_flux_labels.f90
!!        module m_energy_flux_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      logical function check_enegy_fluxes(field_name)
!!
!!      integer(kind = kint) function num_energy_fluxes()
!!      subroutine set_energy_flux_names(n_comps, names, maths)
!! !!!!!  energy flux names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names  [address]
!!
!!   inertia_work             [i_m_advect_work]:  Work of Reynolds stress
!!                                           -u \cdot (\omega \times u)
!!   Lorentz_work             [i_ujb]:  Work of Lorentz force
!!                                            u \cdot (J \times B)
!!   work_against_Lorentz     [i_nega_ujb]:  Work against Lorentz force
!!                                           -u \cdot (J \times B)
!!   mag_tension_work         [i_m_tension_wk]: Work of magnetic tension
!!                                            u \cdot( (B \nabla) B)
!!   buoyancy_flux            [i_buo_gen]:       Thermal buoyancy flux
!!                                           -u \cdot (\alpha_{T} g T)
!!   composite_buoyancy_flux  [i_c_buo_gen]:  Compositional buoyancy flux
!!                                           -u \cdot (\alpha_{C} g C)
!!
!!   magnetic_ene_generation  [i_me_gen]:
!!           energy flux by magneitic induction
!!                              B \cdot (\nabla \times (u \times B))
!!   magnetic_stretch_flux  [i_mag_stretch_flux]:
!!           energy flux by magneitic streatch  B \cdot ((B \nabla) u)
!!
!!   temp_generation       [i_temp_gen]: heat advection flux
!!                                     T (u \cdot \nabla) T
!!   pert_temp_generation  [i_par_t_gen]:
!!           perturbation of heat advection flux
!!                                     \Theta (u \cdot \nabla) \Theta
!!   comp_generation       [i_comp_gen]:    composition advection flux 
!!                                     C (u \cdot \nabla) C
!!   pert_comp_generation  [i_par_c_gen]:
!!          perturbation of composition advection flux
!!                                     C (u \cdot \nabla) (C-C_0)
!!
!!   viscous_ene_diffusion   [i_vis_e_diffuse]:
!!         Energy dissipation by Viscousity   u ( \nabla^{2} u)
!!   magnetic_ene_diffusion  [i_mag_e_diffuse]:
!!         Energy dissipation by Ohmic dissipation  B ( \nabla^{2} B)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_energy_flux_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nene_flux = 14
!
!>        Field label of work of inertia
!!         @f$ -u_{i} (e_{ijk} \omega_{j} u_{k}) @f$
      type(field_def), parameter :: inertia_work                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'inertia_work',                            &
     &                math = '$ -u_{i} (e_{ijk} \omega_{j} u_{k}) $')
!>        Field label of work against Lorentz force
!!         @f$ - u_{i} (e_{ijk} J_{j} B_{k}) @f$
      type(field_def), parameter :: work_against_Lorentz                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'work_against_Lorentz',                    &
     &                math = '$ -u_{i} (e_{ijk} J_{j} B_{k}) $')
!>        Field label of work of Lorentz force
!!         @f$ u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      type(field_def), parameter :: Lorentz_work                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Lorentz_work',                            &
     &                math = '$  u_{i} (e_{ijk} J_{j} B_{k}) $')
!>        Field address of work of magnetic tension
!!         @f$ u_{i} (B_{j} \partial_{j}) B_{i} @f$
      type(field_def), parameter :: mag_tension_work                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'mag_tension_work',                        &
     &                math = '$ u_{i} (B_{j} \partial_{j}) B_{i} $')
!
!>        Field label of buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} T @f$
      type(field_def), parameter :: buoyancy_flux                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'buoyancy_flux',                           &
     &                math = '$ -u_{i} \alpha_{T} g_{i} T $')
!>        Field label of compositional buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} C @f$
      type(field_def), parameter :: composite_buoyancy_flux             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'composite_buoyancy_flux',                 &
     &                math = '$ -u_{i} \alpha_{C} g_{i} C $')
!!
!>        Field label of magnetic energy flux
!>       @f$ B_{i}e_{ijk} \partial_{j} (e_{klm}u_{l}B_{m}) @f$
      type(field_def), parameter :: magnetic_ene_generation             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magnetic_ene_generation',                 &
     &                math = '$ B_{i}e_{ijk} \partial_{j}'              &
     &                    // ' (e_{klm}u_{l}B_{m}) $')
!>        Field label of energy flux of magnetic stretch term
!!       @f$ B_{i} (B_{j} \partial_{j} u_{i}) @f$
      type(field_def), parameter :: magnetic_stretch_flux               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magnetic_stretch_flux',                   &
     &                math = '$ B_{i} (B_{j} \partial_{j} u_{i} $')
!
!>        Field label of temperature flux
!!         @f$ T (u_{i} \partial_{i}) T @f$
      type(field_def), parameter :: temp_generation                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'temp_generation',                         &
     &                math = '$ T (u_{i} \partial_{i}) T $')
!>        Field label of perturbation temperature flux
!!         @f$ \Theta (u_{i} \partial_{i}) \Theta @f$
      type(field_def), parameter :: pert_temp_generation                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pert_temp_generation',                    &
     &                math = '$ \Theta (u_{i} \partial_{i}) \Theta $')
!>        Field label of composition flux
!!         @f$ C (u_{i} \partial_{i}) @f$
      type(field_def), parameter :: comp_generation                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'comp_generation',                         &
     &                math = '$ C (u_{i} \partial_{i}) C $')
!>        Field label of perturbation composition flux
!!         @f$ (C - C_0) (u_{i} \partial_{i}) (C - C_0) @f$
      type(field_def), parameter :: pert_comp_generation                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pert_comp_generation',                    &
     &          math = '$ \Theta_C} (u_{i} \partial_{i}) \Theta_{C} $')
!
!>        Field label of energy flux by viscous diffusion
!!         @f$ u_{i} (\partial_{j}\partial_{j} u_{i}) @f$
      type(field_def), parameter :: viscous_ene_diffusion               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'viscous_ene_diffusion',                   &
     &             math = '$ u_{i} (\partial_{j}\partial_{j} u_{i}) $')
!>        Field label of energy flux by magnetic diffusion
!!         @f$ B_{i} \left( \partial_{j}\partial_{j} B_{i} \right) @f$
      type(field_def), parameter :: magnetic_ene_diffusion              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magnetic_ene_diffusion',                  &
     &             math = '$ B_{i} (\partial_{j}\partial_{j} B_{i}) $')
!
!   --------------------------------------------------------------------
!
!>        Old Field label for buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} T @f$
      type(field_def), parameter :: buoyancy_work                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'buoyancy_work',                           &
     &                math = '$ -u_{i} \alpha_{T} g_{i} T $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_enegy_fluxes(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_enegy_fluxes = .FALSE.
      if (    (field_name .eq. inertia_work%name)                       &
     &   .or. (field_name .eq. work_against_Lorentz%name)               &
     &   .or. (field_name .eq. Lorentz_work%name)                       &
     &   .or. (field_name .eq. mag_tension_work%name)                   &
     &   .or. (field_name .eq. buoyancy_flux%name)                      &
     &   .or. (field_name .eq. composite_buoyancy_flux%name)            &
!
     &   .or. (field_name .eq. magnetic_ene_generation%name)            &
     &   .or. (field_name .eq. magnetic_stretch_flux%name)              &
!
     &   .or. (field_name .eq. temp_generation%name)                    &
     &   .or. (field_name .eq. pert_temp_generation%name)               &
     &   .or. (field_name .eq. comp_generation%name)                    &
     &   .or. (field_name .eq. pert_comp_generation%name)               &
!
     &   .or. (field_name .eq. viscous_ene_diffusion%name)              &
     &   .or. (field_name .eq. magnetic_ene_diffusion%name)             &
     &      )   check_enegy_fluxes = .TRUE.
!
      end function check_enegy_fluxes
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_energy_fluxes()
      num_energy_fluxes = nene_flux
      return
      end function num_energy_fluxes
!
! ----------------------------------------------------------------------
!
      subroutine set_energy_flux_names(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nene_flux)
      character(len = kchara), intent(inout) :: names(nene_flux)
      character(len = kchara), intent(inout) :: maths(nene_flux)
!
!
      call set_field_labels(inertia_work,                               &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(work_against_Lorentz,                       &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(Lorentz_work,                               &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(mag_tension_work,                           &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(buoyancy_flux,                              &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(composite_buoyancy_flux,                    &
     &    n_comps( 6), names( 6), maths( 6))
!
      call set_field_labels(magnetic_ene_generation,                    &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(magnetic_stretch_flux,                      &
     &    n_comps( 8), names( 8), maths( 8))
!
      call set_field_labels(temp_generation,                            &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(pert_temp_generation,                       &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(comp_generation,                            &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(pert_comp_generation,                       &
     &    n_comps(12), names(12), maths(12))
!
      call set_field_labels(viscous_ene_diffusion,                      &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(magnetic_ene_diffusion,                     &
     &    n_comps(14), names(14), maths(14))
!
      end subroutine set_energy_flux_names
!
! ----------------------------------------------------------------------
!
      end module m_energy_flux_labels
