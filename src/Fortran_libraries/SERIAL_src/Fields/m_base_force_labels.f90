!>@file   m_base_force_labels.f90
!!        module m_base_force_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      logical function check_scalar_advection(field_name)
!!      logical function check_force_vectors(field_name)
!!      logical function check_flux_tensors(field_name)
!!      logical function check_asym_flux_tensors(field_name)
!!
!!      integer(kind = kint) function num_base_forces()
!!      subroutine set_base_force_labels(n_comps, names, maths)
!!
!! !!!!!  Base force names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field name
!!
!!   pressure_gradient  [i_press_grad]:     pressure gradient    u
!!   inertia    [i_m_advect]:        inertia (\omega \times u)
!!   Lorentz_force   [i_lorentz]:  Lorentz force     J \times B
!!   magnetic_tension  [i_m_tension]:  magnetic tension   (B \nabla) B
!!   Coriolis_force   [i_coriolis]:  Coriolis force     2 \Omega \times u
!!   buoyancy   [i_buoyancy]:   Thermal buoyancy       - \alpha_{T} g T
!!   composite_buoyancy   [i_comp_buo]:
!!                       compositional buoyancy  - \alpha_{C} g C
!!
!!   vecp_induction   [i_vp_induct]:     induction         u \times B
!!   magnetic_induction   [i_induction]:
!!                         magneitic induction \nabla \times (u \times B)
!!   magnetic_stretch    [i_mag_stretch]:
!!                         magneitic streatch         (B \nabla) u
!!
!!   heat_advect         [i_h_advect]:  heat advection
!!                                     (u \cdot \nabla) T
!!   pert_heat_advect    [i_ph_advect]:  perturbation of heat advection
!!                                      (u \cdot \nabla) \Theta
!!   composition_advect [i_c_advect]:    composition advection
!!                                      (u \cdot \nabla) C
!!   pert_comp_advect   [i_pc_advect]:
!!                     perturbation of composition advection
!!                                      (u \cdot \nabla) (C-C_0)
!!
!!   momentum_flux   [i_m_flux]:  momentum flux     u_{i} u_{j}
!!   maxwell_tensor    [i_maxwell]:  maxwell tensor       B_{i} B_{j}
!!   induction_tensor    [i_induct_t]:    induction induction tensor
!!                                 u_{i} B_{j}  - B_{i} u_{J}
!!   heat_flux           [i_h_flux]:    heat flux          uT
!!   pert_heat_flux      [i_ph_flux]:  perturbation of heat flux 
!!                                    u\Theta
!!   composite_flux     [i_c_flux]:    composition flux         uC
!!   part_c_flux        [i_pc_flux]:  perturbation of composition flux
!!                                      u(C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_base_force_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nforce_base = 21
!
!>        Field label for pressure gradient
!!         @f$ \partial_{i} p @f$
      type(field_def), parameter :: pressure_gradient                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'pressure_gradient',                       &
     &                math = '$ \partial_{i} p $')
!>        Field label for advection for momentum
!!         @f$ e_{ijk} \omega_{j} u_{k}  @f$ 
!!          or @f$ u_{j} \partial_{j} u_{i} @f$
      type(field_def), parameter :: inertia                             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'inertia',                                 &
     &                math = '$ e_{ijk} \omega_{j} u_{k} $')
!>        Field label for Lorentz force
!!         @f$ e_{ijk} J_{j} B_{k} @f$
      type(field_def), parameter :: Lorentz_force                       &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'Lorentz_force',                           &
     &                math = '$ e_{ijk} J{j} B_{k} $')
!>        start address for magnetic tension
!!         @f$ B_{j} \partial_{j} B_{i} @f$
      type(field_def), parameter :: magnetic_tension                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_tension',                        &
     &                math = '$ B_{j} \partial_{j} B_{i} $')
!>        Field label for Coriolis force
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} @f$
      type(field_def), parameter :: Coriolis_force                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'Coriolis_force',                          &
     &                math = '$ -2 e_{ijk} \Omega_{j} u_{k} $')
!>        Field label for buoyancy
!!         @f$ -\alpha_{T} g_{i} T @f$
      type(field_def), parameter :: buoyancy                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'buoyancy',                                &
     &                math = '$ -\alpha_{T} g_{i} T $')
!>        Field label for compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} C @f$
      type(field_def), parameter :: composite_buoyancy                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'composite_buoyancy',                      &
     &                math = '$ -\alpha_{C} g_{i} C $')
!
!>        Field label for induction for vector potential
!!         @f$ e_{ijk} u_{j} B_{k} @f$
      type(field_def), parameter :: vecp_induction                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vecp_induction',                          &
     &                math = '$ e_{ijk} u_{j} B_{k} $')
!>        Field label for magnetic induction
!!         @f$ e_{ijk} \partial_{j}\left(e_{klm}u_{l}B_{m} \right)@f$
      type(field_def), parameter :: magnetic_induction                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_induction',                      &
     &                math = '$ e_{ijk} \partial_{j} '                  &
     &                     // ' (e_{ijk} u_{j} B_{k} )$')
!>        Field label for magnetic stretch term
!!         @f$ B_{j} \partial_{j} u_{i} @f$
      type(field_def), parameter :: magnetic_stretch                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_stretch',                        &
     &                math = '$ B_{j} \partial_{j} u_{i} $')
!
!
!>        Field label for advection for temperature
!!         @f$ u_{i} \partial_{i} T @f$
      type(field_def), parameter :: heat_advect                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'heat_advect',                             &
     &                math = '$ u_{i} \partial_{i} T $')
!>        Field label for advection for perturbation of temperature
!!         @f$ u_{i} \partial_{i} \Theta @f$
      type(field_def), parameter :: pert_heat_advect                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pert_heat_advect',                        &
     &                math = '$ u_{i} \partial_{i} \Theta $')
!
!>        Field label for advection for composition
!!         @f$ u_{i} \partial_{i} C @f$
      type(field_def), parameter :: composition_advect                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'composition_advect',                      &
     &                math = '$ u_{i} \partial_{i} C $')
!>        Field label for advection for perturbation of composition
!!         @f$ u_{i} \partial_{i} \Theta_{C} @f$
      type(field_def), parameter :: pert_comp_advect                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pert_comp_advect',                        &
     &                math = '$ u_{i} \partial_{i} \Theta_{C} $')
!
!
!>        Field label for momentum flux
!!         @f$ u_{i} u_{j} @f$
      type(field_def), parameter :: momentum_flux                       &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'momentum_flux',                           &
     &                math = '$ u_{i} u_{j} $')
!>        Field label for Maxwell tensor
!!         @f$ B_{i} B_{j} @f$
      type(field_def), parameter :: maxwell_tensor                      &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'maxwell_tensor',                          &
     &                math = '$ B_{i} B_{j} $')
!>        Field label of tensor for magnetic induction
!!         @f$ u_{i} B_{j}  - B_{i} u_{J} @f$
      type(field_def), parameter :: induction_tensor                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'induction_tensor',                        &
     &                math = '$ u_{i} B_{j}  - B_{i} u_{J} $')
!
!>        Field label for heat flux
!!         @f$ u_{i} T @f$
      type(field_def), parameter :: heat_flux                           &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'heat_flux',                               &
     &                math = '$ u_{i} T $')
!>        Field label for perturbation of heat flux
!!         @f$ u_{i} \Theta @f$
      type(field_def), parameter :: pert_heat_flux                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'pert_heat_flux',                          &
     &                math = '$ u_{i} \Theta $')
!>        Field label for compositinoal flux
!!         @f$ u_{i} C @f$
      type(field_def), parameter :: composite_flux                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'composite_flux',                          &
     &                math = '$ u_{i} C $')
!>        Field label for perturbation of composition flux
!!         @f$ u_{i} \Theta_{C} @f$
      type(field_def), parameter :: pert_comp_flux                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'pert_comp_flux',                          &
     &                math = '$ u_{i} \Theta_{C} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_scalar_advection(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_scalar_advection                                            &
     &   =    (field_name .eq. heat_advect%name)                        &
     &   .or. (field_name .eq. pert_heat_advect%name)                   &
     &   .or. (field_name .eq. composition_advect%name)                 &
     &   .or. (field_name .eq. pert_comp_advect%name)
!
      end function check_scalar_advection
!
! ----------------------------------------------------------------------
!
      logical function check_force_vectors(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_force_vectors                                               &
     &   =    (field_name .eq. pressure_gradient%name)                  &
     &   .or. (field_name .eq. inertia%name)                            &
     &   .or. (field_name .eq. Coriolis_force%name)                     &
     &   .or. (field_name .eq. Lorentz_force%name)                      &
     &   .or. (field_name .eq. magnetic_tension%name)                   &
     &   .or. (field_name .eq. buoyancy%name)                           &
     &   .or. (field_name .eq. composite_buoyancy%name)                 &
     &   .or. (field_name .eq. vecp_induction%name)                     &
     &   .or. (field_name .eq. magnetic_induction%name)                 &
     &   .or. (field_name .eq. magnetic_stretch%name)                   &
!
     &   .or. (field_name .eq. heat_flux%name)                          &
     &   .or. (field_name .eq. pert_heat_flux%name)                     &
     &   .or. (field_name .eq. composite_flux%name)                     &
     &   .or. (field_name .eq. pert_comp_flux%name)
!
      end function check_force_vectors
!
! ----------------------------------------------------------------------
!
      logical function check_flux_tensors(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_flux_tensors                                                &
     &   =    (field_name .eq. momentum_flux%name)                      &
     &   .or. (field_name .eq. maxwell_tensor%name)
!
      end function check_flux_tensors
!
! ----------------------------------------------------------------------
!
      logical function check_asym_flux_tensors(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_asym_flux_tensors = (field_name .eq. induction_tensor%name)
!
      end function check_asym_flux_tensors
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_base_forces()
      num_base_forces = nforce_base
      return
      end function num_base_forces
!
! ----------------------------------------------------------------------
!
      subroutine set_base_force_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nforce_base)
      character(len = kchara), intent(inout) :: names(nforce_base)
      character(len = kchara), intent(inout) :: maths(nforce_base)
!
!
      call set_field_labels(pressure_gradient,                          &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(inertia,                                    &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(Coriolis_force,                             &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(Lorentz_force,                              &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(magnetic_tension,                           &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(buoyancy,                                   &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(composite_buoyancy,                         &
     &    n_comps( 7), names( 7), maths( 7))
!
      call set_field_labels(magnetic_induction,                         &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(vecp_induction,                             &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(magnetic_stretch,                           &
     &    n_comps(10), names(10), maths(10))
!
      call set_field_labels(heat_advect,                                &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(pert_heat_advect,                           &
     &    n_comps(12), names(12), maths(12))
      call set_field_labels(composition_advect,                         &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(pert_comp_advect,                           &
     &    n_comps(14), names(14), maths(14))
!
      call set_field_labels(momentum_flux,                              &
     &    n_comps(15), names(15), maths(15))
      call set_field_labels(maxwell_tensor,                             &
     &    n_comps(16), names(16), maths(16))
      call set_field_labels(induction_tensor,                           &
     &    n_comps(17), names(17), maths(17))
!
      call set_field_labels(heat_flux,                                  &
     &    n_comps(18), names(18), maths(18))
      call set_field_labels(pert_heat_flux,                             &
     &    n_comps(19), names(19), maths(19))
      call set_field_labels(composite_flux,                             &
     &    n_comps(20), names(20), maths(20))
      call set_field_labels(pert_comp_flux,                             &
     &    n_comps(21), names(21), maths(21))
!
      end subroutine set_base_force_labels
!
! ----------------------------------------------------------------------
!
      end module m_base_force_labels
