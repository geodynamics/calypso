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
!!      subroutine set_base_force_names_to_ctl(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
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
      subroutine set_base_force_names_to_ctl(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(pressure_gradient,  array_c2i)
      call set_field_label_to_ctl(inertia,            array_c2i)
      call set_field_label_to_ctl(Coriolis_force,     array_c2i)
      call set_field_label_to_ctl(Lorentz_force,      array_c2i)
      call set_field_label_to_ctl(magnetic_tension,   array_c2i)
      call set_field_label_to_ctl(buoyancy,           array_c2i)
      call set_field_label_to_ctl(composite_buoyancy, array_c2i)
      call set_field_label_to_ctl(magnetic_induction, array_c2i)
      call set_field_label_to_ctl(vecp_induction,     array_c2i)
      call set_field_label_to_ctl(magnetic_stretch,   array_c2i)
      call set_field_label_to_ctl(heat_advect,        array_c2i)
      call set_field_label_to_ctl(pert_heat_advect,   array_c2i)
      call set_field_label_to_ctl(composition_advect, array_c2i)
      call set_field_label_to_ctl(pert_comp_advect,   array_c2i)
      call set_field_label_to_ctl(momentum_flux,      array_c2i)
      call set_field_label_to_ctl(maxwell_tensor,     array_c2i)
      call set_field_label_to_ctl(induction_tensor,   array_c2i)
      call set_field_label_to_ctl(heat_flux,          array_c2i)
      call set_field_label_to_ctl(pert_heat_flux,     array_c2i)
      call set_field_label_to_ctl(composite_flux,     array_c2i)
      call set_field_label_to_ctl(pert_comp_flux,     array_c2i)
!
      end subroutine set_base_force_names_to_ctl
!
! ----------------------------------------------------------------------
!
      end module m_base_force_labels
