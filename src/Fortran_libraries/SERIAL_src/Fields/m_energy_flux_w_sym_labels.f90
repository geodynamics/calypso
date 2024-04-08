!>@file   m_energy_flux_w_sym_labels.f90
!!        module m_energy_flux_w_sym_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      logical function check_ene_fluxes_w_sym(field_name)
!!
!!      subroutine set_ene_flux_w_symmetry_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  energy flux names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   u_dot_wsym_x_usym, u_dot_wasym_x_uasym,
!!   u_dot_wsym_x_uasym, u_dot_wasym_x_usym:
!!          Work of Reynolds stress   u \cdot (\omega \times u)
!!   rev_u_dot_Jsym_x_Bsym, rev_u_dot_Jasym_x_Basym,
!!   rev_u_dot_Jsym_x_Basym, rev_u_dot_Jasym_x_Bsym:
!!          Work against Lorentz force       -u \cdot (J \times B)
!!   u_dot_Jsym_x_Bsym, u_dot_Jasym_x_Basym,
!!   u_dot_Jsym_x_Basym, u_dot_Jasym_x_Bsym:
!!          Work of Lorentz force             u \cdot (J \times B)
!!   u_dot_Bsym_nabla_Bsym, u_dot_Basym_nabla_Basym,
!!   u_dot_Bsym_nabla_Basym, u_dot_Basym_nabla_Bsym:
!!          Work of magnetic tension          u \cdot( (B \nabla) B)
!!
!!   sym_termal_buo_flux, asym_termal_buo_flux:
!!          Thermal buoyancy flux            -u \cdot (\alpha_{T} g T)
!!   sym_composite_buo_flux, asym_composite_buo_flux:
!!          Compositional buoyancy flux      -u \cdot (\alpha_{C} g C)
!!
!!   B_rot_Bsym_x_usym, B_rot_Basym_x_uasym,
!!   B_rot_Bsym_x_uasym, B_rot_Basym_x_usym:
!!         Energy flux by magneitic induction
!!                              B \cdot (\nabla \times (u \times B))
!!   B_dot_Bsym_nabla_usym, B_dot_Basym_nabla_uasym,
!!   B_dot_Bsym_nabla_uasym, B_dot_Basym_nabla_usym:
!!        Energy flux by magneitic streatch    B \cdot ((B \nabla) u)
!!
!!   T_usym_nabla_Tsym, T_uasym_nabla_Tasym,
!!   T_usym_nabla_Tasym, T_uasym_nabla_Tsym:
!!                       Heat advection flux   T (u \cdot \nabla) T
!!   pT_usym_nabla_pTsym, pT_uasym_nabla_pTasym,
!!   pT_usym_nabla_pTasym, pT_uasym_nabla_pTsym:
!!       Perturbation of heat advection flux
!!                                     \Theta (u \cdot \nabla) \Theta
!!   C_usym_nabla_Csym, C_uasym_nabla_Casym,
!!   C_usym_nabla_Casym, C_uasym_nabla_Csym:
!!       Composition advection flux            C (u \cdot \nabla) C
!!   pC_usym_nabla_pCsym, pC_uasym_nabla_pCasym,
!!   pC_usym_nabla_pCasym, pC_uasym_nabla_pCsym:
!!   pert_comp_advect:      perturbation of composition advection flux
!!                                   (C-C_0) (u \cdot \nabla) (C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_energy_flux_w_sym_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
      use t_energy_flux_labels
!
      implicit  none
! 
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{symj} \partial_{j} u_{symi}) @f$
!!         or @f$ u \cdot (\omega_{sym} \times u_{sym}) @f$
      type(field_def), parameter :: u_dot_wsym_x_usym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wsym_x_usym',                              &
     &         math = '$ u \cdot (u_{symj} \partial_{j} u_{symi})$, '   &
     &             //' $ u \cdot (\omega_{sym} \times u_{sym})$')
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{asymj} \partial_{j} u_{asymi}) @f$
!!         or @f$ u \cdot (\omega_{asym} \times u_{asym}) @f$
      type(field_def), parameter :: u_dot_wasym_x_uasym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wasym_x_uasym',                            &
     &         math = '$ u \cdot (u_{asymj} \partial_{j} u_{asymi})$, ' &
     &             //' $ u \cdot (\omega_{asym} \times u_{asym})$')
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{symj} \partial_{j} u_{asymi}) @f$
!!         or @f$ u \cdot (\omega_{sym} \times u_{asym}) @f$
      type(field_def), parameter :: u_dot_wsym_x_uasym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wsym_x_uasym',                             &
     &         math = '$ u \cdot (u_{symj} \partial_{j} u_{asymi})$, '  &
     &             //' $ u \cdot (\omega_{sym} \times u_{asym})$')
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{asymj} \partial_{j} u_{symi}) @f$
!!         or @f$ u \cdot (\omega_{asym} \times u_{sym}) @f$
      type(field_def), parameter :: u_dot_wasym_x_usym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wasym_x_usym',                             &
     &         math = '$ u \cdot (u_{asymj} \partial_{j} u_{symi})$, '  &
     &             //' $ u \cdot (\omega_{asym} \times u_{sym})$')
!
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{sym} \times B_{sym}) @f$
      type(field_def), parameter :: rev_u_dot_Jsym_x_Bsym               &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jsym_x_Bsym',                       &
     &            math = '$ -u \cdot (J_{sym} \times B_{sym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{asym} \times B_{asym}) @f$
      type(field_def), parameter :: rev_u_dot_Jasym_x_Basym             &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jasym_x_Basym',                     &
     &            math = '$ -u \cdot (J_{asym} \times B_{asym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{sym} \times B_{asym}) @f$
      type(field_def), parameter :: rev_u_dot_Jsym_x_Basym              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jsym_x_Basym',                      &
     &            math = '$ -u \cdot (J_{sym} \times B_{asym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{asym} \times B_{sym}) @f$
      type(field_def), parameter :: rev_u_dot_Jasym_x_Bsym              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jasym_x_Bsym',                      &
     &            math = '$ -u \cdot (J_{asym} \times B_{sym}) $')
!
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{sym} \times B_{sym}) @f$
      type(field_def), parameter :: u_dot_Jsym_x_Bsym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jsym_x_Bsym',                           &
     &            math = '$ u \cdot (J_{sym} \times B_{sym}) $')
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{asym} \times B_{asym}) @f$
      type(field_def), parameter :: u_dot_Jasym_x_Basym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jasym_x_Basym',                         &
     &            math = '$ u \cdot (J_{asym} \times B_{asym}) $')
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{sym} \times B_{asym}) @f$
      type(field_def), parameter :: u_dot_Jsym_x_Basym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jsym_x_Basym',                          &
     &            math = '$ u \cdot (J_{sym} \times B_{asym}) $')
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{asym} \times B_{sym}) @f$
      type(field_def), parameter :: u_dot_Jasym_x_Bsym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jasym_x_Bsym',                          &
     &            math = '$ u \cdot (J_{asym} \times B_{sym}) $')
!
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      type(field_def), parameter :: u_dot_Bsym_nabla_Bsym               &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Bsym_nabla_Bsym',                       &
     &            math = '$ u \cdot (B_{sym} \cdot \nabla) B_{sym} $')
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      type(field_def), parameter :: u_dot_Basym_nabla_Basym             &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Basym_nabla_Basym',                     &
     &            math = '$ u \cdot (B_{sym} \cdot \nabla) B_{sym} $')
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      type(field_def), parameter :: u_dot_Bsym_nabla_Basym              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Bsym_nabla_Basym',                      &
     &            math = '$ u \cdot (B_{sym} \cdot \nabla) B_{sym} $')
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      type(field_def), parameter :: u_dot_Basym_nabla_Bsym              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Basym_nabla_Bsym',                      &
     &            math = '$ u \cdot (B_{sym} \cdot \nabla) B_{sym} $')
!
!>        Field label of buoyancy flux
!!         @f$ -u \cdot (\alpha_{T} g_{i} T_{sym}) @f$
      type(field_def), parameter :: sym_termal_buo_flux                 &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'sym_termal_buo_flux',                         &
     &            math = '$ -u \cdot (\alpha_{T} g_{i} T_{sym}) $')
!>        Field label of buoyancy flux
!!         @f$ -u \cdot (\alpha_{T} g_{i} T_{asym}) @f$
      type(field_def), parameter :: asym_termal_buo_flux                &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'asym_termal_buo_flux',                        &
     &            math = '$ -u \cdot (\alpha_{T} g_{i} T_{asym}) $')
!
!>        Field label of compositional buoyancy flux
!!         @f$ -u \cdot (\alpha_{C} g_{i} C_{sym}) @f$
      type(field_def), parameter :: sym_composite_buo_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'sym_composite_buo_flux',                      &
     &            math = '$ -u \cdot (\alpha_{C} g_{i} C_{sym}) $')
!>        Field label of compositional buoyancy flux
!!         @f$ -u \cdot (\alpha_{C} g_{i} C_{asym}) @f$
      type(field_def), parameter :: asym_composite_buo_flux             &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'asym_composite_buo_flux',                     &
     &            math = '$ -u \cdot (\alpha_{C} g_{i} C_{asym}) $')
!
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{sym} \times  u_{sym}) @f$
      type(field_def), parameter :: B_rot_Bsym_x_usym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'B_rot_Bsym_x_usym',                     &
     &                  math = '$ B \cdot \nabla \times'                &
     &                      // ' (B_{sym} \times  u_{sym}) $')
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{asym} \times  u_{asym}) @f$
      type(field_def), parameter :: B_rot_Basym_x_uasym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'B_rot_Basym_x_uasym',                   &
     &                  math = '$ B \cdot \nabla \times'                &
     &                      // ' (B_{asym} \times  u_{asym}) $')
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{sym} \times  u_{asym}) @f$
      type(field_def), parameter :: B_rot_Bsym_x_uasym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'B_rot_Bsym_x_uasym',                    &
     &                  math = '$ B \cdot \nabla \times'                &
     &                      // ' (B_{sym} \times  u_{asym}) $')
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{asym} \times  u_{sym}) @f$
      type(field_def), parameter :: B_rot_Basym_x_usym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'B_rot_Basym_x_usym',                    &
     &                  math = '$ B \cdot \nabla \times'                &
     &                      // ' (B_{asym} \times  u_{sym}) $')
!
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{sym} \cdot \nabla \right) u_{sym} @f$
      type(field_def), parameter :: B_dot_Bsym_nabla_usym               &
     &    = field_def(n_comp = n_scalar,                                &
     &           name = 'B_dot_Bsym_nabla_usym',                        &
     &           math = '$ B \cdot \left(B_{sym} \cdot'                 &
     &                // ' \nabla \right) u_{sym} $')
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{asym} \cdot \nabla \right) u_{asym} @f$
      type(field_def), parameter :: B_dot_Basym_nabla_uasym             &
     &    = field_def(n_comp = n_scalar,                                &
     &           name = 'B_dot_Basym_nabla_uasym',                      &
     &           math = '$ B \cdot \left(B_{asym} \cdot'                &
     &                // ' \nabla \right) u_{asym} $')
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{sym} \cdot \nabla \right) u_{asym} @f$
      type(field_def), parameter :: B_dot_Bsym_nabla_uasym              &
     &    = field_def(n_comp = n_scalar,                                &
     &           name = 'B_dot_Bsym_nabla_uasym',                       &
     &           math = '$ B \cdot \left(B_{sym} \cdot'                 &
     &                // ' \nabla \right) u_{asym} $')
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{asym} \cdot \nabla \right) u_{sym} @f$
      type(field_def), parameter :: B_dot_Basym_nabla_usym              &
     &    = field_def(n_comp = n_scalar,                                &
     &           name = 'B_dot_Basym_nabla_usym',                       &
     &           math = '$ B \cdot \left(B_{asym} \cdot'                &
     &                // ' \nabla \right) u_{sym} $')
!
!>        Field label of temperature flux
!!         @f$ T \left(u_{sim} \cdot \nabla \right) T_{sym} @f$
      type(field_def), parameter :: T_usym_nabla_Tsym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'T_usym_nabla_Tsym',                     &
     &                  math = '$ T \left(u_{sim} \cdot \nabla'         &
     &                       // ' \right) T_{sym}  $')
!>        Field label of temperature flux
!!         @f$ T \left(u_{asym} \cdot \nabla \right) T_{asym} @f$
      type(field_def), parameter :: T_uasym_nabla_Tasym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'T_uasym_nabla_Tasym',                   &
     &                  math = '$ T \left(u_{asym} \cdot \nabla'        &
     &                       // ' \right) T_{asym}  $')
!>        Field label of temperature flux
!!         @f$ T \left(u_{sym} \cdot \nabla \right) T_{asym} @f$
      type(field_def), parameter :: T_usym_nabla_Tasym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'T_usym_nabla_Tasym',                    &
     &                  math = '$ T \left(u_{sym} \cdot \nabla'         &
     &                       // ' \right) T_{asym}  $')
!>        Field label of temperature flux
!!         @f$ T \left(u_{asym} \cdot \nabla \right) T_{sym} @f$
      type(field_def), parameter :: T_uasym_nabla_Tsym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'T_uasym_nabla_Tsym',                    &
     &                  math = '$ T \left(u_{asym} \cdot \nabla'        &
     &                       // ' \right) T_{sym}  $')
!
!>        Field label of perturbation temperature flux
!!         @f$ \Theta \left(u_{sym} \cdot \nabla \right) \Theta_{sym} @f$
      type(field_def), parameter :: pT_usym_nabla_pTsym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &       name = 'pT_usym_nabla_pTsym',                              &
     &       math = '$ \Theta \left(u_{sym} \cdot \nabla'               &
     &           // ' \right) \Theta_{sym} $')
!>        Field label of perturbation temperature flux
!!         @f$ \Theta \left(u_{asym} \cdot \nabla \right)
!!             \Theta_{asym} @f$
      type(field_def), parameter :: pT_uasym_nabla_pTasym               &
     &    = field_def(n_comp = n_scalar,                                &
     &       name = 'pT_uasym_nabla_pTasym',                            &
     &       math = '$ \Theta \left(u_{asym} \cdot \nabla'              &
     &           // ' \right) \Theta_{asym} $')
!>        Field label of perturbation temperature flux
!!         @f$ \Theta \left(u_{sym} \cdot \nabla \right)
!!             \Theta_{asym} @f$
      type(field_def), parameter :: pT_usym_nabla_pTasym                &
     &    = field_def(n_comp = n_scalar,                                &
     &       name = 'pT_usym_nabla_pTasym',                             &
     &       math = '$ \Theta \left(u_{sym} \cdot \nabla'               &
     &           // '  \right) \Theta_{asym} $')
!>        Field label of perturbation temperature flux
!!         @f$ \Theta \left(u_{asym} \cdot \nabla \right)
!!             \Theta_{sym} @f$
      type(field_def), parameter :: pT_uasym_nabla_pTsym                &
     &    = field_def(n_comp = n_scalar,                                &
     &       name = 'pT_uasym_nabla_pTsym',                             &
     &       math = '$ \Theta \left(u_{asym} \cdot \nabla'              &
     &           // '  \right) \Theta_{sym} $')
!
!>        Field label of composition flux
!!         @f$ C \left(u_{sim} \cdot \nabla C_{sym} \right) @f$
      type(field_def), parameter :: C_usym_nabla_Csym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'C_usym_nabla_Csym',                     &
     &                  math = '$ C \left(u_{sim} \cdot \nabla'         &
     &                      // '  C_{sim} \right) $')
!>        Field label of composition flux
!!         @f$ C \left(u_{asym} \cdot \nabla C_{asym} \right) @f$
      type(field_def), parameter :: C_uasym_nabla_Casym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'C_uasym_nabla_Casym',                   &
     &                  math = '$ C \left(u_{asym} \cdot \nabla'        &
     &                      // ' C_{asym} \right) $')
!>        Field label of composition flux
!!         @f$ C \left(u_{sym} \cdot \nabla C_{asym} \right) @f$
      type(field_def), parameter :: C_usym_nabla_Casym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'C_usym_nabla_Casym',                    &
     &                  math = '$ C \left(u_{sym} \cdot'                &
     &                      // ' \nabla C_{asym} \right) $')
!>        Field label of composition flux
!!         @f$ C \left(u_{asym} \cdot \nabla C_{sym} \right) @f$
      type(field_def), parameter :: C_uasym_nabla_Csym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'C_uasym_nabla_Csym',                    &
     &                  math = '$ C \left(u_{asym} \cdot'               &
     &                      // ' \nabla C_{sym} \right) $')
!
!>        Field label of composition flux
!!         @f$ (C - C_0) \left(u_{sym} \cdot \partial_{i} \right)
!!             (C_{sym} - C_0) @f$
      type(field_def), parameter :: pC_usym_nabla_pCsym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'pC_usym_nabla_pCsym',                   &
     &                  math = '$ (C - C_0) \left(u_{sym} \cdot'        &
     &                      // ' \partial_{i} \right)'                  &
     &                      // ' (C_{sym} - C_0) $')
!>        Field label of composition flux
!!         @f$ (C - C_0) \left(u_{asym} \cdot  \partial_{i} \right)
!!             (C_{asym} - C_0) @f$
      type(field_def), parameter :: pC_uasym_nabla_pCasym               &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'pC_uasym_nabla_pCasym',                 &
     &                  math = '$ (C - C_0) \left(u_{asym} \cdot'       &
     &                      // ' \partial_{i} \right)'                  &
     &                      // ' (C_{asym} - C_0) $')
!>        Field label of composition flux
!!         @f$ (C - C_0) \left(u_{sym} \cdot \partial_{i} \right)
!!             (C_{asym} - C_0) @f$
      type(field_def), parameter :: pC_usym_nabla_pCasym                &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'pC_usym_nabla_pCasym',                  &
     &                  math = '$ (C - C_0) \left(u_{sym} \cdot'        &
     &                      // ' \partial_{i} \right)'                  &
     &                      // ' (C_{asym} - C_0) $')
!>        Field label of composition flux
!!         @f$ (C - C_0) \left(u_{asym} \cdot \partial_{i} \right)
!!             (C_{sym} - C_0) @f$
      type(field_def), parameter :: pC_uasym_nabla_pCsym                &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'pC_uasym_nabla_pCsym',                  &
     &                  math = '$ (C - C_0) \left(u_{asym} \cdot'       &
     &                      // ' \partial_{i} \right)'                  &
     &                      // ' (C_{sym} - C_0) $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_ene_fluxes_w_sym(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_ene_fluxes_w_sym = .FALSE.
      if (    (field_name .eq. u_dot_wsym_x_usym%name)                  &
     &   .or. (field_name .eq. u_dot_wasym_x_uasym%name)                &
     &   .or. (field_name .eq. u_dot_wsym_x_uasym%name)                 &
     &   .or. (field_name .eq. u_dot_wasym_x_usym%name)                 &
!
     &   .or. (field_name .eq. rev_u_dot_Jsym_x_Bsym%name)              &
     &   .or. (field_name .eq. rev_u_dot_Jasym_x_Basym%name)            &
     &   .or. (field_name .eq. rev_u_dot_Jsym_x_Basym%name)             &
     &   .or. (field_name .eq. rev_u_dot_Jasym_x_Bsym%name)             &
!
     &   .or. (field_name .eq. u_dot_Jsym_x_Bsym%name)                  &
     &   .or. (field_name .eq. u_dot_Jasym_x_Basym%name)                &
     &   .or. (field_name .eq. u_dot_Jsym_x_Basym%name)                 &
     &   .or. (field_name .eq. u_dot_Jasym_x_Bsym%name)                 &
!
     &   .or. (field_name .eq. u_dot_Bsym_nabla_Bsym%name)              &
     &   .or. (field_name .eq. u_dot_Basym_nabla_Basym%name)            &
     &   .or. (field_name .eq. u_dot_Bsym_nabla_Basym%name)             &
     &   .or. (field_name .eq. u_dot_Basym_nabla_Bsym%name)             &
!
     &   .or. (field_name .eq. sym_termal_buo_flux%name)                &
     &   .or. (field_name .eq. asym_termal_buo_flux%name)               &
!
     &   .or. (field_name .eq. sym_composite_buo_flux%name)             &
     &   .or. (field_name .eq. asym_composite_buo_flux%name)            &
!
     &   .or. (field_name .eq. B_rot_Bsym_x_usym%name)                  &
     &   .or. (field_name .eq. B_rot_Basym_x_uasym%name)                &
     &   .or. (field_name .eq. B_rot_Bsym_x_uasym%name)                 &
     &   .or. (field_name .eq. B_rot_Basym_x_usym%name)                 &
!
     &   .or. (field_name .eq. B_dot_Bsym_nabla_usym%name)              &
     &   .or. (field_name .eq. B_dot_Basym_nabla_uasym%name)            &
     &   .or. (field_name .eq. B_dot_Bsym_nabla_uasym%name)             &
     &   .or. (field_name .eq. B_dot_Basym_nabla_usym%name)             &
!
     &   .or. (field_name .eq. T_usym_nabla_Tsym%name)                  &
     &   .or. (field_name .eq. T_uasym_nabla_Tasym%name)                &
     &   .or. (field_name .eq. T_usym_nabla_Tasym%name)                 &
     &   .or. (field_name .eq. T_uasym_nabla_Tsym%name)                 &
!
     &   .or. (field_name .eq. pT_usym_nabla_pTsym%name)                &
     &   .or. (field_name .eq. pT_uasym_nabla_pTasym%name)              &
     &   .or. (field_name .eq. pT_usym_nabla_pTasym%name)               &
     &   .or. (field_name .eq. pT_uasym_nabla_pTsym%name)               &
!
     &   .or. (field_name .eq. C_usym_nabla_Csym%name)                  &
     &   .or. (field_name .eq. C_uasym_nabla_Casym%name)                &
     &   .or. (field_name .eq. C_usym_nabla_Casym%name)                 &
     &   .or. (field_name .eq. C_uasym_nabla_Csym%name)                 &
!
     &   .or. (field_name .eq. pC_usym_nabla_pCsym%name)                &
     &   .or. (field_name .eq. pC_uasym_nabla_pCasym%name)              &
     &   .or. (field_name .eq. pC_usym_nabla_pCasym%name)               &
     &   .or. (field_name .eq. pC_uasym_nabla_pCsym%name)               &
     &      )   check_ene_fluxes_w_sym = .TRUE.
!
      end function check_ene_fluxes_w_sym
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ene_flux_w_symmetry_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(u_dot_wsym_x_usym,       array_c2i)
      call set_field_label_to_ctl(u_dot_wasym_x_uasym,     array_c2i)
      call set_field_label_to_ctl(u_dot_wsym_x_uasym,      array_c2i)
      call set_field_label_to_ctl(u_dot_wasym_x_usym,      array_c2i)
!
      call set_field_label_to_ctl(rev_u_dot_Jsym_x_Bsym,   array_c2i)
      call set_field_label_to_ctl(rev_u_dot_Jasym_x_Basym, array_c2i)
      call set_field_label_to_ctl(rev_u_dot_Jsym_x_Basym,  array_c2i)
      call set_field_label_to_ctl(rev_u_dot_Jasym_x_Bsym,  array_c2i)
!
      call set_field_label_to_ctl(u_dot_Jsym_x_Bsym,       array_c2i)
      call set_field_label_to_ctl(u_dot_Jasym_x_Basym,     array_c2i)
      call set_field_label_to_ctl(u_dot_Jsym_x_Basym,      array_c2i)
      call set_field_label_to_ctl(u_dot_Jasym_x_Bsym,      array_c2i)
!
      call set_field_label_to_ctl(u_dot_Bsym_nabla_Bsym,   array_c2i)
      call set_field_label_to_ctl(u_dot_Basym_nabla_Basym, array_c2i)
      call set_field_label_to_ctl(u_dot_Bsym_nabla_Basym,  array_c2i)
      call set_field_label_to_ctl(u_dot_Basym_nabla_Bsym,  array_c2i)
!
      call set_field_label_to_ctl(sym_termal_buo_flux,     array_c2i)
      call set_field_label_to_ctl(asym_termal_buo_flux,    array_c2i)
!
      call set_field_label_to_ctl(sym_composite_buo_flux,  array_c2i)
      call set_field_label_to_ctl(asym_composite_buo_flux, array_c2i)
!
      call set_field_label_to_ctl(B_rot_Bsym_x_usym,       array_c2i)
      call set_field_label_to_ctl(B_rot_Basym_x_uasym,     array_c2i)
      call set_field_label_to_ctl(B_rot_Bsym_x_uasym,      array_c2i)
      call set_field_label_to_ctl(B_rot_Basym_x_usym,      array_c2i)
!
      call set_field_label_to_ctl(B_dot_Bsym_nabla_usym,   array_c2i)
      call set_field_label_to_ctl(B_dot_Basym_nabla_uasym, array_c2i)
      call set_field_label_to_ctl(B_dot_Bsym_nabla_uasym,  array_c2i)
      call set_field_label_to_ctl(B_dot_Basym_nabla_usym,  array_c2i)
!
      call set_field_label_to_ctl(T_usym_nabla_Tsym,       array_c2i)
      call set_field_label_to_ctl(T_uasym_nabla_Tasym,     array_c2i)
      call set_field_label_to_ctl(T_usym_nabla_Tasym,      array_c2i)
      call set_field_label_to_ctl(T_uasym_nabla_Tsym,      array_c2i)
!
      call set_field_label_to_ctl(pT_usym_nabla_pTsym,     array_c2i)
      call set_field_label_to_ctl(pT_uasym_nabla_pTasym,   array_c2i)
      call set_field_label_to_ctl(pT_usym_nabla_pTasym,    array_c2i)
      call set_field_label_to_ctl(pT_uasym_nabla_pTsym,    array_c2i)
!
      call set_field_label_to_ctl(C_usym_nabla_Csym,       array_c2i)
      call set_field_label_to_ctl(C_uasym_nabla_Casym,     array_c2i)
      call set_field_label_to_ctl(C_usym_nabla_Casym,      array_c2i)
      call set_field_label_to_ctl(C_uasym_nabla_Csym,      array_c2i)
!
      call set_field_label_to_ctl(pC_usym_nabla_pCsym,     array_c2i)
      call set_field_label_to_ctl(pC_uasym_nabla_pCasym,   array_c2i)
      call set_field_label_to_ctl(pC_usym_nabla_pCasym,    array_c2i)
      call set_field_label_to_ctl(pC_uasym_nabla_pCsym,    array_c2i)
!
      end subroutine set_ene_flux_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      end module m_energy_flux_w_sym_labels
