!>@file   m_force_w_sym_labels.f90
!!        module m_force_w_sym_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses of basic forces
!!
!!@verbatim
!!      logical function check_scalar_advection_w_sym(field_name)
!!      logical function check_forces_w_sym(field_name)
!!      logical function check_flux_tensors_w_sym(field_name)
!!      logical function check_flux_asym_tensors_w_sym(field_name)
!!
!!      integer(kind = kint) function num_forces_w_symmetry()
!!      subroutine set_force_w_symmetry_names(n_comps, names, maths)
!!
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   inertia
!!                 :        inertia (\omega \times u)
!!   momentum_flux
!!                 :  momentum flux     u_{i} u_{j}
!!   Lorentz_force
!!                 :  Lorentz force     J \times B
!!   magnetic_tension
!!                 :  magnetic tension   (B \nabla) B
!!   maxwell_tensor_sym_sym, maxwell_tensor_asym_asym,
!!   maxwell_tensor_sym_asym
!!                 :  maxwell tensor       B_{i} B_{j}
!!
!!   sym_thermal_buoyancy, asym_thermal_buoyancy
!!                 :   Thermal buoyancy       - \alpha_{T} g T
!!   sym_composite_buoyancy, asym_composite_buoyancy
!!                 :   compositional buoyancy  - \alpha_{C} g C
!!
!!   usym_x_Bsym, uasym_x_Basym, usym_x_Basym, uasym_x_Bsym
!!                 :     induction                           u \times B
!!   Bsym_nabla_usym, Basym_nabla_uasym,
!!   Bsym_nabla_uasym, Basym_nabla_usym
!!                 :    magneitic streatch         (B \nabla) u
!!   usym_Bsym, uasym_Basym, usym_Basym
!!                 :    induction induction tensor
!!                                 u_{i} B_{j}  - B_{i} u_{J}
!!
!!   usym_nabla_Tsym, uasym_nabla_Tasym,
!!   usym_nabla_Tasym, uasym_nabla_Tsym
!!                 :    heat advection     (u \cdot \nabla) T
!!   usym_nabla_pTsym, uasym_nabla_pTasym,
!!   usym_nabla_pTasym, uasym_nabla_pTsym
!!                 :  perturbation of heat advection
!!                                      (u \cdot \nabla) \Theta
!!   heat_flux_sym_sym, heat_flux_asym_asym,
!!   heat_flux_sym_asym, heat_flux_asym_sym
!!                 :    heat flux                   uT
!!   pert_h_flux_sym_sym, pert_h_flux_asym_asym
!!   pert_h_flux_sym_asym, pert_h_flux_asym_sym
!!                 :  perturbation of heat flux   u\Theta
!!
!!   usym_nabla_Csym, uasym_nabla_Casym
!!   usym_nabla_Casym, uasym_nabla_Csym
!!                 :    composition advection     (u \cdot \nabla) C
!!   usym_nabla_pCsym, uasym_nabla_pCasym,
!!   usym_nabla_pCasym, uasym_nabla_pCsym
!!                 :  perturbation of composition advection
!!                                      (u \cdot \nabla) (C-C_0)
!!   composite_flux_sym_sym, composite_flux_asym_asym, 
!!   composite_flux_sym_asym, composite_flux_asym_sym
!!                 :    composition flux                   uC
!!   pert_c_flux_sym_sym, pert_c_flux_asym_asym,
!!   pert_c_flux_sym_asym, pert_c_flux_asym_sym
!!                 :  perturbation of composition flux   u(C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_force_w_sym_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nforce_w_sym = 70
!
!>        Field label of advection of momentum
!!         @f$ u_{symj} \partial_{j} u_{symi} @f$
!!         or @f$ \omega_{sym} \times u_{sym} @f$
      type(field_def), parameter :: wsym_x_usym                         &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'wsym_x_usym',                           &
     &                  math = '$ u_{symj} \partial_{j} u_{symi} $, '   &
     &                      //' $ \omega_{sym} \times u_{sym} $')
!>        Field label of advection of momentum
!!         @f$ u_{asymj} \partial_{j} u_{asymi} @f$
!!         or @f$ \omega_{asym} \times u_{asym} @f$
      type(field_def), parameter :: wasym_x_uasym                       &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'wasym_x_uasym',                         &
     &                  math = '$ u_{asymj} \partial_{j} u_{asymi} $, ' &
     &                      //' $ \omega_{asym} \times u_{asym} $')
!>        Field label of advection of momentum
!!         @f$ u_{symj} \partial_{j} u_{asymi} @f$
!!         or @f$ \omega_{sym} \times u_{asym} @f$
      type(field_def), parameter :: wsym_x_uasym                        &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'wsym_x_uasym',                          &
     &                  math = '$ u_{symj} \partial_{j} u_{asymi} $, '  &
     &                      //' $ \omega_{sym} \times u_{asym} $')
!>        Field label of advection of momentum
!!         @f$ u_{asymj} \partial_{j} u_{symi} @f$
!!         or @f$ \omega_{asym} \times u_{sym} @f$
      type(field_def), parameter :: wasym_x_usym                        &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'wasym_x_usym',                          &
     &                  math = '$ u_{asymj} \partial_{j} u_{symi} $, '  &
     &                      //' $ \omega_{asym} \times u_{sym} $')
!
!>        Field label of momentum flux
!!         @f$ u_{symi} u_{symj} @f$
      type(field_def), parameter :: m_flux_sym_sym                      &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                  name = 'm_flux_sym_sym',                        &
     &                  math = '$ u_{symi} u_{symj} $')
!>        Field label of momentum flux
!!         @f$ u_{asymi} u_{asymj} @f$
      type(field_def), parameter :: m_flux_asym_asym                    &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                  name = 'm_flux_asym_asym',                      &
     &                  math = '$ u_{asymi} u_{asymj} $')
!>        Field label of momentum flux
!!         @f$ u_{symi} u_{asymj} @f$
      type(field_def), parameter :: m_flux_sym_asym                     &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                  name = 'm_flux_sym_asym',                       &
     &                  math = '$ u_{symi} u_{asymj} $')
!
!>        Field label of Lorentz force
!!         @f$ J_{sym} \times B_{sym} @f$
      type(field_def), parameter :: Jsym_x_Bsym                         &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Jsym_x_Bsym',                           &
     &                  math = '$ J_{sym} \times B_{sym} $')
!>        Field label of Lorentz force
!!         @f$ J_{asym} \times B_{asym} @f$
      type(field_def), parameter :: Jasym_x_Basym                       &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Jasym_x_Basym',                         &
     &                  math = '$ J_{asym} \times B_{asym} $')
!>        Field label of Lorentz force
!!         @f$ J_{sym} \times B_{asym} @f$
      type(field_def), parameter :: Jsym_x_Basym                        &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Jsym_x_Basym',                          &
     &                  math = '$ J_{sym} \times B_{asym} $')
!>        Field label of Lorentz force
!!         @f$ J_{asym} \times B_{sym} @f$
      type(field_def), parameter :: Jasym_x_Bsym                        &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Jasym_x_Bsym',                          &
     &                  math = '$ J_{asym} \times B_{sym} $')
!
!>        start address of magnetic tension
!!         @f$ (B_{sym} \cdot \nabla) B_{sym} @f$
      type(field_def), parameter :: Bsym_nabla_Bsym                     &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Bsym_nabla_Bsym',                       &
     &                  math = '$ (B_{sym} \cdot \nabla) B_{sym} $')
!>        start address of magnetic tension
!!         @f$ (B_{asym} \cdot \nabla) B_{asym} @f$
      type(field_def), parameter :: Basym_nabla_Basym                   &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Basym_nabla_Basym',                     &
     &                  math = '$ (B_{asym} \cdot \nabla) B_{asym} $')
!>        start address of magnetic tension
!!         @f$ (B_{sym} \cdot \nabla) B_{asym} @f$
      type(field_def), parameter :: Bsym_nabla_Basym                    &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Bsym_nabla_Basym',                      &
     &                  math = '$ (B_{sym} \cdot \nabla) B_{asym} $')
!>        start address of magnetic tension
!!         @f$ (B_{asym} \cdot \nabla) B_{sym} @f$
      type(field_def), parameter :: Basym_nabla_Bsym                    &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Basym_nabla_Bsym',                      &
     &                  math = '$ (B_{asym} \cdot \nabla) B_{sym} $')
!
!>        Field label of Maxwell tensor
!!         @f$ B_{sym} B_{sym} @f$
      type(field_def), parameter :: maxwell_tensor_sym_sym              &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                  name = 'maxwell_tensor_sym_sym',                &
     &                  math = '$ B_{sym} B_{sym} $')
!>        Field label ofof Maxwell tensor
!!         @f$ B_{asym} B_{asym} @f$
      type(field_def), parameter :: maxwell_tensor_asym_asym            &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                  name = 'maxwell_tensor_asym_asym',              &
     &                  math = '$ B_{asym} B_{asym} $')
!>        Field label of Maxwell tensor
!!         @f$ B_{sym} B_{asym} @f$
      type(field_def), parameter :: maxwell_tensor_sym_asym             &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                  name = 'maxwell_tensor_sym_asym',               &
     &                  math = '$ B_{sym} B_{asym} $')
!
!>        Field label of buoyancy
!!         @f$ -\alpha_{T} g_{i} T_{sym} @f$
      type(field_def), parameter :: sym_thermal_buoyancy                 &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'sym_thermal_buoyancy',                   &
     &                  math = '$ -\alpha_{T} g_{i} T_{sym} $')
!>        Field label of buoyancy
!!         @f$ -\alpha_{T} g_{i} T_{asym} @f$
      type(field_def), parameter :: asym_thermal_buoyancy                &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'asym_thermal_buoyancy',                  &
     &                  math = '$ -\alpha_{T} g_{i} T_{asym} $')
!
!>        Field label of compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} C_{sym} @f$
      type(field_def), parameter :: sym_composite_buoyancy              &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'sym_composite_buoyancy',                &
     &                  math = '$ -\alpha_{C} g_{i} C_{sym} $')
!>        Field label of compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} C_{asym} @f$
      type(field_def), parameter :: asym_composite_buoyancy             &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'asym_composite_buoyancy',               &
     &                  math = '$ -\alpha_{C} g_{i} C_{asym} $')
!!
!>        Field label of induction of vector potential
!!         @f$ u_{sym} \times B_{sym} @f$
      type(field_def), parameter :: usym_x_Bsym                         &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'usym_x_Bsym',                           &
     &                  math = '$ u_{sym} \times B_{sym} $')
!>        Field label of induction of vector potential
!!         @f$ u_{asym} \times B_{asym} @f$
      type(field_def), parameter :: uasym_x_Basym                       &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'uasym_x_Basym',                         &
     &                  math = '$ u_{asym} \times B_{asym} $')
!>        Field label of induction of vector potential
!!         @f$ u_{sym} \times B_{asym} @f$
      type(field_def), parameter :: usym_x_Basym                        &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'usym_x_Basym',                          &
     &                  math = '$ u_{sym} \times B_{asym} $')
!>        Field label of induction of vector potential
!!         @f$ u_{asym} \times B_{sym} @f$
      type(field_def), parameter :: uasym_x_Bsym                        &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'uasym_x_Bsym',                          &
     &                  math = '$ u_{asym} \times B_{asym} $')
!
!>        Field label of magnetic induction
!!         @f$ \nabla \times (u_{sym} \times B_{sym}) @f$
      type(field_def), parameter :: rot_usym_x_Bsym                     &
     &    = field_def(n_comp = n_vector,                                &
     &           name = 'rot_usym_x_Bsym',                              &
     &           math = '$ \nabla \times (u_{sym} \times B_{sym}) $')
!>        Field label of magnetic induction
!!         @f$ \nabla \times (u_{asym} \times B_{asym}) @f$
      type(field_def), parameter :: rot_uasym_x_Basym                   &
     &    = field_def(n_comp = n_vector,                                &
     &           name = 'rot_uasym_x_Basym',                            &
     &           math = '$ \nabla \times (u_{asym} \times B_{asym}) $')
!>        Field label of magnetic induction
!!         @f$ \nabla \times (u_{sym} \times B_{asym}) @f$
      type(field_def), parameter :: rot_usym_x_Basym                    &
     &    = field_def(n_comp = n_vector,                                &
     &           name = 'rot_usym_x_Basym',                             &
     &           math = '$ \nabla \times (u_{sym} \times B_{asym}) $')
!>        Field label of magnetic induction
!!         @f$ \nabla \times (u_{asym} \times B_{sym}) @f$
      type(field_def), parameter :: rot_uasym_x_Bsym                    &
     &    = field_def(n_comp = n_vector,                                &
     &           name = 'rot_uasym_x_Bsym',                             &
     &           math = '$ \nabla \times (u_{asym} \times B_{sym}) $')
!
!>        Field label of magnetic stretch term
!!         @f$ (B_{sym} \cdot \nabla) u_{sym} @f$
      type(field_def), parameter :: Bsym_nabla_usym                     &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Bsym_nabla_usym',                       &
     &                  math = '$ (B_{sym} \cdot \nabla) u_{sym} $')
!>        Field label of magnetic stretch term
!!         @f$ (B_{asym} \cdot \nabla) u_{asym} @f$
      type(field_def), parameter :: Basym_nabla_uasym                   &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Basym_nabla_uasym',                     &
     &                  math = '$ (B_{asym} \cdot \nabla) u_{asym} $')
!>        Field label of magnetic stretch term
!!         @f$ (B_{sym} \cdot \nabla) u_{asym} @f$
      type(field_def), parameter :: Bsym_nabla_uasym                    &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Bsym_nabla_uasym',                      &
     &                  math = '$ (B_{sym} \cdot \nabla) u_{asym} $')
!>        Field label of magnetic stretch term
!!         @f$ (B_{asym} \cdot \nabla) u_{sym} @f$
      type(field_def), parameter :: Basym_nabla_usym                    &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'Basym_nabla_usym',                      &
     &                  math = '$ (B_{asym} \cdot \nabla) u_{sym} $')
!
!>        Field label of Tensor of magnetic induction
!!         @f$ u_{sym} B_{sym}  - B_{sym} u_{sym} @f$
      type(field_def), parameter :: usym_Bsym                           &
     &    = field_def(n_comp = n_vector,                                &
     &           name = 'usym_Bsym',                                    &
     &           math = '$ u_{sym} B_{sym}  - B_{sym} u_{sym} $')
!>        Field label of Tensor of magnetic induction
!!         @f$ u_{asym} B_{asym}  - B_{asym} u_{asym} @f$
      type(field_def), parameter :: uasym_Basym                         &
     &    = field_def(n_comp = n_vector,                                &
     &           name = 'uasym_Basym',                                  &
     &           math = '$ u_{asym} B_{asym}  - B_{asym} u_{asym} $')
!>        Field label of Tensor of magnetic induction
!!         @f$ u_{sym} B_{asym}  - B_{asym} u_{sym} @f$
      type(field_def), parameter :: usym_Basym                          &
     &    = field_def(n_comp = n_vector,                                &
     &           name = 'usym_Basym',                                   &
     &           math = '$ u_{sym} B_{asym}  - B_{asym} u_{sym} $')
!>        Field label of Tensor of magnetic induction
!!         @f$ u_{asym} B_{sym}  - B_{sym} u_{asym} @f$
      type(field_def), parameter :: uasym_Bsym                          &
     &    = field_def(n_comp = n_vector,                                &
     &           name = 'uasym_Bsym',                                   &
     &           math = '$ u_{asym} B_{asym}  - B_{asym} u_{asym} $')
!
!>        Field label of advection of temperature
!!         @f$ u_{sim} \nabla T_{sym} @f$
      type(field_def), parameter :: usym_nabla_Tsym                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'usym_nabla_Tsym',                       &
     &                  math = '$ u_{sim} \nabla T_{sim} $')
!>        Field label of advection of temperature
!!         @f$ u_{asym} \nabla T_{asym} @f$
      type(field_def), parameter :: uasym_nabla_Tasym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'uasym_nabla_Tasym',                     &
     &                  math = '$ u_{asym} \nabla T_{asym} $')
!>        Field label of advection of temperature
!!         @f$ u_{sym} \nabla T_{asym} @f$
      type(field_def), parameter :: usym_nabla_Tasym                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'usym_nabla_Tasym',                      &
     &                  math = '$ u_{sim} \nabla T_{asym} $')
!>        Field label of advection of temperature
!!         @f$ u_{asym} \nabla T_{sym} @f$
      type(field_def), parameter :: uasym_nabla_Tsym                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'uasym_nabla_Tsym',                      &
     &                  math = '$ u_{asym} \nabla T_{sym} $')
!
!>        Field label of advection of perturbation of temperature
!!         @f$ u_{sym} \partial_{i} \Theta_{sym} @f$
      type(field_def), parameter :: usym_nabla_pTsym                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'usym_nabla_pTsym',                      &
     &                  math = '$ u_{sym} \nabla \Theta__{sym} $')
!>        Field label of advection of perturbation of temperature
!!         @f$ u_{asym} \partial_{i} \Theta_{asym} @f$
      type(field_def), parameter :: uasym_nabla_pTasym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'uasym_nabla_pTasym',                    &
     &                  math = '$ u_{asym} \nabla \Theta__{asym} $')
!>        Field label of advection of perturbation of temperature
!!         @f$ u_{sym} \partial_{i} \Theta_{asym} @f$
      type(field_def), parameter :: usym_nabla_pTasym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'usym_nabla_pTasym',                     &
     &                  math = '$ u_{sym} \nabla \Theta__{asym} $')
!>        Field label of advection of perturbation of temperature
!!         @f$ u_{asym} \partial_{i} \Theta_{sym} @f$
      type(field_def), parameter :: uasym_nabla_pTsym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'uasym_nabla_pTsym',                     &
     &                  math = '$ u_{asym} \nabla \Theta__{sym} $')
!
!>        Field label of heat flux
!!         @f$ u_{sym} T_{sym} @f$
      type(field_def), parameter :: heat_flux_sym_sym                   &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'heat_flux_sym_sym',                     &
     &                  math = '$ u_{sym} T_{sym} $')
!>        Field label of heat flux
!!         @f$ u_{asym} T_{asym} @f$
      type(field_def), parameter :: heat_flux_asym_asym                 &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'heat_flux_asym_asym',                   &
     &                  math = '$ u_{asym} T_{asym} $')
!>        Field label of heat flux
!!         @f$ u_{sym} T_{asym} @f$
      type(field_def), parameter :: heat_flux_sym_asym                  &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'heat_flux_sym_asym',                    &
     &                  math = '$ u_{sym} T_{asym} $')
!>        Field label of heat flux
!!         @f$ u_{asym} T_{sym} @f$
      type(field_def), parameter :: heat_flux_asym_sym                  &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'heat_flux_asym_sym',                    &
     &                  math = '$ u_{asym} T_{sym} $')
!
!>        Field label of perturbation of heat flux
!!         @f$ u_{sym} \Theta_{sym} @f$
      type(field_def), parameter :: pert_h_flux_sym_sym                 &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'pert_h_flux_sym_sym',                   &
     &                  math = '$ u_{sym} \Theta_{sym} $')
!>        Field label of perturbation of heat flux
!!         @f$ u_{asym} \Theta_{asym} @f$
      type(field_def), parameter :: pert_h_flux_asym_asym               &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'pert_h_flux_asym_asym',                 &
     &                  math = '$ u_{asym} \Theta_{asym} $')
!>        Field label of heat flux
!>        Field label of perturbation of heat flux
!!         @f$ u_{sym} \Theta_{asym} @f$
      type(field_def), parameter :: pert_h_flux_sym_asym                &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'pert_h_flux_sym_asym',                  &
     &                  math = '$ u_{sym} \Theta_{asym} $')
!>        Field label of perturbation of heat flux
!!         @f$ u_{asym} \Theta_{sym} @f$
      type(field_def), parameter :: pert_h_flux_asym_sym                &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'pert_h_flux_asym_sym',                  &
     &                  math = '$ u_{asym} \Theta_{sym} $')
!
!>        Field label of advection of composition
!!         @f$ u_{sim} \nabla C_{sym} @f$
      type(field_def), parameter :: usym_nabla_Csym                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'usym_nabla_Csym',                       &
     &                  math = '$ u_{sim} \nabla C_{sim} $')
!>        Field label of advection of composition
!!         @f$ u_{asym} \nabla C_{asym} @f$
      type(field_def), parameter :: uasym_nabla_Casym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'uasym_nabla_Casym',                     &
     &                  math = '$ u_{asym} \nabla C_{asym} $')
!>        Field label of advection of composition
!!         @f$ u_{sym} \nabla C_{asym} @f$
      type(field_def), parameter :: usym_nabla_Casym                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'usym_nabla_Casym',                      &
     &                  math = '$ u_{sym} \nabla C_{asym} $')
!>        Field label of advection of composition
!!         @f$ u_{asym} \nabla C_{sym} @f$
      type(field_def), parameter :: uasym_nabla_Csym                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'uasym_nabla_Csym',                      &
     &                  math = '$ u_{asym} \nabla C_{sym} $')
!
!>        Field label of advection of perturbation of composition
!!         @f$ u_{sym} \nabla (C_{sym} - C0) @f$
      type(field_def), parameter :: usym_nabla_pCsym                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'usym_nabla_pCsym',                      &
     &                  math = '$ u_{sym} \nabla (C_{sym} - C0) $')
!>        Field label of advection of perturbation of composition
!!         @f$ u_{asym} \nabla (C_{asym} - C0) @f$
      type(field_def), parameter :: uasym_nabla_pCasym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'uasym_nabla_pCasym',                    &
     &                  math = '$ u_{asym} \nabla (C_{asym} - C0) $')
!>        Field label of advection of perturbation of composition
!!         @f$ u_{sym} \nabla (C_{asym} - C0) @f$
      type(field_def), parameter :: usym_nabla_pCasym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'usym_nabla_pCasym',                     &
     &                  math = '$ u_{sym} \nabla (C_{asym} - C0) $')
!>        Field label of advection of perturbation of composition
!!         @f$ u_{asym} \nabla (C_{sym} - C0) @f$
      type(field_def), parameter :: uasym_nabla_pCsym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'uasym_nabla_pCsym',                     &
     &                  math = '$ u_{asym} \nabla (C_{sym} - C0) $')
!
!>        Field label of composition flux
!!         @f$ u_{sym} C_{sym} @f$
      type(field_def), parameter :: composite_flux_sym_sym              &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'composite_flux_sym_sym',                &
     &                  math = '$ u_{sym} C_{sym} $')
!>        Field label of composition flux
!!         @f$ u_{asym} C_{asym} @f$
      type(field_def), parameter :: composite_flux_asym_asym            &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'composite_flux_asym_asym',              &
     &                  math = '$ u_{asym} C_{asym} $')
!>        Field label of composition flux
!!         @f$ u_{sym} C_{asym} @f$
      type(field_def), parameter :: composite_flux_sym_asym             &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'composite_flux_sym_asym',               &
     &                  math = '$ u_{sym} C_{asym} $')
!>        Field label of composition flux
!!         @f$ u_{asym} C_{sym} @f$
      type(field_def), parameter :: composite_flux_asym_sym             &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'composite_flux_asym_sym',               &
     &                  math = '$ u_{asym} C_{sym} $')
!
!>        Field label of perturbation of composition flux
!!         @f$ u_{sym} (C_{sym} - C0) @f$
      type(field_def), parameter :: pert_c_flux_sym_sym                 &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'pert_c_flux_sym_sym',                   &
     &                  math = '$ u_{sym} (C_{sym} - C0) $')
!>        Field label of perturbation of composition flux
!!         @f$ u_{asym} (C_{asym} - C0) @f$
      type(field_def), parameter :: pert_c_flux_asym_asym               &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'pert_c_flux_asym_asym',                 &
     &                  math = '$ u_{asym} (C_{asym} - C0) $')
!>        Field label of perturbation of composition flux
!!         @f$ u_{sym} (C_{asym} - C0) @f$
      type(field_def), parameter :: pert_c_flux_sym_asym                &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'pert_c_flux_sym_asym',                  &
     &                  math = '$ u_{sym} (C_{asym} - C0) $')
!>        Field label of perturbation of composition flux
!!         @f$ u_{asym} (C_{sym} - C0) @f$
      type(field_def), parameter :: pert_c_flux_asym_sym                &
     &    = field_def(n_comp = n_vector,                                &
     &                  name = 'pert_c_flux_asym_sym',                  &
     &                  math = '$ u_{asym} (C_{sym} - C0) $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_scalar_advection_w_sym(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_scalar_advection_w_sym = .FALSE.
      if (    (field_name .eq. usym_nabla_Tsym%name)                    &
     &   .or. (field_name .eq. uasym_nabla_Tasym%name)                  &
     &   .or. (field_name .eq. usym_nabla_Tasym%name)                   &
     &   .or. (field_name .eq. uasym_nabla_Tsym%name)                   &
!
     &   .or. (field_name .eq. usym_nabla_pTsym%name)                   &
     &   .or. (field_name .eq. uasym_nabla_pTasym%name)                 &
     &   .or. (field_name .eq. usym_nabla_pTasym%name)                  &
     &   .or. (field_name .eq. uasym_nabla_pTsym%name)                  &
!
     &   .or. (field_name .eq. usym_nabla_Csym%name)                    &
     &   .or. (field_name .eq. uasym_nabla_Casym%name)                  &
     &   .or. (field_name .eq. usym_nabla_Casym%name)                   &
     &   .or. (field_name .eq. uasym_nabla_Csym%name)                   &
!
     &   .or. (field_name .eq. usym_nabla_pCsym%name)                   &
     &   .or. (field_name .eq. uasym_nabla_pCasym%name)                 &
     &   .or. (field_name .eq. usym_nabla_pCasym%name)                  &
     &   .or. (field_name .eq. uasym_nabla_pCsym%name)                  &
     &      )   check_scalar_advection_w_sym = .TRUE.
!
      end function check_scalar_advection_w_sym
!
! ----------------------------------------------------------------------
!
      logical function check_forces_w_sym(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_forces_w_sym = .FALSE.
      if (    (field_name .eq. wsym_x_usym%name)                        &
     &   .or. (field_name .eq. wasym_x_uasym%name)                      &
     &   .or. (field_name .eq. wsym_x_uasym%name)                       &
     &   .or. (field_name .eq. wasym_x_usym%name)                       &
!
     &   .or. (field_name .eq. Jsym_x_Bsym%name)                        &
     &   .or. (field_name .eq. Jasym_x_Basym%name)                      &
     &   .or. (field_name .eq. Jsym_x_Basym%name)                       &
     &   .or. (field_name .eq. Jasym_x_Bsym%name)                       &
!
     &   .or. (field_name .eq. Bsym_nabla_Bsym%name)                    &
     &   .or. (field_name .eq. Basym_nabla_Basym%name)                  &
     &   .or. (field_name .eq. Bsym_nabla_Basym%name)                   &
     &   .or. (field_name .eq. Basym_nabla_Bsym%name)                   &
!
     &   .or. (field_name .eq. sym_thermal_buoyancy%name)                &
     &   .or. (field_name .eq. asym_thermal_buoyancy%name)               &
!
     &   .or. (field_name .eq. sym_composite_buoyancy%name)             &
     &   .or. (field_name .eq. asym_composite_buoyancy%name)            &
!
     &   .or. (field_name .eq. usym_x_Bsym%name)                        &
     &   .or. (field_name .eq. uasym_x_Basym%name)                      &
     &   .or. (field_name .eq. usym_x_Basym%name)                       &
     &   .or. (field_name .eq. uasym_x_Bsym%name)                       &
!
     &   .or. (field_name .eq. rot_usym_x_Bsym%name)                    &
     &   .or. (field_name .eq. rot_uasym_x_Basym%name)                  &
     &   .or. (field_name .eq. rot_usym_x_Basym%name)                   &
     &   .or. (field_name .eq. rot_uasym_x_Bsym%name)                   &
!
     &   .or. (field_name .eq. Bsym_nabla_usym%name)                    &
     &   .or. (field_name .eq. Basym_nabla_uasym%name)                  &
     &   .or. (field_name .eq. Bsym_nabla_uasym%name)                   &
     &   .or. (field_name .eq. Basym_nabla_usym%name)                   &
!
     &   .or. (field_name .eq. heat_flux_sym_sym%name)                  &
     &   .or. (field_name .eq. heat_flux_asym_asym%name)                &
     &   .or. (field_name .eq. heat_flux_sym_asym%name)                 &
     &   .or. (field_name .eq. heat_flux_asym_sym%name)                 &
!
     &   .or. (field_name .eq. pert_h_flux_sym_sym%name)                &
     &   .or. (field_name .eq. pert_h_flux_asym_asym%name)              &
     &   .or. (field_name .eq. pert_h_flux_sym_asym%name)               &
     &   .or. (field_name .eq. pert_h_flux_asym_sym%name)               &
!
     &   .or. (field_name .eq. composite_flux_sym_sym%name)             &
     &   .or. (field_name .eq. composite_flux_asym_asym%name)           &
     &   .or. (field_name .eq. composite_flux_sym_asym%name)            &
     &   .or. (field_name .eq. composite_flux_asym_sym%name)            &
!
     &   .or. (field_name .eq. pert_c_flux_sym_sym%name)                &
     &   .or. (field_name .eq. pert_c_flux_asym_asym%name)              &
     &   .or. (field_name .eq. pert_c_flux_sym_asym%name)               &
     &   .or. (field_name .eq. pert_c_flux_asym_sym%name)               &
     &      )   check_forces_w_sym = .TRUE.
!
      end function check_forces_w_sym
!
! ----------------------------------------------------------------------
!
      logical function check_flux_tensors_w_sym(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_flux_tensors_w_sym = .FALSE.
      if (    (field_name .eq. m_flux_sym_sym%name)                     &
     &   .or. (field_name .eq. m_flux_asym_asym%name)                   &
     &   .or. (field_name .eq. m_flux_sym_asym%name)                    &
!
     &   .or. (field_name .eq. maxwell_tensor_sym_sym%name)             &
     &   .or. (field_name .eq. maxwell_tensor_asym_asym%name)           &
     &   .or. (field_name .eq. maxwell_tensor_sym_asym%name)            &
     &      )   check_flux_tensors_w_sym = .TRUE.
!
      end function check_flux_tensors_w_sym
!
! ----------------------------------------------------------------------
!
      logical function check_flux_asym_tensors_w_sym(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_flux_asym_tensors_w_sym = .FALSE.
      if (    (field_name .eq. usym_Bsym%name)                          &
     &   .or. (field_name .eq. uasym_Basym%name)                        &
     &   .or. (field_name .eq. usym_Basym%name)                         &
     &   .or. (field_name .eq. uasym_Bsym%name)                         &
     &      )   check_flux_asym_tensors_w_sym = .TRUE.
!
      end function check_flux_asym_tensors_w_sym
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_forces_w_symmetry()
      num_forces_w_symmetry = nforce_w_sym
      return
      end function num_forces_w_symmetry
!
! ----------------------------------------------------------------------
!
      subroutine set_force_w_symmetry_names(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nforce_w_sym)
      character(len = kchara), intent(inout) :: names(nforce_w_sym)
      character(len = kchara), intent(inout) :: maths(nforce_w_sym)
!
!
!
      call set_field_labels(wsym_x_usym,                                &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(wasym_x_uasym,                              &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(wsym_x_uasym,                               &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(wasym_x_usym,                               &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(Jsym_x_Bsym,                                &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(Jasym_x_Basym,                              &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(Jsym_x_Basym,                               &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(Jasym_x_Bsym,                               &
     &    n_comps( 8), names( 8), maths( 8))
!
      call set_field_labels(Bsym_nabla_Bsym,                            &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(Basym_nabla_Basym,                          &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(Bsym_nabla_Basym,                           &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(Basym_nabla_Bsym,                           &
     &    n_comps(12), names(12), maths(12))
!
      call set_field_labels(sym_thermal_buoyancy,                        &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(asym_thermal_buoyancy,                       &
     &    n_comps(14), names(14), maths(14))
!
      call set_field_labels(sym_composite_buoyancy,                     &
     &    n_comps(15), names(15), maths(15))
      call set_field_labels(asym_composite_buoyancy,                    &
     &    n_comps(16), names(16), maths(16))
!
      call set_field_labels(usym_x_Bsym,                                &
     &    n_comps(17), names(17), maths(17))
      call set_field_labels(uasym_x_Basym,                              &
     &    n_comps(18), names(18), maths(18))
      call set_field_labels(usym_x_Basym,                               &
     &    n_comps(19), names(19), maths(19))
      call set_field_labels(uasym_x_Bsym,                               &
     &    n_comps(20), names(20), maths(20))
!
      call set_field_labels(rot_usym_x_Bsym,                            &
     &    n_comps(21), names(21), maths(21))
      call set_field_labels(rot_uasym_x_Basym,                          &
     &    n_comps(22), names(22), maths(22))
      call set_field_labels(rot_usym_x_Basym,                           &
     &    n_comps(23), names(23), maths(23))
      call set_field_labels(rot_uasym_x_Bsym,                           &
     &    n_comps(24), names(24), maths(24))
!
      call set_field_labels(Bsym_nabla_usym,                            &
     &    n_comps(25), names(25), maths(25))
      call set_field_labels(Basym_nabla_uasym,                          &
     &    n_comps(26), names(26), maths(26))
      call set_field_labels(Bsym_nabla_uasym,                           &
     &    n_comps(27), names(27), maths(27))
      call set_field_labels(Basym_nabla_usym,                           &
     &    n_comps(28), names(28), maths(28))
!
      call set_field_labels(usym_nabla_Tsym,                            &
     &    n_comps(29), names(29), maths(29))
      call set_field_labels(uasym_nabla_Tasym,                          &
     &    n_comps(30), names(30), maths(30))
      call set_field_labels(usym_nabla_Tasym,                           &
     &    n_comps(31), names(31), maths(31))
      call set_field_labels(uasym_nabla_Tsym,                           &
     &    n_comps(32), names(32), maths(32))
!
      call set_field_labels(usym_nabla_pTsym,                           &
     &    n_comps(33), names(33), maths(33))
      call set_field_labels(uasym_nabla_pTasym,                         &
     &    n_comps(34), names(34), maths(34))
      call set_field_labels(usym_nabla_pTasym,                          &
     &    n_comps(35), names(35), maths(35))
      call set_field_labels(uasym_nabla_pTsym,                          &
     &    n_comps(36), names(36), maths(36))
!
      call set_field_labels(usym_nabla_Csym,                            &
     &    n_comps(37), names(37), maths(37))
      call set_field_labels(uasym_nabla_Casym,                          &
     &    n_comps(38), names(38), maths(38))
      call set_field_labels(usym_nabla_Casym,                           &
     &    n_comps(39), names(39), maths(39))
      call set_field_labels(uasym_nabla_Csym,                           &
     &    n_comps(40), names(40), maths(40))
!
      call set_field_labels(usym_nabla_pCsym,                           &
     &    n_comps(41), names(41), maths(41))
      call set_field_labels(uasym_nabla_pCasym,                         &
     &    n_comps(42), names(42), maths(42))
      call set_field_labels(usym_nabla_pCasym,                          &
     &    n_comps(43), names(43), maths(43))
      call set_field_labels(uasym_nabla_pCsym,                          &
     &    n_comps(44), names(44), maths(44))
!
      call set_field_labels(heat_flux_sym_sym,                          &
     &    n_comps(45), names(45), maths(45))
      call set_field_labels(heat_flux_asym_asym,                        &
     &    n_comps(46), names(46), maths(46))
      call set_field_labels(heat_flux_sym_asym,                         &
     &    n_comps(47), names(47), maths(47))
      call set_field_labels(heat_flux_asym_sym,                         &
     &    n_comps(48), names(48), maths(48))
!
      call set_field_labels(pert_h_flux_sym_sym,                        &
     &    n_comps(49), names(49), maths(49))
      call set_field_labels(pert_h_flux_asym_asym,                      &
     &    n_comps(50), names(50), maths(50))
      call set_field_labels(pert_h_flux_sym_asym,                       &
     &    n_comps(51), names(51), maths(51))
      call set_field_labels(pert_h_flux_asym_sym,                       &
     &    n_comps(52), names(52), maths(52))
!
      call set_field_labels(composite_flux_sym_sym,                     &
     &    n_comps(53), names(53), maths(53))
      call set_field_labels(composite_flux_asym_asym,                   &
     &    n_comps(54), names(54), maths(54))
      call set_field_labels(composite_flux_sym_asym,                    &
     &    n_comps(55), names(55), maths(55))
      call set_field_labels(composite_flux_asym_sym,                    &
     &    n_comps(56), names(56), maths(56))
!
      call set_field_labels(pert_c_flux_sym_sym,                        &
     &    n_comps(57), names(57), maths(57))
      call set_field_labels(pert_c_flux_asym_asym,                      &
     &    n_comps(58), names(58), maths(58))
      call set_field_labels(pert_c_flux_sym_asym,                       &
     &    n_comps(59), names(59), maths(59))
      call set_field_labels(pert_c_flux_asym_sym,                       &
     &    n_comps(60), names(60), maths(60))
!
      call set_field_labels(m_flux_sym_sym,                             &
     &    n_comps(61), names(61), maths(61))
      call set_field_labels(m_flux_asym_asym,                           &
     &    n_comps(62), names(62), maths(62))
      call set_field_labels(m_flux_sym_asym,                            &
     &    n_comps(63), names(63), maths(63))
!
      call set_field_labels(maxwell_tensor_sym_sym,                     &
     &    n_comps(64), names(64), maths(64))
      call set_field_labels(maxwell_tensor_asym_asym,                   &
     &    n_comps(65), names(65), maths(65))
      call set_field_labels(maxwell_tensor_sym_asym,                    &
     &    n_comps(66), names(66), maths(66))
!
      call set_field_labels(usym_Bsym,                                  &
     &    n_comps(67), names(67), maths(67))
      call set_field_labels(uasym_Basym,                                &
     &    n_comps(68), names(68), maths(68))
      call set_field_labels(usym_Basym,                                 &
     &    n_comps(69), names(69), maths(69))
      call set_field_labels(uasym_Bsym,                                 &
     &    n_comps(70), names(70), maths(70))
!
      end subroutine set_force_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      end module m_force_w_sym_labels
