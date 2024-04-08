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
!!      subroutine set_force_w_symmetry_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
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
     &   .or. (field_name .eq. sym_thermal_buoyancy%name)               &
     &   .or. (field_name .eq. asym_thermal_buoyancy%name)              &
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
      subroutine set_force_w_symmetry_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(wsym_x_usym,              array_c2i)
      call set_field_label_to_ctl(wasym_x_uasym,            array_c2i)
      call set_field_label_to_ctl(wsym_x_uasym,             array_c2i)
      call set_field_label_to_ctl(wasym_x_usym,             array_c2i)
!
      call set_field_label_to_ctl(Jsym_x_Bsym,              array_c2i)
      call set_field_label_to_ctl(Jasym_x_Basym,            array_c2i)
      call set_field_label_to_ctl(Jsym_x_Basym,             array_c2i)
      call set_field_label_to_ctl(Jasym_x_Bsym,             array_c2i)
!
      call set_field_label_to_ctl(Bsym_nabla_Bsym,          array_c2i)
      call set_field_label_to_ctl(Basym_nabla_Basym,        array_c2i)
      call set_field_label_to_ctl(Bsym_nabla_Basym,         array_c2i)
      call set_field_label_to_ctl(Basym_nabla_Bsym,         array_c2i)
!
      call set_field_label_to_ctl(sym_thermal_buoyancy,     array_c2i)
      call set_field_label_to_ctl(asym_thermal_buoyancy,    array_c2i)
      call set_field_label_to_ctl(sym_composite_buoyancy,   array_c2i)
      call set_field_label_to_ctl(asym_composite_buoyancy,  array_c2i)
!
      call set_field_label_to_ctl(usym_x_Bsym,              array_c2i)
      call set_field_label_to_ctl(uasym_x_Basym,            array_c2i)
      call set_field_label_to_ctl(usym_x_Basym,             array_c2i)
      call set_field_label_to_ctl(uasym_x_Bsym,             array_c2i)

      call set_field_label_to_ctl(rot_usym_x_Bsym,          array_c2i)
      call set_field_label_to_ctl(rot_uasym_x_Basym,        array_c2i)
      call set_field_label_to_ctl(rot_usym_x_Basym,         array_c2i)
      call set_field_label_to_ctl(rot_uasym_x_Bsym,         array_c2i)
!
      call set_field_label_to_ctl(Bsym_nabla_usym,          array_c2i)
      call set_field_label_to_ctl(Basym_nabla_uasym,        array_c2i)
      call set_field_label_to_ctl(Bsym_nabla_uasym,         array_c2i)
      call set_field_label_to_ctl(Basym_nabla_usym,         array_c2i)
!
      call set_field_label_to_ctl(usym_nabla_Tsym,          array_c2i)
      call set_field_label_to_ctl(uasym_nabla_Tasym,        array_c2i)
      call set_field_label_to_ctl(usym_nabla_Tasym,         array_c2i)
      call set_field_label_to_ctl(uasym_nabla_Tsym,         array_c2i)
!
      call set_field_label_to_ctl(usym_nabla_pTsym,         array_c2i)
      call set_field_label_to_ctl(uasym_nabla_pTasym,       array_c2i)
      call set_field_label_to_ctl(usym_nabla_pTasym,        array_c2i)
      call set_field_label_to_ctl(uasym_nabla_pTsym,        array_c2i)
!
      call set_field_label_to_ctl(usym_nabla_Csym,          array_c2i)
      call set_field_label_to_ctl(uasym_nabla_Casym,        array_c2i)
      call set_field_label_to_ctl(usym_nabla_Casym,         array_c2i)
!
      call set_field_label_to_ctl(usym_nabla_pCsym,         array_c2i)
      call set_field_label_to_ctl(uasym_nabla_pCasym,       array_c2i)
      call set_field_label_to_ctl(usym_nabla_pCasym,        array_c2i)
      call set_field_label_to_ctl(uasym_nabla_pCsym,        array_c2i)
!
      call set_field_label_to_ctl(heat_flux_sym_sym,        array_c2i)
      call set_field_label_to_ctl(heat_flux_asym_asym,      array_c2i)
      call set_field_label_to_ctl(heat_flux_sym_asym,       array_c2i)
      call set_field_label_to_ctl(heat_flux_asym_sym,       array_c2i)
!
      call set_field_label_to_ctl(pert_h_flux_sym_sym,      array_c2i)
      call set_field_label_to_ctl(pert_h_flux_asym_asym,    array_c2i)
      call set_field_label_to_ctl(pert_h_flux_sym_asym,     array_c2i)
      call set_field_label_to_ctl(pert_h_flux_asym_sym,     array_c2i)
!
      call set_field_label_to_ctl(composite_flux_sym_sym,   array_c2i)
      call set_field_label_to_ctl(composite_flux_asym_asym, array_c2i)
      call set_field_label_to_ctl(composite_flux_sym_asym,  array_c2i)
      call set_field_label_to_ctl(composite_flux_asym_sym,  array_c2i)
!
      call set_field_label_to_ctl(pert_c_flux_sym_sym,      array_c2i)
      call set_field_label_to_ctl(pert_c_flux_asym_asym,    array_c2i)
      call set_field_label_to_ctl(pert_c_flux_sym_asym,     array_c2i)
      call set_field_label_to_ctl(pert_c_flux_asym_sym,     array_c2i)
!
      call set_field_label_to_ctl(m_flux_sym_sym,           array_c2i)
      call set_field_label_to_ctl(m_flux_asym_asym,         array_c2i)
      call set_field_label_to_ctl(m_flux_sym_asym,          array_c2i)
!
      call set_field_label_to_ctl(maxwell_tensor_sym_sym,   array_c2i)
      call set_field_label_to_ctl(maxwell_tensor_asym_asym, array_c2i)
      call set_field_label_to_ctl(maxwell_tensor_sym_asym,  array_c2i)
!
      call set_field_label_to_ctl(usym_Bsym,                array_c2i)
      call set_field_label_to_ctl(uasym_Basym,              array_c2i)
      call set_field_label_to_ctl(usym_Basym,               array_c2i)
      call set_field_label_to_ctl(uasym_Bsym,               array_c2i)
!
      end subroutine set_force_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      end module m_force_w_sym_labels
