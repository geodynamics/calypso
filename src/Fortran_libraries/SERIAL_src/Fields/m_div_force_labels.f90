!>@file  m_div_force_labels.f90
!!       module m_div_force_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!> @brief Labels and addresses for divergence of forces by filtered field
!!
!!@verbatim
!!      logical function check_div_force(field_name)
!!      logical function check_div_flux_tensor(field_name)
!!      logical function check_div_scalar_flux(field_name)
!!
!!      subroutine set_div_force_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  divergence of forces by filtered field !!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   div_inertia                [div_forces%i_m_advect]
!!   div_Coriolis_force         [div_forces%i_Coriolis]
!!   div_Lorentz_force          [div_forces%i_lorentz]
!!   div_buoyancy               [div_forces%i_buoyancy]
!!   div_composite_buoyancy     [div_forces%i_comp_buo]
!!
!!   div_momentum_flux          [div_forces%i_m_flux]
!!   div_maxwell_tensor         [div_forces%i_maxwell]
!!   div_induction_tensor       [div_forces%i_induct_t]
!!
!!   div_heat_flux              [div_forces%i_h_flux]
!!   div_pert_heat_flux         [div_forces%i_ph_flux]
!!   div_composition_flux       [div_forces%i_c_flux]
!!   div_pert_composition_flux  [div_forces%i_pc_flux]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_div_force_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
!  divergence of momentum equations
!>        Field label for divergence of advection
!!         @f$ - \partial_{i} 
!!           \left(e_{ijk} \omega_{j} u_{k} \right) @f$
      type(field_def), parameter :: div_inertia                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_inertia',                             &
     &                math = '$ \partial_{i}'                           &
     &                // '(e_{ijk} \omega_{j} u_{k})$')
!>        Field label for divergence of Lorentz force
!!         @f$ \partial_{i}
!!            \left(e_{ijk} J_{j} B_{k} \right) @f$
      type(field_def), parameter :: div_Coriolis_force                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_Coriolis_force',                      &
     &                math = '$ -2 \partial_{i}'                        &
     &                // '(e_{ijk} \Omega_{j} u_{k})$')
!>        Field label for divergence of Lorentz force
!!         @f$ \partial_{i}
!!            \left(e_{ijk} J_{j} B_{k} \right) @f$
      type(field_def), parameter :: div_Lorentz_force                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_Lorentz_force',                       &
     &                math = '$ \partial_{i}'                           &
     &                    // '(e_{ijk} J_{j} B_{k})$')
!
!>        Field label for divergence of filtered buoyancy
!!         @f$ -\partial_{i} \alpha_{T} g_{i} T @f$
      type(field_def), parameter :: div_buoyancy                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_buoyancy',                            &
     &                math = '$-\partial_{i} \alpha_{T} T g_{i}$')
!>        Field label for divergence of filtered compositional buoyancy
!!         @f$ -\partial_{i} \alpha_{C} g_{i} C @f$
      type(field_def), parameter :: div_composite_buoyancy              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_composite_buoyancy',                  &
     &                math = '$-\partial_{i} \alpha_{C} C g_{i}$')
!
!>        Field label for divergence of momentum flux
!!         @f$ \partial_{j} (u_{i} u_{j}) @f$
      type(field_def), parameter :: div_momentum_flux                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'div_momentum_flux',                       &
     &                math = '$-\partial_{j} (u_{i} u_{j})$')
!>        Field label for divergence of Maxwell stress
!!         @f$ \partial_{j} (B_{i} B_{j}) @f$
      type(field_def), parameter :: div_maxwell_tensor                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'div_maxwell_tensor',                      &
     &                math = '$ \partial_{j} (B_{i} B_{j})$')
!>        Field label for divergence of magnetic induction
!!         @f$ \partial_{i} (u_{i} B_{j} - B_{i} u]_{J}) @f$
      type(field_def), parameter :: div_induction_tensor                &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'div_induction_tensor',                    &
     &                math = '$ \partial_{i} (u_{i} B_{j}'              &
     &                  //' - B_{i} u_{J})$')
!
!>        Field label for divergence of heat flux
!!         @f$ \partial_{i} (u_{i} T) @f$
      type(field_def), parameter :: div_heat_flux                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_heat_flux',                           &
     &                math = '$-\partial_{i} u_{i} T $')
!>        Field label for divergence of perturbation of heat flux
!!         @f$ \partial_{i} (u_{i} \Theta) @f$
      type(field_def), parameter :: div_pert_heat_flux                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_pert_heat_flux',                      &
     &                math = '$-\partial_{i} u_{i} \Therta $')
!
!>        Field label for divergence of composition flux
!!         @f$ -\partial_{i} u_{i} C @f$
      type(field_def), parameter :: div_composition_flux                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_composition_flux',                    &
     &                math = '$-\partial_{i} u_{i} C $')
!>        Field label for divergence of perturbation of compopstion flux
!!         @f$ -\partial_{i} u_{i} \Theta_{C} @f$
      type(field_def), parameter :: div_pert_composition_flux           &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_pert_composition_flux',               &
     &                math = '$-\partial_{i} u_{i} \Theta_{C} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_div_force(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_force                                                   &
     &   =    (field_name .eq. div_inertia%name)                        &
     &   .or. (field_name .eq. div_Coriolis_force%name)                 &
     &   .or. (field_name .eq. div_Lorentz_force%name)                  &
     &   .or. (field_name .eq. div_buoyancy%name)                       &
     &   .or. (field_name .eq. div_composite_buoyancy%name)
!
      end function check_div_force
!
! ----------------------------------------------------------------------
!
      logical function check_div_flux_tensor(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_flux_tensor                                             &
     &   =    (field_name .eq. div_momentum_flux%name)                  &
     &   .or. (field_name .eq. div_maxwell_tensor%name)                 &
     &   .or. (field_name .eq. div_induction_tensor%name)
!
      end function check_div_flux_tensor
!
! ----------------------------------------------------------------------
!
      logical function check_div_scalar_flux(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_scalar_flux                                             &
     &   =    (field_name .eq. div_heat_flux%name)                      &
     &   .or. (field_name .eq. div_pert_heat_flux%name)                 &
     &   .or. (field_name .eq. div_composition_flux%name)               &
     &   .or. (field_name .eq. div_pert_composition_flux%name)
!
      end function check_div_scalar_flux
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_div_force_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(div_inertia,               array_c2i)
      call set_field_label_to_ctl(div_Coriolis_force,        array_c2i)
      call set_field_label_to_ctl(div_Lorentz_force,         array_c2i)
      call set_field_label_to_ctl(div_buoyancy,              array_c2i)
      call set_field_label_to_ctl(div_composite_buoyancy,    array_c2i)
      call set_field_label_to_ctl(div_momentum_flux,         array_c2i)
      call set_field_label_to_ctl(div_maxwell_tensor,        array_c2i)
      call set_field_label_to_ctl(div_induction_tensor,      array_c2i)
      call set_field_label_to_ctl(div_heat_flux,             array_c2i)
      call set_field_label_to_ctl(div_pert_heat_flux,        array_c2i)
      call set_field_label_to_ctl(div_composition_flux,      array_c2i)
      call set_field_label_to_ctl(div_pert_composition_flux, array_c2i)
!
      end subroutine set_div_force_names
!
! ----------------------------------------------------------------------
!
      end module m_div_force_labels
