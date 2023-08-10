!>@file  m_grad_field_labels.f90
!!       module m_grad_field_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!!      logical function check_divergence_field(field_name)
!!      logical function check_gradient_field(field_name)
!!
!!      subroutine set_divergence_field_names(array_c2i)
!!      subroutine set_gradient_field_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!! !!!!!  physical values!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!  div_velocity             [i_div_v]:  divergence of velocity
!!  div_magnetic             [i_div_b]:  divergence of magnetic field
!!  div_vector_potential     [i_div_a]:  divergence of vector potential
!!
!!  grad_temp                   [i_grad_temp]:  gradient of temperature
!!  grad_pert_temp              [i_grad_per_t]:
!!                     gradient of perturbation of temperature
!!
!!  grad_composition            [i_grad_composit]:
!!                     gradient of composition
!!  grad_pert_composition       [i_grad_per_c]:
!!                     gradient of perturbation of composition
!!
!!  grad_density            [i_grad_density]:  gradient of density
!!  grad_pert_density       [i_grad_per_density]:
!!                     gradient of perturbation of density
!!
!!  grad_entropy            [i_grad_entropy]:  gradient of entropy
!!  grad_pert_entropy       [i_grad_per_entropy]:
!!                     gradient of perturbation of entropy
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_grad_field_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit none
!
!>        Divergence of velocity
!!         @f$ \partial_{i} u_{i} @f$
      type(field_def), parameter :: div_velocity                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_velocity',                            &
     &                math = '$ \partial_{i} u_{i} $')
!>        Divergence of magnetic field
!!         @f$ \partial_{igrad} B_{i} @f$
      type(field_def), parameter :: div_magnetic                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_magnetic',                            &
     &                math = '$ \partial_{i} B_{i} $')
!>        Divergence of magnetic vector potential
!!         @f$ \partial_{i} A_{i} @f$
      type(field_def), parameter :: div_vector_potential                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_vector_potential',                    &
     &                math = '$ \partial_{i} A_{i} $')
!
!>        Field label for gradient of @f$ T @f$
!!         @f$  \partial_{i} T / dz@f$
      type(field_def), parameter :: grad_temp                           &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_temp',                               &
     &                math = '$ \partial_{i} T $')
!>        Field label for gradient of @f$ \Theta @f$
!!         @f$  \partial_{i} \Theta / dz@f$
      type(field_def), parameter :: grad_pert_temp                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_pert_temp',                          &
     &                math = '$ \partial_{i} \Theta $')
!
!>        Field label for gradient of @f$ C @f$
!!         @f$  \partial_{i} C / dz@f$
      type(field_def), parameter :: grad_composition                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_composition',                        &
     &                math = '$ \partial_{i} C $')
!>        Field label for gradient of perturbation of composition
!!         @f$  \partial_{i} \Theta_{C} / dz@f$
      type(field_def), parameter :: grad_pert_composition               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_pert_composition',                   &
     &                math = '$ \partial_{i} \Theta_{C} $')
!
!>        Field label for gradient of density
!!         @f$  \partial_{i} \rho / dz@f$
      type(field_def), parameter :: grad_density                        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_density',                            &
     &                math = '$ \partial_{i} \rho $')
!>        Field label for gradient of perturbation of density
!!         @f$  \partial_{i} \Theta_{\rho} / dz@f$
      type(field_def), parameter :: grad_pert_density                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_pert_density',                       &
     &                math = '$ \partial_{i} \Theta_{\rho} $')
!
!>        Field label for gradient of entropy
!!         @f$  \partial_{i} S / dz@f$
      type(field_def), parameter :: grad_entropy                        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_entropy',                            &
     &                math = '$ \partial_{i} S $')
!>        Field label for gradient of perturbation of entropy
!!         @f$  \partial_{i} \Theta_{S} / dz@f$
      type(field_def), parameter :: grad_pert_entropy                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_pert_entropy',                       &
     &                math = '$ \partial_{i} \Theta_{S} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_divergence_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_divergence_field                                            &
     &   =    (field_name .eq. div_velocity%name)                       &
     &   .or. (field_name .eq. div_magnetic%name)                       &
     &   .or. (field_name .eq. div_vector_potential%name)
!
      end function check_divergence_field
!
! ----------------------------------------------------------------------
!
      logical function check_gradient_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_gradient_field                                              &
     &   =    (field_name .eq. grad_temp%name)                          &
     &   .or. (field_name .eq. grad_pert_temp%name)                     &
!
     &   .or. (field_name .eq. grad_composition%name)                   &
     &   .or. (field_name .eq. grad_pert_composition%name)              &
!
     &   .or. (field_name .eq. grad_density%name)                       &
     &   .or. (field_name .eq. grad_pert_density%name)                  &
!
     &   .or. (field_name .eq. grad_entropy%name)                       &
     &   .or. (field_name .eq. grad_pert_entropy%name)
!
      end function check_gradient_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_divergence_field_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(div_velocity,         array_c2i)
      call set_field_label_to_ctl(div_magnetic,         array_c2i)
      call set_field_label_to_ctl(div_vector_potential, array_c2i)
!
      end subroutine set_divergence_field_names
!
! ----------------------------------------------------------------------
!
      subroutine set_gradient_field_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(grad_temp,             array_c2i)
      call set_field_label_to_ctl(grad_pert_temp,        array_c2i)
      call set_field_label_to_ctl(grad_composition,      array_c2i)
      call set_field_label_to_ctl(grad_pert_composition, array_c2i)
      call set_field_label_to_ctl(grad_density,          array_c2i)
      call set_field_label_to_ctl(grad_pert_density,     array_c2i)
      call set_field_label_to_ctl(grad_entropy,          array_c2i)
      call set_field_label_to_ctl(grad_pert_entropy,     array_c2i)
!
      end subroutine set_gradient_field_names
!
! ----------------------------------------------------------------------
!
      end module m_grad_field_labels
