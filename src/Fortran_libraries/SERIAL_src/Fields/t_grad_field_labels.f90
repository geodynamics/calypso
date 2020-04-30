!>@file  t_grad_field_labels.f90
!!       module t_grad_field_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!!      logical function check_divergence_field(field_name)
!!      logical function check_gradient_field(field_name)
!!      subroutine set_gradient_field_addresses                         &
!!     &         (i_phys, field_name, grad_fld, flag)
!!        type(gradient_field_address), intent(inout) :: grad_fld
!!
!!      integer(kind = kint) function num_divergence_fields()
!!      integer(kind = kint) function num_gradient_fields()
!!      subroutine set_divergence_field_labels(n_comps, names, maths)
!!      subroutine set_gradient_field_labels(n_comps, names, maths)
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
!!  grad_reference_temp         [i_grad_ref_t]:
!!                     gradient of reference temperature
!!
!!  grad_composition            [i_grad_composit]:
!!                     gradient of composition
!!  grad_pert_composition       [i_grad_per_c]:
!!                     gradient of perturbation of composition
!!  grad_reference_composition  [i_grad_ref_c]:
!!                     gradient of reference composition
!!
!!  grad_density            [i_grad_density]:  gradient of density
!!  grad_pert_density       [i_grad_per_density]:
!!                     gradient of perturbation of density
!!  grad_reference_density  [i_grad_ref_density]:
!!                     gradient of reference density
!!
!!  grad_entropy            [i_grad_entropy]:  gradient of entropy
!!  grad_pert_entropy       [i_grad_per_entropy]:
!!                     gradient of perturbation of entropy
!!  grad_reference_entropy  [i_grad_ref_entropy]:
!!                     gradient of reference entropy
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_grad_field_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit none
!
      integer(kind = kint), parameter, private :: ndiv_vector =   3
      integer(kind = kint), parameter, private :: ngrad_scalar = 12
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
!>        Field label for gradient of reference temperature
!!         @f$  \partial_{i} T_{0} / dz@f$
      type(field_def), parameter :: grad_reference_temp                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_reference_temp',                     &
     &                math = '$ \partial_{i} T_{0} $')
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
!>        Field label for gradient of reference composition
!!         @f$  \partial_{i} C_{0} / dz@f$
      type(field_def), parameter :: grad_reference_composition          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_reference_composition',              &
     &                math = '$ \partial_{i} C_{0} $')
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
!>        Field label for gradient of reference density
!!         @f$  \partial_{i} \rho_{0} / dz@f$
      type(field_def), parameter :: grad_reference_density              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_reference_density',                  &
     &                math = '$ \partial_{i} \rho_{0} $')
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
!>        Field label for gradient of reference density
!!         @f$  \partial_{i} S_{0} / dz@f$
      type(field_def), parameter :: grad_reference_entropy              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_reference_entropy',                  &
     &                math = '$ \partial_{i} S_{0} $')
!
!
      type gradient_field_address
!>        start address for velocity
!!         @f$ \partial_{i} u_{i} @f$
        integer (kind=kint) :: i_div_v =           izero
!>        start address for magnetic field
!!         @f$ \partial_{i} B_{i} @f$
        integer (kind=kint) :: i_div_b =           izero
!>        start address for magnetic vector potential
!!         @f$ \partial_{i} A_{i} @f$
        integer (kind=kint) :: i_div_a =           izero
!
!>        Field address for gradient of @f$ T @f$
!!         @f$  \partial_{i} T / dz@f$
        integer (kind=kint) :: i_grad_temp =           izero
!>        Field address for gradient of @f$ \Theta @f$
!!         @f$  \partial_{i} \Theta / dz@f$
        integer (kind=kint) :: i_grad_per_t =      izero
!>        Field address for gradient of reference temperature
!!         @f$  \partial_{i} T_{0} / dz@f$
        integer (kind=kint) :: i_grad_ref_t =      izero
!
!>        Field address for gradient of @f$ C @f$
!!         @f$  \partial_{i} C / dz@f$
        integer (kind=kint) :: i_grad_composit =    izero
!>        Field address for gradient of perturbation of composition
!!         @f$  \partial_{i} \Theta_{C} / dz@f$
        integer (kind=kint) :: i_grad_per_c =       izero
!>        Field address for gradient of reference composition
!!         @f$  \partial_{i} C_{0} / dz@f$
        integer (kind=kint) :: i_grad_ref_c =       izero
!
!>        Field address for gradient of @f$ \rho @f$
!!         @f$  \partial_{i} \rho / dz@f$
        integer (kind=kint) :: i_grad_density =    izero
!>        Field address for gradient of perturbation of density
!!         @f$  \partial_{i} \Theta_{\rho} / dz@f$
        integer (kind=kint) :: i_grad_per_density =    izero
!>        Field address for gradient of reference density
!!         @f$  \partial_{i} \rho_{0} / dz@f$
        integer (kind=kint) :: i_grad_ref_density =    izero
!
!>        Field address for gradient of @f$ S @f$
!!         @f$  \partial_{i} S / dz@f$
        integer (kind=kint) :: i_grad_entropy =     izero
!>        Field address for gradient of perturbation of entropy
!!         @f$  \partial_{i} \Theta_{S} / dz@f$
        integer (kind=kint) :: i_grad_per_entropy = izero
!>        Field address for gradient of reference density
!!         @f$  \partial_{i} S_{0} / dz@f$
        integer (kind=kint) :: i_grad_ref_entropy = izero
      end type gradient_field_address
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
     &   .or. (field_name .eq. grad_reference_temp%name)                &
!
     &   .or. (field_name .eq. grad_composition%name)                   &
     &   .or. (field_name .eq. grad_pert_composition%name)              &
     &   .or. (field_name .eq. grad_reference_composition%name)         &
!
     &   .or. (field_name .eq. grad_density%name)                       &
     &   .or. (field_name .eq. grad_pert_density%name)                  &
     &   .or. (field_name .eq. grad_reference_density%name)             &
!
     &   .or. (field_name .eq. grad_entropy%name)                       &
     &   .or. (field_name .eq. grad_pert_entropy%name)                  &
     &   .or. (field_name .eq. grad_reference_entropy%name)
!
      end function check_gradient_field
!
! ----------------------------------------------------------------------
!
      subroutine set_gradient_field_addresses                           &
     &         (i_phys, field_name, grad_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(gradient_field_address), intent(inout) :: grad_fld
      logical, intent(inout) :: flag
!
!
      flag = check_divergence_field(field_name)                         &
     &      .or. check_gradient_field(field_name)
      if(flag) then
        if     (field_name .eq. div_velocity%name) then
          grad_fld%i_div_v =    i_phys
        else if(field_name .eq. div_magnetic%name) then
          grad_fld%i_div_b =    i_phys
        else if(field_name .eq. div_vector_potential%name) then
          grad_fld%i_div_a =    i_phys
!
        else if(field_name .eq. grad_temp%name) then
          grad_fld%i_grad_temp =  i_phys
        else if(field_name .eq. grad_pert_temp%name) then
          grad_fld%i_grad_per_t = i_phys
        else if(field_name .eq. grad_reference_temp%name) then
          grad_fld%i_grad_ref_t = i_phys
!
        else if(field_name .eq. grad_composition%name) then
          grad_fld%i_grad_composit = i_phys
        else if (field_name .eq. grad_pert_composition%name) then
          grad_fld%i_grad_per_c =    i_phys
        else if (field_name .eq. grad_reference_composition%name) then
          grad_fld%i_grad_ref_c =    i_phys
!
        else if (field_name .eq. grad_density%name) then
          grad_fld%i_grad_density =     i_phys
        else if (field_name .eq. grad_pert_density%name) then
          grad_fld%i_grad_per_density = i_phys
        else if (field_name .eq. grad_reference_density%name) then
          grad_fld%i_grad_ref_density = i_phys
!
        else if (field_name .eq. grad_entropy%name) then
          grad_fld%i_grad_entropy =     i_phys
        else if (field_name .eq. grad_pert_entropy%name) then
          grad_fld%i_grad_per_entropy = i_phys
        else if (field_name .eq. grad_reference_entropy%name) then
          grad_fld%i_grad_ref_entropy = i_phys
        end if
      end if
!
      end subroutine set_gradient_field_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! 
      integer(kind = kint) function num_divergence_fields()
      num_divergence_fields = ndiv_vector
      return
      end function num_divergence_fields
!
! ----------------------------------------------------------------------
! 
      integer(kind = kint) function num_gradient_fields()
      num_gradient_fields = ngrad_scalar
      return
      end function num_gradient_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_divergence_field_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(ndiv_vector)
      character(len = kchara), intent(inout) :: names(ndiv_vector)
      character(len = kchara), intent(inout) :: maths(ndiv_vector)
!
!
      call set_field_labels(div_velocity,                               &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(div_magnetic,                               &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(div_vector_potential,                       &
     &    n_comps( 3), names( 3), maths( 3))
!
      end subroutine set_divergence_field_labels
!
! ----------------------------------------------------------------------
!
      subroutine set_gradient_field_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(ngrad_scalar)
      character(len = kchara), intent(inout) :: names(ngrad_scalar)
      character(len = kchara), intent(inout) :: maths(ngrad_scalar)
!
!
      call set_field_labels(grad_temp,                                  &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(grad_pert_temp,                             &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(grad_reference_temp,                        &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(grad_composition,                           &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(grad_pert_composition,                      &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(grad_reference_composition,                 &
     &    n_comps( 6), names( 6), maths( 6))
!
      call set_field_labels(grad_density,                               &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(grad_pert_density,                          &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(grad_reference_density,                     &
     &    n_comps( 9), names( 9), maths( 9))
!
      call set_field_labels(grad_entropy,                               &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(grad_pert_entropy,                          &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(grad_reference_entropy,                     &
     &    n_comps(12), names(12), maths(12))
!
      end subroutine set_gradient_field_labels
!
! ----------------------------------------------------------------------
!
      end module t_grad_field_labels
