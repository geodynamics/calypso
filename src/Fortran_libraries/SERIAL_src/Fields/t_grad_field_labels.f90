!>@file  t_grad_field_labels.f90
!!       module t_grad_field_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!!      subroutine set_gradient_field_addresses                         &
!!     &         (i_phys, field_name, grad_fld, flag)
!!        type(gradient_field_address), intent(inout) :: grad_fld
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
        integer (kind=kint) :: i_grad_temp =       izero
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
      subroutine set_gradient_field_addresses                           &
     &         (i_phys, field_name, grad_fld, flag)
!
      use m_grad_field_labels
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
!
      end module t_grad_field_labels
