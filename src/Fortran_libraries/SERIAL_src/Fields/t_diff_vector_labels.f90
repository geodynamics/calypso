!>@file   t_diff_vector_labels.f90
!!       module t_diff_vector_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of difference of vectors
!!
!!@verbatim
!!      subroutine set_diff_vector_addresses                            &
!!     &         (i_phys, field_name, diff_vector, flag)
!!        type(diff_vector_address), intent(inout) :: diff_vector
!!
!! !!!!!  diffrence of vector fields !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names [Address]
!!
!!   grad_v_1, grad_v_2, grad_v_3  [i_grad_vx] [i_grad_vy], [i_grad_vz]:
!!            difference of velocity
!!   grad_w_1, grad_w_2, grad_w_3  [i_grad_wx] [i_grad_wy], [i_grad_wz]:
!!            difference of vorticity
!!   grad_b_1, grad_b_2, grad_b_3  [i_grad_bx] [i_grad_by], [i_grad_bz]:
!!            difference of magnetic field
!!   grad_a_1, grad_a_2, grad_a_3  [i_grad_ax] [i_grad_ay], [i_grad_az]:
!!            difference of vector potential
!!   grad_j_1, grad_j_2, grad_j_3  [i_grad_jx] [i_grad_jy], [i_grad_jz]:
!!            difference of current density
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_diff_vector_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit none
!
!
      type diff_vector_address
!>        Field address for gradient of velocity
!!             @f$ \partial_{i} u_{x} @f$
        integer (kind=kint) :: i_grad_vx = izero
!>        Field address for gradient of velocity
!!             @f$ \partial_{i} u_{y} @f$
        integer (kind=kint) :: i_grad_vy = izero
!>        Field address for gradient of velocity
!!             @f$ \partial_{i} u_{z} @f$
        integer (kind=kint) :: i_grad_vz = izero
!
!>        Field address for gradient of vorticity
!!             @f$ \partial_{i} \omega_{x} @f$
        integer (kind=kint) :: i_grad_wx = izero
!>        Field address for gradient of vorticity
!!             @f$ \partial_{i} \omega_{y} @f$
        integer (kind=kint) :: i_grad_wy = izero
!>        Field address for gradient of vorticity
!!             @f$ \partial_{i} \omega_{z} @f$
        integer (kind=kint) :: i_grad_wz = izero
!
!>        Field address for gradient of vector potential
!!             @f$ \partial_{i} A_{x} @f$
        integer (kind=kint) :: i_grad_ax = izero
!>        Field address for gradient of vector potential
!!             @f$ \partial_{i} A_{y} @f$
        integer (kind=kint) :: i_grad_ay = izero
!>        Field address for gradient of vector potential
!!             @f$ \partial_{i} A_{z} @f$
        integer (kind=kint) :: i_grad_az = izero
!
!>        Field address for gradient of magnetic field
!!             @f$ \partial_{i} B_{x} @f$
        integer (kind=kint) :: i_grad_bx = izero
!>        Field address for gradient of magnetic field
!!             @f$ \partial_{i} B_{y} @f$
        integer (kind=kint) :: i_grad_by = izero
!>        Field address for gradient of magnetic field
!!             @f$ \partial_{i} B_{z} @f$
        integer (kind=kint) :: i_grad_bz = izero
!
!>        Field address for gradient of current density
!!             @f$ \partial_{i} J_{x} @f$
        integer (kind=kint) :: i_grad_jx = izero
!>        Field address for gradient of current density
!!             @f$ \partial_{i} J_{y} @f$
        integer (kind=kint) :: i_grad_jy = izero
!>        Field address for gradient of current density
!!             @f$ \partial_{i} J_{z} @f$
        integer (kind=kint) :: i_grad_jz = izero
      end type diff_vector_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_diff_vector_addresses                              &
     &         (i_phys, field_name, diff_vector, flag)
!
      use m_diff_vector_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(diff_vector_address), intent(inout) :: diff_vector
      logical, intent(inout) :: flag
!
!
      flag = check_difference_vectors(field_name)
      if(flag) then
        if     (field_name .eq. grad_v_1%name) then
          diff_vector%i_grad_vx =   i_phys
        else if(field_name .eq. grad_v_2%name) then
          diff_vector%i_grad_vy =   i_phys
        else if(field_name .eq. grad_v_3%name) then
          diff_vector%i_grad_vz =   i_phys
!
        else if(field_name .eq. grad_w_1%name) then
          diff_vector%i_grad_wx =   i_phys
        else if(field_name .eq. grad_w_2%name) then
          diff_vector%i_grad_wy =   i_phys
        else if(field_name .eq. grad_w_3%name) then
          diff_vector%i_grad_wz =   i_phys
!
        else if(field_name .eq. grad_b_1%name) then
          diff_vector%i_grad_bx =   i_phys
        else if(field_name .eq. grad_b_2%name) then
          diff_vector%i_grad_by =   i_phys
        else if(field_name .eq. grad_b_3%name) then
          diff_vector%i_grad_bz =   i_phys
!
        else if (field_name .eq. grad_a_1%name) then
          diff_vector%i_grad_ax =   i_phys
        else if (field_name .eq. grad_a_2%name) then
          diff_vector%i_grad_ay =   i_phys
        else if (field_name .eq. grad_a_3%name) then
          diff_vector%i_grad_az =   i_phys
!
        else if (field_name .eq. grad_j_1%name) then
          diff_vector%i_grad_jx =   i_phys
        else if (field_name .eq. grad_j_2%name) then
          diff_vector%i_grad_jy =   i_phys
        else if (field_name .eq. grad_j_3%name) then
          diff_vector%i_grad_jz =   i_phys
        end if
      end if
!
      end subroutine set_diff_vector_addresses
!
! ----------------------------------------------------------------------
!
      end module t_diff_vector_labels
