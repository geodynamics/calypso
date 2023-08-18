!>@file   m_diff_vector_labels.f90
!!       module m_diff_vector_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of difference of vectors
!!
!!@verbatim
!!      logical function check_difference_vectors(field_name)
!!
!!      subroutine set_differnce_vector_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
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
      module m_diff_vector_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit none
!
!  difference of field
!>        Field label for gradient of velocity
!!             @f$ \partial_{i} u_{x} @f$
      type(field_def), parameter :: grad_v_1                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_v_1',                                &
     &                math = '$ \partial_{i} u_{1} $')
!>        Field label for gradient of velocity
!!             @f$ \partial_{i} u_{y} @f$
      type(field_def), parameter :: grad_v_2                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_v_2',                                &
     &                math = '$ \partial_{i} u_{2} $')
!>        Field label for gradient of velocity
!!             @f$ \partial_{i} u_{z} @f$
      type(field_def), parameter :: grad_v_3                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_v_3',                                &
     &                math = '$ \partial_{i} u_{3} $')
!
!>        Field label for gradient of vorticity
!!             @f$ \partial_{i} \omega_{x} @f$
      type(field_def), parameter :: grad_w_1                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_w_1',                                &
     &                math = '$ \partial_{i} \omega_{1} $')
!>        Field label for gradient of vorticity
!!             @f$ \partial_{i} \omega_{y} @f$
      type(field_def), parameter :: grad_w_2                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_w_2',                                &
     &                math = '$ \partial_{i} \omega_{2} $')
!>        Field label for gradient of vorticity
!!             @f$ \partial_{i} \omega_{z} @f$
      type(field_def), parameter :: grad_w_3                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_w_3',                                &
     &                math = '$ \partial_{i} \omega_{3} $')
!
!>        Field label for gradient of vector potential
!!             @f$ \partial_{i} A_{x} @f$
      type(field_def), parameter :: grad_a_1                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_a_1',                                &
     &                math = '$ \partial_{i} A_{1} $')
!>        Field label for gradient of vector potential
!!             @f$ \partial_{i} A_{y} @f$
      type(field_def), parameter :: grad_a_2                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_a_2',                                &
     &                math = '$ \partial_{i} A_{2} $')
!>        Field label for gradient of vector potential
!!             @f$ \partial_{i} A_{z} @f$
      type(field_def), parameter :: grad_a_3                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_a_3',                                &
     &                math = '$ \partial_{i} A_{3} $')
!
!>        Field label for gradient of magnetic field
!!             @f$ \partial_{i} B_{x} @f$
      type(field_def), parameter :: grad_b_1                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_b_1',                                &
     &                math = '$ \partial_{i} B_{1} $')
!>        Field label for gradient of magnetic field
!!             @f$ \partial_{i} B_{y} @f$
      type(field_def), parameter :: grad_b_2                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_b_2',                                &
     &                math = '$ \partial_{i} B_{2} $')
!>        Field label for gradient of magnetic field
!!             @f$ \partial_{i} B_{z} @f$
      type(field_def), parameter :: grad_b_3                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_b_3',                                &
     &                math = '$ \partial_{i} B_{3} $')
!
!>        Field label for gradient of current density
!!             @f$ \partial_{i} J_{x} @f$
      type(field_def), parameter :: grad_j_1                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_j_1',                                &
     &                math = '$ \partial_{i} J_{1} $')
!>        Field label for gradient of current density
!!             @f$ \partial_{i} J_{y} @f$
      type(field_def), parameter :: grad_j_2                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_j_2',                                &
     &                math = '$ \partial_{i} J_{2} $')
!>        Field label for gradient of current density
!!             @f$ \partial_{i} J_{z} @f$
      type(field_def), parameter :: grad_j_3                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_j_3',                                &
     &                math = '$ \partial_{i} J_{3} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_difference_vectors(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_difference_vectors                                          &
     &   =    (field_name .eq. grad_v_1%name)                           &
     &   .or. (field_name .eq. grad_v_2%name)                           &
     &   .or. (field_name .eq. grad_v_3%name)                           &
!
     &   .or. (field_name .eq. grad_w_1%name)                           &
     &   .or. (field_name .eq. grad_w_2%name)                           &
     &   .or. (field_name .eq. grad_w_3%name)                           &
!
     &   .or. (field_name .eq. grad_a_1%name)                           &
     &   .or. (field_name .eq. grad_a_2%name)                           &
     &   .or. (field_name .eq. grad_a_3%name)                           &
!
     &   .or. (field_name .eq. grad_b_1%name)                           &
     &   .or. (field_name .eq. grad_b_2%name)                           &
     &   .or. (field_name .eq. grad_b_3%name)                           &
!
     &   .or. (field_name .eq. grad_j_1%name)                           &
     &   .or. (field_name .eq. grad_j_2%name)                           &
     &   .or. (field_name .eq. grad_j_3%name)
!
      end function check_difference_vectors
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_differnce_vector_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(grad_v_1,  array_c2i)
      call set_field_label_to_ctl(grad_v_2,  array_c2i)
      call set_field_label_to_ctl(grad_v_3,  array_c2i)
      call set_field_label_to_ctl(grad_w_1,  array_c2i)
      call set_field_label_to_ctl(grad_w_2,  array_c2i)
      call set_field_label_to_ctl(grad_w_3,  array_c2i)
      call set_field_label_to_ctl(grad_b_1,  array_c2i)
      call set_field_label_to_ctl(grad_b_2,  array_c2i)
      call set_field_label_to_ctl(grad_b_3,  array_c2i)
      call set_field_label_to_ctl(grad_a_1,  array_c2i)
      call set_field_label_to_ctl(grad_a_2,  array_c2i)
      call set_field_label_to_ctl(grad_a_3,  array_c2i)
      call set_field_label_to_ctl(grad_j_1,  array_c2i)
      call set_field_label_to_ctl(grad_j_2,  array_c2i)
      call set_field_label_to_ctl(grad_j_3,  array_c2i)
!
      end subroutine set_differnce_vector_names
!
! ----------------------------------------------------------------------
!
      end module m_diff_vector_labels
