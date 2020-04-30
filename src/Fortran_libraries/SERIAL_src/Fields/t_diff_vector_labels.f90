!>@file   t_diff_vector_labels.f90
!!       module t_diff_vector_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of difference of vectors
!!
!!@verbatim
!!      logical function check_difference_vectors(field_name)
!!      subroutine set_diff_vector_addresses                            &
!!     &         (i_phys, field_name, diff_vector, flag)
!!        type(diff_vector_address), intent(inout) :: diff_vector
!!
!!      integer(kind = kint) function num_difference_vector()
!!      subroutine set_differnce_vector_labels(n_comps, names, maths)
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
      integer(kind = kint), parameter, private :: ngrad_vector = 15
!
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
!
      subroutine set_diff_vector_addresses                              &
     &         (i_phys, field_name, diff_vector, flag)
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
! ----------------------------------------------------------------------
! 
      integer(kind = kint) function num_difference_vector()
      num_difference_vector = ngrad_vector
      return
      end function num_difference_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_differnce_vector_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(ngrad_vector)
      character(len = kchara), intent(inout) :: names(ngrad_vector)
      character(len = kchara), intent(inout) :: maths(ngrad_vector)
!
!
      call set_field_labels(grad_v_1,                                   &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(grad_v_2,                                   &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(grad_v_3,                                   &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(grad_w_1,                                   &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(grad_w_2,                                   &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(grad_w_3,                                   &
     &    n_comps( 6), names( 6), maths( 6))
!
      call set_field_labels(grad_b_1,                                   &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(grad_b_2,                                   &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(grad_b_3,                                   &
     &    n_comps( 9), names( 9), maths( 9))
!
      call set_field_labels(grad_a_1,                                   &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(grad_a_2,                                   &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(grad_a_3,                                   &
     &    n_comps(12), names(12), maths(12))
!
      call set_field_labels(grad_j_1,                                   &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(grad_j_2,                                   &
     &    n_comps(14), names(14), maths(14))
      call set_field_labels(grad_j_3,                                   &
     &    n_comps(15), names(15), maths(15))
!
      end subroutine set_differnce_vector_labels
!
! ----------------------------------------------------------------------
!
      end module t_diff_vector_labels
