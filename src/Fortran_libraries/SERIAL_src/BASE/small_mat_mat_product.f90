!>@file   small_mat_mat_product.f90
!!        module small_mat_mat_product
!!
!!@author H. Matsui
!!@date Programmed in May, 2013
!!
!>@brief  Take matrix-matrix product for small matrices
!!
!!@verbatim
!!      subroutine mat_2x2_product(a_left, a_right, a_prod)
!!      subroutine mat_3x3_product(a_left, a_right, a_prod)
!!      subroutine mat_4x4_product(a_left, a_right, a_prod)
!!      subroutine mat_5x5_product(a_left, a_right, a_prod)
!!
!!      subroutine prod_mat33_vec3(A, V, prod)
!!      subroutine prod_mat44_vec3(A, V, prod)
!!@endverbatim
!!
!!@n @param a_left(n,n)       input matrix on left  (2x2 to 5x5)
!!@n @param a_right(n,n)      input matrix on right (2x2 to 5x5)
!!@n @param a_prod(n,n)       produced matrix       (2x2 to 5x5)
!
      module small_mat_mat_product
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mat_2x2_product(a_left, a_right, a_prod)
!
      real(kind= kreal), intent(in) ::    a_left(2,2)
      real(kind= kreal), intent(in) ::    a_right(2,2)
      real(kind= kreal), intent(inout) :: a_prod(2,2)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 2
        a_prod(1,nd) =  a_left(1,1) * a_right(1,nd)                     &
                      + a_left(1,2) * a_right(2,nd)
        a_prod(2,nd) =  a_left(2,1) * a_right(1,nd)                     &
                      + a_left(2,2) * a_right(2,nd)
      end do
!
      end subroutine mat_2x2_product
!
! -----------------------------------------------------------------------
!
      subroutine mat_3x3_product(a_left, a_right, a_prod)
!
      real(kind= kreal), intent(in) ::    a_left(3,3)
      real(kind= kreal), intent(in) ::    a_right(3,3)
      real(kind= kreal), intent(inout) :: a_prod(3,3)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        a_prod(1,nd) =  a_left(1,1) * a_right(1,nd)                     &
                      + a_left(1,2) * a_right(2,nd)                     &
                      + a_left(1,3) * a_right(3,nd)
        a_prod(2,nd) =  a_left(2,1) * a_right(1,nd)                     &
                      + a_left(2,2) * a_right(2,nd)                     &
                      + a_left(2,3) * a_right(3,nd)
        a_prod(3,nd) =  a_left(3,1) * a_right(1,nd)                     &
                      + a_left(3,2) * a_right(2,nd)                     &
                      + a_left(3,3) * a_right(3,nd)
      end do
!
      end subroutine mat_3x3_product
!
! -----------------------------------------------------------------------
!
      subroutine mat_4x4_product(a_left, a_right, a_prod)
!
      real(kind= kreal), intent(in) ::    a_left(4,4)
      real(kind= kreal), intent(in) ::    a_right(4,4)
      real(kind= kreal), intent(inout) :: a_prod(4,4)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 4
        a_prod(1,nd) =  a_left(1,1) * a_right(1,nd)                     &
                      + a_left(1,2) * a_right(2,nd)                     &
                      + a_left(1,3) * a_right(3,nd)                     &
                      + a_left(1,4) * a_right(4,nd)
        a_prod(2,nd) =  a_left(2,1) * a_right(1,nd)                     &
                      + a_left(2,2) * a_right(2,nd)                     &
                      + a_left(2,3) * a_right(3,nd)                     &
                      + a_left(2,4) * a_right(4,nd)
        a_prod(3,nd) =  a_left(3,1) * a_right(1,nd)                     &
                      + a_left(3,2) * a_right(2,nd)                     &
                      + a_left(3,3) * a_right(3,nd)                     &
                      + a_left(3,4) * a_right(4,nd)
        a_prod(4,nd) =  a_left(4,1) * a_right(1,nd)                     &
                      + a_left(4,2) * a_right(2,nd)                     &
                      + a_left(4,3) * a_right(3,nd)                     &
                      + a_left(4,4) * a_right(4,nd)
      end do
!
      end subroutine mat_4x4_product
!
! -----------------------------------------------------------------------
!
      subroutine mat_5x5_product(a_left, a_right, a_prod)
!
      real(kind= kreal), intent(in) ::    a_left(5,5)
      real(kind= kreal), intent(in) ::    a_right(5,5)
      real(kind= kreal), intent(inout) :: a_prod(5,5)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 5
        a_prod(1,nd) =  a_left(1,1) * a_right(1,nd)                     &
                      + a_left(1,2) * a_right(2,nd)                     &
                      + a_left(1,3) * a_right(3,nd)                     &
                      + a_left(1,4) * a_right(4,nd)                     &
                      + a_left(1,5) * a_right(5,nd)
        a_prod(2,nd) =  a_left(2,1) * a_right(1,nd)                     &
                      + a_left(2,2) * a_right(2,nd)                     &
                      + a_left(2,3) * a_right(3,nd)                     &
                      + a_left(2,4) * a_right(4,nd)                     &
                      + a_left(2,5) * a_right(5,nd)
        a_prod(3,nd) =  a_left(3,1) * a_right(1,nd)                     &
                      + a_left(3,2) * a_right(2,nd)                     &
                      + a_left(3,3) * a_right(3,nd)                     &
                      + a_left(3,4) * a_right(4,nd)                     &
                      + a_left(3,5) * a_right(5,nd)
        a_prod(4,nd) =  a_left(4,1) * a_right(1,nd)                     &
                      + a_left(4,2) * a_right(2,nd)                     &
                      + a_left(4,3) * a_right(3,nd)                     &
                      + a_left(4,4) * a_right(4,nd)                     &
                      + a_left(4,5) * a_right(5,nd)
        a_prod(5,nd) =  a_left(5,1) * a_right(1,nd)                     &
                      + a_left(5,2) * a_right(2,nd)                     &
                      + a_left(5,3) * a_right(3,nd)                     &
                      + a_left(5,4) * a_right(4,nd)                     &
                      + a_left(5,5) * a_right(5,nd)
      end do
!
      end subroutine mat_5x5_product
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine prod_mat33_vec3(A, V, prod)
!
      real (kind=kreal), intent(in) :: A(3,3)
      real (kind=kreal), intent(in) :: V(3)
!
      real (kind=kreal), intent(inout) :: prod(3)
!
!
      prod(1) =  A(1,1)*V(1) + A(1,2)*V(2) + A(1,3)*V(3)
      prod(2) =  A(2,1)*V(1) + A(2,2)*V(2) + A(2,3)*V(3)
      prod(3) =  A(3,1)*V(1) + A(3,2)*V(2) + A(3,3)*V(3)
!
      end subroutine prod_mat33_vec3
!
! ----------------------------------------------------------------------
!
      subroutine prod_mat44_vec3(A, V, prod)
!
      real (kind=kreal), intent(in) :: A(4,4)
      real (kind=kreal), intent(in) :: V(3)
!
      real (kind=kreal), intent(inout) :: prod(3)
!
!
      prod(1) =  A(1,1)*V(1) + A(1,2)*V(2) + A(1,3)*V(3) + A(1,4)
      prod(2) =  A(2,1)*V(1) + A(2,2)*V(2) + A(2,3)*V(3) + A(2,4)
      prod(3) =  A(3,1)*V(1) + A(3,2)*V(2) + A(3,3)*V(3) + A(3,4)
!
      end subroutine prod_mat44_vec3
!
! ----------------------------------------------------------------------
!
      end module small_mat_mat_product
