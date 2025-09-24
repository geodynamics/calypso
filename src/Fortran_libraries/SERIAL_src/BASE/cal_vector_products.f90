!>@file   cal_vector_products.f90
!!        module cal_vector_products
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!!
!>@brief Products of vector fields
!!
!!@verbatim
!!      subroutine cal_dot_product_w_coef                               &
!!     &         (nnod, coef, vect1, vect2, prod)
!!             prod(:) = coef * vect1(:,:) \cdot vect2(:,:)
!!      subroutine cal_dot_product_no_coef(nnod, vect1, vect2, prod)
!!             prod(:) = vect1(:,:) \cdot vect2(:,:)
!!        integer (kind=kint), intent(in) :: nnod
!!        real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!!        real (kind=kreal), intent(in) :: coef
!!        real (kind=kreal), intent(inout) :: prod(nnod)
!!
!!      subroutine cal_cross_product_w_coef                             &
!!     &         (nnod, coef, vect1, vect2, prod)
!!             prod(:,:) = coef * vect1(:,:) \times vect2(:,:)
!!      subroutine add_cross_product_w_coef                             &
!!     &         (nnod, coef, vect1, vect2, prod)
!!             prod(:,:) = coef * vect1(:,:) \times vect2(:,:)
!!      subroutine cal_cross_product_no_coef                            &
!!     &         (nnod, vect1, vect2, prod)
!!             prod(:,:) = vect1(:,:) \times vect2(:,:)
!!       integer (kind=kint), intent(in) :: nnod
!!       real (kind=kreal), intent(in) :: coef
!!       real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!!       real (kind=kreal), intent(inout) :: prod(nnod,3)
!!
!!      subroutine cal_tensor_vec_product_no_coef                       &
!!     &         (nnod, tensor, vector, prod)
!!        integer (kind=kint), intent(in) :: nnod
!!        real (kind=kreal), intent(in) :: vector(nnod,3), tensor(nnod,6)
!!        real (kind=kreal), intent(inout) :: prod(nnod,3)
!!             prod(:,:) = tensor(:,:) \cdot vector(:)
!!
!!      real(kind = kreal) function single_dot_product(vect1, vect2)
!!        real (kind=kreal), intent(in) :: vect1(3), vect2(3)
!!      subroutine single_cross_product(vect1, vect2, prod)
!!        real (kind=kreal), intent(in) :: vect1(3), vect2(3)
!!        real (kind=kreal), intent(inout) :: prod(3)
!!@endverbatim
!
      module cal_vector_products
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_dot_product_w_coef                                 &
     &         (nnod, coef, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
!
!$omp parallel workshare
      prod(1:nnod) = (vect1(1:nnod,1)*vect2(1:nnod,1)                   &
     &              + vect1(1:nnod,2)*vect2(1:nnod,2)                   &
     &              + vect1(1:nnod,3)*vect2(1:nnod,3)) * coef
!$omp end parallel workshare
!
      end subroutine cal_dot_product_w_coef
!
! ----------------------------------------------------------------------
!
      subroutine cal_dot_product_no_coef(nnod, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
!
!$omp parallel workshare
      prod(1:nnod) =  vect1(1:nnod,1)*vect2(1:nnod,1)                   &
     &            + vect1(1:nnod,2)*vect2(1:nnod,2)                     &
     &            + vect1(1:nnod,3)*vect2(1:nnod,3)
!$omp end parallel workshare
!
      end subroutine cal_dot_product_no_coef
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_cross_product_w_coef                               &
     &         (nnod, coef, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
!
!$omp parallel workshare
      prod(1:nnod,1) = (vect1(1:nnod,2)*vect2(1:nnod,3)                 &
     &              - vect1(1:nnod,3)*vect2(1:nnod,2) ) * coef
      prod(1:nnod,2) = (vect1(1:nnod,3)*vect2(1:nnod,1)                 &
     &              - vect1(1:nnod,1)*vect2(1:nnod,3) ) * coef
      prod(1:nnod,3) = (vect1(1:nnod,1)*vect2(1:nnod,2)                 &
     &              - vect1(1:nnod,2)*vect2(1:nnod,1) ) * coef
!$omp end parallel workshare
!
      end subroutine cal_cross_product_w_coef
!
! ----------------------------------------------------------------------
!
      subroutine add_cross_product_w_coef                               &
     &         (nnod, coef, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
!
!$omp parallel workshare
      prod(1:nnod,1) = prod(1:nnod,1)                                   &
     &              + (vect1(1:nnod,2)*vect2(1:nnod,3)                  &
     &               - vect1(1:nnod,3)*vect2(1:nnod,2) ) * coef
      prod(1:nnod,2) = prod(1:nnod,2)                                   &
     &              + (vect1(1:nnod,3)*vect2(1:nnod,1)                  &
     &               - vect1(1:nnod,1)*vect2(1:nnod,3) ) * coef
      prod(1:nnod,3) = prod(1:nnod,3)                                   &
     &              + (vect1(1:nnod,1)*vect2(1:nnod,2)                  &
     &               - vect1(1:nnod,2)*vect2(1:nnod,1) ) * coef
!$omp end parallel workshare
!
      end subroutine add_cross_product_w_coef
!
! ----------------------------------------------------------------------
!
      subroutine cal_cross_product_no_coef                              &
     &         (nnod, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
!
!$omp parallel workshare
      prod(1:nnod,1) = (vect1(1:nnod,2)*vect2(1:nnod,3)                 &
     &                - vect1(1:nnod,3)*vect2(1:nnod,2) )
      prod(1:nnod,2) = (vect1(1:nnod,3)*vect2(1:nnod,1)                 &
     &                - vect1(1:nnod,1)*vect2(1:nnod,3) )
      prod(1:nnod,3) = (vect1(1:nnod,1)*vect2(1:nnod,2)                 &
     &                - vect1(1:nnod,2)*vect2(1:nnod,1) )
!$omp end parallel workshare
!
      end subroutine cal_cross_product_no_coef
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_tensor_vec_product_no_coef                         &
     &         (nnod, tensor, vector, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: vector(nnod,3), tensor(nnod,6)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
!
!$omp parallel workshare
      prod(1:nnod,1) =  tensor(1:nnod,1) * vector(1:nnod,1)             &
     &                + tensor(1:nnod,2) * vector(1:nnod,2)             &
     &                + tensor(1:nnod,3) * vector(1:nnod,3)
      prod(1:nnod,2) =  tensor(1:nnod,2) * vector(1:nnod,1)             &
     &                + tensor(1:nnod,4) * vector(1:nnod,2)             &
     &                + tensor(1:nnod,5) * vector(1:nnod,3)
      prod(1:nnod,3) =  tensor(1:nnod,3) * vector(1:nnod,1)             &
     &                + tensor(1:nnod,5) * vector(1:nnod,2)             &
     &                + tensor(1:nnod,6) * vector(1:nnod,3)
!$omp end parallel workshare
!
      end subroutine cal_tensor_vec_product_no_coef
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      real(kind = kreal) function single_dot_product(vect1, vect2)
!
      real (kind=kreal), intent(in) :: vect1(3), vect2(3)
      real (kind=kreal) :: prod
!
      prod = vect1(1)*vect2(1) + vect1(2)*vect2(2) + vect1(3)*vect2(3)
      single_dot_product = prod
!
      end function single_dot_product
!
! ----------------------------------------------------------------------
!
      subroutine single_cross_product(vect1, vect2, prod)
!
      real (kind=kreal), intent(in) :: vect1(3), vect2(3)
      real (kind=kreal), intent(inout) :: prod(3)
!
      prod(1) = (vect1(2)*vect2(3) - vect1(3)*vect2(2))
      prod(2) = (vect1(3)*vect2(1) - vect1(1)*vect2(3))
      prod(3) = (vect1(1)*vect2(2) - vect1(2)*vect2(1))
!
      end subroutine single_cross_product
!
! ----------------------------------------------------------------------
!
      end module cal_vector_products
