!>@file   cal_products_smp.f90
!!        module cal_products_smp
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!!
!>@brief Obtain products of two fields
!!
!!@verbatim
!!      subroutine cal_scalar_product_w_coef                            &
!!     &         (nnod, coef, scalar1, scalar2, prod)
!!      subroutine cal_scalar_product_no_coef                           &
!!     &         (nnod, scalar1, scalar2, prod)
!!       integer (kind=kint), intent(in) :: nnod
!!       real (kind=kreal), intent(in) :: scalar1(nnod), scalar2(nnod)
!!       real (kind=kreal), intent(in) :: coef
!!       real (kind=kreal), intent(inout) :: prod(nnod)
!!
!!      subroutine cal_vec_scalar_product_no_coef(nnod, vect1, scalar,  &
!!     &                                          prod)
!!        integer (kind=kint), intent(in) :: nnod
!!        real (kind=kreal), intent(in) :: vect1(nnod,3), scalar(nnod)
!!        real (kind=kreal), intent(inout) :: prod(nnod,3)
!!             prod(:,:) = vect1(:,:)  * scalar(:)
!!      subroutine cal_vec_scalar_product_w_coef                       &
!!     &         (nnod, coef, vect1, scalar, prod)
!!        integer (kind=kint), intent(in) :: nnod
!!        real (kind=kreal), intent(in) :: coef
!!        real (kind=kreal), intent(in) :: vect1(nnod,3), scalar(nnod)
!!        real (kind=kreal), intent(inout) :: prod(nnod,3)
!!             prod(:,:) = coef * vect1(:,:)  * scalar(:)
!!
!!       subroutine cal_scalar_mag_vector_prod_smp                      &
!!     &          (nnod, scalar, vector, prod)
!!        integer (kind=kint), intent(in) :: nnod
!!        real (kind=kreal), intent(in) :: scalar(nnod), vector(nnod,3)
!!        real (kind=kreal), intent(inout) :: prod(nnod)
!!             prod(:) = |vect1(:,:)|  * scalar(:)
!!
!!      subroutine triple_vector_product_w_coef                         &
!!     &          (nnod, coef, vect1, vect2, vect3, prod)
!!        integer (kind=kint), intent(in) :: nnod
!!        real (kind = kreal), intent(in) :: coef
!!        real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!!        real (kind=kreal), intent(in) :: vect3(nnod,3)
!!        real (kind=kreal), intent(inout) :: prod(nnod)
!!             prod(:,:) = coef * vect1(:,:)
!!                            \cdot (vect2(:,:) \times vect3(:,:))
!!
!!      subroutine vector_vector_product(nnod, vect1, vect2, prod)
!!        integer (kind=kint), intent(in) :: nnod
!!        real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!!        real (kind=kreal), intent(inout) :: prod(nnod,3)
!!             prod(:,:) = vect1(:,nd) * vect2(:,nd)
!!      subroutine tensor_tensor_product(nnod, tensor1, tensor2, prod)
!!        integer (kind=kint), intent(in) :: nnod
!!        real (kind=kreal), intent(in) :: tensor1(nnod,6)
!!        real (kind=kreal), intent(in) :: tensor2(nnod,6)
!!        real (kind=kreal), intent(inout) :: prod(nnod,6)
!!             prod(:,:) = tensor1(:,nd) * tensor2(:,nd)
!!@endverbatim
!!
!!@n @param  nnod     Number of data points
!!@n @param  scalar1(nnod)     Input scalar data 1
!!@n @param  scalar2(nnod)     Input scalar data 2
!!@n @param  vect1(nnod,3)     Input vector data 1
!!@n @param  vect2(nnod,3)     Input vector data 2
!!@n @param  tensor1(nnod,6)   Input symmetric tensor data 1
!!@n @param  tensor2(nnod,6)   Input symmetric tenso data 2
!!
!!@n @param  prod(nnod,NB)     Product
!!                      (scalar, vector, or symmetric tensor)
!
      module cal_products_smp
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
      subroutine cal_scalar_product_w_coef                              &
     &         (nnod, coef, scalar1, scalar2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: scalar1(nnod), scalar2(nnod)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
!
!$omp parallel workshare
      prod(1:nnod) =  scalar1(1:nnod)*scalar2(1:nnod)*coef
!$omp end parallel workshare
!
      end subroutine cal_scalar_product_w_coef
!
! ----------------------------------------------------------------------
!
      subroutine cal_scalar_product_no_coef                             &
     &         (nnod, scalar1, scalar2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: scalar1(nnod), scalar2(nnod)
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
!
!$omp parallel workshare
      prod(1:nnod) =  scalar1(1:nnod)*scalar2(1:nnod)
!$omp end parallel workshare
!
      end subroutine cal_scalar_product_no_coef
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_vec_scalar_product_no_coef(nnod, vect1, scalar,    &
     &                                          prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: vect1(nnod,3), scalar(nnod)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
!
!$omp parallel workshare
      prod(1:nnod,1) = vect1(1:nnod,1)*scalar(1:nnod)
      prod(1:nnod,2) = vect1(1:nnod,2)*scalar(1:nnod)
      prod(1:nnod,3) = vect1(1:nnod,3)*scalar(1:nnod)
!$omp end parallel workshare
!
      end subroutine cal_vec_scalar_product_no_coef
!
! ----------------------------------------------------------------------
!
      subroutine cal_vec_scalar_product_w_coef                          &
     &         (nnod, coef, vect1, scalar, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vect1(nnod,3), scalar(nnod)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
!
!$omp parallel workshare
          prod(1:nnod,1) = vect1(1:nnod,1)*scalar(1:nnod) * coef
          prod(1:nnod,2) = vect1(1:nnod,2)*scalar(1:nnod) * coef
          prod(1:nnod,3) = vect1(1:nnod,3)*scalar(1:nnod) * coef
!$omp end parallel workshare
!
      end subroutine cal_vec_scalar_product_w_coef
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine cal_scalar_mag_vector_prod_smp                        &
     &          (nnod, scalar, vector, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: scalar(nnod), vector(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
!$omp parallel workshare
      prod(1:nnod) = scalar(1:nnod)                                     &
     &               * sqrt(vector(1:nnod,1)*vector(1:nnod,1)           &
     &                    + vector(1:nnod,2)*vector(1:nnod,2)           &
     &                    + vector(1:nnod,3)*vector(1:nnod,3))
!$omp end parallel workshare
!
       end subroutine cal_scalar_mag_vector_prod_smp
!
!-----------------------------------------------------------------------
!
      subroutine cal_tri_product_w_coef_smp                             &
     &          (nnod, coef, vect1, vect2, vect3, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind = kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
      real (kind=kreal), intent(in) :: vect3(nnod,3)
      real (kind=kreal), intent(inout) :: prod(nnod)
!
!
!$omp parallel workshare
      prod(1:nnod) = vect1(1:nnod,1)                                    &
     &                * ( vect2(1:nnod,2)*vect3(1:nnod,3)               &
     &                  - vect2(1:nnod,3)*vect3(1:nnod,2) )             &
     &              + vect1(1:nnod,2)                                   &
     &                * ( vect2(1:nnod,3)*vect3(1:nnod,1)               &
     &                  - vect2(1:nnod,1)*vect3(1:nnod,3) )             &
     &              + vect1(1:nnod,3)                                   &
     &                * ( vect2(1:nnod,1)*vect3(1:nnod,2)               &
     &                  - vect2(1:nnod,2)*vect3(1:nnod,1) )
!
      prod(1:nnod) =  coef * prod(1:nnod)
!$omp end parallel workshare
!
!
      end subroutine cal_tri_product_w_coef_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine vector_vector_prod_smp(nnod, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
!
!$omp parallel workshare
          prod(1:nnod,1) = vect1(1:nnod,1) * vect2(1:nnod,1)
          prod(1:nnod,2) = vect1(1:nnod,2) * vect2(1:nnod,2)
          prod(1:nnod,3) = vect1(1:nnod,3) * vect2(1:nnod,3)
!$omp end parallel workshare
!
      end subroutine vector_vector_prod_smp
!
! ----------------------------------------------------------------------
!
      subroutine tensor_tensor_prod_smp(nnod, tensor1, tensor2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: tensor1(nnod,6), tensor2(nnod,6)
!
      real (kind=kreal), intent(inout) :: prod(nnod,6)
!
!
!$omp parallel workshare
      prod(1:nnod,1) = tensor1(1:nnod,1) * tensor2(1:nnod,1)
      prod(1:nnod,2) = tensor1(1:nnod,2) * tensor2(1:nnod,2)
      prod(1:nnod,3) = tensor1(1:nnod,3) * tensor2(1:nnod,3)
      prod(1:nnod,4) = tensor1(1:nnod,4) * tensor2(1:nnod,4)
      prod(1:nnod,5) = tensor1(1:nnod,5) * tensor2(1:nnod,5)
      prod(1:nnod,6) = tensor1(1:nnod,6) * tensor2(1:nnod,6)
!$omp end parallel workshare
!
      end subroutine tensor_tensor_prod_smp
!
! ----------------------------------------------------------------------
!
      end module cal_products_smp
