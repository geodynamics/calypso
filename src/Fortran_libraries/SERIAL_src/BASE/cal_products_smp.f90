!>@file   cal_products_smp.f90
!!@brief  module cal_products_smp
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!
!>@brief Obtain products of two fields
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_scalar_prod_w_coef_smp(np_smp, nnod,             &
!!     &          inod_smp_stack, coef, scalar1, scalar2, prod)
!!      subroutine cal_scalar_prod_no_coef_smp(np_smp, nnod,            &
!!     &          inod_smp_stack, scalar1, scalar2, prod)
!!
!!      subroutine cal_dot_prod_w_coef_smp(np_smp, nnod,                &
!!     &          inod_smp_stack, coef, vect1, vect2, prod)
!!             prod(:) = coef * vect1(:,:) \cdot vect2(:,:)
!!      subroutine cal_dot_prod_no_coef_smp(np_smp, nnod,               &
!!     &          inod_smp_stack, vect1, vect2, prod)
!!             prod(:) = vect1(:,:) \cdot vect2(:,:)
!!
!!      subroutine cal_cross_prod_w_coef_smp(np_smp, nnod,              &
!!     &          inod_smp_stack, coef, vect1, vect2, prod)
!!             prod(:,:) = coef * vect1(:,:) \times vect2(:,:)
!!      subroutine cal_cross_prod_no_coef_smp(np_smp, nnod,             &
!!     &          inod_smp_stack, vect1, vect2, prod)
!!             prod(:,:) = vect1(:,:) \times vect2(:,:)
!!
!!      subroutine cal_vec_scalar_prod_no_coef_smp(np_smp, nnod,        &
!!     &          inod_smp_stack, vect1, scalar, prod)
!!             prod(:,:) = vect1(:,:)  * scalar(:)
!!      subroutine cal_vec_scalar_prod_w_coef_smp(np_smp, nnod,         &
!!     &          inod_smp_stack, coef, vect1, scalar, prod)
!!             prod(:,:) = coef * vect1(:,:)  * scalar(:)
!!
!!      subroutine cal_tensor_vec_prod_no_coef_smp(np_smp, nnod,        &
!!     &          inod_smp_stack, tensor, vector, prod)
!!             prod(:,:) = tensor(:,:) \cdot vector(:)
!!
!!       subroutine cal_scalar_mag_vector_prod_smp(np_smp, nnod,        &
!!     &          inod_smp_stack, scalar, vector, prod)
!!             prod(:) = |vect1(:,:)|  * scalar(:)
!!
!!      subroutine cal_tri_product_w_coef_smp(np_smp, nnod,             &
!!     &          inod_smp_stack, coef, vect1, vect2, vect3, prod)
!!             prod(:,:) = coef * vect1(:,:)
!!                            \cdot (vect2(:,:) \times vect3(:,:))
!!
!!      subroutine vector_vector_prod_smp(np_smp, nnod,                 &
!!     &          inod_smp_stack, vect1, vect2, prod)
!!             prod(:,:) = vect1(:,nd) * vect2(:,nd)
!!      subroutine tensor_tensor_prod_smp(np_smp, nnod,                 &
!!     &          inod_smp_stack, tensor1, tensor2, prod)
!!             prod(:,:) = tensor1(:,nd) * tensor2(:,nd)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  nnod     Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
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
      subroutine cal_scalar_prod_w_coef_smp(np_smp, nnod,               &
     &          inod_smp_stack, coef, scalar1, scalar2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar1(nnod), scalar2(nnod)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod) =  scalar1(inod)*scalar2(inod)*coef
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_scalar_prod_w_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_scalar_prod_no_coef_smp(np_smp, nnod,              &
     &          inod_smp_stack, scalar1, scalar2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar1(nnod), scalar2(nnod)
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod) =  scalar1(inod)*scalar2(inod)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_scalar_prod_no_coef_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_dot_prod_w_coef_smp(np_smp, nnod,                  &
     &          inod_smp_stack, coef, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod) = (vect1(inod,1)*vect2(inod,1)                     &
     &                + vect1(inod,2)*vect2(inod,2)                     &
     &                + vect1(inod,3)*vect2(inod,3)) * coef
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_dot_prod_w_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_dot_prod_no_coef_smp(np_smp, nnod,                 &
     &          inod_smp_stack, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod) =  vect1(inod,1)*vect2(inod,1)                     &
     &                + vect1(inod,2)*vect2(inod,2)                     &
     &                + vect1(inod,3)*vect2(inod,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_dot_prod_no_coef_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_cross_prod_w_coef_smp(np_smp, nnod,                &
     &          inod_smp_stack, coef, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod,1) = (vect1(inod,2)*vect2(inod,3)                   &
     &                  - vect1(inod,3)*vect2(inod,2) ) * coef
          prod(inod,2) = (vect1(inod,3)*vect2(inod,1)                   &
     &                  - vect1(inod,1)*vect2(inod,3) ) * coef
          prod(inod,3) = (vect1(inod,1)*vect2(inod,2)                   &
     &                  - vect1(inod,2)*vect2(inod,1) ) * coef
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_cross_prod_w_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_cross_prod_no_coef_smp(np_smp, nnod,               &
     &          inod_smp_stack, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod,1) = (vect1(inod,2)*vect2(inod,3)                   &
     &                  - vect1(inod,3)*vect2(inod,2) )
          prod(inod,2) = (vect1(inod,3)*vect2(inod,1)                   &
     &                  - vect1(inod,1)*vect2(inod,3) )
          prod(inod,3) = (vect1(inod,1)*vect2(inod,2)                   &
     &                  - vect1(inod,2)*vect2(inod,1) )
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_cross_prod_no_coef_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_vec_scalar_prod_no_coef_smp(np_smp, nnod,          &
     &          inod_smp_stack, vect1, scalar, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vect1(nnod,3), scalar(nnod)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod,1) = vect1(inod,1)*scalar(inod)
          prod(inod,2) = vect1(inod,2)*scalar(inod)
          prod(inod,3) = vect1(inod,3)*scalar(inod)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_vec_scalar_prod_no_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_vec_scalar_prod_w_coef_smp(np_smp, nnod,           &
     &          inod_smp_stack, coef, vect1, scalar, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vect1(nnod,3), scalar(nnod)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod,1) = vect1(inod,1)*scalar(inod) * coef
          prod(inod,2) = vect1(inod,2)*scalar(inod) * coef
          prod(inod,3) = vect1(inod,3)*scalar(inod) * coef
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_vec_scalar_prod_w_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_tensor_vec_prod_no_coef_smp(np_smp, nnod,          &
     &          inod_smp_stack, tensor, vector, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector(nnod,3), tensor(nnod,6)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod,1) =  tensor(inod,1) * vector(inod,1)               &
     &                  + tensor(inod,2) * vector(inod,2)               &
     &                  + tensor(inod,3) * vector(inod,3)
          prod(inod,2) =  tensor(inod,2) * vector(inod,1)               &
     &                  + tensor(inod,4) * vector(inod,2)               &
     &                  + tensor(inod,5) * vector(inod,3)
          prod(inod,3) =  tensor(inod,3) * vector(inod,1)               &
     &                  + tensor(inod,5) * vector(inod,2)               &
     &                  + tensor(inod,6) * vector(inod,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_tensor_vec_prod_no_coef_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine cal_scalar_mag_vector_prod_smp(np_smp, nnod,          &
     &          inod_smp_stack, scalar, vector, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar(nnod), vector(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
       integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          prod(inod) = scalar(inod)                                     &
     &                   * sqrt(vector(inod,1)*vector(inod,1)           &
     &                        + vector(inod,2)*vector(inod,2)           &
     &                        + vector(inod,3)*vector(inod,3) )
        end do
      end do
!$omp end do
!
       end subroutine cal_scalar_mag_vector_prod_smp
!
!-----------------------------------------------------------------------
!
      subroutine cal_tri_product_w_coef_smp(np_smp, nnod,               &
     &          inod_smp_stack, coef, vect1, vect2, vect3, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind = kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
      real (kind=kreal), intent(in) :: vect3(nnod,3)
      real (kind=kreal), intent(inout) :: prod(nnod)
      integer(kind = kint) :: ip, inod, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
!
          prod(inod) = vect1(inod,1) * ( vect2(inod,2)*vect3(inod,3)    &
     &                                 - vect2(inod,3)*vect3(inod,2) )  &
     &               + vect1(inod,2) * ( vect2(inod,3)*vect3(inod,1)    &
     &                                 - vect2(inod,1)*vect3(inod,3) )  &
     &               + vect1(inod,3) * ( vect2(inod,1)*vect3(inod,2)    &
     &                                 - vect2(inod,2)*vect3(inod,1) )
!
          prod(inod) =  coef * prod(inod)
        end do
      end do
!$omp end parallel do
!
!
      end subroutine cal_tri_product_w_coef_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine vector_vector_prod_smp(np_smp, nnod,                   &
     &          inod_smp_stack, vect1, vect2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vect1(nnod,3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod,1) = vect1(inod,1) * vect2(inod,1)
          prod(inod,2) = vect1(inod,2) * vect2(inod,2)
          prod(inod,3) = vect1(inod,3) * vect2(inod,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine vector_vector_prod_smp
!
! ----------------------------------------------------------------------
!
      subroutine tensor_tensor_prod_smp(np_smp, nnod,                   &
     &          inod_smp_stack, tensor1, tensor2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor1(nnod,6), tensor2(nnod,6)
!
      real (kind=kreal), intent(inout) :: prod(nnod,6)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod,1) = tensor1(inod,1) * tensor2(inod,1)
          prod(inod,2) = tensor1(inod,2) * tensor2(inod,2)
          prod(inod,3) = tensor1(inod,3) * tensor2(inod,3)
          prod(inod,4) = tensor1(inod,4) * tensor2(inod,4)
          prod(inod,5) = tensor1(inod,5) * tensor2(inod,5)
          prod(inod,6) = tensor1(inod,6) * tensor2(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine tensor_tensor_prod_smp
!
! ----------------------------------------------------------------------
!
      end module cal_products_smp
