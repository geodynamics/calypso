!>@file   overwrite_products_smp.f90
!!@brief  module overwrite_products_smp
!!
!!@author H. Matsui
!!@date  programmed by H.Matsui on May., 2009
!
!>@brief subroutines to obatine products of two fields
!>@n      need $omp parallel to use these routines
!
!!@verbatim
!!      subroutine ovwrt_scalar_prod_no_coef_smp(np_smp, nnod,          &
!!     &          inod_smp_stack, scalar1, prod)
!!             prod(:) = prod(:)  * scalar(:)
!!      subroutine ovwrt_scalar_prod_w_coef_smp(np_smp, nnod,           &
!!     &          inod_smp_stack, coef, scalar1, scalar2, prod)
!!             prod(:) = coef * prod(:)  * scalar(:)
!!
!!      subroutine ovwrt_vec_scalar_prod_smp(np_smp, nnod,              &
!!     &          inod_smp_stack, scalar, prod)
!!             prod(:,:) = prod(:,:)  * scalar(:)
!!      subroutine ovwrt_vec_scalar_coef_smp(np_smp, nnod,              &
!!     &          inod_smp_stack, coef, vect1, scalar, prod)
!!             prod(:,:) = coef * prod(:,:)  * scalar(:)
!!
!!      subroutine ovwrt_tensor_scalar_prod_smp(np_smp, nnod,           &
!!     &          inod_smp_stack, scalar, prod)
!!      subroutine ovwrt_tensor_scalar_coef_smp(np_smp, nnod,           &
!!     &          inod_smp_stack, coef, scalar, prod)
!!
!!      subroutine ovwrt_tensor_vec_no_coef_smp(np_smp, nnod,           &
!!     &          inod_smp_stack, tensor, prod)
!!             prod(:,:) = tensor(:,:) \cdot prod(:)
!!
!!      subroutine ovwrt_vector_vector_prod_smp(np_smp, nnod,           &
!!     &          inod_smp_stack, vect1, prod)
!!             prod(:,:) = vect1(:,nd) * prod(:,nd)
!!      subroutine ovwrt_tensor_tensor_prod_smp(np_smp, nnod,           &
!!     &          inod_smp_stack, tensor1, prod)
!!             prod(:,:) = tensor1(:,nd) * prod(:,nd)
!!@endverbatim
!
      module overwrite_products_smp
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
      subroutine ovwrt_scalar_prod_no_coef_smp(np_smp, nnod,            &
     &          inod_smp_stack, scalar1, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar1(nnod)
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
          prod(inod) =  scalar1(inod)*prod(inod)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_scalar_prod_no_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_scalar_prod_w_coef_smp(np_smp, nnod,             &
     &          inod_smp_stack, coef, scalar1, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar1(nnod)
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
          prod(inod) =  scalar1(inod)*prod(inod)*coef
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_scalar_prod_w_coef_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ovwrt_vec_scalar_prod_smp(np_smp, nnod,                &
     &          inod_smp_stack, scalar, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar(nnod)
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
          prod(inod,1) = prod(inod,1)*scalar(inod)
          prod(inod,2) = prod(inod,2)*scalar(inod)
          prod(inod,3) = prod(inod,3)*scalar(inod)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_vec_scalar_prod_smp
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_vec_scalar_coef_smp(np_smp, nnod,                &
     &          inod_smp_stack, coef, scalar, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: scalar(nnod)
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
          prod(inod,1) = prod(inod,1)*scalar(inod) * coef
          prod(inod,2) = prod(inod,2)*scalar(inod) * coef
          prod(inod,3) = prod(inod,3)*scalar(inod) * coef
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_vec_scalar_coef_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ovwrt_tensor_scalar_prod_smp(np_smp, nnod,             &
     &          inod_smp_stack, scalar, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar(nnod)
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
          prod(inod,1) = prod(inod,1)*scalar(inod)
          prod(inod,2) = prod(inod,2)*scalar(inod)
          prod(inod,3) = prod(inod,3)*scalar(inod)
          prod(inod,4) = prod(inod,4)*scalar(inod)
          prod(inod,5) = prod(inod,5)*scalar(inod)
          prod(inod,6) = prod(inod,6)*scalar(inod)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_tensor_scalar_prod_smp
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_tensor_scalar_coef_smp(np_smp, nnod,             &
     &          inod_smp_stack, coef, scalar, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: scalar(nnod)
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
          prod(inod,1) = prod(inod,1)*scalar(inod) * coef
          prod(inod,2) = prod(inod,2)*scalar(inod) * coef
          prod(inod,3) = prod(inod,3)*scalar(inod) * coef
          prod(inod,4) = prod(inod,4)*scalar(inod) * coef
          prod(inod,5) = prod(inod,5)*scalar(inod) * coef
          prod(inod,6) = prod(inod,6)*scalar(inod) * coef
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_tensor_scalar_coef_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ovwrt_tensor_vec_no_coef_smp(np_smp, nnod,             &
     &          inod_smp_stack, tensor, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor(nnod,6)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer(kind=kint) :: iproc, inod, ist, ied
      real(kind = kreal) :: v(3)
!
!
!$omp do private(inod,ist,ied,v)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          v(1) = prod(inod,1)
          v(2) = prod(inod,2)
          v(3) = prod(inod,3)
          prod(inod,1) =  tensor(inod,1) * v(1)                         &
     &                  + tensor(inod,2) * v(2)                         &
     &                  + tensor(inod,3) * v(3)
          prod(inod,2) =  tensor(inod,2) * v(1)                         &
     &                  + tensor(inod,4) * v(2)                         &
     &                  + tensor(inod,5) * v(3)
          prod(inod,3) =  tensor(inod,3) * v(1)                         &
     &                  + tensor(inod,5) * v(2)                         &
     &                  + tensor(inod,6) * v(3)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_tensor_vec_no_coef_smp
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine ovwrt_vector_vector_prod_smp(np_smp, nnod,             &
     &          inod_smp_stack, vect1, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vect1(nnod,3)
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
          prod(inod,1) = vect1(inod,1) * prod(inod,1)
          prod(inod,2) = vect1(inod,2) * prod(inod,2)
          prod(inod,3) = vect1(inod,3) * prod(inod,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_vector_vector_prod_smp
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_tensor_tensor_prod_smp(np_smp, nnod,             &
     &          inod_smp_stack, tensor1, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor1(nnod,6)
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
          prod(inod,1) = tensor1(inod,1) * prod(inod,1)
          prod(inod,2) = tensor1(inod,2) * prod(inod,2)
          prod(inod,3) = tensor1(inod,3) * prod(inod,3)
          prod(inod,4) = tensor1(inod,4) * prod(inod,4)
          prod(inod,5) = tensor1(inod,5) * prod(inod,5)
          prod(inod,6) = tensor1(inod,6) * prod(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_tensor_tensor_prod_smp
!
! ----------------------------------------------------------------------
!
      end module overwrite_products_smp
