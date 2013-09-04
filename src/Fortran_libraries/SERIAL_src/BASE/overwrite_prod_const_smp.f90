!>@file   overwrite_prod_const_smp.f90
!!@brief  module overwrite_prod_const_smp
!!
!!@author H. Matsui
!!@date Programmed in May 2009
!
!>@brief subroutines to obatine products and overwrite to original data
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine ovwrt_coef_prod_scalar_smp(np_smp, nnod,             &
!!     &          inod_smp_stack, coef, prod)
!!      subroutine ovwrt_coef_prod_vect_smp(np_smp, nnod,               &
!!     &          inod_smp_stack, coef, prod)
!!      subroutine ovwrt_coef_prod_tensor_smp(np_smp, nnod,             &
!!     &          inod_smp_stack, coef, prod)
!!
!!      subroutine ovwrt_vect_prod_cvec_coef_smp(np_smp, nnod,          &
!!     &          inod_smp_stack, coef, c_vec, prod)
!!             prod(:,:) = coef * c_vec(:) \times prod(:,:)
!!      subroutine ovwrt_vect_prod_cvec_smp(np_smp, nnod,               &
!!     &          inod_smp_stack, c_vec, prod)
!!             prod(:,:) = c_vec(:) \times prod(:,:)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  nnod     Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  coef     Scalar coefficients
!!@n @param  c_vec(3) constant vector
!!
!!@n @param  prod(nnod,NB)     Product
!!                      (scalar, vector, or symmetric tensor)
!
      module overwrite_prod_const_smp
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
      subroutine ovwrt_coef_prod_scalar_smp(np_smp, nnod,               &
     &          inod_smp_stack, coef, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
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
          prod(inod) =  prod(inod)*coef
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_coef_prod_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_coef_prod_vect_smp(np_smp, nnod,                 &
     &          inod_smp_stack, coef, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
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
          prod(inod,1) =  coef*prod(inod,1)
          prod(inod,2) =  coef*prod(inod,2)
          prod(inod,3) =  coef*prod(inod,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_coef_prod_vect_smp
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_coef_prod_tensor_smp(np_smp, nnod,               &
     &          inod_smp_stack, coef, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
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
          prod(inod,1) =  coef*prod(inod,1)
          prod(inod,2) =  coef*prod(inod,2)
          prod(inod,3) =  coef*prod(inod,3)
          prod(inod,4) =  coef*prod(inod,4)
          prod(inod,5) =  coef*prod(inod,5)
          prod(inod,6) =  coef*prod(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_coef_prod_tensor_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ovwrt_vect_prod_cvec_coef_smp(np_smp, nnod,            &
     &          inod_smp_stack, coef, c_vec, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: c_vec(3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
      real (kind=kreal) :: v(3)
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
          prod(inod,1) = (c_vec(2)*v(3) - c_vec(3)*v(2)) * coef
          prod(inod,2) = (c_vec(3)*v(1) - c_vec(1)*v(3)) * coef
          prod(inod,3) = (c_vec(1)*v(2) - c_vec(2)*v(1)) * coef
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_vect_prod_cvec_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_vect_prod_cvec_smp(np_smp, nnod,                 &
     &          inod_smp_stack, c_vec, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: c_vec(3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
      real (kind=kreal) :: v(3)
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
          prod(inod,1) = c_vec(2)*v(3) - c_vec(3)*v(2)
          prod(inod,2) = c_vec(3)*v(1) - c_vec(1)*v(3)
          prod(inod,3) = c_vec(1)*v(2) - c_vec(2)*v(1)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_vect_prod_cvec_smp
!
! ----------------------------------------------------------------------
!
      end module overwrite_prod_const_smp
