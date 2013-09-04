!>@file   cal_products_w_const_smp.f90
!!@brief  module cal_products_w_const_smp
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!
!>@brief Obtain products of field with constant field
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_coef_prod_scalar_smp(np_smp, nnod,               &
!!     &          inod_smp_stack, coef, scalar2, prod)
!!      subroutine cal_coef_prod_vect_smp(np_smp, nnod,                 &
!!     &          inod_smp_stack, coef, vect2, prod)
!!      subroutine cal_coef_prod_tensor_smp(np_smp, nnod,               &
!!     &          inod_smp_stack, coef, tensor2, prod)
!!
!!      subroutine cal_dot_prod_cvec_w_coef_smp(np_smp, nnod,           &
!!     &          inod_smp_stack, coef, c_vec, vect2, prod)
!!             prod(:) = coef * c_vec(:) \cdot vect2(:,:)
!!      subroutine cal_dot_prod_cvec_no_coef_smp(np_smp, nnod,          &
!!     &          inod_smp_stack, c_vec, vect2, prod)
!!             prod(:) = c_vec(:) \cdot vect2(:,:)
!!
!!      subroutine cal_vect_prod_cvec_w_coef_smp(np_smp, nnod,          &
!!     &          inod_smp_stack, coef, c_vec, vect2, prod)
!!             prod(:,:) = coef * c_vec(:) \times vect2(:,:)
!!      subroutine cal_vect_prod_cvec_no_coef_smp(np_smp, nnod,         &
!!     &          inod_smp_stack, c_vec, vect2, prod)
!!             prod(:,:) = c_vec(:) \times vect2(:,:)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  nnod     Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  coef     scalar coefficient
!!@n @param  c_vec(3)          constant vector
!!@n @param  scalar2(nnod)     Input scalar data 2
!!@n @param  vect2(nnod,3)     Input vector data 2
!!@n @param  tensor2(nnod,6)   Input symmetric tenso data 2
!!
!!@n @param  prod(nnod,NB)     Product
!!                      (scalar, vector, or symmetric tensor)
!
      module cal_products_w_const_smp
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
      subroutine cal_coef_prod_scalar_smp(np_smp, nnod,                 &
     &          inod_smp_stack, coef, scalar2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar2(nnod)
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
          prod(inod) =  scalar2(inod)*coef
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_coef_prod_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_coef_prod_vect_smp(np_smp, nnod,                   &
     &          inod_smp_stack, coef, vect2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef, vect2(nnod,3)
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
          prod(inod,1) =  coef*vect2(inod,1)
          prod(inod,2) =  coef*vect2(inod,2)
          prod(inod,3) =  coef*vect2(inod,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_coef_prod_vect_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_coef_prod_tensor_smp(np_smp, nnod,                 &
     &          inod_smp_stack, coef, tensor2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef, tensor2(nnod,6)
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
          prod(inod,1) =  coef*tensor2(inod,1)
          prod(inod,2) =  coef*tensor2(inod,2)
          prod(inod,3) =  coef*tensor2(inod,3)
          prod(inod,4) =  coef*tensor2(inod,4)
          prod(inod,5) =  coef*tensor2(inod,5)
          prod(inod,6) =  coef*tensor2(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_coef_prod_tensor_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_dot_prod_cvec_w_coef_smp(np_smp, nnod,             &
     &          inod_smp_stack, coef, c_vec, vect2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: c_vec(3), vect2(nnod,3)
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
          prod(inod) = (c_vec(1)*vect2(inod,1)                          &
     &                + c_vec(2)*vect2(inod,2)                          &
     &                + c_vec(3)*vect2(inod,3)) * coef
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_dot_prod_cvec_w_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_dot_prod_cvec_no_coef_smp(np_smp, nnod,            &
     &          inod_smp_stack, c_vec, vect2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: c_vec(3), vect2(nnod,3)
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
          prod(inod) =  c_vec(1)*vect2(inod,1)                          &
     &                + c_vec(2)*vect2(inod,2)                          &
     &                + c_vec(3)*vect2(inod,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_dot_prod_cvec_no_coef_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_vect_prod_cvec_w_coef_smp(np_smp, nnod,            &
     &          inod_smp_stack, coef, c_vec, vect2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: c_vec(3), vect2(nnod,3)
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
          prod(inod,1) = (c_vec(2)*vect2(inod,3)                        &
     &                  - c_vec(3)*vect2(inod,2) ) * coef
          prod(inod,2) = (c_vec(3)*vect2(inod,1)                        &
     &                  - c_vec(1)*vect2(inod,3) ) * coef
          prod(inod,3) = (c_vec(1)*vect2(inod,2)                        &
     &                  - c_vec(2)*vect2(inod,1) ) * coef
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_vect_prod_cvec_w_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_prod_cvec_no_coef_smp(np_smp, nnod,           &
     &          inod_smp_stack, c_vec, vect2, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: c_vec(3), vect2(nnod,3)
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
          prod(inod,1) = (c_vec(2)*vect2(inod,3)                        &
     &                  - c_vec(3)*vect2(inod,2) )
          prod(inod,2) = (c_vec(3)*vect2(inod,1)                        &
     &                  - c_vec(1)*vect2(inod,3) )
          prod(inod,3) = (c_vec(1)*vect2(inod,2)                        &
     &                  - c_vec(2)*vect2(inod,1) )
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_vect_prod_cvec_no_coef_smp
!
! ----------------------------------------------------------------------
!
      end module cal_products_w_const_smp
