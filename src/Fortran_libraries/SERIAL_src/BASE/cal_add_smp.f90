!>@file   cal_add_smp.f90
!!@brief  module cal_add_smp
!!
!!@author H. Matsui
!!@date Programmed...when??
!
!>@brief subroutines to add two field data
!!@n      Need $omp parallel to use these routines
!!
!!@verbatim
!!      subroutine add_scalars_smp(np_smp, nnod, inod_smp_stack,        &
!!     &          scalar1, scalar2, added)
!!      subroutine add_vectors_smp(np_smp, nnod, inod_smp_stack,        &
!!     &          vector1, vector2, added)
!!      subroutine add_tensors_smp(np_smp, nnod, inod_smp_stack,        &
!!     &          tensor1, tensor2, added)
!!
!!      subroutine add_const_to_scalar_smp(np_smp, nnod, inod_smp_stack,&
!!     &          scalar, const, added)
!!      subroutine add_const_to_vector_smp(np_smp, nnod, inod_smp_stack,&
!!     &          vector, const, added)
!!      subroutine add_const_to_tensor_smp(np_smp, nnod, inod_smp_stack,&
!!     &          tensor, const, added)
!!
!!      subroutine add_const_to_scalar_smp_ow(np_smp, nnod,             &
!!     &          inod_smp_stack, scalar, const)
!!      subroutine add_const_to_vector_smp_ow(np_smp, nnod,             &
!!     &          inod_smp_stack, vector, const)
!!      subroutine add_const_to_tensor_smp_ow(np_smp, nnod,             &
!!     &          inod_smp_stack, tensor, const)
!!
!!         d_nod(inod,i_r) =  d_nod(inod,i_v1) + d_nod(inod,i_v2)
!!        i_r: result field ID
!!        i_v1, i_v2: source field IDs
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  nnod     Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!
!!@n @param  scalar1(nnod)     Input scalar data 1
!!@n @param  scalar2(nnod)     Input scalar data 2
!!@n @param  vector1(nnod,3)   Input vector data 1
!!@n @param  vector2(nnod,3)   Input vector data 2
!!@n @param  tensor1(nnod,6)   Input symmetric tensor data 1
!!@n @param  tensor2(nnod,6)   Input symmetric tenso data 2
!!@n @param  const             Constant
!!                      (scalar, vector, or symmetric tensor)
!!@n @param  added(nnod,NB)    Solution
!!                      (scalar, vector, or symmetric tensor)
!
      module cal_add_smp
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_scalars_smp(np_smp, nnod, inod_smp_stack,          &
     &          scalar1, scalar2, added)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar1(nnod), scalar2(nnod)
!
      real (kind=kreal), intent(inout) :: added(nnod)
!
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          added(inod) = scalar1(inod) + scalar2(inod)
        end do
      end do
!$omp end do nowait
!
       end subroutine add_scalars_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_vectors_smp(np_smp, nnod, inod_smp_stack,          &
     &          vector1, vector2, added)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector1(nnod,3), vector2(nnod,3)
!
      real (kind=kreal), intent(inout) :: added(nnod,3)
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          added(inod,1) = vector1(inod,1) + vector2(inod,1)
          added(inod,2) = vector1(inod,2) + vector2(inod,2)
          added(inod,3) = vector1(inod,3) + vector2(inod,3)
        end do
      end do
!$omp end do nowait
!
       end subroutine add_vectors_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_tensors_smp(np_smp, nnod, inod_smp_stack,          &
     &          tensor1, tensor2, added)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor1(nnod,6), tensor2(nnod,6)
!
      real (kind=kreal), intent(inout) :: added(nnod,6)
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          added(inod,1) = tensor1(inod,1) + tensor2(inod,1)
          added(inod,2) = tensor1(inod,2) + tensor2(inod,2)
          added(inod,3) = tensor1(inod,3) + tensor2(inod,3)
          added(inod,4) = tensor1(inod,4) + tensor2(inod,4)
          added(inod,5) = tensor1(inod,5) + tensor2(inod,5)
          added(inod,6) = tensor1(inod,6) + tensor2(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine add_tensors_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_const_to_scalar_smp(np_smp, nnod, inod_smp_stack,  &
     &          scalar, const, added)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar(nnod), const
!
      real (kind=kreal), intent(inout) :: added(nnod)
!
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          added(inod) = scalar(inod) + const
        end do
      end do
!$omp end do nowait
!
       end subroutine add_const_to_scalar_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_const_to_vector_smp(np_smp, nnod, inod_smp_stack,  &
     &          vector, const, added)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector(nnod,3), const(3)
!
      real (kind=kreal), intent(inout) :: added(nnod,3)
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          added(inod,1) = vector(inod,1) + const(1)
          added(inod,2) = vector(inod,2) + const(2)
          added(inod,3) = vector(inod,3) + const(3)
        end do
      end do
!$omp end do nowait
!
       end subroutine add_const_to_vector_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_const_to_tensor_smp(np_smp, nnod, inod_smp_stack,  &
     &          tensor, const, added)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor(nnod,6), const(6)
!
      real (kind=kreal), intent(inout) :: added(nnod,6)
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          added(inod,1) = tensor(inod,1) + const(1)
          added(inod,2) = tensor(inod,2) + const(2)
          added(inod,3) = tensor(inod,3) + const(3)
          added(inod,4) = tensor(inod,4) + const(4)
          added(inod,5) = tensor(inod,5) + const(5)
          added(inod,6) = tensor(inod,6) + const(6)
        end do
      end do
!$omp end do nowait
!
      end subroutine add_const_to_tensor_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_const_to_scalar_smp_ow(np_smp, nnod,               &
     &          inod_smp_stack, scalar, const)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: const
!
      real (kind=kreal), intent(inout) :: scalar(nnod)
!
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          scalar(inod) = scalar(inod) + const
        end do
      end do
!$omp end do nowait
!
       end subroutine add_const_to_scalar_smp_ow
!
!-----------------------------------------------------------------------
!
      subroutine add_const_to_vector_smp_ow(np_smp, nnod,               &
     &          inod_smp_stack, vector, const)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: const(3)
!
      real (kind=kreal), intent(inout) :: vector(nnod,3)
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vector(inod,1) = vector(inod,1) + const(1)
          vector(inod,2) = vector(inod,2) + const(2)
          vector(inod,3) = vector(inod,3) + const(3)
        end do
      end do
!$omp end do nowait
!
       end subroutine add_const_to_vector_smp_ow
!
!-----------------------------------------------------------------------
!
      subroutine add_const_to_tensor_smp_ow(np_smp, nnod,               &
     &          inod_smp_stack, tensor, const)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: const(6)
!
      real (kind=kreal), intent(inout) :: tensor(nnod,6)
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          tensor(inod,1) = tensor(inod,1) + const(1)
          tensor(inod,2) = tensor(inod,2) + const(2)
          tensor(inod,3) = tensor(inod,3) + const(3)
          tensor(inod,4) = tensor(inod,4) + const(4)
          tensor(inod,5) = tensor(inod,5) + const(5)
          tensor(inod,6) = tensor(inod,6) + const(6)
        end do
      end do
!$omp end do nowait
!
      end subroutine add_const_to_tensor_smp_ow
!
!-----------------------------------------------------------------------
!
      end module cal_add_smp
