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
!!      subroutine add_scalars_smp(nnod, scalar1, scalar2, added)
!!      subroutine add_vectors_smp(nnod, vector1, vector2, added)
!!      subroutine add_tensors_smp(nnod, tensor1, tensor2, added)
!!
!!      subroutine add_const_to_scalar_smp(nnod, scalar, const, added)
!!      subroutine add_const_to_vector_smp(nnod, vector, const, added)
!!      subroutine add_const_to_tensor_smp(nnod, tensor, const, added)
!!
!!      subroutine add_const_to_scalar_smp_ow(nnod, scalar, const)
!!      subroutine add_const_to_vector_smp_ow(nnod, vector, const)
!!      subroutine add_const_to_tensor_smp_ow(nnod, tensor, const)
!!
!!         d_nod(inod,i_r) =  d_nod(inod,i_v1) + d_nod(inod,i_v2)
!!        i_r: result field ID
!!        i_v1, i_v2: source field IDs
!!@endverbatim
!!
!!@n @param  nnod     Number of data points
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
      subroutine add_scalars_smp(nnod, scalar1, scalar2, added)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: scalar1(nnod), scalar2(nnod)
!
      real (kind=kreal), intent(inout) :: added(nnod)
!
!
!$omp workshare
      added(1:nnod) = scalar1(1:nnod) + scalar2(1:nnod)
!$omp end workshare nowait
!
       end subroutine add_scalars_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_vectors_smp(nnod, vector1, vector2, added)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: vector1(nnod,3), vector2(nnod,3)
!
      real (kind=kreal), intent(inout) :: added(nnod,3)
!
!
!$omp workshare
      added(1:nnod,1) = vector1(1:nnod,1) + vector2(1:nnod,1)
      added(1:nnod,2) = vector1(1:nnod,2) + vector2(1:nnod,2)
      added(1:nnod,3) = vector1(1:nnod,3) + vector2(1:nnod,3)
!$omp end workshare nowait
!
       end subroutine add_vectors_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_tensors_smp(nnod, tensor1, tensor2, added)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: tensor1(nnod,6), tensor2(nnod,6)
!
      real (kind=kreal), intent(inout) :: added(nnod,6)
!
!
!$omp workshare
      added(1:nnod,1) = tensor1(1:nnod,1) + tensor2(1:nnod,1)
      added(1:nnod,2) = tensor1(1:nnod,2) + tensor2(1:nnod,2)
      added(1:nnod,3) = tensor1(1:nnod,3) + tensor2(1:nnod,3)
      added(1:nnod,4) = tensor1(1:nnod,4) + tensor2(1:nnod,4)
      added(1:nnod,5) = tensor1(1:nnod,5) + tensor2(1:nnod,5)
      added(1:nnod,6) = tensor1(1:nnod,6) + tensor2(1:nnod,6)
!$omp end workshare nowait
!
      end subroutine add_tensors_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_const_to_scalar_smp(nnod, scalar, const, added)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: scalar(nnod), const
!
      real (kind=kreal), intent(inout) :: added(nnod)
!
!
!$omp workshare
      added(1:nnod) = scalar(1:nnod) + const
!$omp end workshare nowait
!
      end subroutine add_const_to_scalar_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_const_to_vector_smp(nnod, vector, const, added)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: vector(nnod,3), const(3)
!
      real (kind=kreal), intent(inout) :: added(nnod,3)
!
!
!$omp workshare
      added(1:nnod,1) = vector(1:nnod,1) + const(1)
      added(1:nnod,2) = vector(1:nnod,2) + const(2)
      added(1:nnod,3) = vector(1:nnod,3) + const(3)
!$omp end workshare nowait
!
       end subroutine add_const_to_vector_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_const_to_tensor_smp(nnod, tensor, const, added)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: tensor(nnod,6), const(6)
!
      real (kind=kreal), intent(inout) :: added(nnod,6)
!
!$omp workshare
      added(1:nnod,1) = tensor(1:nnod,1) + const(1)
      added(1:nnod,2) = tensor(1:nnod,2) + const(2)
      added(1:nnod,3) = tensor(1:nnod,3) + const(3)
      added(1:nnod,4) = tensor(1:nnod,4) + const(4)
      added(1:nnod,5) = tensor(1:nnod,5) + const(5)
      added(1:nnod,6) = tensor(1:nnod,6) + const(6)
!$omp end workshare nowait
!
      end subroutine add_const_to_tensor_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_const_to_scalar_smp_ow(nnod, scalar, const)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: const
!
      real (kind=kreal), intent(inout) :: scalar(nnod)
!
!
!$omp workshare
      scalar(1:nnod) = scalar(1:nnod) + const
!$omp end workshare nowait
!
       end subroutine add_const_to_scalar_smp_ow
!
!-----------------------------------------------------------------------
!
      subroutine add_const_to_vector_smp_ow(nnod, vector, const)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: const(3)
!
      real (kind=kreal), intent(inout) :: vector(nnod,3)
!
!
!$omp workshare
      vector(1:nnod,1) = vector(1:nnod,1) + const(1)
      vector(1:nnod,2) = vector(1:nnod,2) + const(2)
      vector(1:nnod,3) = vector(1:nnod,3) + const(3)
!$omp end workshare nowait
!
       end subroutine add_const_to_vector_smp_ow
!
!-----------------------------------------------------------------------
!
      subroutine add_const_to_tensor_smp_ow(nnod, tensor, const)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: const(6)
!
      real (kind=kreal), intent(inout) :: tensor(nnod,6)
!
!$omp workshare
      tensor(1:nnod,1) = tensor(1:nnod,1) + const(1)
      tensor(1:nnod,2) = tensor(1:nnod,2) + const(2)
      tensor(1:nnod,3) = tensor(1:nnod,3) + const(3)
      tensor(1:nnod,4) = tensor(1:nnod,4) + const(4)
      tensor(1:nnod,5) = tensor(1:nnod,5) + const(5)
      tensor(1:nnod,6) = tensor(1:nnod,6) + const(6)
!$omp end workshare nowait
!
      end subroutine add_const_to_tensor_smp_ow
!
!-----------------------------------------------------------------------
!
      end module cal_add_smp
