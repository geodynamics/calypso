!>@file   cal_subtract_smp.f90
!!@brief  module cal_subtract_smp
!!
!!@author H. Matsui
!!@date Programmed...when??
!
!>@brief subroutines to subtract field data
!!@n      Need $omp parallel to use these routines
!!
!!@verbatim
!!      subroutine subtract_scalars_smp(nnod, source, subt, sol)
!!      subroutine subtract_vectors_smp(nnod, source, subt, sol)
!!      subroutine subtract_tensors_smp(nnod, source, subt, sol)
!!         sol(inod,:) =  source(inod,:) - subt(inod,i_v2)
!!
!!      subroutine subtruct_const_4_scalar_smp                          &
!!     &         (nnod, dest, scalar, const)
!!      subroutine subtruct_const_4_vect_smp(nnod, dest, vector, const)
!!      subroutine subtruct_const_4_tensor_smp                          &
!!     &         (nnod, dest, tensor, const)
!!
!!      subroutine subtruct_const_4_scalar_smp_ow(nnod, scalar, const)
!!      subroutine subtruct_const_4_vect_smp_ow(nnod, vector, const)
!!      subroutine subtruct_const_4_tensor_smp_ow(nnod, tensor, const)
!!
!!      subroutine subt_const_to_vector_smp_ow(nnod, vector, const)
!!      subroutine subt_const_to_tensor_smp_ow(nnod, tensor, const)
!!@endverbatim
!!
!!@n @param  nnod     Number of data points
!!
!!@n @param  source(nnod,:)      field to be subtructed
!!@n @param  subt(nnod,:)        field to subtract
!!@n @param  sol(nnod,:)         Solution
!
      module cal_subtract_smp
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
      subroutine subtract_scalars_smp(nnod, source, subt, sol)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: source(nnod),  subt(nnod)
      real (kind=kreal), intent(inout) :: sol(nnod)
!
!
!$omp workshare
      sol(1:nnod) = source(1:nnod) - subt(1:nnod)
!$omp end workshare nowait
!
      end subroutine subtract_scalars_smp
!
!-----------------------------------------------------------------------
!
      subroutine subtract_vectors_smp(nnod, source, subt, sol)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: source(nnod,3),  subt(nnod,3)
      real (kind=kreal), intent(inout) :: sol(nnod,3)
!
!
!$omp workshare
      sol(1:nnod,1) = source(1:nnod,1) - subt(1:nnod,1)
      sol(1:nnod,2) = source(1:nnod,2) - subt(1:nnod,2)
      sol(1:nnod,3) = source(1:nnod,3) - subt(1:nnod,3)
!$omp end workshare nowait
!
      end subroutine subtract_vectors_smp
!
!-----------------------------------------------------------------------
!
      subroutine subtract_tensors_smp(nnod, source, subt, sol)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: source(nnod,6),  subt(nnod,6)
      real (kind=kreal), intent(inout) :: sol(nnod,6)
!
!
!$omp workshare
      sol(1:nnod,1) = source(1:nnod,1) - subt(1:nnod,1)
      sol(1:nnod,2) = source(1:nnod,2) - subt(1:nnod,2)
      sol(1:nnod,3) = source(1:nnod,3) - subt(1:nnod,3)
      sol(1:nnod,4) = source(1:nnod,4) - subt(1:nnod,4)
      sol(1:nnod,5) = source(1:nnod,5) - subt(1:nnod,5)
      sol(1:nnod,6) = source(1:nnod,6) - subt(1:nnod,6)
!$omp end workshare nowait
!
      end subroutine subtract_tensors_smp
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_scalar_smp                            &
     &         (nnod, dest, scalar, const)
!
       integer (kind = kint), intent(in) :: nnod
       real(kind=kreal), intent(in)    :: const
       real(kind=kreal), intent(in) :: scalar(nnod)
!
       real(kind=kreal), intent(inout) :: dest(nnod)
!
!
!$omp workshare
       dest(1:nnod) = scalar(1:nnod) - const
!$omp end workshare nowait
!
      end subroutine subtruct_const_4_scalar_smp
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_vect_smp(nnod, dest, vector, const)
!
       integer (kind = kint), intent(in) :: nnod
       real(kind=kreal), intent(in)    :: const
       real(kind=kreal), intent(in) :: vector(nnod,3)
!
       real(kind=kreal), intent(inout) :: dest(nnod,3)
!
!
!$omp workshare
       dest(1:nnod,1) = vector(1:nnod,1) - const
       dest(1:nnod,2) = vector(1:nnod,2) - const
       dest(1:nnod,3) = vector(1:nnod,3) - const
!$omp end workshare nowait
!
      end subroutine subtruct_const_4_vect_smp
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_tensor_smp                            &
     &         (nnod, dest, tensor, const)
!
       integer (kind = kint), intent(in) :: nnod
       real(kind=kreal), intent(in)    :: const
       real(kind=kreal), intent(in) :: tensor(nnod,6)
!
       real(kind=kreal), intent(inout) :: dest(nnod,6)
!
!
!$omp workshare
       dest(1:nnod,1) = tensor(1:nnod,1) - const
       dest(1:nnod,2) = tensor(1:nnod,2) - const
       dest(1:nnod,3) = tensor(1:nnod,3) - const
       dest(1:nnod,4) = tensor(1:nnod,4) - const
       dest(1:nnod,5) = tensor(1:nnod,5) - const
       dest(1:nnod,6) = tensor(1:nnod,6) - const
!$omp end workshare nowait
!
      end subroutine subtruct_const_4_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_scalar_smp_ow(nnod, scalar, const)
!
      integer (kind=kint), intent(in) :: nnod
       real(kind=kreal), intent(in)    :: const
!
       real(kind=kreal), intent(inout) :: scalar(nnod)
!
!
!$omp workshare
       scalar(1:nnod) = scalar(1:nnod) - const
!$omp end workshare nowait
!
      end subroutine subtruct_const_4_scalar_smp_ow
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_vect_smp_ow(nnod, vector, const)
!
      integer (kind=kint), intent(in) :: nnod
       real(kind=kreal), intent(in)    :: const
!
       real(kind=kreal), intent(inout) :: vector(nnod,3)
!
!
!$omp workshare
       vector(1:nnod,1) = vector(1:nnod,1) - const
       vector(1:nnod,2) = vector(1:nnod,2) - const
       vector(1:nnod,3) = vector(1:nnod,3) - const
!$omp end workshare nowait
!
      end subroutine subtruct_const_4_vect_smp_ow
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_tensor_smp_ow(nnod, tensor, const)
!
      integer (kind=kint), intent(in) :: nnod
      real(kind=kreal), intent(in)    :: const
!
      real(kind=kreal), intent(inout) :: tensor(nnod,6)
!
!
!$omp workshare
      tensor(1:nnod,1) = tensor(1:nnod,1) - const
      tensor(1:nnod,2) = tensor(1:nnod,2) - const
      tensor(1:nnod,3) = tensor(1:nnod,3) - const
      tensor(1:nnod,4) = tensor(1:nnod,4) - const
      tensor(1:nnod,5) = tensor(1:nnod,5) - const
      tensor(1:nnod,6) = tensor(1:nnod,6) - const
!$omp end workshare nowait
!
      end subroutine subtruct_const_4_tensor_smp_ow
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine subt_const_to_vector_smp_ow(nnod, vector, const)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: const(3)
!
      real (kind=kreal), intent(inout) :: vector(nnod,3)
!
!$omp workshare
      vector(1:nnod,1) = vector(1:nnod,1) - const(1)
      vector(1:nnod,2) = vector(1:nnod,2) - const(2)
      vector(1:nnod,3) = vector(1:nnod,3) - const(3)
!$omp end workshare nowait
!
      end subroutine subt_const_to_vector_smp_ow
!
!-----------------------------------------------------------------------
!
      subroutine subt_const_to_tensor_smp_ow(nnod, tensor, const)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: const(6)
!
      real (kind=kreal), intent(inout) :: tensor(nnod,6)
!
!$omp workshare
      tensor(1:nnod,1) = tensor(1:nnod,1) - const(1)
      tensor(1:nnod,2) = tensor(1:nnod,2) - const(2)
      tensor(1:nnod,3) = tensor(1:nnod,3) - const(3)
      tensor(1:nnod,4) = tensor(1:nnod,4) - const(4)
      tensor(1:nnod,5) = tensor(1:nnod,5) - const(5)
      tensor(1:nnod,6) = tensor(1:nnod,6) - const(6)
!$omp end workshare nowait
!
      end subroutine subt_const_to_tensor_smp_ow
!
!-----------------------------------------------------------------------
!
      end module cal_subtract_smp
