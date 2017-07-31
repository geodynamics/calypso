!>@file   copy_field_smp.f90
!!@brief  module copy_field_smp
!!
!!@author H. Matsui
!!@date Programmed in June, 2009
!
!> @brief subroutine to copy field data
!!
!!@verbatim
!!      subroutine copy_nod_scalar_smp(nnod, scalar, copied)
!!      subroutine copy_nod_vector_smp(nnod, vector, copied)
!!      subroutine copy_nod_sym_tensor_smp(nnod, tensor, copied)
!!      subroutine copy_all_field_smp(nnod, ntot_comp, vector, copied)
!!
!!      subroutine copy_nod_integer_smp(nnod, int_scalar, int_copied)
!!@endverbatim
!!
!!@n @param  nnod   Number of data points
!!
!!@n @param  scalar(nnod)     original scalar field
!!@n @param  vector(nnod,3)   original vector field
!!@n @param  tensor(nnod,6)   original symmetric tensor field
!!@n @param  int_scalar(nnod) original integer field
!!
!!@n @param  copied(nnod,n) copied field data
!!@n @param  int_copied(nnod) copied integer field data
!
      module copy_field_smp
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
      subroutine copy_nod_scalar_smp(nnod, scalar, copied)
!
       integer (kind = kint) :: nnod
       real(kind=kreal), intent(in)    :: scalar(nnod)
       real(kind=kreal), intent(inout) :: copied(nnod)
!
!
!$omp workshare
       copied(1:nnod) = scalar(1:nnod)
!$omp end workshare nowait
!
      end subroutine copy_nod_scalar_smp
!
! -----------------------------------------------------------------------
!
      subroutine copy_nod_vector_smp(nnod, vector, copied)
!
       integer (kind = kint) :: nnod
       real(kind=kreal), intent(in)    :: vector(nnod,3)
       real(kind=kreal), intent(inout) :: copied(nnod,3)
!
!
!$omp workshare
       copied(1:nnod,1) = vector(1:nnod,1)
       copied(1:nnod,2) = vector(1:nnod,2)
       copied(1:nnod,3) = vector(1:nnod,3)
!$omp end workshare nowait
!
      end subroutine copy_nod_vector_smp
!
! -----------------------------------------------------------------------
!
      subroutine copy_nod_sym_tensor_smp(nnod, tensor, copied)
!
       integer (kind = kint) :: nnod
       real(kind=kreal), intent(in)    :: tensor(nnod,6)
       real(kind=kreal), intent(inout) :: copied(nnod,6)
!
!
!$omp workshare
       copied(1:nnod,1) = tensor(1:nnod,1)
       copied(1:nnod,2) = tensor(1:nnod,2)
       copied(1:nnod,3) = tensor(1:nnod,3)
       copied(1:nnod,4) = tensor(1:nnod,4)
       copied(1:nnod,5) = tensor(1:nnod,5)
       copied(1:nnod,6) = tensor(1:nnod,6)
!$omp end workshare nowait
!
      end subroutine copy_nod_sym_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine copy_all_field_smp(nnod, ntot_comp, vector, copied)
!
       integer (kind = kint) :: nnod, ntot_comp
       real(kind=kreal), intent(in)    :: vector(nnod,ntot_comp)
       real(kind=kreal), intent(inout) :: copied(nnod,ntot_comp)
!
!
!$omp workshare
       copied(1:nnod,1:ntot_comp) = vector(1:nnod,1:ntot_comp)
!$omp end workshare nowait
!
      end subroutine copy_all_field_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_nod_integer_smp(nnod, int_scalar, int_copied)
!
       integer (kind = kint) :: nnod
       integer (kind = kint), intent(in)    :: int_scalar(nnod)
       integer (kind = kint), intent(inout) :: int_copied(nnod)
!
!
!$omp workshare
       int_copied(1:nnod) = int_scalar(1:nnod)
!$omp end workshare nowait
!
      end subroutine copy_nod_integer_smp
!
! -----------------------------------------------------------------------
!
      end module copy_field_smp
