!>@file   copy_field_smp.f90
!!@brief  module copy_field_smp
!!
!!@author H. Matsui
!!@date Programmed in June, 2009
!
!> @brief subroutine to copy field data
!!
!!@verbatim
!!      subroutine copy_nod_scalar_smp(np_smp, numnod, inod_smp_stack,  &
!!     &          scalar, copied)
!!      subroutine copy_nod_vector_smp(np_smp, numnod, inod_smp_stack,  &
!!     &          vector, copied)
!!      subroutine copy_nod_sym_tensor_smp(np_smp, numnod,              &
!!     &          inod_smp_stack, tensor, copied)
!!
!!      subroutine copy_nod_integer_smp(np_smp, numnod, inod_smp_stack, &
!!     &          int_scalar, int_copied)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!
!!@n @param  scalar(numnod)     original scalar field
!!@n @param  vector(numnod,3)   original vector field
!!@n @param  tensor(numnod,6)   original symmetric tensor field
!!@n @param  int_scalar(numnod) original integer field
!!
!!@n @param  copied(numnod,n) copied field data
!!@n @param  int_copied(numnod) copied integer field data
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
      subroutine copy_nod_scalar_smp(np_smp, numnod, inod_smp_stack,    &
     &          scalar, copied)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: scalar(numnod)
       real(kind=kreal), intent(inout) :: copied(numnod)
!
       integer (kind = kint) :: ip, ist, ied
!
!$omp do private(ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         copied(ist:ied) = scalar(ist:ied)
      end do
!$omp end do nowait
!
      end subroutine copy_nod_scalar_smp
!
! -----------------------------------------------------------------------
!
      subroutine copy_nod_vector_smp(np_smp, numnod, inod_smp_stack,    &
     &          vector, copied)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: vector(numnod,3)
       real(kind=kreal), intent(inout) :: copied(numnod,3)
!
       integer (kind = kint) :: ip, ist, ied
!
!$omp do private(ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         copied(ist:ied,1) = vector(ist:ied,1)
         copied(ist:ied,2) = vector(ist:ied,2)
         copied(ist:ied,3) = vector(ist:ied,3)
      end do
!$omp end do nowait
!
      end subroutine copy_nod_vector_smp
!
! -----------------------------------------------------------------------
!
      subroutine copy_nod_sym_tensor_smp(np_smp, numnod,                &
     &          inod_smp_stack, tensor, copied)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: tensor(numnod,6)
       real(kind=kreal), intent(inout) :: copied(numnod,6)
!
       integer (kind = kint) :: ip, ist, ied
!
!$omp do private(ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         copied(ist:ied,1) = tensor(ist:ied,1)
         copied(ist:ied,2) = tensor(ist:ied,2)
         copied(ist:ied,3) = tensor(ist:ied,3)
         copied(ist:ied,4) = tensor(ist:ied,4)
         copied(ist:ied,5) = tensor(ist:ied,5)
         copied(ist:ied,6) = tensor(ist:ied,6)
      end do
!$omp end do nowait
!
      end subroutine copy_nod_sym_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_nod_integer_smp(np_smp, numnod, inod_smp_stack,   &
     &          int_scalar, int_copied)
!
       integer (kind = kint) :: np_smp, numnod
       integer (kind = kint) :: inod_smp_stack(0:np_smp)
       integer (kind = kint), intent(in)    :: int_scalar(numnod)
       integer (kind = kint), intent(inout) :: int_copied(numnod)
!
       integer (kind = kint) :: ip, ist, ied
!
!$omp do private(ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         int_copied(ist:ied) = int_scalar(ist:ied)
      end do
!$omp end do nowait
!
      end subroutine copy_nod_integer_smp
!
! -----------------------------------------------------------------------
!
      end module copy_field_smp
