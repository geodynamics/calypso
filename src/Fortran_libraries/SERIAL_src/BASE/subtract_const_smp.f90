!>@file   subtract_const_smp.f90
!!@brief  module subtract_const_smp
!!
!!@author H. Matsui
!!@date Programmed...when??
!
!>@brief  subtract constant from field data
!!@n      need $omp parallel to use these routines
!!
!!@verbatim
!!      subroutine subtruct_const_4_scalar_smp(np_smp, nnod,          &
!!     &          inod_smp_stack, dest, scalar, const)
!!      subroutine subtruct_const_4_vect_smp(np_smp, nnod,            &
!!     &          inod_smp_stack, dest, vector, const)
!!      subroutine subtruct_const_4_tensor_smp(np_smp, nnod,          &
!!     &          inod_smp_stack, dest, tensor, const)
!!
!!      subroutine subtruct_const_4_scalar_smp_ow(np_smp, nnod,       &
!!     &          inod_smp_stack, scalar, const)
!!      subroutine subtruct_const_4_vect_smp_ow(np_smp, nnod,         &
!!     &          inod_smp_stack, vector, const)
!!      subroutine subtruct_const_4_tensor_smp_ow(np_smp, nnod,       &
!!     &          inod_smp_stack, tensor, const)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  nnod     Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!
!!@n @param  scalar(nnod)     Input scalar data 1
!!@n @param  vector(nnod,3)   Input vector data 1
!!@n @param  tensor(nnod,6)   Input symmetric tensor data 1
!!@n @param  const            Constant (scalar)
!!
!!@n @param  dest(nnod,NB)    Solution
!!                      (scalar, vector, or symmetric tensor)
!
      module subtract_const_smp
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
      subroutine subtruct_const_4_scalar_smp(np_smp, nnod,              &
     &          inod_smp_stack, dest, scalar, const)
!
       integer (kind = kint), intent(in) :: np_smp, nnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
       real(kind=kreal), intent(in) :: scalar(nnod)
!
       real(kind=kreal), intent(inout) :: dest(nnod)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           dest(inod) = scalar(inod) - const
         end do
       end do
!$omp end do nowait
!
      end subroutine subtruct_const_4_scalar_smp
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_vect_smp(np_smp, nnod,                &
     &          inod_smp_stack, dest, vector, const)
!
       integer (kind = kint), intent(in) :: np_smp, nnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
       real(kind=kreal), intent(in) :: vector(nnod,3)
!
       real(kind=kreal), intent(inout) :: dest(nnod,3)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           dest(inod,1) = vector(inod,1) - const
           dest(inod,2) = vector(inod,2) - const
           dest(inod,3) = vector(inod,3) - const
         end do
       end do
!$omp end do nowait
!
      end subroutine subtruct_const_4_vect_smp
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_tensor_smp(np_smp, nnod,              &
     &          inod_smp_stack, dest, tensor, const)
!
       integer (kind = kint), intent(in) :: np_smp, nnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
       real(kind=kreal), intent(in) :: tensor(nnod,6)
!
       real(kind=kreal), intent(inout) :: dest(nnod,6)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           dest(inod,1) = tensor(inod,1) - const
           dest(inod,2) = tensor(inod,2) - const
           dest(inod,3) = tensor(inod,3) - const
           dest(inod,4) = tensor(inod,4) - const
           dest(inod,5) = tensor(inod,5) - const
           dest(inod,6) = tensor(inod,6) - const
         end do
       end do
!$omp end do nowait
!
      end subroutine subtruct_const_4_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_scalar_smp_ow(np_smp, nnod,           &
     &          inod_smp_stack, scalar, const)
!
       integer (kind = kint), intent(in) :: np_smp, nnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
!
       real(kind=kreal), intent(inout) :: scalar(nnod)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           scalar(inod) = scalar(inod) - const
         end do
       end do
!$omp end do nowait
!
      end subroutine subtruct_const_4_scalar_smp_ow
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_vect_smp_ow(np_smp, nnod,             &
     &          inod_smp_stack, vector, const)
!
       integer (kind = kint), intent(in) :: np_smp, nnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
!
       real(kind=kreal), intent(inout) :: vector(nnod,3)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           vector(inod,1) = vector(inod,1) - const
           vector(inod,2) = vector(inod,2) - const
           vector(inod,3) = vector(inod,3) - const
         end do
       end do
!$omp end do nowait
!
      end subroutine subtruct_const_4_vect_smp_ow
!
! -----------------------------------------------------------------------
!
      subroutine subtruct_const_4_tensor_smp_ow(np_smp, nnod,           &
     &          inod_smp_stack, tensor, const)
!
       integer (kind = kint), intent(in) :: np_smp, nnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: const
!
       real(kind=kreal), intent(inout) :: tensor(nnod,6)
!
       integer (kind = kint) :: ip, ist, ied, inod
!
!$omp do private(ist,ied,inod)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist,ied
           tensor(inod,1) = tensor(inod,1) - const
           tensor(inod,2) = tensor(inod,2) - const
           tensor(inod,3) = tensor(inod,3) - const
           tensor(inod,4) = tensor(inod,4) - const
           tensor(inod,5) = tensor(inod,5) - const
           tensor(inod,6) = tensor(inod,6) - const
         end do
      end do
!$omp end do nowait
!
      end subroutine subtruct_const_4_tensor_smp_ow
!
! -----------------------------------------------------------------------
!
      end module subtract_const_smp
