!>@file   copy_to_send_buf_tri.f90
!!@brief  module copy_to_send_buf_tri
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine copy_to_send_buf_3x1(nnod_org,                       &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X1_org, X2_org, X3_org, WS)
!!      subroutine copy_to_send_buf_3x2(nnod_org,                       &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X1_org, X2_org, X3_org, WS)
!!      subroutine copy_to_send_buf_3x3(nnod_org,                       &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X1_org, X2_org, X3_org, WS)
!!      subroutine copy_to_send_buf_3x6(nnod_org,                       &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X1_org, X2_org, X3_org, WS)
!!      subroutine copy_to_send_buf_3xN(NB, nnod_org,                   &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X1_org, X2_org, X3_org, WS)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  nnod_send   Number of data points to send
!!@n @param  id_pe_send(npe_send)      Process ID to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n
!!@n @param  X_org(NB*nnod_org)   Send data
!
      module copy_to_send_buf_tri
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
      subroutine copy_to_send_buf_3x1(nnod_org,                         &
     &          npe_send, nnod_send, istack_send,                       &
     &          X1_org, X2_org, X3_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X1_org(nnod_org)
      real (kind=kreal), intent(in)::    X2_org(nnod_org)
      real (kind=kreal), intent(in)::    X3_org(nnod_org)
!
      real (kind=kreal), intent(inout):: WS(3*nnod_send)
!
      integer (kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = 1, istack_send(npe_send)
          WS(3*k-2)= X1_org(k  )
          WS(3*k-1)= X2_org(k  )
          WS(3*k  )= X3_org(k  )
      end do
!$omp end parallel do
!
      end subroutine copy_to_send_buf_3x1
!
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_3x2(nnod_org,                         &
     &          npe_send, nnod_send, istack_send,                       &
     &          X1_org, X2_org, X3_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X1_org(2*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(2*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(2*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(6*nnod_send)
!
      integer (kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = 1, istack_send(npe_send)
          WS(6*k-5)= X1_org(2*k-1)
          WS(6*k-4)= X1_org(2*k  )
          WS(6*k-3)= X2_org(2*k-1)
          WS(6*k-2)= X2_org(2*k  )
          WS(6*k-1)= X3_org(2*k-1)
          WS(6*k  )= X3_org(2*k  )
      end do
!$omp end parallel do
!
      end subroutine copy_to_send_buf_3x2
!
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_3x3(nnod_org,                         &
     &          npe_send, nnod_send, istack_send,                       &
     &          X1_org, X2_org, X3_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X1_org(3*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(3*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(3*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(9*nnod_send)
!
      integer (kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = 1, istack_send(npe_send)
          WS(9*k-8)= X1_org(3*k-2)
          WS(9*k-7)= X1_org(3*k-1)
          WS(9*k-6)= X1_org(3*k  )
          WS(9*k-5)= X2_org(3*k-2)
          WS(9*k-4)= X2_org(3*k-1)
          WS(9*k-3)= X2_org(3*k  )
          WS(9*k-2)= X3_org(3*k-2)
          WS(9*k-1)= X3_org(3*k-1)
          WS(9*k  )= X3_org(3*k  )
      end do
!$omp end parallel do
!
      end subroutine copy_to_send_buf_3x3
!
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_3x6(nnod_org,                         &
     &          npe_send, nnod_send, istack_send,                       &
     &          X1_org, X2_org, X3_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X1_org(6*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(6*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(6*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(18*nnod_send)
!
      integer (kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = 1, istack_send(npe_send)
        WS(18*k-17)= X1_org(6*k-5)
        WS(18*k-16)= X1_org(6*k-4)
        WS(18*k-15)= X1_org(6*k-3)
        WS(18*k-14)= X1_org(6*k-2)
        WS(18*k-13)= X1_org(6*k-1)
        WS(18*k-12)= X1_org(6*k  )
        WS(18*k-11)= X2_org(6*k-5)
        WS(18*k-10)= X2_org(6*k-4)
        WS(18*k- 9)= X2_org(6*k-3)
        WS(18*k- 8)= X2_org(6*k-2)
        WS(18*k- 7)= X2_org(6*k-1)
        WS(18*k- 6)= X2_org(6*k  )
        WS(18*k- 5)= X3_org(6*k-5)
        WS(18*k- 4)= X3_org(6*k-4)
        WS(18*k- 3)= X3_org(6*k-3)
        WS(18*k- 2)= X3_org(6*k-2)
        WS(18*k- 1)= X3_org(6*k-1)
        WS(18*k   )= X3_org(6*k  )
      end do
!$omp end parallel do
!
      end subroutine copy_to_send_buf_3x6
!
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_3xN(NB, nnod_org,                     &
     &          npe_send, nnod_send, istack_send,                       &
     &          X1_org, X2_org, X3_org, WS)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X1_org(NB*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(NB*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(3*NB*nnod_send)
!
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, nd, jj, kk
!
!
!$omp parallel private(nd,neib,ist,num)
      do neib = 1, npe_send
        ist = istack_send(neib-1)
        num = istack_send(neib  ) - istack_send(neib-1)
        do nd = 1, NB
!$omp do private(k,jj,kk)
          do k = 1, num
            jj = nd + (k+ist - 1) * NB
            kk = k + (nd-1)*num + NB*ist
            WS(kk         ) = X1_org(jj)
            WS(kk+  nd*num) = X2_org(jj)
            WS(kk+2*nd*num) = X3_org(jj)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine copy_to_send_buf_3xN
!
! ----------------------------------------------------------------------
!
      end module copy_to_send_buf_tri
