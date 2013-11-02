!>@file   copy_to_send_buffer.f90
!!@brief  module copy_to_send_buffer
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine copy_to_send_buf_1(nnod_org,                         &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X_org, WS)
!!      subroutine copy_to_send_buf_2(nnod_org,                         &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X_org, WS)
!!      subroutine copy_to_send_buf_3(nnod_org,                         &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X_org, WS)
!!      subroutine copy_to_send_buf_6(nnod_org,                         &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X_org, WS)
!!      subroutine copy_to_send_buf_N(NB, nnod_org,                     &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X_org, WS)
!!
!!      subroutine copy_to_send_buf_int(nnod_org,                       &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          iX_org, iWS)
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
!!@n @param  inod_export(nnod_send)
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  X_org(NB*nnod_org)   Send data
!
      module copy_to_send_buffer
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
      subroutine copy_to_send_buf_1(nnod_org,                           &
     &          npe_send, nnod_send, istack_send,                       &
     &          X_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X_org(nnod_org)
!
      real (kind=kreal), intent(inout):: WS(nnod_send)
!
      integer (kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = 1, istack_send(npe_send)
          WS(k  )= X_org(k  )
      end do
!$omp end parallel do
!
      end subroutine copy_to_send_buf_1
!
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_2(nnod_org,                           &
     &          npe_send, nnod_send, istack_send,                       &
     &          X_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X_org(2*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(2*nnod_send)
!
      integer (kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = 1, istack_send(npe_send)
          WS(2*k-1)= X_org(2*k-1)
          WS(2*k  )= X_org(2*k  )
      end do
!$omp end parallel do
!
      end subroutine copy_to_send_buf_2
!
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_3(nnod_org,                           &
     &          npe_send, nnod_send, istack_send,                       &
     &          X_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X_org(3*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(3*nnod_send)
!
      integer (kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = 1, istack_send(npe_send)
          WS(3*k-2)= X_org(3*k-2)
          WS(3*k-1)= X_org(3*k-1)
          WS(3*k  )= X_org(3*k  )
      end do
!$omp end parallel do
!
      end subroutine copy_to_send_buf_3
!
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_6(nnod_org,                           &
     &          npe_send, nnod_send, istack_send,                       &
     &          X_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X_org(6*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(6*nnod_send)
!
      integer (kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = 1, istack_send(npe_send)
        WS(6*k- 5)= X_org(6*k-5)
        WS(6*k- 4)= X_org(6*k-4)
        WS(6*k- 3)= X_org(6*k-3)
        WS(6*k- 2)= X_org(6*k-2)
        WS(6*k- 1)= X_org(6*k-1)
        WS(6*k   )= X_org(6*k  )
      end do
!$omp end parallel do
!
      end subroutine copy_to_send_buf_6
!
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_N(NB, nnod_org,                       &
     &          npe_send, nnod_send, istack_send,                       &
     &          X_org, WS)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(NB*nnod_send)
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
            WS(kk         ) = X_org(jj)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine copy_to_send_buf_N
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_int(nnod_org,                         &
     &          npe_send, nnod_send, istack_send,                       &
     &          iX_org, iWS)
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      integer (kind=kint), intent(in)::    iX_org(nnod_org)
!
      integer (kind=kint), intent(inout):: iWS(nnod_send)
!
      integer (kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = 1, istack_send(npe_send)
          iWS(k  )= iX_org(k  )
      end do
!$omp end parallel do
!
      end subroutine copy_to_send_buf_int
!
! ----------------------------------------------------------------------
!
      end module copy_to_send_buffer
