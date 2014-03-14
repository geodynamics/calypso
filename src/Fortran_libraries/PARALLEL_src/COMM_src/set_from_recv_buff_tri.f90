!>@file   set_from_recv_buff_tri.f90
!!@brief  module set_from_recv_buff_tri
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine set_from_recv_buf_3x1(nnod_new,                      &
!!     &          nnod_recv, inod_import, WR, X1_new, X2_new, X3_new)
!!      subroutine set_from_recv_buf_3x2(nnod_new,                      &
!!     &          nnod_recv, inod_import, WR, X1_new, X2_new, X3_new)
!!      subroutine set_from_recv_buf_3x3(nnod_new,                      &
!!     &          nnod_recv, inod_import, WR, X1_new, X2_new, X3_new)
!!      subroutine set_from_recv_buf_3x6(nnod_new,                      &
!!     &          nnod_recv, inod_import, WR, X1_new, X2_new, X3_new)
!!      subroutine set_from_recv_buf_3xN(NB, nnod_new,                  &
!!     &          npe_recv, nnod_recv, istack_recv, inod_import,        &
!!     &          WR, X1_new, X2_new, X3_new)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  nnod_recv   Number of data points to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  inod_import(nnod_recv)
!!                    local node ID to copy from receive buffer
!!@n
!!@n @param  X_new(NB*nnod_new)   Received data
!
      module set_from_recv_buff_tri
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
      subroutine set_from_recv_buf_3x1(nnod_new,                        &
     &          nnod_recv, inod_import, WR, X1_new, X2_new, X3_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(3*nnod_recv)
!
      real (kind=kreal), intent(inout):: X1_new(nnod_new)
      real (kind=kreal), intent(inout):: X2_new(nnod_new)
      real (kind=kreal), intent(inout):: X3_new(nnod_new)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do k= 1, nnod_recv
        j = inod_import(k)
        X1_new(j  ) = WR(3*k-2)
        X2_new(j  ) = WR(3*k-1)
        X3_new(j  ) = WR(3*k  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_3x1
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_3x2(nnod_new,                        &
     &          nnod_recv, inod_import, WR, X1_new, X2_new, X3_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(6*nnod_recv)
!
      real (kind=kreal), intent(inout):: X1_new(2*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(2*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(2*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do k= 1, nnod_recv
        j = inod_import(k)
        X1_new(2*j-1) = WR(6*k-5)
        X1_new(2*j  ) = WR(6*k-4)
        X2_new(2*j-1) = WR(6*k-3)
        X2_new(2*j  ) = WR(6*k-2)
        X3_new(2*j-1) = WR(6*k-1)
        X3_new(2*j  ) = WR(6*k  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_3x2
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_3x3(nnod_new,                        &
     &          nnod_recv, inod_import, WR, X1_new, X2_new, X3_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(9*nnod_recv)
!
      real (kind=kreal), intent(inout):: X1_new(3*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(3*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(3*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do k= 1, nnod_recv
        j = inod_import(k)
        X1_new(3*j-2) = WR(9*k-8)
        X1_new(3*j-1) = WR(9*k-7)
        X1_new(3*j  ) = WR(9*k-6)
        X2_new(3*j-2) = WR(9*k-5)
        X2_new(3*j-1) = WR(9*k-4)
        X2_new(3*j  ) = WR(9*k-3)
        X3_new(3*j-2) = WR(9*k-2)
        X3_new(3*j-1) = WR(9*k-1)
        X3_new(3*j  ) = WR(9*k  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_3x3
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_3x6(nnod_new,                        &
     &          nnod_recv, inod_import, WR, X1_new, X2_new, X3_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(18*nnod_recv)
!
      real (kind=kreal), intent(inout):: X1_new(6*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(6*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(6*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do k= 1, nnod_recv
        j = inod_import(k)
        X1_new(6*j-5) = WR(18*k-17)
        X1_new(6*j-4) = WR(18*k-16)
        X1_new(6*j-3) = WR(18*k-15)
        X1_new(6*j-2) = WR(18*k-14)
        X1_new(6*j-1) = WR(18*k-13)
        X1_new(6*j  ) = WR(18*k-12)
        X2_new(6*j-5) = WR(18*k-11)
        X2_new(6*j-4) = WR(18*k-10)
        X2_new(6*j-3) = WR(18*k- 9)
        X2_new(6*j-2) = WR(18*k- 8)
        X2_new(6*j-1) = WR(18*k- 7)
        X2_new(6*j  ) = WR(18*k- 6)
        X3_new(6*j-5) = WR(18*k- 5)
        X3_new(6*j-4) = WR(18*k- 4)
        X3_new(6*j-3) = WR(18*k- 3)
        X3_new(6*j-2) = WR(18*k- 2)
        X3_new(6*j-1) = WR(18*k- 1)
        X3_new(6*j  ) = WR(18*k   )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_3x6
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_3xN(NB, nnod_new,                    &
     &          npe_recv, nnod_recv, istack_recv, inod_import,          &
     &          WR, X1_new, X2_new, X3_new)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: npe_recv, nnod_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(3*NB*nnod_recv)
!
      real (kind=kreal), intent(inout):: X1_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(NB*nnod_new)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, nd, jj, kk
!
!
!$omp parallel private(nd,neib,ist,num)
      do neib = 1, npe_recv
        ist = istack_recv(neib-1)
        num = istack_recv(neib  ) - istack_recv(neib-1)
        do nd = 1, NB
!$omp do private(k,jj,kk)
          do k = 1, num
            jj   = nd + NB * (inod_import(k+ist) - 1)
            kk   = k + (nd-1) * num + NB*ist
            X1_new(jj) = WR(kk)
            X2_new(jj) = WR(kk+  nd*num)
            X3_new(jj) = WR(kk+2*nd*num)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine set_from_recv_buf_3xN
!
! ----------------------------------------------------------------------
!
      end module set_from_recv_buff_tri
