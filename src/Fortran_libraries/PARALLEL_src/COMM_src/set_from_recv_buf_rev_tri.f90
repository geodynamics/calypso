!>@file   set_from_recv_buf_rev_tri.f90
!!@brief  module set_from_recv_buf_rev_tri
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine set_from_recv_buf_rev_3x1(nnod_new,                  &
!!     &          nnod_recv, irev_import, WR, X1_new, X2_new, X3_new)
!!      subroutine set_from_recv_buf_rev_3x2(nnod_new,                  &
!!     &          nnod_recv, irev_import, WR, X1_new, X2_new, X3_new)
!!      subroutine set_from_recv_buf_rev_3x3(nnod_new,                  &
!!     &          nnod_recv, irev_import, WR, X1_new, X2_new, X3_new)
!!      subroutine set_from_recv_buf_rev_3x6(nnod_new,                  &
!!     &          nnod_recv, irev_import, WR, X1_new, X2_new, X3_new)
!!      subroutine set_from_recv_buf_rev_3xN(NB, nnod_new,              &
!!     &          nnod_recv, irev_import, WR, X1_new, X2_new, X3_new)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_new    Number of components for destination
!!
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  nnod_recv   Number of data points to receive
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_new(NB*nnod_new)   Received data
!
      module set_from_recv_buf_rev_tri
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
      subroutine set_from_recv_buf_rev_3x1(nnod_new,                    &
     &          nnod_recv, irev_import, WR, X1_new, X2_new, X3_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(3*(nnod_recv+1))
!
      real (kind=kreal), intent(inout):: X1_new(nnod_new)
      real (kind=kreal), intent(inout):: X2_new(nnod_new)
      real (kind=kreal), intent(inout):: X3_new(nnod_new)
!
      integer (kind = kint) :: k, j
!
!
      WR(3*nnod_recv+1) = 0.0d0
      WR(3*nnod_recv+2) = 0.0d0
      WR(3*nnod_recv+3) = 0.0d0
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        X1_new(k  ) = WR(3*j-2)
        X2_new(k  ) = WR(3*j-1)
        X3_new(k  ) = WR(3*j  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_rev_3x1
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_rev_3x2(nnod_new,                    &
     &          nnod_recv, irev_import, WR, X1_new, X2_new, X3_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(6*(nnod_recv+1))
!
      real (kind=kreal), intent(inout):: X1_new(2*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(2*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(2*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
      do k = 1, 6
        WR(6*nnod_recv+k) = 0.0d0
      end do
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        X1_new(2*k-1) = WR(6*j-5)
        X1_new(2*k  ) = WR(6*j-4)
        X2_new(2*k-1) = WR(6*j-3)
        X2_new(2*k  ) = WR(6*j-2)
        X3_new(2*k-1) = WR(6*j-1)
        X3_new(2*k  ) = WR(6*j  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_rev_3x2
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_rev_3x3(nnod_new,                    &
     &          nnod_recv, irev_import, WR, X1_new, X2_new, X3_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(9*(nnod_recv+1))
!
      real (kind=kreal), intent(inout):: X1_new(3*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(3*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(3*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
      do k = 1, 9
        WR(9*nnod_recv+k) = 0.0d0
      end do
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        X1_new(3*k-2) = WR(9*j-8)
        X1_new(3*k-1) = WR(9*j-7)
        X1_new(3*k  ) = WR(9*j-6)
        X2_new(3*k-2) = WR(9*j-5)
        X2_new(3*k-1) = WR(9*j-4)
        X2_new(3*k  ) = WR(9*j-3)
        X3_new(3*k-2) = WR(9*j-2)
        X3_new(3*k-1) = WR(9*j-1)
        X3_new(3*k  ) = WR(9*j  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_rev_3x3
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_rev_3x6(nnod_new,                    &
     &          nnod_recv, irev_import, WR, X1_new, X2_new, X3_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(18*(nnod_recv+1))
!
      real (kind=kreal), intent(inout):: X1_new(6*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(6*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(6*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
      do k = 1, 18
        WR(18*nnod_recv+k) = 0.0d0
      end do
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        X1_new(6*k-5) = WR(18*j-17)
        X1_new(6*k-4) = WR(18*j-16)
        X1_new(6*k-3) = WR(18*j-15)
        X1_new(6*k-2) = WR(18*j-14)
        X1_new(6*k-1) = WR(18*j-13)
        X1_new(6*k  ) = WR(18*j-12)
        X2_new(6*k-5) = WR(18*j-11)
        X2_new(6*k-4) = WR(18*j-10)
        X2_new(6*k-3) = WR(18*j- 9)
        X2_new(6*k-2) = WR(18*j- 8)
        X2_new(6*k-1) = WR(18*j- 7)
        X2_new(6*k  ) = WR(18*j- 6)
        X3_new(6*k-5) = WR(18*j- 5)
        X3_new(6*k-4) = WR(18*j- 4)
        X3_new(6*k-3) = WR(18*j- 3)
        X3_new(6*k-2) = WR(18*j- 2)
        X3_new(6*k-1) = WR(18*j- 1)
        X3_new(6*k  ) = WR(18*j   )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_rev_3x6
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_rev_3xN(NB, nnod_new,                &
     &          nnod_recv, irev_import, WR, X1_new, X2_new, X3_new)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(3*NB*(nnod_recv+1))
!
      real (kind=kreal), intent(inout):: X1_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(NB*nnod_new)
!
      integer (kind = kint) :: k, kk, jj, nd
!
!
      do k = 1, 3*NB
        WR(k+3*NB*nnod_recv) = 0.0d0
      end do
!
!$omp parallel do private(k,jj,kk,nd)
      do kk = 1, NB*nnod_new
        nd = 1 + mod(kk-1,NB)
        k =  1 + (kk-nd) / NB
        jj = NB*(irev_import(k)-1) + nd
        X1_new(kk) = WR(3*jj-2)
        X2_new(kk) = WR(3*jj-1)
        X3_new(kk) = WR(3*jj  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_rev_3xN
!
! ----------------------------------------------------------------------
!
      end module set_from_recv_buf_rev_tri
