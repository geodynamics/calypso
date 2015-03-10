!>@file   set_from_recv_buffer.f90
!!@brief  module set_from_recv_buffer
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine set_from_recv_buf_1(nnod_new,                        &
!!     &          nnod_recv, inod_import, WR, X_new)
!!      subroutine set_from_recv_buf_2(nnod_new,                        &
!!     &          nnod_recv, inod_import, WR, X_new)
!!      subroutine set_from_recv_buf_3(nnod_new,                        &
!!     &          nnod_recv, inod_import, WR, X_new)
!!      subroutine set_from_recv_buf_6(nnod_new,                        &
!!     &          nnod_recv, inod_import, WR, X_new)
!!      subroutine set_from_recv_buf_N(NB, nnod_new,                    &
!!     &          nnod_recv, inod_import, WR, X_new)
!!      subroutine set_from_recv_buf_N_mod(NB, nnod_new,                &
!!     &          npe_recv, nnod_recv, istack_recv, inod_import,        &
!!     &          WR, X_new)
!!
!!      subroutine set_from_recv_buf_int(nnod_new,                      &
!!     &          nnod_recv, inod_import, iWR, iX_new)
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
      module set_from_recv_buffer
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
      subroutine set_from_recv_buf_1(nnod_new,                          &
     &          nnod_recv, inod_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(nnod_recv)
!
      real (kind=kreal), intent(inout):: X_new(nnod_new)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do
      do j = 1, nnod_new
        X_new(j) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(k,j)
      do k= 1, nnod_recv
        j = inod_import(k)
        X_new(j  ) = WR(k  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_1
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_2(nnod_new,                          &
     &          nnod_recv, inod_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(2*nnod_recv)
!
      real (kind=kreal), intent(inout):: X_new(2*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do
      do j = 1, 2*nnod_new
        X_new(j) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(k,j)
      do k= 1, nnod_recv
        j = inod_import(k)
        X_new(2*j-1) = WR(2*k-1)
        X_new(2*j  ) = WR(2*k  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_2
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_3(nnod_new,                          &
     &          nnod_recv, inod_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(3*nnod_recv)
!
      real (kind=kreal), intent(inout):: X_new(3*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do
      do j = 1, 3*nnod_new
        X_new(j) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(k,j)
      do k= 1, nnod_recv
        j = inod_import(k)
        X_new(3*j-2) = WR(3*k-2)
        X_new(3*j-1) = WR(3*k-1)
        X_new(3*j  ) = WR(3*k  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_3
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_6(nnod_new,                          &
     &          nnod_recv, inod_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(6*nnod_recv)
!
      real (kind=kreal), intent(inout):: X_new(6*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do
      do j = 1, 6*nnod_new
        X_new(j) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(k,j)
      do k= 1, nnod_recv
        j = inod_import(k)
        X_new(6*j-5) = WR(6*k- 5)
        X_new(6*j-4) = WR(6*k- 4)
        X_new(6*j-3) = WR(6*k- 3)
        X_new(6*j-2) = WR(6*k- 2)
        X_new(6*j-1) = WR(6*k- 1)
        X_new(6*j  ) = WR(6*k   )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_6
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_N(NB, nnod_new,                      &
     &          nnod_recv, inod_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(NB*nnod_recv)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
      integer (kind = kint) :: k, nd, jj, kk
!
!
!$omp parallel do
      do jj = 1, NB*nnod_new
        X_new(jj) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(k,jj,kk,nd)
      do kk = 1, NB*nnod_recv
        nd = 1 + mod(kk-1,NB)
        k =  1 + (kk-nd) / NB
        jj = NB*(inod_import(k)-1) + nd
        X_new(jj) = WR(kk)
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_N
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_N_mod(NB, nnod_new,                  &
     &          npe_recv, nnod_recv, istack_recv, inod_import,          &
     &          WR, X_new)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: npe_recv, nnod_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in):: WR(NB*nnod_recv)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, nd, jj, kk
!
!
!$omp parallel do
      do jj = 1, NB*nnod_new
        X_new(jj) = 0.0d0
      end do
!$omp end parallel do
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
            X_new(jj) = WR(kk)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine set_from_recv_buf_N_mod
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_int(nnod_new,                        &
     &          nnod_recv, inod_import, iWR, iX_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      integer(kind = kint), intent(in):: iWR(nnod_recv)
!
      integer(kind = kint), intent(inout):: iX_new(nnod_new)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do k= 1, nnod_recv
        j = inod_import(k)
        iX_new(j  ) = iWR(k  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_int
!
! ----------------------------------------------------------------------
!
      end module set_from_recv_buffer
