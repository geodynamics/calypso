!>@file   set_to_send_buffer.f90
!!@brief  module set_to_send_buffer
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine set_to_send_buf_1(nnod_org,                          &
!!     &          nnod_send, inod_export, X_org, WS)
!!      subroutine set_to_send_buf_2(nnod_org,                          &
!!     &          nnod_send, inod_export, X_org, WS)
!!      subroutine set_to_send_buf_3(nnod_org,                          &
!!     &          nnod_send, inod_export, X_org, WS)
!!      subroutine set_to_send_buf_6(nnod_org,                          &
!!     &          nnod_send, inod_export, X_org, WS)
!!      subroutine set_to_send_buf_N(NB, nnod_org,                      &
!!     &          nnod_send, inod_export, X_org, WS)
!!      subroutine set_to_send_buf_N_mod(NB, nnod_org,                  &
!!     &          npe_send, nnod_send, istack_send, inod_export,        &
!!     &          X_org, WS)
!!
!!      subroutine set_to_send_buf_int(nnod_org,                        &
!!     &          nnod_send, inod_export, iX_org, iWS)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  nnod_send   Number of data points to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(nnod_send)
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  X_org(NB*nnod_org)   Send data
!
      module set_to_send_buffer
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
      subroutine set_to_send_buf_1(nnod_org,                            &
     &          nnod_send, inod_export, X_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org, nnod_send
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      real (kind=kreal), intent(in)::    X_org(nnod_org)
!
      real (kind=kreal), intent(inout):: WS(nnod_send)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do k = 1, nnod_send
          j = inod_export(k)
          WS(k  )= X_org(j  )
      end do
!$omp end parallel do
!
      end subroutine set_to_send_buf_1
!
! ----------------------------------------------------------------------
!
      subroutine set_to_send_buf_2(nnod_org,                            &
     &          nnod_send, inod_export, X_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org, nnod_send
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      real (kind=kreal), intent(in)::    X_org(2*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(2*nnod_send)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do k = 1, nnod_send
          j = inod_export(k)
          WS(2*k-1)= X_org(2*j-1)
          WS(2*k  )= X_org(2*j  )
      end do
!$omp end parallel do
!
      end subroutine set_to_send_buf_2
!
! ----------------------------------------------------------------------
!
      subroutine set_to_send_buf_3(nnod_org,                            &
     &          nnod_send, inod_export, X_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org, nnod_send
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      real (kind=kreal), intent(in)::    X_org(3*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(3*nnod_send)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do k = 1, nnod_send
        j = inod_export(k)
        WS(3*k-2)= X_org(3*j-2)
        WS(3*k-1)= X_org(3*j-1)
        WS(3*k  )= X_org(3*j  )
      end do
!$omp end parallel do
!
      end subroutine set_to_send_buf_3
!
! ----------------------------------------------------------------------
!
      subroutine set_to_send_buf_6(nnod_org,                            &
     &          nnod_send, inod_export, X_org, WS)
!
      integer(kind = kint), intent(in) :: nnod_org, nnod_send
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      real (kind=kreal), intent(in)::    X_org(6*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(6*nnod_send)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do k = 1, nnod_send
        j = inod_export(k)
        WS(6*k- 5)= X_org(6*j-5)
        WS(6*k- 4)= X_org(6*j-4)
        WS(6*k- 3)= X_org(6*j-3)
        WS(6*k- 2)= X_org(6*j-2)
        WS(6*k- 1)= X_org(6*j-1)
        WS(6*k   )= X_org(6*j  )
      end do
!$omp end parallel do
!
      end subroutine set_to_send_buf_6
!
! ----------------------------------------------------------------------
!
      subroutine set_to_send_buf_N(NB, nnod_org,                        &
     &          nnod_send, inod_export, X_org, WS)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_org, nnod_send
!
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(NB*nnod_send)
!
!
      integer (kind = kint) :: k, nd, jj, kk
!
!
!$omp parallel do private(k,jj,kk,nd)
      do kk = 1, NB*nnod_send
        nd = 1 + mod(kk-1,NB)
        k =  1 + (kk-nd) / NB
        jj = NB*(inod_export(k)-1) + nd
        WS(kk) = X_org(jj)
      end do
!$omp end parallel do
!
      end subroutine set_to_send_buf_N
!
! ----------------------------------------------------------------------
!
      subroutine set_to_send_buf_N_mod(NB, nnod_org,                    &
     &          npe_send, nnod_send, istack_send, inod_export,          &
     &          X_org, WS)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
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
            jj = nd + (inod_export(k+ist) - 1) * NB
            kk = k + (nd-1)*num + NB*ist
            WS(kk) = X_org(jj)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine set_to_send_buf_N_mod
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_to_send_buf_int(nnod_org,                          &
     &          nnod_send, inod_export, iX_org, iWS)
!
      integer(kind = kint), intent(in) :: nnod_org, nnod_send
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer (kind=kint), intent(in)::    iX_org(nnod_org)
!
      integer (kind=kint), intent(inout):: iWS(nnod_send)
!
      integer (kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do k = 1, nnod_send
          j = inod_export(k)
          iWS(k  )= iX_org(j  )
      end do
!$omp end parallel do
!
      end subroutine set_to_send_buf_int
!
! ----------------------------------------------------------------------
!
      end module set_to_send_buffer
