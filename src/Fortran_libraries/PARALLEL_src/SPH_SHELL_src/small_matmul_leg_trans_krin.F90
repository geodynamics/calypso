!>@file   small_matmul_leg_trans_krin.F90
!!@brief  module small_matmul_leg_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  small Matrix products for Legendre transforms
!!
!!@verbatim
!!      subroutine matvec_fwd_leg_trans_Pj(nkr, n_jk, V_k, P_j, S_kj)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: V_k(nkr)
!!        real(kind = kreal), intent(in) :: P_j(n_jk)
!!        real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!!
!!      subroutine matmul8_fwd_leg_trans_Ptj                            &
!!     &         (iflag_matmul, nkr, n_jk, V_kt, P_tj, S_kj)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: V_kt(nkr,8)
!!        real(kind = kreal), intent(in) :: P_tj(8,n_jk)
!!        real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!!      subroutine matmul4_fwd_leg_trans_Ptj                            &
!!     &         (iflag_matmul, nkr, n_jk, V_kt, P_tj, S_kj)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: V_kt(nkr,4)
!!        real(kind = kreal), intent(in) :: P_tj(4,n_jk)
!!        real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!!      subroutine matmul2_fwd_leg_trans_Ptj                            &
!!     &         (iflag_matmul, nkr, n_jk, V_kt, P_tj, S_kj)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: V_kt(nkr,2)
!!        real(kind = kreal), intent(in) :: P_tj(2,n_jk)
!!        real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!!
!!
!!      subroutine matvec_bwd_leg_trans_Pj                              &
!!     &         (iflag_matmul, nkr, n_jk, S_kj, P_j, V_k)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: P_j(n_jk)
!!        real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!!        real(kind = kreal), intent(inout) :: V_k(nkr)
!!
!!      subroutine matmul8_bwd_leg_trans_Pjt                            &
!!     &         (iflag_matmul, nkr, n_jk, S_kj, P_jt, V_kt)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: P_jt(n_jk,8)
!!        real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!!        real(kind = kreal), intent(inout) :: V_kt(nkr,8)
!!      subroutine matmul4_bwd_leg_trans_Pjt                            &
!!     &         (iflag_matmul, nkr, n_jk, S_kj, P_jt, V_kt)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: P_jt(n_jk,4)
!!        real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!!        real(kind = kreal), intent(inout) :: V_kt(nkr,4)
!!      subroutine matmul2_bwd_leg_trans_Pjt                            &
!!     &         (iflag_matmul, nkr, n_jk, S_kj, P_jt, V_kt)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: P_jt(n_jk,2)
!!        real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!!        real(kind = kreal), intent(inout) :: V_kt(nkr,2)
!!@endverbatim
!!
      module small_matmul_leg_trans_krin
!
      use m_precision
      use m_constants
      use matmul_for_legendre_trans
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine matvec_fwd_leg_trans_Pj(nkr, n_jk, V_k, P_j, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: V_k(nkr)
      real(kind = kreal), intent(in) :: P_j(n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          S_kj(kk,jj) = S_kj(kk,jj) + V_k(kk)*P_j(jj)
        end do
      end do
!
      end subroutine matvec_fwd_leg_trans_Pj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine matmul8_fwd_leg_trans_Ptj                              &
     &         (iflag_matmul, nkr, n_jk, V_kt, P_tj, S_kj)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: V_kt(nkr,8)
      real(kind = kreal), intent(in) :: P_tj(8,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
      integer :: n_jk4, nkr4
!
!
      if(iflag_matmul .eq. iflag_INTRINSIC) then
        S_kj(1:nkr,1:n_jk) = S_kj(1:nkr,1:n_jk)                         &
     &                    + matmul(V_kt(1:nkr,1:8),P_tj(1:8,1:n_jk))
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        call DGEMM('N', 'N', nkr4, n_jk4, 8, one,                       &
     &      V_kt, nkr4, P_tj, 8, one, S_kj, nkr4)
#endif
      else
        do jj = 1, n_jk
          do kk = 1, nkr
            S_kj(kk,jj) = S_kj(kk,jj)                                   &
     &             + V_kt(kk,1) * P_tj(1,jj) + V_kt(kk,2) * P_tj(2,jj)  &
     &             + V_kt(kk,3) * P_tj(3,jj) + V_kt(kk,4) * P_tj(4,jj)  &
     &             + V_kt(kk,5) * P_tj(5,jj) + V_kt(kk,6) * P_tj(6,jj)  &
     &             + V_kt(kk,7) * P_tj(7,jj) + V_kt(kk,8) * P_tj(8,jj)
          end do
        end do
      end if
!
      end subroutine matmul8_fwd_leg_trans_Ptj
!
! ----------------------------------------------------------------------
!
      subroutine matmul4_fwd_leg_trans_Ptj                              &
     &         (iflag_matmul, nkr, n_jk, V_kt, P_tj, S_kj)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: V_kt(nkr,4)
      real(kind = kreal), intent(in) :: P_tj(4,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
      integer :: n_jk4, nkr4
!
!
      if(iflag_matmul .eq. iflag_INTRINSIC) then
        S_kj(1:nkr,1:n_jk) = S_kj(1:nkr,1:n_jk)                         &
     &                    + matmul(V_kt(1:nkr,1:4),P_tj(1:4,1:n_jk))
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        call DGEMM('N', 'N', nkr4, n_jk4, 4, one,                       &
     &      V_kt, nkr4, P_tj, 4, one, S_kj, nkr4)
#endif
      else
        do jj = 1, n_jk
          do kk = 1, nkr
            S_kj(kk,jj) = S_kj(kk,jj)                                   &
     &             + V_kt(kk,1) * P_tj(1,jj) + V_kt(kk,2) * P_tj(2,jj)  &
     &             + V_kt(kk,3) * P_tj(3,jj) + V_kt(kk,4) * P_tj(4,jj)
          end do
        end do
      end if
!
      end subroutine matmul4_fwd_leg_trans_Ptj
!
! ----------------------------------------------------------------------
!
      subroutine matmul2_fwd_leg_trans_Ptj                              &
     &         (iflag_matmul, nkr, n_jk, V_kt, P_tj, S_kj)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: V_kt(nkr,2)
      real(kind = kreal), intent(in) :: P_tj(2,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
      integer :: n_jk4, nkr4
!
!
      if(iflag_matmul .eq. iflag_INTRINSIC) then
        S_kj(1:nkr,1:n_jk) = S_kj(1:nkr,1:n_jk)                         &
     &                    + matmul(V_kt(1:nkr,1:2),P_tj(1:2,1:n_jk))
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        call DGEMM('N', 'N', nkr4, n_jk4, 2, one,                       &
     &      V_kt, nkr4, P_tj, 2, one, S_kj, nkr4)
#endif
      else
        do jj = 1, n_jk
          do kk = 1, nkr
            S_kj(kk,jj) = S_kj(kk,jj)                                   &
     &             + V_kt(kk,1) * P_tj(1,jj) + V_kt(kk,2) * P_tj(2,jj)
          end do
        end do
      end if
!
      end subroutine matmul2_fwd_leg_trans_Ptj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine matvec_bwd_leg_trans_Pj                                &
     &         (iflag_matmul, nkr, n_jk, S_kj, P_j, V_k)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: S_kj(nkr,n_jk)
      real(kind = kreal), intent(in) :: P_j(n_jk)
!
      real(kind = kreal), intent(inout) :: V_k(nkr)
!
      integer(kind = kint) :: jj, kk
!
!
      if(nkr .eq. 0) return
      if(n_jk .eq. 0) then
        V_k(1:nkr) = 0.0d0
      else if(iflag_matmul .eq. iflag_INTRINSIC) then
        V_k(1:nkr) = matmul(S_kj(1:nkr,1:n_jk),P_j(1:n_jk))
      else
        V_k(1:nkr) = 0.0d0
        do jj = 1, n_jk
          do kk = 1, nkr
            V_k(kk) = V_k(kk) + S_kj(kk,jj) * P_j(jj)
          end do
        end do
      end if
!
      end subroutine matvec_bwd_leg_trans_Pj
!
! ----------------------------------------------------------------------
!
      subroutine matmul8_bwd_leg_trans_Pjt                              &
     &         (iflag_matmul, nkr, n_jk, S_kj, P_jt, V_kt)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: P_jt(n_jk,8)
      real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!
      real(kind = kreal), intent(inout) :: V_kt(nkr,8)
!
      integer(kind = kint) :: jj, kk
      integer :: n_jk4, nkr4
!
!
      if(nkr .eq. 0) return
      if(n_jk .eq. 0) then
        V_kt(1:nkr,1:8) = 0.0d0
      else if(iflag_matmul .eq. iflag_INTRINSIC) then
        V_kt(1:nkr,1:8) = matmul(S_kj(1:nkr,1:n_jk),P_jt(1:n_jk,1:8))
!
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        call DGEMM('N', 'N', nkr4, 8, n_jk4, one,                       &
     &      S_kj, nkr4, P_jt, n_jk4, zero, V_kt, nkr4)
#endif
      else
        V_kt(1:nkr,1:8) = 0.0d0
        do jj = 1, n_jk
          do kk = 1, nkr
            V_kt(kk,1:8) = V_kt(kk,1:8) + S_kj(kk,jj) * P_jt(jj,1:8)
          end do
        end do
      end if
!
      end subroutine matmul8_bwd_leg_trans_Pjt
!
! ----------------------------------------------------------------------
!
      subroutine matmul4_bwd_leg_trans_Pjt                              &
     &         (iflag_matmul, nkr, n_jk, S_kj, P_jt, V_kt)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: P_jt(n_jk,4)
      real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!
      real(kind = kreal), intent(inout) :: V_kt(nkr,4)
!
      integer(kind = kint) :: jj, kk
      integer :: n_jk4, nkr4
!
!
      if(nkr .eq. 0) return
      if(n_jk .eq. 0) then
        V_kt(1:nkr,1:4) = 0.0d0
      else if(iflag_matmul .eq. iflag_INTRINSIC) then
        V_kt(1:nkr,1:4) = matmul(S_kj(1:nkr,1:n_jk),P_jt(1:n_jk,1:4))
!
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        call DGEMM('N', 'N', nkr4, 4, n_jk4, one,                       &
     &      S_kj, nkr4, P_jt, n_jk4, zero, V_kt, nkr4)
#endif
      else
        V_kt(1:nkr,1:4) = 0.0d0
        do jj = 1, n_jk
          do kk = 1, nkr
            V_kt(kk,1:4) = V_kt(kk,1:4) + S_kj(kk,jj) * P_jt(jj,1:4)
          end do
        end do
      end if
!
      end subroutine matmul4_bwd_leg_trans_Pjt
!
! ----------------------------------------------------------------------
!
      subroutine matmul2_bwd_leg_trans_Pjt                              &
     &         (iflag_matmul, nkr, n_jk, S_kj, P_jt, V_kt)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: P_jt(n_jk,2)
      real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!
      real(kind = kreal), intent(inout) :: V_kt(nkr,2)
!
      integer(kind = kint) :: jj, kk
      integer :: n_jk4, nkr4
!
!
      if(nkr .eq. 0) return
      if(n_jk .eq. 0) then
        V_kt(1:nkr,1:2) = 0.0d0
      else if(iflag_matmul .eq. iflag_INTRINSIC) then
        V_kt(1:nkr,1:2) = matmul(S_kj(1:nkr,1:n_jk),P_jt(1:n_jk,1:2))
!
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        call DGEMM('N', 'N', nkr4, 2, n_jk4, one,                       &
     &      S_kj, nkr4, P_jt, n_jk4, zero, V_kt, nkr4)
#endif
      else
        V_kt(1:nkr,1:2) = 0.0d0
        do jj = 1, n_jk
          do kk = 1, nkr
            V_kt(kk,1:2) = V_kt(kk,1:2) + S_kj(kk,jj) * P_jt(jj,1:2)
          end do
        end do
      end if
!
      end subroutine matmul2_bwd_leg_trans_Pjt
!
! ----------------------------------------------------------------------
!
      end module small_matmul_leg_trans_krin
