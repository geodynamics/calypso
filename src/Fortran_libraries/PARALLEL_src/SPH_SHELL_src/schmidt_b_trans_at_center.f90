!>@file   schmidt_b_trans_at_center.f90
!!@brief  module schmidt_b_trans_at_center
!!
!!@author H. Matsui
!!@date Programmed in June, 2012
!
!>@brief  Spherical transform at center
!!
!!@verbatim
!!      subroutine schmidt_b_trans_center_scalar                        &
!!     &         (ncomp, nvector, nscalar, nnod_pole, v_pl_local)
!!      subroutine schmidt_b_trans_center_vect(ncomp, nvector,          &
!!     &         ist_rtm_order_zero, ist_rtm_order_1s, ist_rtm_order_1c,&
!!     &         nnod_rlm, nnod_pole, idx_gl_1d_rtm_r1, a_r_1d_rtm_r1,  &
!!     &         irev_sr_rlm, n_WR, WR, v_pl_local)
!!
!!------------------------------------------------------------------
!!
!! if r= 0 (Center)
!!
!!      vz =  2 * P(1,0) * S(1,0) / r_c**2
!!         =  2 * S(1,0) / r_c**2
!!      vx =  2 * dPdt(l,1) * S(1,1c) / r_c**2
!!         = - 2 * S(1,1c) / r_c**2
!!      vy =  2 * dPdt(l,1) * S(1,1s) / r_c**2
!!         = - 2 * S(1,1s) / r_c**2
!!
!!------------------------------------------------------------------
!!@endverbatim
!
      module schmidt_b_trans_at_center
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine schmidt_b_trans_center_scalar                          &
     &         (ncomp, nvector, nscalar, nnod_pole, v_pl_local)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: nnod_pole
      real(kind = kreal), intent(inout) :: v_pl_local(nnod_pole,ncomp)
!
      integer(kind = kint) :: nd
!
!
      if(nscalar .le. 0) return
!
      do nd = 3*nvector+1, 3*nvector+nscalar
        v_pl_local(nnod_pole, nd) = v_pl_local(nnod_pole,nd)
!        v_pl_local(nnod_pole, nd) =  two * v_pl_local(nnod_pole,nd)
      end do
!
      end subroutine schmidt_b_trans_center_scalar
!
!------------------------------------------------------------------
!
      subroutine schmidt_b_trans_center_vect(ncomp, nvector,            &
     &         ist_rtm_order_zero, ist_rtm_order_1s, ist_rtm_order_1c,  &
     &         nnod_rlm, nnod_pole, idx_gl_1d_rtm_r1, a_r_1d_rtm_r1,    &
     &         irev_sr_rlm, n_WR, WR, v_pl_local)
!
      integer(kind = kint), intent(in) :: ist_rtm_order_zero
      integer(kind = kint), intent(in) :: ist_rtm_order_1s
      integer(kind = kint), intent(in) :: ist_rtm_order_1c
      integer(kind = kint), intent(in) :: nnod_rlm, nnod_pole
      integer(kind = kint), intent(in) :: idx_gl_1d_rtm_r1
      real(kind = kreal), intent(in) :: a_r_1d_rtm_r1
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
      real(kind = kreal), intent(inout) :: v_pl_local(nnod_pole,ncomp)
!
      integer(kind = kint) :: i_rlm, i_fld, i_recv
!
!
      if(nvector .le. 0) return
      v_pl_local(nnod_pole,1:3*nvector) = zero
!
      if(ist_rtm_order_zero.gt.0 .and. idx_gl_1d_rtm_r1.eq.1) then
        do i_fld = 1, nvector
          i_rlm = ist_rtm_order_zero + 1
          i_recv = 3*i_fld + (irev_sr_rlm(i_rlm)-1) * ncomp
          v_pl_local(nnod_pole,3*i_fld  ) =  two * WR(i_recv-2)         &
     &                  * a_r_1d_rtm_r1*a_r_1d_rtm_r1
        end do
      end if
!
      if(ist_rtm_order_1s.gt.0 .and. idx_gl_1d_rtm_r1.eq.1) then
        do i_fld = 1, nvector
          i_recv = 3*i_fld + (irev_sr_rlm(ist_rtm_order_1s)-1) * ncomp
          v_pl_local(nnod_pole,3*i_fld-2) = -two * WR(i_recv-2)         &
     &                  * a_r_1d_rtm_r1*a_r_1d_rtm_r1
        end do
      end if
!
      if(ist_rtm_order_1c.gt.0 .and. idx_gl_1d_rtm_r1.eq.1) then
        do i_fld = 1, nvector
          i_recv = 3*i_fld + (irev_sr_rlm(ist_rtm_order_1c)-1) * ncomp
          v_pl_local(nnod_pole,3*i_fld-1) = -two * WR(i_recv-2)         &
     &                  * a_r_1d_rtm_r1*a_r_1d_rtm_r1
        end do
      end if
!
      end subroutine schmidt_b_trans_center_vect
!
!------------------------------------------------------------------
!
      end module schmidt_b_trans_at_center
