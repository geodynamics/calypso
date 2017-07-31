!>@file   schmidt_b_trans_at_pole.f90
!!@brief  module schmidt_b_trans_at_pole
!!
!!@author H. Matsui
!!@date Programmed in June, 2012
!
!>@brief  Legendre transform at poles
!!
!!@verbatim
!!      subroutine schmidt_b_trans_pole_scalar(ncomp, nvector, nscalar, &
!!     &          l_truncation, ist_rtm_order_zero, nnod_rlm, nri_rlm,  &
!!     &          jmax_rlm, nnod_pole, istep_rlm, nidx_global_rtp,      &
!!     &          idx_gl_1d_rlm_r, irev_sr_rlm, P_pole_rtm,             &
!!     &          n_WR, WR, v_pl_local)
!!      subroutine schmidt_b_trans_pole_vect                            &
!!     &        (ncomp, nvector, l_truncation,                          &
!!     &         ist_rtm_order_zero, ist_rtm_order_1s, ist_rtm_order_1c,&
!!     &         nnod_rlm, nri_rlm, jmax_rlm, nnod_pole, istep_rlm,     &
!!     &         nidx_global_rtp, idx_gl_1d_rlm_r, a_r_1d_rlm_r,        &
!!     &         irev_sr_rlm, g_sph_rlm, P_pole_rtm, dPdt_pole_rtm,     &
!!     &         n_WR, WR, v_pl_local)
!!
!!------------------------------------------------------------------
!!
!!      vr =  l*(l+1)*Y(l,0)* S(l,0) / r**2
!!      vt =  (dYdt(l,1s)*dSdr(l,1s)
!!           + dYdt(l,1c)*dSdr(l,1c))  / r
!!         + cos(theta) * (d2Ydtdp(l,1s)*T(l,1s) 
!!                       + d2Ydtdp(l,1c)*T(l,1c))  / r
!!      vp = cos(theta) * (d2Ydtdp(l,1s)*dSdr(l,1s)
!!                       + d2Ydtdp(l,1c)*dSdr(l,1c))  / r
!!           -(dYdt(l,1s)*T(l,1s)
!!           + dYdt(l,1c)*T(l,1c))  / r
!!
!!  if phi = 0
!!
!!      vr =  l*(l+1)*P(l,0)* S(l,0) / r**2
!!      vt =  dPdt(l,1)*dSdr(l,1c)  / r
!!         + cos(theta) * dPdt(l,1)*T(l,1s) / r
!!      vp = cos(theta) * dPdt(l,1)*dSdr(l,1s) / r
!!           - dPdt(l,1)*T(l,1c)  / r
!!
!! if z > 0 (North pole)
!!
!!      vx = vt
!!      vy = vp
!!      vz = vr
!!
!! if z < 0 (South pole)
!!
!!      vx = -vt
!!      vy =  vp
!!      vz = -vr
!!
!!------------------------------------------------------------------
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
!!------------------------------------------------------------------
!!@endverbatim
!!
!!@param ncomp   Number of components for all fields
!!@param nvector Number of fields for vector
!!@param nscalar Number of fields for scalar
!
      module schmidt_b_trans_at_pole
!
      use m_precision
!
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
      subroutine schmidt_b_trans_pole_scalar(ncomp, nvector, nscalar,   &
     &          l_truncation, ist_rtm_order_zero, nnod_rlm, nri_rlm,    &
     &          jmax_rlm, nnod_pole, istep_rlm, nidx_global_rtp,        &
     &          idx_gl_1d_rlm_r, irev_sr_rlm, P_pole_rtm,               &
     &          n_WR, WR, v_pl_local)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: ist_rtm_order_zero
      integer(kind = kint), intent(in) :: nnod_rlm, nri_rlm, jmax_rlm
      integer(kind = kint), intent(in) :: nnod_pole
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in) :: nidx_global_rtp(3)
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_r(nri_rlm)
      real(kind = kreal), intent(in) :: P_pole_rtm(2,jmax_rlm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
      real(kind = kreal), intent(inout) :: v_pl_local(nnod_pole,ncomp)
!
      integer(kind = kint) :: k_rlm, nd, k_np, k_sp
      integer(kind = kint) :: jst, jed, j_rlm, i_rlm, i_recv
!
!
      do nd = 1+3*nvector, nscalar+3*nvector
        v_pl_local(1:2*nidx_global_rtp(1),nd) = zero
      end do
!
      if(ist_rtm_order_zero .le. 0) return
!$omp parallel do private(k_rlm,nd,k_np,k_sp,jst,jed,j_rlm,i_rlm,i_recv)
      do k_rlm = 1, nri_rlm
        k_np = idx_gl_1d_rlm_r(k_rlm)
        k_sp = idx_gl_1d_rlm_r(k_rlm) + nidx_global_rtp(1)
        do nd = 1+3*nvector, nscalar+3*nvector
!
          jst = ist_rtm_order_zero
          jed = ist_rtm_order_zero + l_truncation
          do j_rlm = jst, jed
            i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                        &
     &                + (k_rlm-1) * istep_rlm(1)
            i_recv = nd + (irev_sr_rlm(i_rlm)-1) * ncomp
            v_pl_local(k_np,nd) = v_pl_local(k_np,nd)                   &
     &                         + WR(i_recv) * P_pole_rtm(1,j_rlm)
            v_pl_local(k_sp,nd) = v_pl_local(k_sp,nd)                   &
     &                         + WR(i_recv) * P_pole_rtm(2,j_rlm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine schmidt_b_trans_pole_scalar
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_pole_vect                              &
     &        (ncomp, nvector, l_truncation,                            &
     &         ist_rtm_order_zero, ist_rtm_order_1s, ist_rtm_order_1c,  &
     &         nnod_rlm, nri_rlm, jmax_rlm, nnod_pole, istep_rlm,       &
     &         nidx_global_rtp, idx_gl_1d_rlm_r, a_r_1d_rlm_r,          &
     &         irev_sr_rlm, g_sph_rlm, P_pole_rtm, dPdt_pole_rtm,       &
     &         n_WR, WR, v_pl_local)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: ist_rtm_order_zero
      integer(kind = kint), intent(in) :: ist_rtm_order_1s
      integer(kind = kint), intent(in) :: ist_rtm_order_1c
      integer(kind = kint), intent(in) :: nnod_rlm, nri_rlm, jmax_rlm
      integer(kind = kint), intent(in) :: nnod_pole
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in)  :: nidx_global_rtp(3)
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_r(nri_rlm)
      real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nri_rlm)
      real(kind = kreal), intent(in) :: g_sph_rlm(jmax_rlm,17)
      real(kind = kreal), intent(in) :: P_pole_rtm(2,jmax_rlm)
      real(kind = kreal), intent(in) :: dPdt_pole_rtm(2,jmax_rlm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
      real(kind = kreal), intent(inout) :: v_pl_local(nnod_pole,ncomp)
!
      integer(kind = kint) :: k_rlm, nd, k_np, k_sp
      integer(kind = kint) :: jst, jed, j_rlm, i_rlm, i_recv, iflag
!
!
      do nd = 1, 3*nvector
        v_pl_local(1:2*nidx_global_rtp(1),nd) = zero
      end do
!
      if(ist_rtm_order_zero .gt. 0) then
!$omp parallel do private(k_rlm,nd,k_np,k_sp,jst,jed,j_rlm,i_rlm,i_recv)
        do k_rlm = 1, nri_rlm
          k_np = idx_gl_1d_rlm_r(k_rlm)
          k_sp = idx_gl_1d_rlm_r(k_rlm) + nidx_global_rtp(1)
          do nd = 1, nvector
!
            jst = ist_rtm_order_zero
            jed = ist_rtm_order_zero + l_truncation
            do j_rlm = jst, jed
              i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                      &
     &                  + (k_rlm-1) * istep_rlm(1)
              i_recv = 3*nd + (irev_sr_rlm(i_rlm)-1) * ncomp
!
              v_pl_local(k_np,3*nd  ) = v_pl_local(k_np,3*nd  )         &
     &                           + WR(i_recv-2) * P_pole_rtm(1,j_rlm)   &
     &                            * g_sph_rlm(j_rlm,3)
              v_pl_local(k_sp,3*nd  ) = v_pl_local(k_sp,3*nd  )         &
     &                           - WR(i_recv-2) * P_pole_rtm(2,j_rlm)   &
     &                            * g_sph_rlm(j_rlm,3)
            end do
          end do
        end do
!$omp end parallel do
      end if
!
      if(ist_rtm_order_1s .gt. 0) then
!$omp parallel do private(k_rlm,nd,k_np,k_sp,jst,jed,j_rlm,i_rlm,i_recv)
        do k_rlm = 1, nri_rlm
          k_np = idx_gl_1d_rlm_r(k_rlm)
          k_sp = idx_gl_1d_rlm_r(k_rlm) + nidx_global_rtp(1)
          do nd = 1, nvector
!
            jst = ist_rtm_order_1s
            jed = ist_rtm_order_1s + l_truncation-1
            do j_rlm = jst, jed
              i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                      &
     &                  + (k_rlm-1) * istep_rlm(1)
              i_recv = 3*nd + (irev_sr_rlm(i_rlm)-1) * ncomp
!
              v_pl_local(k_np,3*nd-2) = v_pl_local(k_np,3*nd-2)         &
     &                + WR(i_recv  ) * dPdt_pole_rtm(1,j_rlm)
              v_pl_local(k_np,3*nd-1) = v_pl_local(k_np,3*nd-1)         &
     &                + WR(i_recv-1) * dPdt_pole_rtm(1,j_rlm)
!
              v_pl_local(k_sp,3*nd-2) = v_pl_local(k_sp,3*nd-2)         &
     &                + WR(i_recv  ) * dPdt_pole_rtm(2,j_rlm)
              v_pl_local(k_sp,3*nd-1) = v_pl_local(k_sp,3*nd-1)         &
     &                - WR(i_recv-1) * dPdt_pole_rtm(2,j_rlm)
            end do
          end do
        end do
!$omp end parallel do
      end if
!
      if(ist_rtm_order_1c .gt. 0) then
!$omp parallel do private(k_rlm,nd,k_np,k_sp,jst,jed,j_rlm,i_rlm,i_recv)
        do k_rlm = 1, nri_rlm
          k_np = idx_gl_1d_rlm_r(k_rlm)
          k_sp = idx_gl_1d_rlm_r(k_rlm) + nidx_global_rtp(1)
          do nd = 1, nvector
!
            jst = ist_rtm_order_1c
            jed = ist_rtm_order_1c + l_truncation-1
            do j_rlm = jst, jed
              i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                      &
     &                  + (k_rlm-1) * istep_rlm(1)
              i_recv = 3*nd + (irev_sr_rlm(i_rlm)-1) * ncomp
!
              v_pl_local(k_np,3*nd-2) = v_pl_local(k_np,3*nd-2)         &
     &                + WR(i_recv-1) * dPdt_pole_rtm(1,j_rlm)
              v_pl_local(k_np,3*nd-1) = v_pl_local(k_np,3*nd-1)         &
     &                - WR(i_recv  ) * dPdt_pole_rtm(1,j_rlm)
!
              v_pl_local(k_sp,3*nd-2) = v_pl_local(k_sp,3*nd-2)         &
     &                - WR(i_recv-1) * dPdt_pole_rtm(2,j_rlm)
              v_pl_local(k_sp,3*nd-1) = v_pl_local(k_sp,3*nd-1)         &
     &                - WR(i_recv  ) * dPdt_pole_rtm(2,j_rlm)
            end do
          end do
        end do
!$omp end parallel do
      end if
!
      iflag = ist_rtm_order_zero+ist_rtm_order_1s+ist_rtm_order_1c
      if(iflag .gt. 0) then
        do k_rlm = 1, nri_rlm
          k_np = idx_gl_1d_rlm_r(k_rlm)
          k_sp = idx_gl_1d_rlm_r(k_rlm) + nidx_global_rtp(1)
          do nd = 1, nvector
            v_pl_local(k_np,3*nd-2) = v_pl_local(k_np,3*nd-2)           &
     &                      * a_r_1d_rlm_r(k_rlm)
            v_pl_local(k_np,3*nd-1) = v_pl_local(k_np,3*nd-1)           &
     &                      * a_r_1d_rlm_r(k_rlm)
            v_pl_local(k_np,3*nd  ) = v_pl_local(k_np,3*nd  )           &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
            v_pl_local(k_sp,3*nd-2) = v_pl_local(k_sp,3*nd-2)           &
     &                      * a_r_1d_rlm_r(k_rlm)
            v_pl_local(k_sp,3*nd-1) = v_pl_local(k_sp,3*nd-1)           &
     &                      * a_r_1d_rlm_r(k_rlm)
            v_pl_local(k_sp,3*nd  ) = v_pl_local(k_sp,3*nd  )           &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
          end do
        end do
      end if
!
      end subroutine schmidt_b_trans_pole_vect
!
! -----------------------------------------------------------------------
!
      end module schmidt_b_trans_at_pole
