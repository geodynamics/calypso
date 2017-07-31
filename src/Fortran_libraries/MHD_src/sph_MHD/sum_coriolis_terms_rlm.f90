!>@file   sum_coriolis_terms_rlm.f90
!!@brief  module sum_coriolis_terms_rlm
!!
!!@author H. Matsui
!!@date Programmed in 1995
!@n     Modified in Oct., 2009
!
!>@brief  Evaluate curl of Coriolis term by Gaunt integrals
!!
!!@verbatim
!!************************************************
!!
!!      subroutine sum_rot_coriolis_rlm_10(b_trns, nnod_rlm, nidx_rlm,  &
!!     &          a_r_1d_rlm_r, g_sph_rlm, omega_rlm, coef_cor,         &
!!     &          jgi_cor_rlm, jei_cor_rlm, sw_rlm, tw_rlm, NB, n_WR,   &
!!     &          irev_sr_rlm, WR, d_rotp_cor_rlm, d_rott_cor_rlm)
!!      subroutine sum_div_coriolis_rlm_10(b_trns, nnod_rlm, nidx_rlm,  &
!!     &          idx_gl_1d_rlm_j, a_r_1d_rlm_r, omega_rlm, coef_cor,   &
!!     &          jgi_cor_rlm, jei_cor_rlm, sd_rlm, td_rlm,             &
!!     &          NB, n_WR, irev_sr_rlm, WR, d_div_cor_rlm)
!!      subroutine sum_r_coriolis_bc_rlm_10(b_trns, nnod_rlm, nidx_rlm, &
!!     &          idx_gl_1d_rlm_j, a_r_1d_rlm_r, omega_rlm, coef_cor,   &
!!     &          jgi_cor_rlm, jei_cor_rlm, sr_rlm, tr_rlm,             &
!!     &          NB, kr, n_WR, irev_sr_rlm,  WR, d_cor_bc)
!!      subroutine inner_core_rot_z_coriolis_rlm                        &
!!     &         (b_trns, nnod_rlm, nidx_rlm, radius_1d_rlm_r,          &
!!     &          omega_rlm, coef_cor, NB, n_WR, irev_sr_rlm, WR,       &
!!     &          idx_rlm_ICB, idx_rlm_degree_one, dp_rot_cor_rlm)
!!
!!************************************************
!!
!!  Rotation of the Coriolos term
!!     (wss) = wss(jc,1,j3)*w*dyb/r**2
!!            + wss(jc,2,j3)*dw*yb/r**2
!!
!!     (wts) = wts(j3)*w*yb/r**2
!!
!!     (wst) = wst(1,j3)*( dw*dyb/r**2 + w*d2yb/r**2 - 2*w*dyb/r**3 )
!!            + wst(2,j3)*( d2w/r**2 - 2*dw/r**3 )*yb
!!
!!     (wtt) = wtt(jc,1,j3)*dw*yb/r**2
!!            + wtt(jc,2,j3)*w*( dyb/r**2 - 2*yb/r**3 )
!!
!!   Divergence of the Coriolis term
!!     (wsd) = wsd(jc,1,j3)*w*wsb/r**4
!!            + wsd(jc,2,j3)*dw*dwsb/r**2
!!     (wtd) = wtd(j3)*dw*dwtb/r**2
!!
!!  Radial componenet of the Coriolis term
!!     (wsr) = wsr(jc,1,j3)*dw*dusb/r**2
!!     (wtr) = wtr(j3)*dw*wtb/r**2
!!
!!************************************************
!!
!!************************************************
!!
!!     wss(jc,1,j3) = sw_rlm(jc,1,j3)
!!     wss(jc,2,j3) = sw_rlm(jc,2,j3)
!!     wts(jc,j3)   = sw_rlm(jc,3,j3)
!!     wst(jc,1,j3) = tw_rlm(jc,1,j3)
!!     wst(jc,2,j3) = tw_rlm(jc,2,j3)
!!     wtt(jc,1,j3) = tw_rlm(jc,3,j3)
!!     wtt(jc,2,j3) = tw_rlm(jc,4,j3)
!!
!!     wsd(jc,1,j3) = sd_rlm(jc,1,j3)
!!     wsd(jc,2,j3) = sd_rlm(jc,2,j3)
!!     wtd(jc,j3)   = td_rlm(jc,j3)
!!
!!     wsr(jc,j3) =   sr_rlm(jc,j3)
!!     wtr(jc,j3) =   tr_rlm(jc,j3)
!!
!!************************************************
!!@endverbatim
!!
!
      module sum_coriolis_terms_rlm
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      use t_phys_address
      use t_gaunt_coriolis_rlm
!
      implicit none
!
!   ------------------------------------------------------------------
!
      contains
!
!   ------------------------------------------------------------------
!
      subroutine sum_rot_coriolis_rlm_10(b_trns, nnod_rlm, nidx_rlm,    &
     &          a_r_1d_rlm_r, g_sph_rlm, omega_rlm, coef_cor,           &
     &          jgi_cor_rlm, jei_cor_rlm, sw_rlm, tw_rlm, NB, n_WR,     &
     &          irev_sr_rlm, WR, d_rotp_cor_rlm, d_rott_cor_rlm)
!
      type(phys_address), intent(in) :: b_trns
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
      real(kind = kreal), intent(in) :: omega_rlm(nidx_rlm(1),0:2)
      real(kind = kreal), intent(in) :: coef_cor
!
      integer(kind = kint), intent(in) :: jgi_cor_rlm(nidx_rlm(2),2)
      integer(kind = kint), intent(in) :: jei_cor_rlm(nidx_rlm(2),1)
      real(kind = kreal), intent(in) :: sw_rlm(2,3,nidx_rlm(2))
      real(kind = kreal), intent(in) :: tw_rlm(2,4,nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: NB, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: d_rotp_cor_rlm(nnod_rlm)
      real(kind = kreal), intent(inout) :: d_rott_cor_rlm(nnod_rlm)
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: i11, i21, i12, ir_11, ir_21, ir_12
      real(kind = kreal) :: sp_dvp_k1, sp_dvp_k2, sp_vp_k1, sp_vp_k2
      real(kind = kreal) :: sp_wp_l1, sp_d2vp_l1, sp_vp_l1
      real(kind = kreal) :: sp_wp_k1, sp_wp_k2, sp_dwp_k1, sp_dwp_k2
!
!
!$omp  parallel do                                                      &
!$omp& private(k_rlm,j_rlm,i_rlm,i11,i21,i12,ir_11,ir_21,ir_12,         &
!$omp&         sp_dvp_k1,sp_dvp_k2,sp_vp_k1,sp_vp_k2,sp_wp_l1,          &
!$omp&         sp_d2vp_l1,sp_vp_l1,sp_wp_k1,sp_wp_k2,sp_dwp_k1,         &
!$omp&         sp_dwp_k2)
      do k_rlm = 1, nidx_rlm(1)
        do j_rlm = 1, nidx_rlm(2)
          i_rlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
!
!          write(*,*) 'jgi_cor_rlm', jgi_cor_rlm(j_rlm,1:2),            &
!     &                              jei_cor_rlm(j_rlm,1)
          i11 = jgi_cor_rlm(j_rlm,1) + (k_rlm-1)*nidx_rlm(2)
          i21 = jgi_cor_rlm(j_rlm,2) + (k_rlm-1)*nidx_rlm(2)
          i12 = jei_cor_rlm(j_rlm,1) + (k_rlm-1)*nidx_rlm(2)
!
          ir_11 = irev_sr_rlm(i11) - 1
          ir_21 = irev_sr_rlm(i21) - 1
          ir_12 = irev_sr_rlm(i12) - 1
!
          sp_dvp_k1 = WR(b_trns%i_velo+1+NB*ir_11)
          sp_dvp_k2 = WR(b_trns%i_velo+1+NB*ir_21)
          sp_wp_l1 =  WR(b_trns%i_vort+  NB*ir_12)
          sp_vp_k1 =  WR(b_trns%i_velo+  NB*ir_11)
          sp_vp_k2 =  WR(b_trns%i_velo+  NB*ir_21)
!
          sp_d2vp_l1 = (a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)         &
     &        * g_sph_rlm(j_rlm,3)*WR(b_trns%i_velo+  NB*ir_12)         &
     &                           - WR(b_trns%i_vort+2+NB*ir_12) )
          sp_vp_l1 = WR(b_trns%i_velo+NB*ir_12)
          sp_wp_k1 = WR(b_trns%i_vort+NB*ir_11)
          sp_wp_k2 = WR(b_trns%i_vort+NB*ir_21)
          sp_dwp_k1 = (WR(b_trns%i_vort+1+NB*ir_11)                     &
     &         - two*a_r_1d_rlm_r(k_rlm)*WR(b_trns%i_vort+NB*ir_11))
          sp_dwp_k2 = (WR(b_trns%i_vort+1+NB*ir_21)                     &
     &         - two*a_r_1d_rlm_r(k_rlm)*WR(b_trns%i_vort+NB*ir_21))
!
          d_rotp_cor_rlm(i_rlm)                                         &
     &     =  sw_rlm(1,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k1        &
     &      + sw_rlm(2,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k2        &
     &      + sw_rlm(1,3,j_rlm) * omega_rlm(k_rlm,0) * sp_wp_l1         &
     &      + sw_rlm(1,2,j_rlm) * omega_rlm(k_rlm,1) * sp_vp_k1         &
     &      + sw_rlm(2,2,j_rlm) * omega_rlm(k_rlm,1) * sp_vp_k2
!*
          d_rott_cor_rlm(i_rlm)                                         &
     &     =  tw_rlm(1,1,j_rlm) * omega_rlm(k_rlm,0) * sp_d2vp_l1       &
     &      - tw_rlm(1,2,j_rlm) * omega_rlm(k_rlm,2) * sp_vp_l1         &
     &      + tw_rlm(1,3,j_rlm) * omega_rlm(k_rlm,1) * sp_wp_k1         &
     &      + tw_rlm(2,3,j_rlm) * omega_rlm(k_rlm,1) * sp_wp_k2         &
     &      + tw_rlm(1,4,j_rlm) * omega_rlm(k_rlm,0) * sp_dwp_k1        &
     &      + tw_rlm(2,4,j_rlm) * omega_rlm(k_rlm,0) * sp_dwp_k2
!
!
          d_rotp_cor_rlm(i_rlm)                                         &
     &          = -coef_cor * d_rotp_cor_rlm(i_rlm)                     &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
          d_rott_cor_rlm(i_rlm)                                         &
     &          = -coef_cor * d_rott_cor_rlm(i_rlm)                     &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_rot_coriolis_rlm_10
!*
!*   ------------------------------------------------------------------
!*
      subroutine sum_div_coriolis_rlm_10(b_trns, nnod_rlm, nidx_rlm,    &
     &          idx_gl_1d_rlm_j, a_r_1d_rlm_r, omega_rlm, coef_cor,     &
     &          jgi_cor_rlm, jei_cor_rlm, sd_rlm, td_rlm,               &
     &          NB, n_WR, irev_sr_rlm, WR, d_div_cor_rlm)
!
      type(phys_address), intent(in) :: b_trns
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: omega_rlm(nidx_rlm(1),0:2)
      real(kind = kreal), intent(in) :: coef_cor
!
      integer(kind = kint), intent(in) :: jgi_cor_rlm(nidx_rlm(2),2)
      integer(kind = kint), intent(in) :: jei_cor_rlm(nidx_rlm(2),1)
      real(kind = kreal), intent(in) :: sd_rlm(2,2,nidx_rlm(2))
      real(kind = kreal), intent(in) :: td_rlm(2,nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: NB, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: d_div_cor_rlm(nnod_rlm)
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm, j11
      integer(kind = kint) :: i11, i21, i12, ir_11, ir_21, ir_12
      real(kind = kreal) :: sp_wt_l1, sp_wp_k1, sp_wp_k2
      real(kind = kreal) :: sp_dwp_k1, sp_dwp_k2
!
!
!$omp  parallel do                                                      &
!$omp& private(k_rlm,j_rlm,i_rlm,i11,i21,i12,ir_11,ir_21,ir_12,         &
!$omp&         sp_wt_l1,sp_wp_k1,sp_wp_k2,sp_dwp_k1,sp_dwp_k2)
      do k_rlm = 1, nidx_rlm(1)
        do j_rlm = 1, nidx_rlm(2)
          i_rlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
!
          i11 = jgi_cor_rlm(j_rlm,1) + (k_rlm-1)*nidx_rlm(2)
          i21 = jgi_cor_rlm(j_rlm,2) + (k_rlm-1)*nidx_rlm(2)
          i12 = jei_cor_rlm(j_rlm,1) + (k_rlm-1)*nidx_rlm(2)
!
          ir_11 = irev_sr_rlm(i11) - 1
          ir_21 = irev_sr_rlm(i21) - 1
          ir_12 = irev_sr_rlm(i12) - 1
!
          sp_wt_l1 =        WR(b_trns%i_vort+2+NB*ir_12)
          sp_wp_k1 = half * WR(b_trns%i_vort+  NB*ir_11)
          sp_wp_k2 = half * WR(b_trns%i_vort+  NB*ir_21)
          sp_dwp_k1 =       WR(b_trns%i_vort+1+NB*ir_11)
          sp_dwp_k2 =       WR(b_trns%i_vort+1+NB*ir_21)
!
          d_div_cor_rlm(i_rlm)                                          &
     &     =  td_rlm(1,j_rlm) *   omega_rlm(k_rlm,1) * sp_wt_l1         &
     &      + sd_rlm(1,1,j_rlm) * omega_rlm(k_rlm,2) * sp_wp_k1         &
     &      + sd_rlm(2,1,j_rlm) * omega_rlm(k_rlm,2) * sp_wp_k2         &
     &      + sd_rlm(1,2,j_rlm) * omega_rlm(k_rlm,1) * sp_dwp_k1        &
     &      + sd_rlm(2,2,j_rlm) * omega_rlm(k_rlm,1) * sp_dwp_k2
!
          d_div_cor_rlm(i_rlm)                                          &
     &     = -coef_cor * d_div_cor_rlm(i_rlm)                           &
     &                 * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
      end do
!$omp end parallel do
!
!  Degree 0
      j_rlm = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,  &
     &       izero, izero)
      j11 = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,    &
     &       ione, izero)
!
      if( (j_rlm*j11) .gt. 0) then
!$omp  parallel do private(k_rlm,i_rlm,i11,ir_11,sp_wp_k1,sp_dwp_k1)
        do k_rlm = 1, nidx_rlm(1)
          i_rlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
          i11 =  j11 + (k_rlm-1)*nidx_rlm(2)
          ir_11 = irev_sr_rlm(i11)
!
          sp_wp_k1 = half * WR(b_trns%i_vort+  NB*ir_11)
          sp_dwp_k1 =       WR(b_trns%i_vort+1+NB*ir_11)
!
          d_div_cor_rlm(i_rlm)                                          &
     &       =  four*(two/three) * sp_wp_k1                             &
     &        + four*(two/three) * omega_rlm(k_rlm,1) * sp_dwp_k1
!
          d_div_cor_rlm(i_rlm)                                          &
     &       = -coef_cor * d_div_cor_rlm(i_rlm)                         &
     &                   * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
!$omp end parallel do
      end if
!
      end subroutine sum_div_coriolis_rlm_10
!*
!*   ------------------------------------------------------------------
!*
      subroutine sum_r_coriolis_bc_rlm_10(b_trns, nnod_rlm, nidx_rlm,   &
     &          idx_gl_1d_rlm_j, a_r_1d_rlm_r, omega_rlm, coef_cor,     &
     &          jgi_cor_rlm, jei_cor_rlm, sr_rlm, tr_rlm,               &
     &          NB, kr, n_WR, irev_sr_rlm,  WR, d_cor_bc)
!
      type(phys_address), intent(in) :: b_trns
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: omega_rlm(nidx_rlm(1),0:2)
      real(kind = kreal), intent(in) :: coef_cor
!
      integer(kind = kint), intent(in) :: jgi_cor_rlm(nidx_rlm(2),2)
      integer(kind = kint), intent(in) :: jei_cor_rlm(nidx_rlm(2),1)
      real(kind = kreal), intent(in) :: sr_rlm(2,nidx_rlm(2))
      real(kind = kreal), intent(in) :: tr_rlm(2,nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: NB, kr, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: d_cor_bc(nidx_rlm(2))
!
      integer(kind = kint) :: j_rlm
      integer(kind = kint) :: j11, i11, i21, i12, ir_11, ir_21, ir_12
      real(kind = kreal) :: sp_dwp_l1, sp_vt_k1, sp_vt_k2
!
!
      if(kr .le. 0) return
!
!$omp  parallel do private(j_rlm,i11,i21,i12,ir_11,ir_21,ir_12,         &
!$omp&                     sp_dwp_l1,sp_vt_k1,sp_vt_k2)
      do j_rlm = 1, nidx_rlm(2)
        i11 = jgi_cor_rlm(j_rlm,1) + (kr-1)*nidx_rlm(2)
        i21 = jgi_cor_rlm(j_rlm,2) + (kr-1)*nidx_rlm(2)
        i12 = jei_cor_rlm(j_rlm,1) + (kr-1)*nidx_rlm(2)
!
        ir_11 = irev_sr_rlm(i11) - 1
        ir_21 = irev_sr_rlm(i21) - 1
        ir_12 = irev_sr_rlm(i12) - 1
!
        sp_dwp_l1 = WR(b_trns%i_velo+1+NB*ir_12)
        sp_vt_k1 =  WR(b_trns%i_velo+2+NB*ir_11)
        sp_vt_k2 =  WR(b_trns%i_velo+2+NB*ir_21)
!
        d_cor_bc(j_rlm)                                                 &
     &       =  sr_rlm(1,j_rlm) * omega_rlm(kr,1) * sp_dwp_l1           &
     &        + tr_rlm(1,j_rlm) * omega_rlm(kr,1) * sp_vt_k1            &
     &        + tr_rlm(2,j_rlm) * omega_rlm(kr,1) * sp_vt_k2
!
        d_cor_bc(j_rlm) = -coef_cor*a_r_1d_rlm_r(kr)*a_r_1d_rlm_r(kr)   &
     &                   * d_cor_bc(j_rlm)
      end do
!$omp end parallel do
!
!  Degree 0
      j_rlm = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,  &
     &       izero, izero)
      j11 = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,    &
     &       ione, izero)
      if( (j_rlm*j11) .gt. 0) then
        i11 =  j11 + (kr-1)*nidx_rlm(2)
        ir_11 = irev_sr_rlm(i11)
!
        sp_vt_k1 = WR(b_trns%i_velo+2+NB*ir_11)
!
!        d_cor_bc(j_rlm) = -four*pi*(two/three)                         &
!     &                   * omega_rlm(kr,1) * sp_vt_k1
        d_cor_bc(j_rlm) = -(two/three)                                  &
     &                   * omega_rlm(kr,1) * sp_vt_k1
        d_cor_bc(j_rlm) = -coef_cor*a_r_1d_rlm_r(kr)*a_r_1d_rlm_r(kr)   &
     &                   * d_cor_bc(j_rlm)
      end if
!
      end subroutine sum_r_coriolis_bc_rlm_10
!*
!*   ------------------------------------------------------------------
!
      subroutine inner_core_rot_z_coriolis_rlm                          &
     &         (b_trns, nnod_rlm, nidx_rlm, radius_1d_rlm_r,            &
     &          omega_rlm, coef_cor, NB, n_WR, irev_sr_rlm, WR,         &
     &          idx_rlm_ICB, idx_rlm_degree_one, dp_rot_cor_rlm)
!
      type(phys_address), intent(in) :: b_trns
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: radius_1d_rlm_r(nidx_rlm(2))
      real(kind = kreal), intent(in) :: omega_rlm(nidx_rlm(1),0:2)
      real(kind = kreal), intent(in) :: coef_cor
      integer(kind = kint), intent(in) :: idx_rlm_ICB
      integer(kind = kint), intent(in) :: idx_rlm_degree_one(-1:1)
!
      integer(kind = kint), intent(in) :: NB, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: dp_rot_cor_rlm(nnod_rlm)
!
      integer(kind = kint) :: i11s, i10c, i11c, ir_11c, ir_11s
      real(kind = kreal) :: sp_wp_11c, sp_wp_11s
!
!
      if(idx_rlm_ICB .eq. 0) return
      if((idx_rlm_degree_one(-1)*idx_rlm_degree_one(1)) .eq. 0) return
!
      i11s = idx_rlm_degree_one(-1) + (idx_rlm_ICB-1)*nidx_rlm(2)
      i10c = idx_rlm_degree_one( 0) + (idx_rlm_ICB-1)*nidx_rlm(2)
      i11c = idx_rlm_degree_one( 1) + (idx_rlm_ICB-1)*nidx_rlm(2)
!
      ir_11s = irev_sr_rlm(i11s) - 1
      ir_11c = irev_sr_rlm(i11c) - 1
!
      sp_wp_11s = WR(b_trns%i_vort+NB*ir_11s)
      sp_wp_11c = WR(b_trns%i_vort+NB*ir_11c)
!
      dp_rot_cor_rlm(i10c) = zero
      dp_rot_cor_rlm(i11s)                                              &
     &       = -two*coef_cor*radius_1d_rlm_r(idx_rlm_ICB)               &
     &        * omega_rlm(idx_rlm_ICB,0)*sp_wp_11c
      dp_rot_cor_rlm(i11c)                                              &
     &       =  two*coef_cor*radius_1d_rlm_r(idx_rlm_ICB)               &
     &        * omega_rlm(idx_rlm_ICB,0)*sp_wp_11s
!
      end subroutine inner_core_rot_z_coriolis_rlm
!
! ----------------------------------------------------------------------
!
      end module sum_coriolis_terms_rlm
