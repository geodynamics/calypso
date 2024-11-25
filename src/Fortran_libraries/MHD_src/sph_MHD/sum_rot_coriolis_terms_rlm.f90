!>@file   sum_rot_coriolis_terms_rlm.f90
!!@brief  module sum_rot_coriolis_terms_rlm
!!
!!@author H. Matsui
!!@date Programmed in 1995
!@n     Modified in Oct., 2009
!
!>@brief  Evaluate curl of Coriolis term by Gaunt integrals
!!
!!@verbatim
!!************************************************
!!      subroutine select_sum_rot_coriolis_rlm_10(b_trns, sph_rlm, leg, &
!!     &         omega_sph, gt_cor, coef_cor, NB, n_WR, irev_sr_rlm, WR,&
!!     &         d_rotp_cor_rlm, d_rott_cor_rlm)
!!        type(phys_address), intent(in) :: b_trns
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!!        integer(kind = kint), intent(in) :: NB, n_WR
!!        integer(kind = kint), intent(in)                              &
!!     &                               :: irev_sr_rlm(sph_rlm%nnod_rlm)
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        real(kind = kreal), intent(in) :: coef_cor
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: d_rotp_cor_rlm(sph_rlm%nnod_rlm)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: d_rott_cor_rlm(sph_rlm%nnod_rlm)
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
!!  (  wst(jc,0,j3) = tw_rlm(jc,0,j3) )
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
      module sum_rot_coriolis_terms_rlm
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      use t_spheric_rlm_data
      use t_phys_address
      use t_base_field_labels
      use t_poloidal_rotation
      use t_schmidt_poly_on_rtm
      use t_gaunt_coriolis_rlm
!
      implicit none
!
      private :: sum_rot_coriolis_rlm_10, sum_rot_coriolis_lrm_10
!
!   ------------------------------------------------------------------
!
      contains
!
!   ------------------------------------------------------------------
!
      subroutine select_sum_rot_coriolis_rlm_10(b_trns, sph_rlm, leg,   &
     &         omega_sph, gt_cor, coef_cor, NB, n_WR, irev_sr_rlm, WR,  &
     &         d_rotp_cor_rlm, d_rott_cor_rlm)
!
      type(phys_address), intent(in) :: b_trns
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(sph_rotation), intent(in) :: omega_sph
!
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!
      integer(kind = kint), intent(in) :: NB, n_WR
      integer(kind = kint), intent(in)                                  &
     &                               :: irev_sr_rlm(sph_rlm%nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
      real(kind = kreal), intent(in) :: coef_cor
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_rotp_cor_rlm(sph_rlm%nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_rott_cor_rlm(sph_rlm%nnod_rlm)
!
!
      if(sph_rlm%istep_rlm(1) .eq. 1) then
        call sum_rot_coriolis_rlm_10(b_trns%base,                       &
     &    sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%a_r_1d_rlm_r,     &
     &    leg%g_sph_rlm, omega_sph%ws_rlm, coef_cor,                    &
     &    gt_cor%jgi_rlm, gt_cor%jei_rlm, gt_cor%sw_rlm, gt_cor%tw_rlm, &
     &    NB, n_WR, irev_sr_rlm, WR, d_rotp_cor_rlm, d_rott_cor_rlm)
      else
        call sum_rot_coriolis_lrm_10(b_trns%base,                       &
     &    sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%a_r_1d_rlm_r,     &
     &    leg%g_sph_rlm, omega_sph%ws_rlm, coef_cor,                    &
     &    gt_cor%jgi_rlm, gt_cor%jei_rlm, gt_cor%sw_rlm, gt_cor%tw_rlm, &
     &    NB, n_WR, irev_sr_rlm, WR, d_rotp_cor_rlm, d_rott_cor_rlm)
      end if
!
      end subroutine select_sum_rot_coriolis_rlm_10
!
!   ------------------------------------------------------------------
!   ------------------------------------------------------------------
!
      subroutine sum_rot_coriolis_lrm_10(t_base, nnod_rlm, nidx_rlm,    &
     &          a_r_1d_rlm_r, g_sph_rlm, omega_rlm, coef_cor,           &
     &          jgi_cor_rlm, jei_cor_rlm, sw_rlm, tw_rlm, NB, n_WR,     &
     &          irev_sr_rlm, WR, d_rotp_cor_rlm, d_rott_cor_rlm)
!
      type(base_field_address), intent(in) :: t_base
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
      real(kind = kreal) :: sp_vt_l1, sp_d2vp_l1, sp_vp_l1
      real(kind = kreal) :: sp_wp_k1, sp_wp_k2, sp_dwp_k1, sp_dwp_k2
!
!
!$omp  parallel do                                                      &
!$omp& private(k_rlm,j_rlm,i_rlm,i11,i21,i12,ir_11,ir_21,ir_12,         &
!$omp&         sp_dvp_k1,sp_dvp_k2,sp_vp_k1,sp_vp_k2,sp_vt_l1,          &
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
          sp_dvp_k1 = WR(t_base%i_velo+1 + NB*ir_11)
          sp_dvp_k2 = WR(t_base%i_velo+1 + NB*ir_21)
          sp_vt_l1 =  WR(t_base%i_velo+2 + NB*ir_12)
          sp_vp_k1 =  WR(t_base%i_velo +  NB*ir_11)
          sp_vp_k2 =  WR(t_base%i_velo +  NB*ir_21)
!
          sp_d2vp_l1 = (a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)         &
     &        * g_sph_rlm(j_rlm,3)*WR(t_base%i_velo +  NB*ir_12)        &
     &                           - WR(t_base%i_vort+2 + NB*ir_12))
          sp_vp_l1 = WR(t_base%i_velo+NB*ir_12)
          sp_wp_k1 = WR(t_base%i_vort+NB*ir_11)
          sp_wp_k2 = WR(t_base%i_vort+NB*ir_21)
          sp_dwp_k1 = (WR(t_base%i_vort+1+NB*ir_11)                     &
     &       - two*a_r_1d_rlm_r(k_rlm)*WR(t_base%i_vort+NB*ir_11))
          sp_dwp_k2 = (WR(t_base%i_vort+1+NB*ir_21)                     &
     &       - two*a_r_1d_rlm_r(k_rlm)*WR(t_base%i_vort+NB*ir_21))
!
          d_rotp_cor_rlm(i_rlm)                                         &
     &     =  sw_rlm(1,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k1        &
     &      + sw_rlm(2,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k2        &
     &      + sw_rlm(1,3,j_rlm) * omega_rlm(k_rlm,0) * sp_vt_l1         &
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
      end subroutine sum_rot_coriolis_lrm_10
!*
!*   ------------------------------------------------------------------
!
      subroutine sum_rot_coriolis_rlm_10(t_base, nnod_rlm, nidx_rlm,    &
     &          a_r_1d_rlm_r, g_sph_rlm, omega_rlm, coef_cor,           &
     &          jgi_cor_rlm, jei_cor_rlm, sw_rlm, tw_rlm, NB, n_WR,     &
     &          irev_sr_rlm, WR, d_rotp_cor_rlm, d_rott_cor_rlm)
!
      type(base_field_address), intent(in) :: t_base
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
      real(kind = kreal) :: sp_vt_l1, sp_d2vp_l1, sp_vp_l1
      real(kind = kreal) :: sp_wp_k1, sp_wp_k2, sp_dwp_k1, sp_dwp_k2
!
!
!$omp  parallel do                                                      &
!$omp& private(k_rlm,j_rlm,i_rlm,i11,i21,i12,ir_11,ir_21,ir_12,         &
!$omp&         sp_dvp_k1,sp_dvp_k2,sp_vp_k1,sp_vp_k2,sp_vt_l1,          &
!$omp&         sp_d2vp_l1,sp_vp_l1,sp_wp_k1,sp_wp_k2,sp_dwp_k1,         &
!$omp&         sp_dwp_k2)
      do j_rlm = 1, nidx_rlm(2)
        do k_rlm = 1, nidx_rlm(1)
          i_rlm = (j_rlm - 1)*nidx_rlm(1) + k_rlm
!
!          write(*,*) 'jgi_cor_rlm', jgi_cor_rlm(j_rlm,1:2),            &
!     &                              jei_cor_rlm(j_rlm,1)
          i11 = (jgi_cor_rlm(j_rlm,1) -1)*nidx_rlm(1) + k_rlm
          i21 = (jgi_cor_rlm(j_rlm,2) -1)*nidx_rlm(1) + k_rlm
          i12 = (jei_cor_rlm(j_rlm,1) -1)*nidx_rlm(1) + k_rlm
!
          ir_11 = irev_sr_rlm(i11) - 1
          ir_21 = irev_sr_rlm(i21) - 1
          ir_12 = irev_sr_rlm(i12) - 1
!
          sp_dvp_k1 = WR(t_base%i_velo+1 + NB*ir_11)
          sp_dvp_k2 = WR(t_base%i_velo+1 + NB*ir_21)
          sp_vt_l1 =  WR(t_base%i_velo+2 + NB*ir_12)
          sp_vp_k1 =  WR(t_base%i_velo +  NB*ir_11)
          sp_vp_k2 =  WR(t_base%i_velo +  NB*ir_21)
!
          sp_d2vp_l1 = (a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)         &
     &        * g_sph_rlm(j_rlm,3)*WR(t_base%i_velo +   NB*ir_12)       &
     &                           - WR(t_base%i_vort+2 + NB*ir_12))
          sp_vp_l1 = WR(t_base%i_velo+NB*ir_12)
          sp_wp_k1 = WR(t_base%i_vort+NB*ir_11)
          sp_wp_k2 = WR(t_base%i_vort+NB*ir_21)
          sp_dwp_k1 = (WR(t_base%i_vort+1+NB*ir_11)                     &
     &       - two*a_r_1d_rlm_r(k_rlm)*WR(t_base%i_vort+NB*ir_11))
          sp_dwp_k2 = (WR(t_base%i_vort+1+NB*ir_21)                     &
     &       - two*a_r_1d_rlm_r(k_rlm)*WR(t_base%i_vort+NB*ir_21))
!
          d_rotp_cor_rlm(i_rlm)                                         &
     &     =  sw_rlm(1,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k1        &
     &      + sw_rlm(2,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k2        &
     &      + sw_rlm(1,3,j_rlm) * omega_rlm(k_rlm,0) * sp_vt_l1         &
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
!
      end module sum_rot_coriolis_terms_rlm
