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
!!      subroutine select_sum_coriolis_rlm_10                           &
!!     &         (b_trns, sph_rlm, omega_sph, gt_cor,                   &
!!     &          coef_cor, NB, n_WR, irev_sr_rlm, WR,                  &
!!     &          pol_cor_rlm, hrz_cor_rlm, tor_cor_rlm)
!!        type(phys_address), intent(in) :: b_trns
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!!        integer(kind = kint), intent(in) :: NB, n_WR
!!        integer(kind = kint), intent(in)                              &
!!     &                               :: irev_sr_rlm(sph_rlm%nnod_rlm)
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        real(kind = kreal), intent(in) :: coef_cor
!!        real(kind = kreal), intent(inout)                             &
!!     &                               :: pol_cor_rlm(sph_rlm%nnod_rlm)
!!        real(kind = kreal), intent(inout)                             &
!!     &                               :: hrz_cor_rlm(sph_rlm%nnod_rlm)
!!        real(kind = kreal), intent(inout)                             &
!!     &                               :: tor_cor_rlm(sph_rlm%nnod_rlm)
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
!!     (wsr) = wsr(jc,1,j3)*dw*uhb
!!     (wtr) = wtr(j3)*dw*utb
!!
!!  Horizontal componenet of the Coriolis term
!!     (wsh) = wsh(jc,j3)*dw*urb/r**2
!!            + whh(jc,j3)*w*uhb/r**2
!!     (wth) = wth(j3)*dw*utb/r**2
!!
!!************************************************
!!
!!************************************************
!!
!!     wss(jc,1,j3) = sw_rlm(jc,1,j3)
!!     wss(jc,2,j3) = sw_rlm(jc,2,j3)
!!     wts(jc,j3)   = sw_rlm(jc,3,j3)
!!
!!     wsr(jc,j3) =   sr_rlm(jc,j3)
!!     wtr(jc,j3) =   tr_rlm(jc,j3)
!!
!!     wsh(jc,j3) =   sh_rlm(jc,j3)
!!     whh(jc,j3) =   hh_rlm(jc,j3)
!!     wth(jc,j3) =   th_rlm(jc,j3)
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
      use t_spheric_rlm_data
      use t_phys_address
      use t_base_field_labels
      use t_poloidal_rotation
      use t_gaunt_coriolis_rlm
!
      implicit none
!
      private :: sum_coriolis_rlm_10, sum_coriolis_lrm_10
!
!   ------------------------------------------------------------------
!
      contains
!
!   ------------------------------------------------------------------
!
      subroutine select_sum_coriolis_rlm_10                             &
     &         (b_trns, sph_rlm, omega_sph, gt_cor,                     &
     &          coef_cor, NB, n_WR, irev_sr_rlm, WR,                    &
     &          pol_cor_rlm, hrz_cor_rlm, tor_cor_rlm)
!
      type(phys_address), intent(in) :: b_trns
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rotation), intent(in) :: omega_sph
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!
      integer(kind = kint), intent(in) :: NB, n_WR
      integer(kind = kint), intent(in)                                  &
     &                               :: irev_sr_rlm(sph_rlm%nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
      real(kind = kreal), intent(in) :: coef_cor
!
      real(kind = kreal), intent(inout)                                 &
     &                               :: pol_cor_rlm(sph_rlm%nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &                               :: hrz_cor_rlm(sph_rlm%nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &                               :: tor_cor_rlm(sph_rlm%nnod_rlm)
!
!
      if(sph_rlm%istep_rlm(1) .eq. 1) then
        call sum_coriolis_rlm_10                                        &
     &     (b_trns%base, sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,            &
     &      sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r,              &
     &      omega_sph%ws_rlm, coef_cor, gt_cor%jgi_rlm, gt_cor%jei_rlm, &
     &      gt_cor%sw_rlm, gt_cor%sr_rlm, gt_cor%tr_rlm,                &
     &      gt_cor%sh_rlm, gt_cor%hh_rlm, gt_cor%th_rlm,                &
     &      NB, n_WR, irev_sr_rlm, WR,                                  &
     &      pol_cor_rlm, hrz_cor_rlm, tor_cor_rlm)
      else
        call sum_coriolis_lrm_10                                        &
     &     (b_trns%base, sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,            &
     &      sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r,              &
     &      omega_sph%ws_rlm, coef_cor, gt_cor%jgi_rlm, gt_cor%jei_rlm, &
     &      gt_cor%sw_rlm, gt_cor%sr_rlm, gt_cor%tr_rlm,                &
     &      gt_cor%sh_rlm, gt_cor%hh_rlm, gt_cor%th_rlm,                &
     &      NB, n_WR, irev_sr_rlm, WR,                                  &
     &      pol_cor_rlm, hrz_cor_rlm, tor_cor_rlm)
      end if
!
      end subroutine select_sum_coriolis_rlm_10
!
!   ------------------------------------------------------------------
!
      subroutine sum_coriolis_lrm_10(t_base,                            &
     &          nnod_rlm, nidx_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r,      &
     &          omega_rlm, coef_cor, jgi_cor_rlm, jei_cor_rlm,          &
     &          sw_rlm, sr_rlm, tr_rlm, sh_rlm, hh_rlm, th_rlm,         &
     &          NB, n_WR, irev_sr_rlm, WR,                              &
     &          pol_cor_rlm, hrz_cor_rlm, tor_cor_rlm)
!
      type(base_field_address), intent(in) :: t_base
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
      real(kind = kreal), intent(in) :: sw_rlm(2,3,nidx_rlm(2))
      real(kind = kreal), intent(in) :: sr_rlm(2,nidx_rlm(2))
      real(kind = kreal), intent(in) :: tr_rlm(2,nidx_rlm(2))
      real(kind = kreal), intent(in) :: sh_rlm(2,nidx_rlm(2))
      real(kind = kreal), intent(in) :: hh_rlm(2,nidx_rlm(2))
      real(kind = kreal), intent(in) :: th_rlm(2,nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: NB, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: pol_cor_rlm(nnod_rlm)
      real(kind = kreal), intent(inout) :: hrz_cor_rlm(nnod_rlm)
      real(kind = kreal), intent(inout) :: tor_cor_rlm(nnod_rlm)
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm, j11
      integer(kind = kint) :: i11, i21, i12, ir_11, ir_21, ir_12
      real(kind = kreal) :: sp_vr_l1, sp_vh_l1, sp_vt_k1, sp_vt_k2
      real(kind = kreal) :: sp_dvp_k1, sp_dvp_k2, sp_vp_k1, sp_vp_k2
      real(kind = kreal) :: sp_vt_l1
!
!
!$omp  parallel do                                                      &
!$omp& private(k_rlm,j_rlm,i_rlm,i11,i21,i12,ir_11,ir_21,ir_12,         &
!$omp&         sp_vr_l1,sp_vh_l1,sp_vt_k1,sp_vt_k2,sp_dvp_k1,sp_dvp_k2, &
!$omp&         sp_vp_k1,sp_vp_k2,sp_vt_l1)
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
          sp_vr_l1 = WR(t_base%i_velo +   NB*ir_12)
          sp_vh_l1 = WR(t_base%i_velo+1 + NB*ir_12)
          sp_vt_k1 = WR(t_base%i_velo+2 + NB*ir_11)
          sp_vt_k2 = WR(t_base%i_velo+2 + NB*ir_21)
          sp_dvp_k1 = WR(t_base%i_velo+1 + NB*ir_11)
          sp_dvp_k2 = WR(t_base%i_velo+1 + NB*ir_21)
          sp_vt_l1 =  WR(t_base%i_velo+2 +  NB*ir_12)
          sp_vp_k1 =  WR(t_base%i_velo +  NB*ir_11)
          sp_vp_k2 =  WR(t_base%i_velo +  NB*ir_21)
!
          pol_cor_rlm(i_rlm)                                            &
     &     =  sr_rlm(1,j_rlm) * omega_rlm(k_rlm,1) * sp_vh_l1           &
     &      + tr_rlm(1,j_rlm) * omega_rlm(k_rlm,1) * sp_vt_k1           &
     &      + tr_rlm(2,j_rlm) * omega_rlm(k_rlm,1) * sp_vt_k2
!*
          hrz_cor_rlm(i_rlm)                                            &
     &     =  sh_rlm(1,j_rlm) * omega_rlm(k_rlm,1) * sp_vr_l1           &
     &      + hh_rlm(1,j_rlm) * omega_rlm(k_rlm,0) * sp_vh_l1           &
     &      + th_rlm(1,j_rlm) * omega_rlm(k_rlm,0) * sp_vt_k1           &
     &      + th_rlm(2,j_rlm) * omega_rlm(k_rlm,0) * sp_vt_k2
!*
          tor_cor_rlm(i_rlm)                                            &
     &     =  sw_rlm(1,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k1        &
     &      + sw_rlm(2,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k2        &
     &      + sw_rlm(1,3,j_rlm) * omega_rlm(k_rlm,0) * sp_vt_l1         &
     &      + sw_rlm(1,2,j_rlm) * omega_rlm(k_rlm,1) * sp_vp_k1         &
     &      + sw_rlm(2,2,j_rlm) * omega_rlm(k_rlm,1) * sp_vp_k2
!
!
          pol_cor_rlm(i_rlm) = -coef_cor * pol_cor_rlm(i_rlm)
          hrz_cor_rlm(i_rlm) = -coef_cor * hrz_cor_rlm(i_rlm)           &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
          tor_cor_rlm(i_rlm) = -coef_cor * tor_cor_rlm(i_rlm)           &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
      end do
!$omp end parallel do
!
!  Degree 0
      j_rlm = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,  &
     &       izero, izero)
      j11 = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,    &
     &       ione, izero)
      if( (j_rlm*j11) .le. 0) return
!$omp  parallel do                                                      &
!$omp& private(k_rlm,i_rlm,i11,ir_11,sp_vt_k1)
      do k_rlm = 1, nidx_rlm(1)
          i_rlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
!
!          write(*,*) 'jgi_cor_rlm', jgi_cor_rlm(j_rlm,1:2),            &
!     &                              jei_cor_rlm(j_rlm,1)
        i11 =  j11 + (k_rlm-1)*nidx_rlm(2)
        ir_11 = irev_sr_rlm(i11) - 1
        sp_vt_k1 = WR(t_base%i_velo+2 + NB*ir_11)
!
        pol_cor_rlm(i_rlm)                                              &
     &     = -(two/three) * omega_rlm(k_rlm,1) * sp_vt_k1
        pol_cor_rlm(i_rlm) = -coef_cor * pol_cor_rlm(i_rlm)
      end do
!$omp end parallel do
!
      end subroutine sum_coriolis_lrm_10
!*
!*   ------------------------------------------------------------------
!
      subroutine sum_coriolis_rlm_10(t_base,                            &
     &          nnod_rlm, nidx_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r,      &
     &          omega_rlm, coef_cor, jgi_cor_rlm, jei_cor_rlm,          &
     &          sw_rlm, sr_rlm, tr_rlm, sh_rlm, hh_rlm, th_rlm,         &
     &          NB, n_WR, irev_sr_rlm, WR,                              &
     &          pol_cor_rlm, hrz_cor_rlm, tor_cor_rlm)
!
      type(base_field_address), intent(in) :: t_base
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
      real(kind = kreal), intent(in) :: sw_rlm(2,3,nidx_rlm(2))
      real(kind = kreal), intent(in) :: sr_rlm(2,nidx_rlm(2))
      real(kind = kreal), intent(in) :: tr_rlm(2,nidx_rlm(2))
      real(kind = kreal), intent(in) :: sh_rlm(2,nidx_rlm(2))
      real(kind = kreal), intent(in) :: hh_rlm(2,nidx_rlm(2))
      real(kind = kreal), intent(in) :: th_rlm(2,nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: NB, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: pol_cor_rlm(nnod_rlm)
      real(kind = kreal), intent(inout) :: hrz_cor_rlm(nnod_rlm)
      real(kind = kreal), intent(inout) :: tor_cor_rlm(nnod_rlm)
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm, j11
      integer(kind = kint) :: i11, i21, i12, ir_11, ir_21, ir_12
      real(kind = kreal) :: sp_vr_l1, sp_vh_l1, sp_vt_k1, sp_vt_k2
      real(kind = kreal) :: sp_dvp_k1, sp_dvp_k2, sp_vp_k1, sp_vp_k2
      real(kind = kreal) :: sp_vt_l1
!
!
!$omp  parallel do                                                      &
!$omp& private(k_rlm,j_rlm,i_rlm,i11,i21,i12,ir_11,ir_21,ir_12,         &
!$omp&         sp_vr_l1,sp_vh_l1,sp_vt_k1,sp_vt_k2,sp_dvp_k1,sp_dvp_k2, &
!$omp&         sp_vp_k1,sp_vp_k2,sp_vt_l1)
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
          sp_vr_l1 = WR(t_base%i_velo +   NB*ir_12)
          sp_vh_l1 = WR(t_base%i_velo+1 + NB*ir_12)
          sp_vt_k1 = WR(t_base%i_velo+2 + NB*ir_11)
          sp_vt_k2 = WR(t_base%i_velo+2 + NB*ir_21)
          sp_dvp_k1 = WR(t_base%i_velo+1 + NB*ir_11)
          sp_dvp_k2 = WR(t_base%i_velo+1 + NB*ir_21)
          sp_vt_l1 =  WR(t_base%i_velo+2 + NB*ir_12)
          sp_vp_k1 =  WR(t_base%i_velo +  NB*ir_11)
          sp_vp_k2 =  WR(t_base%i_velo +  NB*ir_21)
!
          pol_cor_rlm(i_rlm)                                            &
     &     =  sr_rlm(1,j_rlm) * omega_rlm(k_rlm,1) * sp_vh_l1           &
     &      + tr_rlm(1,j_rlm) * omega_rlm(k_rlm,1) * sp_vt_k1           &
     &      + tr_rlm(2,j_rlm) * omega_rlm(k_rlm,1) * sp_vt_k2
!*
          hrz_cor_rlm(i_rlm)                                            &
     &     =  sh_rlm(1,j_rlm) * omega_rlm(k_rlm,1) * sp_vr_l1           &
     &      + hh_rlm(1,j_rlm) * omega_rlm(k_rlm,0) * sp_vh_l1           &
     &      + th_rlm(1,j_rlm) * omega_rlm(k_rlm,0) * sp_vt_k1           &
     &      + th_rlm(2,j_rlm) * omega_rlm(k_rlm,0) * sp_vt_k2
!*
          tor_cor_rlm(i_rlm)                                            &
     &     =  sw_rlm(1,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k1        &
     &      + sw_rlm(2,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k2        &
     &      + sw_rlm(1,3,j_rlm) * omega_rlm(k_rlm,0) * sp_vt_l1         &
     &      + sw_rlm(1,2,j_rlm) * omega_rlm(k_rlm,1) * sp_vp_k1         &
     &      + sw_rlm(2,2,j_rlm) * omega_rlm(k_rlm,1) * sp_vp_k2
!
          pol_cor_rlm(i_rlm) = -coef_cor * pol_cor_rlm(i_rlm)
          hrz_cor_rlm(i_rlm) = -coef_cor * hrz_cor_rlm(i_rlm)           &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
          tor_cor_rlm(i_rlm) = -coef_cor * tor_cor_rlm(i_rlm)           &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
      end do
!$omp end parallel do
!
!  Degree 0
      j_rlm = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,  &
     &       izero, izero)
      j11 = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,    &
     &       ione, izero)
      if( (j_rlm*j11) .le. 0) return
!$omp  parallel do                                                      &
!$omp& private(k_rlm,i_rlm,i11,ir_11,sp_vh_l1,sp_vt_k1)
        do k_rlm = 1, nidx_rlm(1)
          i_rlm = (j_rlm - 1)*nidx_rlm(1) + k_rlm
!
!          write(*,*) 'jgi_cor_rlm', jgi_cor_rlm(j_rlm,1:2),            &
!     &                              jei_cor_rlm(j_rlm,1)
          i11 = (j11-1)*nidx_rlm(1) + k_rlm
          ir_11 = irev_sr_rlm(i11) - 1
          sp_vt_k1 = WR(t_base%i_velo+2 + NB*ir_11)
!
          pol_cor_rlm(i_rlm)                                            &
     &     =  -(two/three) * omega_rlm(k_rlm,1) * sp_vt_k1
          pol_cor_rlm(i_rlm) = -coef_cor * pol_cor_rlm(i_rlm)
        end do
!$omp end parallel do
!
      end subroutine sum_coriolis_rlm_10
!*
!*   ------------------------------------------------------------------
!
      end module sum_coriolis_terms_rlm
