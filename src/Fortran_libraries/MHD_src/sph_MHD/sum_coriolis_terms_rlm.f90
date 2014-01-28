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
!!      subroutine sum_rot_coriolis_rlm_10(ncomp, sp_rlm)
!!      subroutine sum_div_coriolis_rlm_10(ncomp, sp_rlm)
!!      subroutine sum_r_coriolis_bc_rlm_10(ncomp, kr, sp_rlm, d_cor_bc)
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
      use m_spheric_parameter
      use m_physical_property
!
      use m_poloidal_rotation
      use m_schmidt_poly_on_rtm
      use m_gaunt_coriolis_rlm
!
      implicit none
!
!   ------------------------------------------------------------------
!
      contains
!
!   ------------------------------------------------------------------
!
      subroutine sum_rot_coriolis_rlm_10(ncomp, sp_rlm)
!
      use m_coriolis_terms_rlm
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: k_rlm, j_rlm
      integer(kind = kint) :: irlm,i11,i21,i12
      integer(kind = kint) :: ivp_rlm_k1, ivp_rlm_k2, ivp_rlm_l1
      integer(kind = kint) :: iwp_rlm_k1, iwp_rlm_k2, iwp_rlm_l1
      integer(kind = kint) :: ivd_rlm_k1, ivd_rlm_k2, iwt_rlm_l1
      integer(kind = kint) :: iwd_rlm_k1, iwd_rlm_k2
      integer(kind = kint) :: irlm_pol_cor, irlm_tor_cor
!
!
!$omp  parallel do                                                      &
!$omp& private(k_rlm,j_rlm,irlm,i11,i21,i12,ivp_rlm_k1,ivp_rlm_k2,      &
!$omp&         ivp_rlm_l1,iwp_rlm_k1,iwp_rlm_k2,iwp_rlm_l1,             &
!$omp&         ivd_rlm_k1,ivd_rlm_k2,iwt_rlm_l1,iwd_rlm_k1,iwd_rlm_k2,  &
!$omp&         irlm_pol_cor,irlm_tor_cor)
      do k_rlm = 1, nidx_rlm(1)
        do j_rlm = 1, nidx_rlm(2)
          irlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
!
!          write(*,*) 'jgi_cor_rlm', jgi_cor_rlm(j_rlm,1:2), jei_cor_rlm(j_rlm,1)
          i11 = jgi_cor_rlm(j_rlm,1) + (k_rlm-1)*nidx_rlm(2)
          i21 = jgi_cor_rlm(j_rlm,2) + (k_rlm-1)*nidx_rlm(2)
          i12 = jei_cor_rlm(j_rlm,1) + (k_rlm-1)*nidx_rlm(2)
!
          ivp_rlm_k1 = b_trns%i_velo + (i11-1) * ncomp
          ivp_rlm_k2 = b_trns%i_velo + (i21-1) * ncomp
          ivp_rlm_l1 = b_trns%i_velo + (i12-1) * ncomp
!
          iwp_rlm_k1 = b_trns%i_vort + (i11-1) * ncomp
          iwp_rlm_k2 = b_trns%i_vort + (i21-1) * ncomp
          iwp_rlm_l1 = b_trns%i_vort + (i12-1) * ncomp
!
          ivd_rlm_k1 = ivp_rlm_k1 + 1
          ivd_rlm_k2 = ivp_rlm_k2 + 1
!
          iwd_rlm_k1 = iwp_rlm_k1 + 1
          iwd_rlm_k2 = iwp_rlm_k2 + 1
!
          iwt_rlm_l1 = iwp_rlm_l1 + 2
!
          irlm_pol_cor = ip_rlm_rot_cor + ncomp_coriolis_rlm * (irlm-1)
          irlm_tor_cor = it_rlm_rot_cor + ncomp_coriolis_rlm * (irlm-1)
!
          d_cor_rlm(irlm_pol_cor)                                       &
     &     =  sw_rlm(1,1,j_rlm) * omega_rlm(k_rlm,0)*sp_rlm(ivd_rlm_k1) &
     &      + sw_rlm(2,1,j_rlm) * omega_rlm(k_rlm,0)*sp_rlm(ivd_rlm_k2) &
     &      + sw_rlm(1,3,j_rlm) * omega_rlm(k_rlm,0)*sp_rlm(iwp_rlm_l1) &
     &      + sw_rlm(1,2,j_rlm) * omega_rlm(k_rlm,1)*sp_rlm(ivp_rlm_k1) &
     &      + sw_rlm(2,2,j_rlm) * omega_rlm(k_rlm,1)*sp_rlm(ivp_rlm_k2)
!*
          d_cor_rlm(irlm_tor_cor)                                       &
     &     =  tw_rlm(1,1,j_rlm) * omega_rlm(k_rlm,0)                    &
     &       * (a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)                 &
     &        * g_sph_rlm(j_rlm,3)*sp_rlm(ivp_rlm_l1)                   &
     &                           - sp_rlm(iwt_rlm_l1) )                 &
     &      - tw_rlm(1,2,j_rlm) * omega_rlm(k_rlm,2)*sp_rlm(ivp_rlm_l1) &
     &      + tw_rlm(1,3,j_rlm) * omega_rlm(k_rlm,1)*sp_rlm(iwp_rlm_k1) &
     &      + tw_rlm(2,3,j_rlm) * omega_rlm(k_rlm,1)*sp_rlm(iwp_rlm_k2) &
     &      + tw_rlm(1,4,j_rlm) * omega_rlm(k_rlm,0)                    &
     &       * (sp_rlm(iwd_rlm_k1)                                      &
     &         - two*a_r_1d_rlm_r(k_rlm)*sp_rlm(iwp_rlm_k1))            &
     &      + tw_rlm(2,4,j_rlm) * omega_rlm(k_rlm,0)                    &
     &       * (sp_rlm(iwd_rlm_k2)                                      &
     &         - two*a_r_1d_rlm_r(k_rlm)*sp_rlm(iwp_rlm_k2))
!
!
          d_cor_rlm(irlm_pol_cor) = -coef_cor * d_cor_rlm(irlm_pol_cor) &
     &                        * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
          d_cor_rlm(irlm_tor_cor) = -coef_cor * d_cor_rlm(irlm_tor_cor) &
     &                        * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_rot_coriolis_rlm_10
!*
!*   ------------------------------------------------------------------
!*
      subroutine sum_div_coriolis_rlm_10(ncomp, sp_rlm)
!
      use m_coriolis_terms_rlm
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: k_rlm, j_rlm, j11
      integer(kind = kint) :: irlm,i11,i21,i12
      integer(kind = kint) :: iwp_rlm_k1, iwp_rlm_k2, iwp_rlm_l1
      integer(kind = kint) :: iwd_rlm_k1, iwd_rlm_k2, iwt_rlm_l1
      integer(kind = kint) :: irlm_div_cor
!
!
!$omp  parallel do                                                      &
!$omp& private(k_rlm,j_rlm,irlm,i11,i21,i12,iwp_rlm_k1,iwp_rlm_k2,      &
!$omp&         iwp_rlm_l1,iwt_rlm_l1,iwd_rlm_k1,iwd_rlm_k2,             &
!$omp&         irlm_div_cor)
      do k_rlm = 1, nidx_rlm(1)
        do j_rlm = 1, nidx_rlm(2)
          irlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
!
          i11 = jgi_cor_rlm(j_rlm,1) + (k_rlm-1)*nidx_rlm(2)
          i21 = jgi_cor_rlm(j_rlm,2) + (k_rlm-1)*nidx_rlm(2)
          i12 = jei_cor_rlm(j_rlm,1) + (k_rlm-1)*nidx_rlm(2)
!
          iwp_rlm_k1 = b_trns%i_vort + (i11-1)*ncomp
          iwp_rlm_k2 = b_trns%i_vort + (i21-1)*ncomp
          iwp_rlm_l1 = b_trns%i_vort + (i12-1)*ncomp
!
          iwd_rlm_k1 = iwp_rlm_k1 + 1
          iwd_rlm_k2 = iwp_rlm_k2 + 1
!
          iwt_rlm_l1 = iwp_rlm_l1 + 2
!
          irlm_div_cor = ip_rlm_div_cor + ncomp_coriolis_rlm * (irlm-1)
!
          d_cor_rlm(irlm_div_cor)                                       &
     &     =  td_rlm(1,j_rlm) *   omega_rlm(k_rlm,1)*sp_rlm(iwt_rlm_l1) &
     &      + sd_rlm(1,1,j_rlm) * half*omega_rlm(k_rlm,2)               &
     &                            * sp_rlm(iwp_rlm_k1)                  &
     &      + sd_rlm(2,1,j_rlm) * half*omega_rlm(k_rlm,2)               &
     &                            * sp_rlm(iwp_rlm_k2)                  &
     &      + sd_rlm(1,2,j_rlm) * omega_rlm(k_rlm,1)*sp_rlm(iwd_rlm_k1) &
     &      + sd_rlm(2,2,j_rlm) * omega_rlm(k_rlm,1)*sp_rlm(iwd_rlm_k2)
!
          d_cor_rlm(irlm_div_cor) = -coef_cor * d_cor_rlm(irlm_div_cor) &
     &                        * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
      end do
!$omp end parallel do
!
!  Degree 0
      j_rlm = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,  &
     &       izero)
      j11 = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,    &
     &       itwo)
      if( (j_rlm*j11) .gt. 0) then
!$omp  parallel do                                                      &
!$omp& private(k_rlm,irlm,i11,iwp_rlm_k1,iwd_rlm_k1,irlm_div_cor)
        do k_rlm = 1, nidx_rlm(1)
          irlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
          i11 =  j11 + (k_rlm-1)*nidx_rlm(2)
!
          iwp_rlm_k1 = b_trns%i_vort + (i11-1)*ncomp
          iwd_rlm_k1 = iwp_rlm_k1 + 1
          irlm_div_cor = ip_rlm_div_cor + (irlm-1)
!
          d_cor_rlm(irlm_div_cor)                                       &
     &       =  four*(two/three) * half * sp_rlm(iwp_rlm_k1)            &
     &        + four*(two/three) * omega_rlm(k_rlm,1)                   &
     &          * sp_rlm(iwd_rlm_k1)
!
          d_cor_rlm(irlm_div_cor) = -coef_cor * d_cor_rlm(irlm_div_cor) &
     &                        * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
!$omp end parallel do
      end if
!
      end subroutine sum_div_coriolis_rlm_10
!*
!*   ------------------------------------------------------------------
!*
      subroutine sum_r_coriolis_bc_rlm_10(ncomp, kr, sp_rlm, d_cor_bc)
!
      use m_addresses_trans_sph_MHD
      use m_coriolis_terms_rlm
!
      integer(kind = kint), intent(in) :: ncomp, kr
      real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
      real(kind = kreal), intent(inout) :: d_cor_bc(nidx_rlm(2))
!
      integer(kind = kint) :: j_rlm, j11, i11,i21,i12
      integer(kind = kint) :: ivp_rlm_k1, ivp_rlm_k2, ivp_rlm_l1
      integer(kind = kint) :: ivt_rlm_k1, ivt_rlm_k2, ivd_rlm_l1
!
!
      if(kr .le. 0) return
!
!$omp  parallel do                                                      &
!$omp& private(j_rlm,i11,i21,i12,ivp_rlm_k1,ivp_rlm_k2,                 &
!$omp&         ivp_rlm_l1,ivd_rlm_l1,ivt_rlm_k1,ivt_rlm_k2)
      do j_rlm = 1, nidx_rlm(2)
        i11 = jgi_cor_rlm(j_rlm,1) + (kr-1)*nidx_rlm(2)
        i21 = jgi_cor_rlm(j_rlm,2) + (kr-1)*nidx_rlm(2)
        i12 = jei_cor_rlm(j_rlm,1) + (kr-1)*nidx_rlm(2)
!
        ivp_rlm_k1 = b_trns%i_velo + (i11-1) * ncomp
        ivp_rlm_k2 = b_trns%i_velo + (i21-1) * ncomp
        ivp_rlm_l1 = b_trns%i_velo + (i12-1) * ncomp
!
        ivd_rlm_l1 = ivp_rlm_l1 + 1
!
        ivt_rlm_k1 = ivp_rlm_k1 + 2
        ivt_rlm_k2 = ivp_rlm_k2 + 2
!
        d_cor_bc(j_rlm)                                                 &
     &       =  sr_rlm(1,j_rlm) * omega_rlm(kr,1)*sp_rlm(ivd_rlm_l1)    &
     &        + tr_rlm(1,j_rlm) * omega_rlm(kr,1)*sp_rlm(ivt_rlm_k1)    &
     &        + tr_rlm(2,j_rlm) * omega_rlm(kr,1)*sp_rlm(ivt_rlm_k2)
!
        d_cor_bc(j_rlm) = -coef_cor*a_r_1d_rlm_r(kr)*a_r_1d_rlm_r(kr)   &
     &                   * d_cor_bc(j_rlm)
      end do
!$omp end parallel do
!
!  Degree 0
      j_rlm = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,  &
     &       izero)
      j11 = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,    &
     &       itwo)
      if( (j_rlm*j11) .gt. 0) then
        i11 =  j11 + (kr-1)*nidx_rlm(2)
        ivp_rlm_k1 = b_trns%i_velo + (i11-1) * ncomp
        ivt_rlm_k1 = ivp_rlm_k1 + 2
!
!        d_cor_bc(j_rlm) = -four*pi*(two/three)                         &
!     &                   * omega_rlm(kr,1) * sp_rlm(ivt_rlm_k1)
        d_cor_bc(j_rlm) = -(two/three)                                  &
     &                   * omega_rlm(kr,1) * sp_rlm(ivt_rlm_k1)
        d_cor_bc(j_rlm) = -coef_cor*a_r_1d_rlm_r(kr)*a_r_1d_rlm_r(kr)   &
     &                   * d_cor_bc(j_rlm)
      end if
!
      end subroutine sum_r_coriolis_bc_rlm_10
!*
!*   ------------------------------------------------------------------
!
      subroutine inner_core_rot_z_coriolis_rlm(ncomp, sp_rlm)
!
      use m_coriolis_terms_rlm
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: i11s, i10c, i11c
      integer(kind = kint) :: iwp_rlm_i11s, iwp_rlm_i11c
      integer(kind = kint) :: ipol_cor_i11s, ipol_cor_i11c
      integer(kind = kint) :: ipol_cor_i10c
!
!
      if(idx_rlm_ICB .eq. 0) return
      if((idx_rlm_degree_one(-1)*idx_rlm_degree_one(1)) .eq. 0) return
!
      i11s = idx_rlm_degree_one(-1) + (idx_rlm_ICB-1)*nidx_rlm(2)
      i10c = idx_rlm_degree_one( 0) + (idx_rlm_ICB-1)*nidx_rlm(2)
      i11c = idx_rlm_degree_one( 1) + (idx_rlm_ICB-1)*nidx_rlm(2)
!
      iwp_rlm_i11s = b_trns%i_vort + (i11s-1)*ncomp
!      iwp_rlm_i10c = b_trns%i_vort + (i10c-1)*ncomp
      iwp_rlm_i11c = b_trns%i_vort + (i11c-1)*ncomp
!
      ipol_cor_i11s = ip_rlm_rot_cor + ncomp_coriolis_rlm * (i11s-1)
      ipol_cor_i10c = ip_rlm_rot_cor + ncomp_coriolis_rlm * (i10c-1)
      ipol_cor_i11c = ip_rlm_rot_cor + ncomp_coriolis_rlm * (i11c-1)
!
      d_cor_rlm(ipol_cor_i10c) = zero
      d_cor_rlm(ipol_cor_i11s)                                          &
     &       = -two*coef_cor*radius_1d_rj_r(idx_rlm_ICB)                &
     &        * omega_rlm(idx_rlm_ICB,0)*sp_rlm(iwp_rlm_i11c)
      d_cor_rlm(ipol_cor_i11c)                                          &
     &       =  two*coef_cor*radius_1d_rj_r(idx_rlm_ICB)                &
     &        * omega_rlm(idx_rlm_ICB,0)*sp_rlm(iwp_rlm_i11s)
!
      end subroutine inner_core_rot_z_coriolis_rlm
!
! ----------------------------------------------------------------------
!*
      end module sum_coriolis_terms_rlm
