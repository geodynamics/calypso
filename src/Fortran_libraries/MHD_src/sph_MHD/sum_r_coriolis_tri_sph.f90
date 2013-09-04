!>@file   sum_r_coriolis_tri_sph.f90
!!@brief  module sum_r_coriolis_tri_sph
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Calculate rotation of Coriolis term using Gaunt integrals
!!
!!@verbatim
!!      subroutine s_sum_r_coriolis_tri_sph(kr, coef_cor, is_rot_f)
!!
!!      subroutine sum_r_coriolis_rj_10_on_sph(kr, coef_cor, is_rot_f)
!!      subroutine sum_r_coriolis_rj_xy_on_sph(kr, coef_cor, is_rot_f)
!!
!!**************************************************
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
!!  Divergence of the Coriolis term
!!     (wsd) = wsd(jc,1,j3)*w*wsb/r**4
!!            + wsd(jc,2,j3)*dw*dwsb/r**2
!!     (wtd) = wtd(j3)*dw*dwtb/r**2
!!
!!  Radial componenet of the Coriolis term
!!     (wsr) = wsr(jc,1,j3)*dw*dusb/r**2
!!     (wtr) = wtr(j3)*dw*wtb/r**2
!!
!!*************************************************
!!
!!*************************************************
!!
!!     wss(jc,1,j3) = sw(jc,1,j3)
!!     wss(jc,2,j3) = sw(jc,2,j3)
!!     wts(jc,j3)   = sw(jc,3,j3)
!!     wst(jc,1,j3) = tw(jc,1,j3)
!!     wst(jc,2,j3) = tw(jc,2,j3)
!!     wtt(jc,1,j3) = tw(jc,3,j3)
!!     wtt(jc,2,j3) = tw(jc,4,j3)
!!
!!     wsd(jc,1,j3) = sd(jc,1,j3)
!!     wsd(jc,2,j3) = sd(jc,2,j3)
!!     wtd(jc,j3)   = td(jc,j3)
!!
!!     wsr(jc,j3) =   sr(jc,j3)
!!     wtr(jc,j3) =   tr(jc,j3)
!!
!!*************************************************
!!@endverbatim
!!
!!@n @param kr        radial ID
!!@n @param coef_cor  Coefficient for Coriolis term
!!@n @param is_rot_f     Poloidal address of curl of Coriolis force
!!
      module sum_r_coriolis_tri_sph
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_integrals_4_sph_coriolis
!
      implicit none
!
      private :: sum_r_coriolis_rj_10_on_sph
      private :: sum_r_coriolis_rj_xy_on_sph
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine s_sum_r_coriolis_tri_sph(kr, coef_cor, is_rot_f)
!
      integer(kind = kint), intent(in) :: kr, is_rot_f
      real(kind = kreal), intent(in) :: coef_cor
!
!
      if (iflag_debug.eq.1) write(*,*)                                &
     &        'sum_r_coriolis_rj_10_on_sph', omega_rj(1,2,1:3)
      call sum_r_coriolis_rj_10_on_sph(kr, coef_cor, is_rot_f)
!
      if( omega_rj(1,2,1).ne.zero .or. omega_rj(1,2,3).ne.zero) then
        if (iflag_debug.eq.1) write(*,*) 'sum_r_coriolis_rj_xy_on_sph'
        call sum_r_coriolis_rj_xy_on_sph(kr, coef_cor, is_rot_f)
      end if
!
      end subroutine s_sum_r_coriolis_tri_sph
!
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!*
      subroutine sum_r_coriolis_rj_10_on_sph(kr, coef_cor, is_rot_f)
!
      use m_schmidt_poly_on_rtm
      use m_coriolis_coefs_tri_sph
!
      integer(kind = kint), intent(in) :: kr, is_rot_f
      real(kind = kreal), intent(in) :: coef_cor
!
      integer(kind = kint) :: j, j30
      integer(kind = kint) :: inod,i11,i21,i12
!
!
      do j = idx_rj_degree_zero + 1, nidx_rj(2)
        j30 = idx_gl_1d_rj_j(j,1)
!
        inod = j + (kr-1)*nidx_rj(2)
        i11 = jlc_kcor(j30,1,2) + (kr-1)*nidx_j_cor
        i21 = jlc_kcor(j30,2,2) + (kr-1)*nidx_j_cor
        i12 = jlc_lcor(j30,1,2) + (kr-1)*nidx_j_cor
!
        d_rj(inod,is_rot_f)                                             &
     &       =  sr(1,j30) * omega_rj(kr,1,2) * d_sph_cor(i12,ic_dvp)    &
     &        + tr(1,j30) * omega_rj(kr,1,2) * d_sph_cor(i11,ic_vt)     &
     &        + tr(2,j30) * omega_rj(kr,1,2) * d_sph_cor(i21,ic_vt)
      end do
!
      do j = 1, idx_rj_degree_zero
        inod = j + (kr-1)*nidx_rj(2)
        i11 = idx_rj_degree_one(0) + (kr-1)*nidx_j_cor
!
!        d_rj(inod,is_rot_f) = -four*pi*(two/three) * omega_rj(kr,1,2)  &
!     &                               * d_rj(i11,itor%i_velo)
        d_rj(inod,is_rot_f) = -(two/three) * omega_rj(kr,1,2)           &
     &                               * d_rj(i11,itor%i_velo)
      end do
!
      do j = 1, nidx_rj(2)
        inod = j + (kr-1)*nidx_rj(2)
        d_rj(inod,is_rot_f) = -coef_cor*ar_1d_rj(kr,2)                  &
     &                       * d_rj(inod,is_rot_f)
      end do
!
      end subroutine sum_r_coriolis_rj_10_on_sph
!
!*   ------------------------------------------------------------------
!
      subroutine sum_r_coriolis_rj_xy_on_sph(kr, coef_cor, is_rot_f)
!
      use m_schmidt_poly_on_rtm
      use m_coriolis_coefs_tri_sph
!
      integer(kind = kint), intent(in) :: kr, is_rot_f
      real(kind = kreal), intent(in) :: coef_cor
!
      integer(kind = kint) :: j, j30, inod
      integer(kind = kint) :: i11,i21,i31,i41,i12,i22
      integer(kind = kint) :: l11,l21,l31,l41,l12,l22
      real(kind = kreal) :: ct1, ct3
!
!
      do j = idx_rj_degree_zero + 1, nidx_rj(2)
        j30 = idx_gl_1d_rj_j(j,1)
!
        inod = j + (kr-1)*nidx_rj(2)
        i11 = jlc_kcor(j30,1,1) + (kr-1)*nidx_j_cor
        i21 = jlc_kcor(j30,2,1) + (kr-1)*nidx_j_cor
        i31 = jlc_kcor(j30,3,1) + (kr-1)*nidx_j_cor
        i41 = jlc_kcor(j30,4,1) + (kr-1)*nidx_j_cor
        i12 = jlc_lcor(j30,1,1) + (kr-1)*nidx_j_cor
        i22 = jlc_lcor(j30,2,1) + (kr-1)*nidx_j_cor
!
        l11 = jlc_kcor(j30,1,3) + (kr-1)*nidx_j_cor
        l21 = jlc_kcor(j30,2,3) + (kr-1)*nidx_j_cor
        l31 = jlc_kcor(j30,3,3) + (kr-1)*nidx_j_cor
        l41 = jlc_kcor(j30,4,3) + (kr-1)*nidx_j_cor
        l12 = jlc_lcor(j30,1,3) + (kr-1)*nidx_j_cor
        l22 = jlc_lcor(j30,2,3) + (kr-1)*nidx_j_cor
!
        ct1 =  sr1(1,j30) * omega_rj(kr,1,1) * d_sph_cor(i12,ic_dvp)    &
     &       + sr1(2,j30) * omega_rj(kr,1,1) * d_sph_cor(i22,ic_dvp)    &
     &       + sr1(1,j30) * omega_rj(kr,1,1) * d_sph_cor(i11,ic_vt)     &
     &       + sr1(2,j30) * omega_rj(kr,1,1) * d_sph_cor(i21,ic_vt)     &
     &       + sr1(3,j30) * omega_rj(kr,1,1) * d_sph_cor(i31,ic_vt)     &
     &       + sr1(4,j30) * omega_rj(kr,1,1) * d_sph_cor(i41,ic_vt)
!
!
        ct3 =  sr3(1,j30) * omega_rj(kr,1,3) * d_sph_cor(l12,ic_dvp)    &
     &       + sr3(2,j30) * omega_rj(kr,1,3) * d_sph_cor(l22,ic_dvp)    &
     &       + sr3(1,j30) * omega_rj(kr,1,3) * d_sph_cor(l11,ic_vt)     &
     &       + sr3(2,j30) * omega_rj(kr,1,3) * d_sph_cor(l21,ic_vt)     &
     &       + sr3(3,j30) * omega_rj(kr,1,3) * d_sph_cor(l31,ic_vt)     &
     &       + sr3(4,j30) * omega_rj(kr,1,3) * d_sph_cor(l41,ic_vt)
!
        d_rj(inod,is_rot_f) = d_rj(inod,is_rot_f)                       &
     &                    - coef_cor*ar_1d_rj(kr,2) * (ct1 + ct3)
      end do
!
      do j = 1, idx_rj_degree_zero
        inod = j + (kr-1)*nidx_rj(2)
        i11 = idx_rj_degree_one(-1) + (kr-1)*nidx_j_cor
        l11 = idx_rj_degree_one( 1) + (kr-1)*nidx_j_cor
!
!        ct1 = -four*pi*(two/three)                                     &
!     &       * omega_rj(kr,1,2)*d_rj(i11,itor%i_velo)
!        ct3 = -four*pi*(two/three)                                     &
!     &       * omega_rj(kr,1,2)*d_rj(l11,itor%i_velo)
!
        ct1 = -four*(two/three)                                         &
     &       * omega_rj(kr,1,2)*d_rj(i11,itor%i_velo)
        ct3 = -four*(two/three)                                         &
     &       * omega_rj(kr,1,2)*d_rj(l11,itor%i_velo)
!
        d_rj(inod,is_rot_f) = d_rj(inod,is_rot_f)                       &
     &                     - coef_cor*ar_1d_rj(kr,2) * (ct1 + ct3)
      end do
!
      end subroutine sum_r_coriolis_rj_xy_on_sph
!*
!*   ------------------------------------------------------------------
!
      end module sum_r_coriolis_tri_sph
