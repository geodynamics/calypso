!>@file   sum_div_coriolis_rj_sph.f90
!!@brief  module sum_div_coriolis_rj_sph
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate derivatives with no boundary conditions
!!
!!@verbatim
!!      subroutine s_sum_div_coriolis_rj_sph(kr_in, kr_out, coef_cor)
!!
!!      subroutine sum_div_coriolis_rj_10(kr_in, kr_out,               &
!!     &          coef_cor, angular)
!!      subroutine sum_div_coriolis_rj_xy(kr_in, kr_out,               &
!!     &          coef_cor, anglar)
!!
!!
!!*************************************************
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
!!@param kr_in     Radial ID for inner boundary
!!@param kr_out    Radial ID for outer boundary
!!@param coef_cor  Coefficient for Coriolis term
!!@param is_div_f  Poloidal address of divergence of Coriolis force
!
      module sum_div_coriolis_rj_sph
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
      private :: sum_div_coriolis_rj_10, sum_div_coriolis_rj_xy
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine s_sum_div_coriolis_rj_sph(kr_in, kr_out,               &
     &          coef_cor, is_div_f)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_div_f
      real(kind = kreal), intent(in) :: coef_cor
!
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &        'sum_div_coriolis_rj_10', omega_rj(1,2,1:3)
      call sum_div_coriolis_rj_10(kr_in, kr_out, coef_cor, is_div_f)
!
      if( omega_rj(1,2,1).ne.zero .or. omega_rj(1,2,3).ne.zero) then
        if (iflag_debug.eq.1) write(*,*) 'sum_div_coriolis_rj_xy'
        call sum_div_coriolis_rj_xy(kr_in, kr_out, coef_cor, is_div_f)
      end if
!
      end subroutine s_sum_div_coriolis_rj_sph
!
!*   ------------------------------------------------------------------
!*
      subroutine sum_div_coriolis_rj_10(kr_in, kr_out,                  &
     &          coef_cor, is_div_f)
!
      use m_schmidt_poly_on_rtm
      use m_coriolis_coefs_tri_sph
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_div_f
      real(kind = kreal), intent(in) :: coef_cor
!
      integer(kind = kint) :: k, j, j30
      integer(kind = kint) :: inod,i11,i21,i12
!
!
!cdir select(concur)
!$omp parallel do private(k,j,j30,inod,i11,i21,i12)
      do k = kr_in, kr_out
        do j = idx_rj_degree_zero + 1, nidx_rj(2)
          j30 = idx_gl_1d_rj_j(j,1)
!
          inod = j + (k-1)*nidx_rj(2)
          i11 = jlc_kcor(j30,1,2) + (k-1)*nidx_j_cor
          i21 = jlc_kcor(j30,2,2) + (k-1)*nidx_j_cor
          i12 = jlc_lcor(j30,1,2) + (k-1)*nidx_j_cor
!
          d_rj(inod,is_div_f)                                           &
     &       =  td(1,j30) * omega_rj(k,1,2)                             &
     &         * (g_sph_rj(j,3)*ar_1d_rj(k,2) * d_sph_cor(i12,ic_vp)    &
     &           - d_sph_cor(i12,ic_d2vp) )                             &
     &        + sd(1,1,j30) * half*omega_rj(k,2,2)*d_sph_cor(i11,ic_vt) &
     &        + sd(2,1,j30) * half*omega_rj(k,2,2)*d_sph_cor(i21,ic_vt) &
     &        + sd(1,2,j30) * omega_rj(k,1,2) * d_sph_cor(i11,ic_dvt)   &
     &        + sd(2,2,j30) * omega_rj(k,1,2) * d_sph_cor(i21,ic_dvt)
        end do
!
        if(idx_rj_degree_zero .gt. 0) then
          inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
          i11 = idx_rj_degree_one(0) + (k-1)*nidx_rj(2)
!
!          d_rj(inod,is_div_f)                                          &
!     &       =  four*(two/three) * half * d_rj(i11,ipol%i_vort)        &
!     &        + four*(two/three) * omega_rj(k,1,2)                     &
!     &          * d_rj(i11,idpdr%i_vort)
        end if
!
        do j = 1, nidx_rj(2)
          inod = j + (k-1)*nidx_rj(2)
          d_rj(inod,is_div_f) = -coef_cor * ar_1d_rj(k,2)               &
     &                                * d_rj(inod,is_div_f)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_div_coriolis_rj_10
!*
!*   ------------------------------------------------------------------
!*
      subroutine sum_div_coriolis_rj_xy(kr_in, kr_out,                  &
     &          coef_cor, is_div_f)
!
      use m_schmidt_poly_on_rtm
      use m_coriolis_coefs_tri_sph
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_div_f
      real(kind = kreal), intent(in) :: coef_cor
!
      integer(kind = kint) :: k, j, j30, inod
      integer(kind = kint) :: i11,i21,i31,i41,i12,i22
      integer(kind = kint) :: l11,l21,l31,l41,l12,l22
      real(kind = kreal) :: ct1, ct3
!
!
!cdir select(concur)
!$omp parallel do private(k,j,j30,inod,i11,i21,i31,i41,   &
!$omp&           i12,i22,l11,l21,l31,l41,l12,l22,ct1,ct3)
      do k = kr_in, kr_out
        do j = idx_rj_degree_zero + 1, nidx_rj(2)
          j30 = idx_gl_1d_rj_j(j,1)
!
          inod = j + (k-1)*nidx_rj(2)
          i11 = jlc_kcor(j30,1,1) + (k-1)*nidx_j_cor
          i21 = jlc_kcor(j30,2,1) + (k-1)*nidx_j_cor
          i31 = jlc_kcor(j30,3,1) + (k-1)*nidx_j_cor
          i41 = jlc_kcor(j30,4,1) + (k-1)*nidx_j_cor
          i12 = jlc_lcor(j30,1,1) + (k-1)*nidx_j_cor
          i22 = jlc_lcor(j30,2,1) + (k-1)*nidx_j_cor
!
          l11 = jlc_kcor(j30,1,3) + (k-1)*nidx_j_cor
          l21 = jlc_kcor(j30,2,3) + (k-1)*nidx_j_cor
          l31 = jlc_kcor(j30,3,3) + (k-1)*nidx_j_cor
          l41 = jlc_kcor(j30,4,3) + (k-1)*nidx_j_cor
          l12 = jlc_lcor(j30,1,3) + (k-1)*nidx_j_cor
          l22 = jlc_lcor(j30,2,3) + (k-1)*nidx_j_cor
!
          ct1 = td1(1,j30) * omega_rj(k,1,1)                            &
     &         * (g_sph_rj(j,3)*ar_1d_rj(k,2) * d_sph_cor(i12,ic_vp)    &
     &           - d_sph_cor(i12,ic_d2vp) )                             &
     &       + td1(2,j30) * omega_rj(k,1,1)                             &
     &         * (g_sph_rj(j,3)*ar_1d_rj(k,2) * d_sph_cor(i22,ic_vp)    &
     &           - d_sph_cor(i22,ic_d2vp) )                             &
     &       + sd1(1,1,j30) * half*omega_rj(k,2,1)*d_sph_cor(i11,ic_vt) &
     &       + sd1(2,1,j30) * half*omega_rj(k,2,1)*d_sph_cor(i21,ic_vt) &
     &       + sd1(3,1,j30) * half*omega_rj(k,2,1)*d_sph_cor(i31,ic_vt) &
     &       + sd1(4,1,j30) * half*omega_rj(k,2,1)*d_sph_cor(i41,ic_vt) &
     &       + sd1(1,2,j30) * omega_rj(k,1,1) * d_sph_cor(i11,ic_dvt)   &
     &       + sd1(2,2,j30) * omega_rj(k,1,1) * d_sph_cor(i21,ic_dvt)   &
     &       + sd1(3,2,j30) * omega_rj(k,1,1) * d_sph_cor(i31,ic_dvt)   &
     &       + sd1(4,2,j30) * omega_rj(k,1,1) * d_sph_cor(i41,ic_dvt)
!
!
          ct3 = td3(1,j30) * omega_rj(k,1,3)                            &
     &         * (g_sph_rj(j,3)*ar_1d_rj(k,2) * d_sph_cor(l12,ic_vp)    &
     &           - d_sph_cor(l12,ic_d2vp) )                             &
     &       + td3(2,j30) * omega_rj(k,1,3)                             &
     &         * (g_sph_rj(j,3)*ar_1d_rj(k,2) * d_sph_cor(l22,ic_vp)    &
     &           - d_sph_cor(l22,ic_d2vp) )                             &
     &       + sd3(1,1,j30) * half*omega_rj(k,2,3)*d_sph_cor(l11,ic_vt) &
     &       + sd3(2,1,j30) * half*omega_rj(k,2,3)*d_sph_cor(l21,ic_vt) &
     &       + sd3(3,1,j30) * half*omega_rj(k,2,3)*d_sph_cor(l31,ic_vt) &
     &       + sd3(4,1,j30) * half*omega_rj(k,2,3)*d_sph_cor(l41,ic_vt) &
     &       + sd3(1,2,j30) * omega_rj(k,1,3) * d_sph_cor(l11,ic_dvt)   &
     &       + sd3(2,2,j30) * omega_rj(k,1,3) * d_sph_cor(l21,ic_dvt)   &
     &       + sd3(3,2,j30) * omega_rj(k,1,3) * d_sph_cor(l31,ic_dvt)   &
     &       + sd3(4,2,j30) * omega_rj(k,1,3) * d_sph_cor(l41,ic_dvt)
!
          d_rj(inod,is_div_f) = d_rj(inod,is_div_f)                     &
     &             - coef_cor * ar_1d_rj(k,2) * (ct1 + ct3)
        end do
!
        if(idx_rj_degree_zero .gt. 0) then
          inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
          i11 = idx_rj_degree_one(-1) + (k-1)*nidx_rj(2)
          l11 = idx_rj_degree_one( 1) + (k-1)*nidx_rj(2)
!
!          ct1 = four*pi*(two/three) * half * d_rj(i11,ipol%i_vort)     &
!     &        + four*pi*(two/three) * omega_rj(k,1,2)                  &
!     &          * d_rj(i11,idpdr%i_vort)
!
!          ct3 = four*pi*(two/three) * half * d_rj(l11,ipol%i_vort)     &
!     &        + four*pi*(two/three) * omega_rj(k,1,2)                  &
!     &          * d_rj(l11,idpdr%i_vort)
!
          ct1 = four*(two/three) * half * d_rj(i11,ipol%i_vort)         &
     &        + four*(two/three) * omega_rj(k,1,2)                      &
     &          * d_rj(i11,idpdr%i_vort)
!
          ct3 = four*(two/three) * half * d_rj(l11,ipol%i_vort)         &
     &        + four*(two/three) * omega_rj(k,1,2)                      &
     &          * d_rj(l11,idpdr%i_vort)
!
!
          d_rj(inod,is_div_f) = d_rj(inod,is_div_f)                     &
     &         - coef_cor * ar_1d_rj(k,2) * (ct1 + ct3)
        end if
      end do
!$omp end parallel do
!
      end subroutine sum_div_coriolis_rj_xy
!*
!*   ------------------------------------------------------------------
!
      end module sum_div_coriolis_rj_sph
