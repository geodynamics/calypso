!>@file   cal_circle_transform.f90
!!@brief  module cal_circle_transform
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine circle_lag_transfer_vector(ltr_circle, jmax_circle,  &
!!     &          P_circle, dPdt_circle, ar_circle, ar2_circle,         &
!!     &          jmax, d_rj_circle, vcirc_rtm)
!!      subroutine circle_lag_transfer_scalar(ltr_circle, jmax_circle,  &
!!     &          P_circle, jmax, d_rj_circle, vcirc_rtm)
!!
!!      subroutine copy_circle_spectrum_4_fft(numdir, ltr_circle,       &
!!     &          vcirc_rtm, mphi_circle, v_rtp_circle)
!!      subroutine cal_circle_spectrum_vector(numdir, ltr_circle,       &
!!     &          vcirc_rtm, mphi_circle, vrtm_mag, vrtm_phase)
!!
!!      subroutine overwrt_circle_sph_vect_2_cyl                        &
!!     &         (theta_circle, ltr_circle, vcirc_rtm)
!!@endverbatim
!!
!!@n @param  ltr      Truncation of spherical harmonics
!!@n @param  jmax     Number of modes for harmonincs except for 0 degree
!!@n @param  d_rj_circle(0:jmax,3)   Spectr field data
!!@n @param  numdir   Number of components of field
!!@n @param v_rtp_circle(mphi_circle,numdir)  Field along circle
!!@n @param vrtm_mag(0:mphi_circle,numdir)  Amplitude of spectrum data
!!                                        along with the circle
!!@n @param vrtm_phase(0:mphi_circle,numdir)    Phase of spectrum data
!!                                        along with the circle
!
      module cal_circle_transform
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_FFT_selector
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine circle_lag_transfer_vector(ltr_circle, jmax_circle,    &
     &          P_circle, dPdt_circle, ar_circle, ar2_circle,           &
     &          jmax, d_rj_circle, vcirc_rtm)
!
      integer(kind = kint), intent(in) :: ltr_circle
      integer(kind = kint), intent(in) :: jmax_circle
      real(kind = kreal), intent(in) :: ar_circle, ar2_circle
      real(kind = kreal), intent(in) :: P_circle(0:jmax_circle)
      real(kind = kreal), intent(in) :: dPdt_circle(0:jmax_circle)
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: d_rj_circle(0:jmax,3)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: vcirc_rtm(-ltr_circle:ltr_circle,3)
!
      integer(kind = kint) :: l, m, j
!
!
      vcirc_rtm = 0.0d0
      do l = 1, ltr_circle
        do m = -l, l
          j = l*(l+1) + m
          vcirc_rtm(m,1) = vcirc_rtm(m,1) + d_rj_circle(j,1)            &
     &                  * P_circle(j) * dble(l)*dble(l+1)
          vcirc_rtm(m,2) = vcirc_rtm(m,2) + d_rj_circle(j,2)            &
     &                  * dPdt_circle(j)
          vcirc_rtm(m,3) = vcirc_rtm(m,3) - d_rj_circle(j,3)            &
     &                  * dPdt_circle(j)
        end do
!
        do m = -l, l
          j = l*(l+1) + m
          vcirc_rtm(-m,2) = vcirc_rtm(-m,2) + d_rj_circle(j,3)          &
     &                   * P_circle(j) * dble(-m)
!
          vcirc_rtm(-m,3) = vcirc_rtm(-m,3) + d_rj_circle(j,2)          &
     &                   * P_circle(j) * dble(-m)
        end do
      end do
!
      do m = -ltr_circle, ltr_circle
        vcirc_rtm(m,1) = vcirc_rtm(m,1) * ar2_circle
        vcirc_rtm(m,2) = vcirc_rtm(m,2) * ar_circle
        vcirc_rtm(m,3) = vcirc_rtm(m,3) * ar_circle
      end do
!
      end subroutine circle_lag_transfer_vector
!
! ----------------------------------------------------------------------
!
      subroutine circle_lag_transfer_scalar(ltr_circle, jmax_circle,    &
     &          P_circle, jmax, d_rj_circle, vcirc_rtm)
!
      integer(kind = kint), intent(in) :: ltr_circle
      integer(kind = kint), intent(in) :: jmax_circle
      real(kind = kreal), intent(in) :: P_circle(0:jmax_circle)
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: d_rj_circle(0:jmax)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: vcirc_rtm(-ltr_circle:ltr_circle,3)
!
      integer(kind = kint) :: l, m, j
!
!
      vcirc_rtm = 0.0d0
      vcirc_rtm(0,1) = d_rj_circle(0)
      do l = 1, ltr_circle
        do m = -l, l
          j = l*(l+1) + m
          vcirc_rtm(m,1) = vcirc_rtm(m,1) + d_rj_circle(j)              &
     &                    * P_circle(j)
        end do
      end do
!
      end subroutine circle_lag_transfer_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_circle_spectrum_4_fft(numdir, ltr_circle,         &
     &          vcirc_rtm, mphi_circle, v_rtp_circle)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: ltr_circle
      real(kind = kreal), intent(in)                                    &
     &                   :: vcirc_rtm(-ltr_circle:ltr_circle,3)
      integer(kind = kint), intent(in) :: mphi_circle
!
      real(kind = kreal), intent(inout)                                 &
     &                  :: v_rtp_circle(mphi_circle,numdir)
!
      integer(kind = kint) :: nd, m
!
!
      v_rtp_circle = 0.0d0
!
!$omp parallel do private(nd,m)
      do nd = 1, numdir
        do m = 1, ltr_circle-1
          v_rtp_circle(2*m+1,nd) = vcirc_rtm( m,nd)
          v_rtp_circle(2*m+2,nd) = vcirc_rtm(-m,nd)
        end do
      end do
!$omp end parallel do
!
      do nd = 1, numdir
        v_rtp_circle(1,nd) = vcirc_rtm(0,nd)
!
        if(ltr_circle .eq. (mphi_circle/2)) then
          v_rtp_circle(2,nd) = vcirc_rtm(ltr_circle,nd)
        else
          v_rtp_circle(2*ltr_circle+1,nd) = vcirc_rtm( ltr_circle,nd)
          v_rtp_circle(2*ltr_circle+2,nd) = vcirc_rtm(-ltr_circle,nd)
        end if
      end do
!
      end subroutine copy_circle_spectrum_4_fft
!
! ----------------------------------------------------------------------
!
      subroutine cal_circle_spectrum_vector(numdir, ltr_circle,         &
     &          vcirc_rtm, mphi_circle, vrtm_mag, vrtm_phase)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: ltr_circle
      real(kind = kreal), intent(in)                                    &
     &                   :: vcirc_rtm(-ltr_circle:ltr_circle,3)
      integer(kind = kint), intent(in) :: mphi_circle
!
      real(kind = kreal), intent(inout)                                 &
     &            :: vrtm_mag(0:mphi_circle,numdir)
      real(kind = kreal), intent(inout)                                 &
     &            :: vrtm_phase(0:mphi_circle,numdir)
!
      integer(kind = kint) :: nd, m
!
!$omp parallel do private(nd,m)
      do nd = 1, numdir
        do m = 1, ltr_circle-1
          vrtm_mag(m,nd) = sqrt(vcirc_rtm(-m,nd)**2                     &
     &                        + vcirc_rtm( m,nd)**2)
          vrtm_phase(m,nd) = atan2(vcirc_rtm(-m,nd),vcirc_rtm( m,nd))
        end do
      end do
!$omp end parallel do
!
      do nd = 1, numdir
        vrtm_mag(0,nd) = abs(vcirc_rtm(0,nd))
        vrtm_phase(0,nd) = zero
!
        if(ltr_circle .eq. (mphi_circle/2)) then
          vrtm_mag(ltr_circle,nd) = abs(vcirc_rtm(ltr_circle,nd))
          vrtm_phase(ltr_circle,nd) = zero
        else
          vrtm_mag(ltr_circle,nd) = sqrt(vcirc_rtm(-ltr_circle,nd)**2   &
     &                                 + vcirc_rtm( ltr_circle,nd)**2)
          vrtm_phase(ltr_circle,nd) = atan2(vcirc_rtm(-ltr_circle,nd),  &
     &                                      vcirc_rtm( ltr_circle,nd))
        end if
      end do
!
      end subroutine cal_circle_spectrum_vector
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine overwrt_circle_sph_vect_2_cyl                          &
     &         (theta_circle, ltr_circle, vcirc_rtm)
!
      integer(kind = kint) :: ltr_circle
      real(kind = kreal), intent(in) :: theta_circle
      real(kind = kreal), intent(inout)                                 &
     &                   :: vcirc_rtm(-ltr_circle:ltr_circle,3)
!
       integer (kind = kint) :: m
       real(kind=kreal) :: vr, vt, vp
!
!
!$omp parallel do private(vr,vt,vp)
      do m = -ltr_circle, ltr_circle
        vr = vcirc_rtm(m,1)
        vt = vcirc_rtm(m,2)
        vp = vcirc_rtm(m,3)
!
        vcirc_rtm(m,1) =  vr * sin( theta_circle )                      &
     &                  + vt * cos( theta_circle )
        vcirc_rtm(m,2) =  vp
        vcirc_rtm(m,3) =  vr * cos( theta_circle )                      &
     &                  - vt * sin( theta_circle )
       end do
!$omp end parallel do
!
      end subroutine overwrt_circle_sph_vect_2_cyl
!
! -----------------------------------------------------------------------
!
     end module cal_circle_transform
