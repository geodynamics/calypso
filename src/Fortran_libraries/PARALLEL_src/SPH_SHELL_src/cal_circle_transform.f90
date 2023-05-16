!>@file   cal_circle_transform.f90
!!@brief  module cal_circle_transform
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine copy_circle_spectrum_4_fft(numdir, ltr_circle,       &
!!     &          vcirc_rtm, mphi_circle, v_rtp_circle)
!!        integer(kind = kint), intent(in) :: numdir
!!        integer(kind = kint), intent(in) :: ltr_circle
!!        real(kind = kreal), intent(in)                                &
!!       &                   :: vcirc_rtm(-ltr_circle:ltr_circle,numdir)
!!        integer(kind = kint), intent(in) :: mphi_circle
!!        real(kind = kreal), intent(inout)                             &
!!       &                  :: v_rtp_circle(mphi_circle,numdir)
!!      subroutine cal_circle_spectrum_vector(numdir, ltr_circle,       &
!!     &          vcirc_rtm, vrtm_mag, vrtm_phase)
!!        integer(kind = kint), intent(in) :: numdir
!!        integer(kind = kint), intent(in) :: ltr_circle
!!        real(kind = kreal), intent(in)                                &
!!       &                   :: vcirc_rtm(-ltr_circle:ltr_circle,numdir)
!!        real(kind = kreal), intent(inout)                             &
!!       &            :: vrtm_mag(0:ltr_circle,numdir)
!!        real(kind = kreal), intent(inout)                             &
!!       &            :: vrtm_phase(0:ltr_circle,numdir)
!!
!!      subroutine overwrt_circle_sph_vect_2_cyl                        &
!!     &         (theta_circle, ltr_circle, vcirc_rtm)
!!@endverbatim
!!
!!@n @param  ltr      Truncation of spherical harmonics
!!@n @param  jmax     Number of modes for harmonincs except for 0 degree
!!@n @param  numdir   Number of components of field
!!@n @param v_rtp_circle(mphi_circle,numdir)  Field along circle
!!@n @param vrtm_mag(0:ltr_circle,numdir)  Amplitude of spectrum data
!!                                        along with the circle
!!@n @param vrtm_phase(0:ltr_circle,numdir)    Phase of spectrum data
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
      subroutine copy_circle_spectrum_4_fft(numdir, ltr_circle,         &
     &          vcirc_rtm, mphi_circle, v_rtp_circle)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: ltr_circle
      real(kind = kreal), intent(in)                                    &
     &                   :: vcirc_rtm(-ltr_circle:ltr_circle,numdir)
      integer(kind = kint), intent(in) :: mphi_circle
!
      real(kind = kreal), intent(inout)                                 &
     &                  :: v_rtp_circle(mphi_circle,numdir)
!
      integer(kind = kint) :: nd, m, ncou
!
!
!$omp parallel workshare
      v_rtp_circle(1:mphi_circle,1:numdir) = 0.0d0
!$omp end parallel workshare
!
      ncou = min(ltr_circle,mphi_circle/2)
!$omp parallel do private(nd,m)
      do nd = 1, numdir
        v_rtp_circle(1,nd) = vcirc_rtm(0,nd)
        do m = 1, ncou-1
          v_rtp_circle(2*m+1,nd) = vcirc_rtm( m,nd)
          v_rtp_circle(2*m+2,nd) = vcirc_rtm(-m,nd)
        end do
!
        if(ltr_circle .eq. (mphi_circle/2)) then
          v_rtp_circle(2,nd) = vcirc_rtm(ltr_circle,nd)
        else if(ltr_circle .le. (mphi_circle/2)) then
          v_rtp_circle(2*ltr_circle+1,nd) = vcirc_rtm( ltr_circle,nd)
          v_rtp_circle(2*ltr_circle+2,nd) = vcirc_rtm(-ltr_circle,nd)
        end if
      end do
!$omp end parallel do
!
      end subroutine copy_circle_spectrum_4_fft
!
! ----------------------------------------------------------------------
!
      subroutine cal_circle_spectrum_vector(numdir, ltr_circle,         &
     &          vcirc_rtm, vrtm_mag, vrtm_phase)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: ltr_circle
      real(kind = kreal), intent(in)                                    &
     &                   :: vcirc_rtm(-ltr_circle:ltr_circle,numdir)
!
      real(kind = kreal), intent(inout)                                 &
     &            :: vrtm_mag(0:ltr_circle,numdir)
      real(kind = kreal), intent(inout)                                 &
     &            :: vrtm_phase(0:ltr_circle,numdir)
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
        vrtm_mag(ltr_circle,nd) = sqrt(vcirc_rtm(-ltr_circle,nd)**2     &
     &                               + vcirc_rtm( ltr_circle,nd)**2)
        vrtm_phase(ltr_circle,nd) = atan2(vcirc_rtm(-ltr_circle,nd),    &
     &                                    vcirc_rtm( ltr_circle,nd))
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
