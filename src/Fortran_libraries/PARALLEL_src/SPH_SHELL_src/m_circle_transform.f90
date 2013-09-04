!>@file   m_circle_transform.f90
!!@brief  module m_circle_transform
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine allocate_circle_transform(ltr)
!!      subroutine deallocate_circle_transform
!!
!!      subroutine circle_lag_transfer_vector(jmax, d_rj_circle)
!!      subroutine circle_lag_transfer_scalar(jmax, d_rj_circle)
!!
!1      subroutine copy_circle_spectrum_4_fft(numdir, v_rtp_circle)
!!      subroutine cal_circle_spectrum_vector(numdir,                   &
!!     &          vrtm_mag, vrtm_phase)
!!
!!      subroutine overwrt_circle_sph_vect_2_cyl
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
      module m_circle_transform
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_FFT_selector
!
      implicit none
!
!
!>      Use spherical coordinate for circle data
      integer(kind = kint), parameter :: iflag_circle_sph = 1
!>      Use Cylindrical coordinate for circle data
      integer(kind = kint), parameter :: iflag_circle_cyl = 2
!>      Flag for coordinate system for circle data
      integer(kind = kint) :: iflag_circle_coord = iflag_circle_sph
!
!
!>      Number of gird points for a circle
      integer(kind = kint) :: mphi_circle
!>      Truncation level for spherical transform at equator
      integer(kind = kint) :: ltr_circle
!>      Number of modes for spherical transform at equator
      integer(kind = kint) :: jmax_circle
!>      end address of SMP parallelization for scalar Fourier transform
      integer(kind = kint), allocatable :: istack_circfft_smp(:)
!
!>      Radius for specific circle
      real(kind = kreal) :: r_circle
!>      @f$ 1/ r @f$ for specific circle
      real(kind = kreal) :: ar_circle
!>      @f$ 1/ r^{2} @f$ for specific circle
      real(kind = kreal) :: ar2_circle
!
!>      colatitude for specific circle
      real(kind = kreal) :: theta_circle
!
!>      associated Lagender polynomial at circle
      real(kind = kreal), allocatable :: P_circle(:)
!>       @f$ dP_{l}^{m}/ d\theta @f$ at circle
      real(kind = kreal), allocatable :: dPdt_circle(:)
!
!>      spectr data for Fourier transform at a circle
      real(kind = kreal), allocatable :: vcirc_rtm(:,:)
!
!>      Working structure for Fourier transform at mid-depth equator
!!@n      (Save attribute is necessary for Hitachi compiler for SR16000)
      type(working_FFTs), save :: WK_circle_fft
!
      private :: vcirc_rtm
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_circle_transform(ltr)
!
      integer(kind = kint), intent(in) :: ltr
!
!
      ltr_circle =  ltr
      jmax_circle = ltr_circle*(ltr_circle+2)
!
      allocate(P_circle(0:jmax_circle))
      allocate(dPdt_circle(0:jmax_circle))
      P_circle =    zero
      dPdt_circle = zero
!
      allocate( vcirc_rtm(-ltr_circle:ltr_circle,3) )
      vcirc_rtm = zero
!
      allocate(istack_circfft_smp(0:np_smp))
      istack_circfft_smp(1:np_smp) = 1
      istack_circfft_smp(0) =        0
!
      end subroutine allocate_circle_transform
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_circle_transform
!
!
      deallocate(P_circle, dPdt_circle)
      deallocate(vcirc_rtm)
      deallocate(istack_circfft_smp)
!
      end subroutine deallocate_circle_transform
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine circle_lag_transfer_vector(jmax, d_rj_circle)
!
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: d_rj_circle(0:jmax,3)
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
      subroutine circle_lag_transfer_scalar(jmax, d_rj_circle)
!
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: d_rj_circle(0:jmax)
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
      subroutine copy_circle_spectrum_4_fft(numdir, v_rtp_circle)
!
      integer(kind = kint), intent(in) :: numdir
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
      subroutine cal_circle_spectrum_vector(numdir,                     &
     &          vrtm_mag, vrtm_phase)
!
      integer(kind = kint), intent(in) :: numdir
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
      subroutine overwrt_circle_sph_vect_2_cyl
!
       integer (kind = kint) :: m
       real(kind=kreal) :: vr, vt, vp
!
!
!$omp parallel do private(vr,vt,vp)
      do m = 1, mphi_circle
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
     end module m_circle_transform
