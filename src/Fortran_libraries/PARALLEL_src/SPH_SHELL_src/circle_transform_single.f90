!>@file   circle_transform_single.f90
!!@brief  module circle_transform_single
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  perform spherical transform at a specific circle at (r, theta)
!!
!!@verbatim
!!      subroutine initialize_circle_transform(ltr, s_circ, z_circ)
!!
!!      subroutine circle_transfer_vector(d_rj_circle, v_rtp_circle,    &
!!     &          vrtm_mag, vrtm_phase)
!!      subroutine circle_transfer_scalar(d_rj_circle, v_rtp_circle,    &
!!     &          vrtm_mag, vrtm_phase)
!!      subroutine circle_transfer_sym_tensor(d_rj_circle, v_rtp_circle,&
!!     &          vrtm_mag, vrtm_phase)
!!@endverbatim
!!
!!@n @param  ltr      Truncation of spherical harmonics
!!@n @param  mphi_eq  Number of points along with circle
!!@n @param  d_rj_circle(0:jmax_circle,3)   Spectr field data
!!@n @param v_rtp_circle(mphi_eq,numdir)  Field along circle
!!@n @param vrtm_mag(0:mphi_eq,numdir)    Amplitude of spectrum data
!!                                        along with the circle
!!@n @param vrtm_phase(0:mphi_eq,numdir)    Phase of spectrum data
!!                                        along with the circle
!
      module circle_transform_single
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
      subroutine initialize_circle_transform(ltr, s_circ, z_circ)
!
      use calypso_mpi
      use m_schmidt_polynomial
      use m_circle_transform
!
      integer(kind = kint), intent(in) :: ltr
      real(kind = kreal), intent(in) :: s_circ, z_circ
!
      integer(kind = kint) :: l, m, mm, j
!
!
      call allocate_circle_transform(ltr)
!
      r_circle = sqrt(s_circ**2 + z_circ**2)
      theta_circle = acos(z_circ / r_circle)
!
      ar_circle = one / r_circle
      ar2_circle = ar_circle*ar_circle
!
!
      nth = ltr_circle
      call allocate_schmidt_polynomial
!
      dth = theta_circle
      call dschmidt
!
      do l = 1, ltr_circle
        do m = -l, l
          j = l*(l+1) + m
          mm = abs(m)
          P_circle(j) =     p(mm,l)
          dPdt_circle(j) = dp(mm,l)
        end do
      end do
!
      call deallocate_schmidt_polynomial
!
      if(my_rank .gt. 0) return
!
      write(*,*) 'np_smp', np_smp
      write(*,*) 'istack_circfft_smp', istack_circfft_smp
      write(*,*) 'mphi_circle', mphi_circle
      call initialize_FFT_sel_t(my_rank, np_smp, istack_circfft_smp,    &
     &    mphi_circle, WK_circle_fft)
!
      end subroutine initialize_circle_transform
!
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_vector(d_rj_circle, v_rtp_circle,      &
     &          vrtm_mag, vrtm_phase)
!
      use m_geometry_constants
      use m_circle_transform
      use FFT_selector
!
      real(kind = kreal), intent(in) :: d_rj_circle(0:jmax_circle,3)
!
      real(kind = kreal), intent(inout) :: v_rtp_circle(mphi_circle,3)
      real(kind = kreal), intent(inout) :: vrtm_mag(0:mphi_circle,3)
      real(kind = kreal), intent(inout) :: vrtm_phase(0:mphi_circle,3)
!
!
      call circle_lag_transfer_vector(jmax_circle, d_rj_circle)
!
      if(iflag_circle_coord .eq. iflag_circle_cyl) then
        call overwrt_circle_sph_vect_2_cyl
      end if
!
      call cal_circle_spectrum_vector(ithree, vrtm_mag, vrtm_phase)
      call copy_circle_spectrum_4_fft(ithree, v_rtp_circle)
!
      call backward_FFT_sel_t(np_smp, istack_circfft_smp, ione,         &
     &    mphi_circle, v_rtp_circle(1,1), WK_circle_fft)
      call backward_FFT_sel_t(np_smp, istack_circfft_smp, ione,         &
     &    mphi_circle, v_rtp_circle(1,2), WK_circle_fft)
      call backward_FFT_sel_t(np_smp, istack_circfft_smp, ione,         &
     &    mphi_circle, v_rtp_circle(1,3), WK_circle_fft)
!
      end subroutine circle_transfer_vector
!
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_scalar(d_rj_circle, v_rtp_circle,      &
     &          vrtm_mag, vrtm_phase)
!
      use m_circle_transform
      use FFT_selector
!
      real(kind = kreal), intent(in) :: d_rj_circle(0:jmax_circle)
      real(kind = kreal), intent(inout) :: v_rtp_circle(mphi_circle)
      real(kind = kreal), intent(inout) :: vrtm_mag(0:mphi_circle)
      real(kind = kreal), intent(inout) :: vrtm_phase(0:mphi_circle)
!
!
      call circle_lag_transfer_scalar(jmax_circle, d_rj_circle)
!
      call cal_circle_spectrum_vector(ione, vrtm_mag(0), vrtm_phase(0))
      call copy_circle_spectrum_4_fft(ione, v_rtp_circle(1))
!
      call backward_FFT_sel_t(np_smp, istack_circfft_smp, ione,         &
     &    mphi_circle, v_rtp_circle(1), WK_circle_fft)
!
      end subroutine circle_transfer_scalar
!
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_sym_tensor(d_rj_circle, v_rtp_circle,  &
     &          vrtm_mag, vrtm_phase)
!
      use m_phys_constants
      use m_circle_transform
!
      real(kind = kreal), intent(in) :: d_rj_circle(0:jmax_circle,6)
      real(kind = kreal), intent(inout) :: v_rtp_circle(mphi_circle,6)
      real(kind = kreal), intent(inout) :: vrtm_mag(0:mphi_circle,6)
      real(kind = kreal), intent(inout) :: vrtm_phase(0:mphi_circle,6)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, n_sym_tensor
        call circle_transfer_scalar(d_rj_circle(1,nd),                  &
     &      v_rtp_circle(1,nd), vrtm_mag(0,nd), vrtm_phase(0,nd) )
      end do
!
      end subroutine circle_transfer_sym_tensor
!
! ----------------------------------------------------------------------
!
     end module circle_transform_single
