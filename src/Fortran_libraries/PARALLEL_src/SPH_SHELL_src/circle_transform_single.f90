!>@file   circle_transform_single.f90
!!@brief  module circle_transform_single
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine initialize_circle_transform                          &
!!     &          (circle, circ_spec, WK_circle_fft)
!!
!!      subroutine circle_transfer_vector                               &
!!     &         (ifld, circle, circ_spec, WK_circle_fft)
!!      subroutine circle_transfer_scalar                               &
!!     &         (ifld, circle, circ_spec, WK_circle_fft)
!!      subroutine circle_transfer_sym_tensor                           &
!!     &         (ifld, circle, circ_spec, WK_circle_fft)
!!        type(fields_on_circle), intent(inout) :: circle
!!        type(circle_transform_spetr), intent(inout) :: circ_spec
!!        type(working_FFTs), intent(inout) :: WK_circle_fft
!!@endverbatim
!!
!
      module circle_transform_single
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_FFT_selector
      use t_schmidt_polynomial
      use t_circle_transform
!
      implicit none
!
!
      type(legendre_polynomials), save :: leg_c
!
      private :: leg_c
      private :: alloc_circle_transform
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_circle_transform                            &
     &          (circle, circ_spec, WK_circle_fft)
!
      use calypso_mpi
!
      type(fields_on_circle), intent(in) :: circle
!
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer(kind = kint) :: l, m, mm, j
!
!
      circ_spec%r_circle                                                &
     &      = sqrt(circle%s_circle**2 + circle%z_circle**2)
      circ_spec%theta_circle                                            &
     &      = acos(circle%z_circle / circ_spec%r_circle)
!
      circ_spec%ar_circle = one / circ_spec%r_circle
      circ_spec%ar2_circle = circ_spec%ar_circle**2
!
!
      call alloc_schmidt_polynomial(circ_spec%ltr_circle, leg_c)
      call dschmidt(circ_spec%theta_circle, leg_c)
!
      do l = 1, circ_spec%ltr_circle
        do m = -l, l
          j = l*(l+1) + m
          mm = abs(m)
          circ_spec%P_circle(j) =    leg_c%p(mm,l)
          circ_spec%dPdt_circle(j) = leg_c%dp(mm,l)
        end do
      end do
!
      call dealloc_schmidt_polynomial(leg_c)
!
      if(my_rank .gt. 0) return
!
      write(*,*) 'np_smp', np_smp
      write(*,*) 'istack_circfft_smp', circ_spec%istack_circfft_smp
      write(*,*) 'mphi_circle', circle%mphi_circle
      call initialize_FFT_select                                        &
     &   (my_rank, np_smp, circ_spec%istack_circfft_smp,                &
     &    circle%mphi_circle, WK_circle_fft)
!
      end subroutine initialize_circle_transform
!
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_vector                                 &
     &         (ifld, circle, circ_spec, WK_circle_fft)
!
      use m_geometry_constants
      use cal_circle_transform
!
      integer(kind = kint), intent(in) :: ifld
      type(fields_on_circle), intent(inout) :: circle
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
!
      call circle_lag_transfer_vector                                   &
     &   (circ_spec%ltr_circle, circ_spec%jmax_circle,                  &
     &    circ_spec%P_circle, circ_spec%dPdt_circle,                    &
     &    circ_spec%ar_circle, circ_spec%ar2_circle,                    &
     &    circ_spec%jmax_circle, circle%d_rj_circle(0,ifld),            &
     &    circ_spec%vcirc_rtm)
!
      if(circle%iflag_circle_coord .eq. iflag_circle_cyl) then
        call overwrt_circle_sph_vect_2_cyl(circ_spec%theta_circle,      &
     &      circ_spec%ltr_circle, circ_spec%vcirc_rtm)
      end if
!
      call cal_circle_spectrum_vector                                   &
     &   (ithree, circ_spec%ltr_circle, circ_spec%vcirc_rtm,            &
     &    circle%mphi_circle, circle%vrtm_mag(0,ifld),                  &
     &    circle%vrtm_phase(0,ifld))
      call copy_circle_spectrum_4_fft                                   &
     &   (ithree, circ_spec%ltr_circle, circ_spec%vcirc_rtm,            &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1))
!
      call backward_FFT_select                                          &
     &   (np_smp, circ_spec%istack_circfft_smp, ione,                   &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1),                 &
     &    WK_circle_fft)
      call backward_FFT_select                                          &
     &   (np_smp, circ_spec%istack_circfft_smp, ione,                   &
     &    circle%mphi_circle, circle%v_rtp_circle(1,2),                 &
     &    WK_circle_fft)
      call backward_FFT_select                                          &
     &   (np_smp, circ_spec%istack_circfft_smp, ione,                   &
     &    circle%mphi_circle, circle%v_rtp_circle(1,3),                 &
     &    WK_circle_fft)
!
      end subroutine circle_transfer_vector
!
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_scalar                                 &
     &         (ifld, circle, circ_spec, WK_circle_fft)
!
      use m_FFT_selector
      use cal_circle_transform
!
      integer(kind = kint), intent(in) :: ifld
      type(fields_on_circle), intent(inout) :: circle
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
!
      call circle_lag_transfer_scalar                                   &
     &   (circ_spec%ltr_circle, circ_spec%jmax_circle,                  &
     &    circ_spec%P_circle, circ_spec%jmax_circle,                    &
     &    circle%d_rj_circle(0,ifld), circ_spec%vcirc_rtm)
!
      call cal_circle_spectrum_vector                                   &
     &   (ione, circ_spec%ltr_circle, circ_spec%vcirc_rtm,              &
     &    circle%mphi_circle, circle%vrtm_mag(0,ifld),                  &
     &    circle%vrtm_phase(0,ifld))
      call copy_circle_spectrum_4_fft                                   &
     &   (ione, circ_spec%ltr_circle, circ_spec%vcirc_rtm,              &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1))
!
      call backward_FFT_select                                          &
     &   (np_smp, circ_spec%istack_circfft_smp, ione,                   &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1),                 &
     &    WK_circle_fft)
!
      end subroutine circle_transfer_scalar
!
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_sym_tensor                             &
     &         (ifld, circle, circ_spec, WK_circle_fft)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: ifld
      type(fields_on_circle), intent(inout) :: circle
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer(kind = kint) :: nd
!
!
      do nd = 0, n_sym_tensor-1
        call circle_transfer_scalar                                     &
     &     (ifld+nd, circle, circ_spec, WK_circle_fft)
      end do
!
      end subroutine circle_transfer_sym_tensor
!
! ----------------------------------------------------------------------
!
     end module circle_transform_single
