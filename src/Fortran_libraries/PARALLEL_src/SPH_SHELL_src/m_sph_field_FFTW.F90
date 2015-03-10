!>@file   m_sph_field_FFTW.F90
!!@brief  module m_sph_field_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_field_FFTW
!!      subroutine finalize_sph_field_FFTW
!!      subroutine verify_sph_field_FFTW
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_field_fwd_FFTW_to_send                           &
!!     &         (ncomp, n_WS, irev_sr_rtp, X_rtp, WS)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine sph_field_back_FFTW_from_recv                        &
!!     &         (ncomp, n_WR, irev_sr_rtp, WR, X_rtp)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by FFTW3
!!
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!!       i = 1:     a_{0}
!!       i = 2:     a_{Nfft/2}
!!       i = 3:     a_{1}
!!       i = 4:     b_{1}
!!       ...
!!       i = 2*k+1: a_{k}
!!       i = 2*k+2: b_{k}
!!       ...
!!       i = Nfft-1:   a_{Nfft/2-1}
!!       i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param Nstacksmp(0:np_smp)   End number for each SMP process
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!
      module m_sph_field_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_sph_field_FFTW
!
      implicit none
!
!>      Structure to use FFTW
      type(work_for_sgl_FFTW), save :: sph_fld_FFTW
!
      private :: sph_fld_FFTW
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_field_FFTW
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
!
      call init_sph_field_FFTW_t(nidx_rtp, irt_rtp_smp_stack,           &
     &    sph_fld_FFTW)
!
      end subroutine init_sph_field_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_field_FFTW
!
!
      call finalize_sph_field_FFTW_t(sph_fld_FFTW)
!
      end subroutine finalize_sph_field_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_field_FFTW
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
!
      call verify_sph_field_FFTW_t(nnod_rtp, nidx_rtp,                  &
     &    irt_rtp_smp_stack, sph_fld_FFTW)
!
      end subroutine verify_sph_field_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_field_fwd_FFTW_to_send                             &
     &         (ncomp, n_WS, irev_sr_rtp, X_rtp, WS)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &         :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      call sph_fld_fwd_FFTW_to_send_t(ncomp, nnod_rtp, nidx_rtp,        &
     &    irt_rtp_smp_stack, n_WS, irev_sr_rtp, X_rtp, WS,              &
     &    sph_fld_FFTW)
!
      end subroutine sph_field_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_field_back_FFTW_from_recv                          &
     &         (ncomp, n_WR, irev_sr_rtp, WR, X_rtp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &          :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      call sph_fld_back_FFTW_from_recv_t(ncomp, nnod_rtp, nidx_rtp,     &
     &    irt_rtp_smp_stack, n_WR, irev_sr_rtp, WR, X_rtp,              &
     &    sph_fld_FFTW)
!
      end subroutine sph_field_back_FFTW_from_recv
!
! ------------------------------------------------------------------
!
      end module m_sph_field_FFTW
