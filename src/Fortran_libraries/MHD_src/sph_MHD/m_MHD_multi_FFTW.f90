!>@file   m_MHD_multi_FFTW.F90
!!@brief  module m_MHD_multi_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_MHD_multi_FFTW(ncomp, ncomp_fwd, ncomp_bwd)
!!      subroutine finalize_MHD_multi_FFTW
!!      subroutine verify_MHD_multi_FFTW(ncomp, ncomp_fwd, ncomp_bwd)
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine MHD_multi_fwd_FFTW_to_send                           &
!!     &         (ncomp_fwd, n_WS, irev_sr_rtp, X_rtp, WS)
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
!!      subroutine MHD_multi_back_FFTW_from_recv                        &
!!     &         (ncomp_bwd, n_WR, irev_sr_rtp, WR, X_rtp)
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
      module m_MHD_multi_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_sph_multi_FFTW
!
      implicit none
!
!>      Structure to use FFTW
      type(work_for_sgl_FFTW), save :: MHD_mul_FFTW
!
      private :: MHD_mul_FFTW
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_MHD_multi_FFTW(ncomp, ncomp_fwd, ncomp_bwd)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp, ncomp_fwd, ncomp_bwd
!
!
      call init_sph_multi_FFTW_t(ncomp, ncomp_fwd, ncomp_bwd,           &
     &    nidx_rtp, irt_rtp_smp_stack, MHD_mul_FFTW)
!
      end subroutine init_MHD_multi_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_MHD_multi_FFTW
!
!
      call finalize_sph_multi_FFTW_t(MHD_mul_FFTW)
!
      end subroutine finalize_MHD_multi_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_MHD_multi_FFTW(ncomp, ncomp_fwd, ncomp_bwd)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp, ncomp_fwd, ncomp_bwd
!
!
      call verify_sph_multi_FFTW_t(ncomp, ncomp_fwd, ncomp_bwd,         &
     &    nnod_rtp, nidx_rtp, irt_rtp_smp_stack, MHD_mul_FFTW)
!
      end subroutine verify_MHD_multi_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine MHD_multi_fwd_FFTW_to_send                             &
     &         (ncomp_fwd, n_WS, irev_sr_rtp, X_rtp, WS)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &      :: X_rtp(nidx_rtp(3),irt_rtp_smp_stack(np_smp)*ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      call sph_mul_fwd_FFTW_to_send_t(ncomp_fwd, nnod_rtp, nidx_rtp,    &
     &    irt_rtp_smp_stack, n_WS, irev_sr_rtp, X_rtp, WS,              &
     &    MHD_mul_FFTW)
!
      end subroutine MHD_multi_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine MHD_multi_back_FFTW_from_recv                          &
     &         (ncomp_bwd, n_WR, irev_sr_rtp, WR, X_rtp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &      :: X_rtp(nidx_rtp(3),irt_rtp_smp_stack(np_smp)*ncomp_bwd)
!
      call sph_mul_back_FFTW_from_recv_t(ncomp_bwd, nnod_rtp, nidx_rtp, &
     &    irt_rtp_smp_stack, n_WR, irev_sr_rtp, WR, X_rtp,              &
     &    MHD_mul_FFTW)
!
      end subroutine MHD_multi_back_FFTW_from_recv
!
! ------------------------------------------------------------------
!
      end module m_MHD_multi_FFTW
