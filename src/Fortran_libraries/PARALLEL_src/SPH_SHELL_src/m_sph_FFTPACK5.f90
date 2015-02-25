!>@file   m_sph_FFTPACK5.f90
!!@brief  module m_sph_FFTPACK5
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_FFTPACK5(ncomp)
!!      subroutine finalize_sph_FFTPACK5
!!      subroutine verify_sph_FFTPACK5(Nstacksmp, Nfft)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine sph_RFFTMF_to_send                                   &
!!     &         (ncomp, n_WS, irev_sr_rtp, X_rtp, WS)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTPACK5
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \cos (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          * \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine sph_RFFTMB_from_recv                                 &
!!     &         (ncomp, n_WR, irev_sr_rtp, WR, X_rtp)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by FFTPACK5
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
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!
      module m_sph_FFTPACK5
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_sph_FFTPACK5
!
      implicit none
!
!>      Structure to use FFTPACK
      type(work_for_fftpack), save :: sph_FFTPACK
!
      private :: sph_FFTPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_FFTPACK5(ncomp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
!
!
      call init_sph_FFTPACK5_t(ncomp, nidx_rtp, maxirt_rtp_smp,         &
     &    sph_FFTPACK)
!
      end subroutine init_sph_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_FFTPACK5
!
!
      call finalize_sph_FFTPACK5_t(sph_FFTPACK)
!
      end subroutine finalize_sph_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_FFTPACK5(ncomp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) ::  ncomp
!
      call verify_sph_FFTPACK5_t(ncomp, nidx_rtp, maxirt_rtp_smp,       &
     &    sph_FFTPACK)
!
      end subroutine verify_sph_FFTPACK5
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_RFFTMF_to_send                                     &
     &         (ncomp, n_WS, irev_sr_rtp, X_rtp, WS)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call sph_RFFTMF_to_send_t(ncomp, nnod_rtp, nidx_rtp,              &
     &    irt_rtp_smp_stack, n_WS, irev_sr_rtp, X_rtp, WS, sph_FFTPACK)
!
      end subroutine sph_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_RFFTMB_from_recv                                   &
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
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
!
      call sph_RFFTMB_from_recv_t(ncomp, nnod_rtp, nidx_rtp,            &
     &    irt_rtp_smp_stack, n_WR, irev_sr_rtp, WR, X_rtp, sph_FFTPACK)
!
      end subroutine sph_RFFTMB_from_recv
!
! ------------------------------------------------------------------
!
      end module m_sph_FFTPACK5
