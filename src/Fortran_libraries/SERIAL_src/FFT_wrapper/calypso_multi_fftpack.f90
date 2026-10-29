!>@file   calypso_multi_fftpack.f90
!!@brief  module calypso_multi_fftpack
!!
!!@author H. Matsui
!!@date Programmed on Apr., 2013
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ------------------------------------------------------------------
!!
!!      subroutine init_WK_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WK)
!!        integer(kind = kint), intent(in) ::  Nfft
!!        integer(kind = kint), intent(in) ::  Nsmp
!!        integer(kind = kint), intent(in) ::  Nstacksmp(0:Nsmp)
!!        type(working_FFTPACK), intent(inout) :: WK
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine multi_RFFTMF_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,    &
!!     &                                X_FFTPACK5, lSAVE, WSAVE, WORK)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        integer(kind = kint), intent(in) :: lSAVE
!!        real(kind = 8), intent(in) :: WSAVE(lSAVE)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        real(kind = 8), intent(inout) :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!        real(kind = 8), intent(inout) :: WORK(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTPACK5
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          * \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine multi_RFFTMB_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,    &
!!     &                                X_FFTPACK5, lSAVE, WSAVE, WORK)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        integer(kind = kint), intent(in) :: lSAVE
!!        real(kind = 8), intent(in) :: WSAVE(lSAVE)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        real(kind = 8), intent(inout) :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!        real(kind = 8), intent(inout) :: WORK(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
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
!!@n @param WK          Work structure for FFTPACK5
!
      module calypso_multi_fftpack
!
      use m_precision
      use m_constants
      use t_FFTPACK5_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_WK_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp
      integer(kind = kint), intent(in) ::  Nstacksmp(0:Nsmp)
!
      type(working_FFTPACK), intent(inout) :: WK
!
!
      call alloc_const_4_FFTPACK_t(Nsmp, Nfft, WK)
      call count_FFTPACK_smp(Nsmp, Nstacksmp, WK)
!
      call init_CALYPSO_FFTPACK                                         &
     &   (Nfft, WK%lsave_FFTPACK, WK%WSAVE_FFTPACK)
!
      call alloc_work_4_FFTPACK_t(Nsmp, Nfft, WK)
!
      end subroutine init_WK_FFTPACK_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_RFFTMF_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,      &
     &                            X_FFTPACK5, lSAVE, WSAVE, WORK)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: lSAVE
      real(kind = 8), intent(in) :: WSAVE(lSAVE)
!
      real(kind = 8), intent(inout) :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: num, nsize
      integer(kind = kint) :: ip, ierr
!
!
!$omp do private(num,nsize)
      do ip = 1, Nsmp
        num = Nstacksmp(ip) - Nstacksmp(ip-1)
        if(num .le. 0) cycle
!
        nsize = num*Nfft
        call RFFTMF(num, ione, Nfft, num, X_FFTPACK5(1,ip), nsize,      &
     &              WSAVE, lSAVE, WORK(1,ip), nsize, ierr)
      end do
!$omp end do
!
      end subroutine multi_RFFTMF_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_RFFTMB_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,      &
     &                            X_FFTPACK5, lSAVE, WSAVE, WORK)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: lSAVE
      real(kind = 8), intent(in) :: WSAVE(lSAVE)
!
      real(kind = 8), intent(inout) :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: num, nsize
      integer(kind = kint) :: ip, ierr
!
!
!$omp do private(num,nsize)
      do ip = 1, Nsmp
        num = Nstacksmp(ip) - Nstacksmp(ip-1)
        if(num .le. 0) cycle
!
        nsize = num*Nfft
        call RFFTMB(num, ione, Nfft, num, X_FFTPACK5(1,ip), nsize,      &
     &              WSAVE, lSAVE, WORK(1,ip), nsize, ierr)
      end do
!$omp end do nowait
!
      end subroutine multi_RFFTMB_smp
!
! ------------------------------------------------------------------
!
      end module calypso_multi_fftpack
