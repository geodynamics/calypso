!>@file   single_pin_FFTPACK_smp.f90
!!@brief  module single_pin_FFTPACK_smp
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!
!>@brief  FFTPACK5 wrapper
!!
!!@verbatim
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine single_pin_RFFTMF_smp(Nsmp, Nstacksmp, M, Nfft, X,   &
!!     &          lSAVE, WSAVE, WORK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        integer(kind = kint), intent(in) :: lSAVE
!!        real(kind = 8), intent(in) :: WSAVE(lSAVE)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        real(kind = 8), intent(inout) :: WORK(Nfft,Nsmp)
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
!!      subroutine single_pin_RFFTMB_smp(Nsmp, Nstacksmp, M, Nfft, X,   &
!!     &          lSAVE, WSAVE, WORK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        integer(kind = kint), intent(in) :: lSAVE
!!        real(kind = 8), intent(in) :: WSAVE(lSAVE)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        real(kind = 8), intent(inout) :: WORK(Nfft,Nsmp)
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
!!@n @param X(Nfft,M)  Data for Fourier transform
!!
!!@n @param lSAVE                     Size of work constant for FFTPACK
!!@n @param WSAVE(lSAVE)              Work constatnts for FFTPACK
!!@n @param WORK(Nfft,Nsmp)  Work area for FFTPACK
!
      module single_pin_FFTPACK_smp
!
      use omp_lib
!
      use m_precision
      use m_constants
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine single_pin_RFFTMF_smp(Nsmp, Nstacksmp, M, Nfft, X,     &
     &          lSAVE, WSAVE, WORK, elapsed_fft, elapsed_cpy)
!
      use normalize_for_FFTPACK
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
      integer(kind = kint), intent(in) :: lSAVE
      real(kind = 8), intent(in) :: WSAVE(lSAVE)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = 8), intent(inout) :: WORK(Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      real(kind = kreal) :: X_tmp
      integer(kind = kint) :: ismp, ist, ied, inum
      integer(kind = kint) :: ierr
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(ist,ied,inum,X_tmp,st_c,st_f)                 &
!$omp&            reduction(+:ed_c,ed_f)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1) + 1
        ied = Nstacksmp(ismp  )
        do inum = ist, ied
          st_f = OMP_GET_WTIME()
          call RFFTMF(ione, ione, Nfft, ione, X(1,inum), Nfft,          &
     &              WSAVE, lSAVE, WORK(1,ismp), Nfft, ierr)
          ed_f = ed_f + OMP_GET_WTIME() - st_f
!
          st_c = OMP_GET_WTIME()
!          X(1,inum) = X(1,inum)
          X_tmp =          X(Nfft,inum)
          X(3:Nfft,inum) = X(2:Nfft-1,inum)
          X(2,inum) =      X_tmp
          ed_c = ed_c + OMP_GET_WTIME() - st_c
        end do
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine single_pin_RFFTMF_smp
!
! ------------------------------------------------------------------
!
      subroutine single_pin_RFFTMB_smp(Nsmp, Nstacksmp, M, Nfft, X,     &
     &          lSAVE, WSAVE, WORK, elapsed_fft, elapsed_cpy)
!
      use normalize_for_FFTPACK
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
      integer(kind = kint), intent(in) :: lSAVE
      real(kind = 8), intent(in) :: WSAVE(lSAVE)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = 8), intent(inout) :: WORK(Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      real(kind = kreal) :: X_tmp
      integer(kind = kint) ::  ismp, ist, ied, inum
      integer(kind = kint) :: ierr
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(ist,ied,inum,X_tmp,st_c,st_f)                 &
!$omp&            reduction(+:ed_c,ed_f)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1) + 1
        ied = Nstacksmp(ismp  )
        do inum = ist, ied
!
!   normalization
          st_c = OMP_GET_WTIME()
!          X(1,inum) =        X(1,inum)
          X_tmp =            X(2,inum)
          X(2:Nfft-1,inum) = X(3:Nfft,inum)
          X(Nfft,inum) =     X_tmp
          ed_c = ed_c + OMP_GET_WTIME() - st_c
!
          st_f = OMP_GET_WTIME()
          call RFFTMB(ione, ione, Nfft, ione, X(1,inum), Nfft,          &
     &                WSAVE, lSAVE, WORK(1,ismp), Nfft, ierr)
          ed_f = OMP_GET_WTIME() - st_f
        end do
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine single_pin_RFFTMB_smp
!
! ------------------------------------------------------------------
!
      end module single_pin_FFTPACK_smp
