!>@file   multi_pin_FFTPACK_smp.f90
!!@brief  module multi_pin_FFTPACK_smp
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
!!      subroutine multi_pin_RFFTMF_smp(Nsmp, Nstacksmp, M, Nfft,       &
!!     &          X, X_FFTPACK5, Mmax_smp, lSAVE, WSAVE, WORK,          &
!!     &          elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        integer(kind = kint), intent(in) :: lSAVE, Mmax_smp
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
!!          *  \cos (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          * \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine multi_pin_RFFTMB_smp(Nsmp, Nstacksmp, M, Nfft,       &
!!     &          X, X_FFTPACK5, Mmax_smp, lSAVE, WSAVE, WORK,          &
!!     &          elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        integer(kind = kint), intent(in) :: lSAVE, Mmax_smp
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
!!@n @param X(Nfft,M)  Data for Fourier transform
!!
!!@n @param Mmax_smp    Maximum number of component for each SMP process
!!@n @param X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!                 Data for multiple Fourier transform
!!@n @param lSAVE                     Size of work constant for FFTPACK
!!@n @param WSAVE(lSAVE)              Work constatnts for FFTPACK
!!@n @param WORK(Mmax_smp*Nfft,Nsmp)  Work area for FFTPACK
!
      module multi_pin_FFTPACK_smp
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
      subroutine multi_pin_RFFTMF_smp(Nsmp, Nstacksmp, M, Nfft,         &
     &          X, X_FFTPACK5, Mmax_smp, lSAVE, WSAVE, WORK,            &
     &          elapsed_fft, elapsed_cpy)
!
      use normalize_for_FFTPACK
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
      integer(kind = kint), intent(in) :: lSAVE, Mmax_smp
      real(kind = 8), intent(in) :: WSAVE(lSAVE)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = 8), intent(inout) :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK(Mmax_smp*Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      integer(kind = kint) :: ismp, ist, num, nsize
      integer(kind = kint) :: ierr
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(ist,num,nsize,st_c,st_f)                      &
!$omp&            reduction(+:ed_c,ed_f)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        nsize = num*Nfft
!
        st_c = OMP_GET_WTIME()
        call swap_prt_fld_to_RFFTMF_smp(ist, num, Nfft, M, X,           &
     &                                  Mmax_smp, X_FFTPACK5(1,ismp))
        ed_c = ed_c + OMP_GET_WTIME() - st_c
!
        st_f = OMP_GET_WTIME()
        call RFFTMF(num, ione, Nfft, num, X_FFTPACK5(1,ismp), nsize,    &
     &              WSAVE, lSAVE, WORK(1,ismp), nsize, ierr)
        ed_f = ed_f + OMP_GET_WTIME() - st_f
!
        st_c = OMP_GET_WTIME()
        call swap_prt_spectr_from_RFFTMF_smp(ist, num, Nfft, Mmax_smp,  &
     &                                       X_FFTPACK5(1,ismp), M, X)
        ed_c = ed_c + OMP_GET_WTIME() - st_c
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine multi_pin_RFFTMF_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_pin_RFFTMB_smp(Nsmp, Nstacksmp, M, Nfft,         &
     &          X, X_FFTPACK5, Mmax_smp, lSAVE, WSAVE, WORK,            &
     &          elapsed_fft, elapsed_cpy)
!
      use normalize_for_FFTPACK
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
      integer(kind = kint), intent(in) :: lSAVE, Mmax_smp
      real(kind = 8), intent(in) :: WSAVE(lSAVE)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = 8), intent(inout) :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK(Mmax_smp*Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      integer(kind = kint) ::  ismp, ist, num, nsize
      integer(kind = kint) :: ierr
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(ist,num,nsize,st_c,st_f)                      &
!$omp&            reduction(+:ed_c,ed_f)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        nsize = num*Nfft
!
!   normalization
        st_c = OMP_GET_WTIME()
        call swap_prt_spectr_to_RFFTMB_smp(ist, num, Nfft, M, X,        &
     &                                    Mmax_smp, X_FFTPACK5(1,ismp))
        ed_c = ed_c + OMP_GET_WTIME() - st_c
!
        st_f = OMP_GET_WTIME()
        call RFFTMB(num, ione, Nfft, num, X_FFTPACK5(1,ismp), nsize,    &
     &              WSAVE, lSAVE, WORK(1,ismp), nsize, ierr)
        ed_f = ed_f + OMP_GET_WTIME() - st_f
!
        st_c = OMP_GET_WTIME()
        call swap_prt_fld_from_RFFTMB_smp(ist, num, Nfft, Mmax_smp,     &
     &                                    X_FFTPACK5(1,ismp), M, X)
        ed_c = ed_c + OMP_GET_WTIME() - st_c
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine multi_pin_RFFTMB_smp
!
! ------------------------------------------------------------------
!
      end module multi_pin_FFTPACK_smp