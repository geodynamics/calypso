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
!!      subroutine calypso_pin_RFFTMF                                   &
!!     &         (M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        type(working_FFTPACK), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
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
!!      subroutine calypso_pin_RFFTMB                                   &
!!     &         (M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        type(working_FFTPACK), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
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
      subroutine calypso_pin_RFFTMF                                     &
     &         (M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      use t_FFTPACK5_wrapper
      use swap_prt_data_for_FFTPACK
      use calypso_multi_fftpack

      integer(kind = kint), intent(in) :: M, Nfft
!
      type(working_FFTPACK), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call swap_prt_fld_to_RFFTMF                                       &
     &   (WK%Nplan_FFTPACK, WK%istack_FFTPACK, WK%Mmax_smp,             &
     &    Nfft, M, X(1,1), WK%X_FFTPACK5(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_RFFTMF_smp                                             &
     &   (WK%Nplan_FFTPACK, WK%istack_FFTPACK, WK%Mmax_smp,             &
     &    Nfft, WK%X_FFTPACK5, WK%lsave_FFTPACK,                        &
     &    WK%WSAVE_FFTPACK, WK%WORK_FFTPACK)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call swap_prt_spectr_from_RFFTMF                                  &
     &   (WK%Nplan_FFTPACK, WK%istack_FFTPACK, WK%Mmax_smp,             &
     &    Nfft, WK%X_FFTPACK5(1,1), M, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine calypso_pin_RFFTMF
!
! ------------------------------------------------------------------
!
      subroutine calypso_pin_RFFTMB                                     &
     &         (M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      use t_FFTPACK5_wrapper
      use swap_prt_data_for_FFTPACK
      use calypso_multi_fftpack
!
      integer(kind = kint), intent(in) :: M, Nfft
!
      type(working_FFTPACK), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call swap_prt_spectr_to_RFFTMB                                    &
     &   (WK%Nplan_FFTPACK, WK%istack_FFTPACK, WK%Mmax_smp,             &
     &    Nfft, M, X(1,1), WK%X_FFTPACK5(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_RFFTMB_smp                                             &
     &   (WK%Nplan_FFTPACK, WK%istack_FFTPACK, WK%Mmax_smp,             &
     &    Nfft, WK%X_FFTPACK5, WK%lsave_FFTPACK,                        &
     &    WK%WSAVE_FFTPACK, WK%WORK_FFTPACK)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call swap_prt_fld_from_RFFTMB                                     &
     &   (WK%Nplan_FFTPACK, WK%istack_FFTPACK, WK%Mmax_smp,             &
     &    Nfft, WK%X_FFTPACK5(1,1), M, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine calypso_pin_RFFTMB
!
! ------------------------------------------------------------------
!
      end module multi_pin_FFTPACK_smp
