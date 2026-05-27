!>@file   t_multi_FFTW_wrapper.f90
!!@brief  module t_multi_FFTW_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Work area for multiple FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine alloc_mul_FFTW_plan_t(Nplan, Ncomp, Nfft, WK)
!!      subroutine dealloc_mul_FFTW_plan_t(WK)
!!        integer(kind = kint), intent(in) :: Nplan, Ncomp, Nfft
!!        type(working_mul_FFTW), intent(inout) :: WK
!!
!! ------------------------------------------------------------------
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!! ------------------------------------------------------------------
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
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!!@n @param WK          Work structure for FFTW3
!
      module t_multi_FFTW_wrapper
!
      use m_precision
      use m_constants
      use m_fftw_parameters
!
      use multi_pout_FFTW3_smp
!
      implicit none
!
!>      structure for working data for FFTW
      type working_mul_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan), allocatable :: plan_back_mul(:)
!>        plan ID for forward transform
        integer(kind = fftw_plan), allocatable :: plan_fowd_mul(:)
!
!>        number of component for complex data
        integer(kind = kint) :: Nfft_c
!>        normalization parameter for FFTW (= 1 / Nfft)
        real(kind = kreal) :: aNfft
!>        real data for multiple Fourier transform
        real(kind = kreal), allocatable :: X_FFTW_mul(:,:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), allocatable :: C_FFTW_mul(:,:)
!>        flag for number of components for Fourier transform
        integer(kind = kint) :: iflag_fft_mul_len =  -1
      end type working_mul_FFTW
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine alloc_mul_FFTW_plan_t(Nplan, Ncomp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nplan, Ncomp, Nfft
      type(working_mul_FFTW), intent(inout) :: WK
!
!
      allocate(WK%plan_fowd_mul(Nplan))
      allocate(WK%plan_back_mul(Nplan))
!
      WK%iflag_fft_mul_len = Nfft*Ncomp
      WK%Nfft_c = (Nfft+1)/2 + 1
      WK%aNfft =  one / dble(Nfft)
      allocate( WK%X_FFTW_mul(Nfft,Ncomp) )
      allocate( WK%C_FFTW_mul(WK%Nfft_c,Ncomp) )
      WK%X_FFTW_mul = 0.0d0
      WK%C_FFTW_mul = 0.0d0
!
      end subroutine alloc_mul_FFTW_plan_t
!
! ------------------------------------------------------------------
!
      subroutine dealloc_mul_FFTW_plan_t(WK)
!
      type(working_mul_FFTW), intent(inout) :: WK
!
      deallocate(WK%X_FFTW_mul, WK%C_FFTW_mul)
      deallocate(WK%plan_fowd_mul, WK%plan_back_mul)
      WK%iflag_fft_mul_len = 0
!
      end subroutine dealloc_mul_FFTW_plan_t
!
! ------------------------------------------------------------------
!
      end module t_multi_FFTW_wrapper
