!>@file   t_single_FFTW_wrapper.f90
!!@brief  module t_single_FFTW_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Work area for single FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine alloc_work_4_FFTW_t(Nsmp, Nfft, WK)
!!      subroutine dealloc_work_4_FFTW_t(WK)
!!        integer(kind = kint), intent(in) :: Nsmp, Nfft
!!        type(working_FFTW), intent(inout) :: WK
!!
!! ------------------------------------------------------------------
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          * \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          * \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          * \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
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
      module t_single_FFTW_wrapper
!
      use m_precision
      use m_constants
      use m_fftw_parameters
!
      use single_pout_FFTW3_smp
!
      implicit none
!
!>      structure for working data for FFTW
      type working_FFTW
!>        number of FFT plans for SMP
        integer(kind = kint) :: Nplan_sFFTW = 1
!>        plan ID for backward transform
        integer(kind = fftw_plan), allocatable :: plan_backward(:)
!>        plan ID for forward transform
        integer(kind = fftw_plan), allocatable :: plan_forward(:)
!
!>        number of component for each FFT
        integer(kind = kint_gl), allocatable :: istack_sFFTW(:)
!>        Maximum nuber of components for each SMP process
        integer(kind = kint_gl) :: Mmax_smp
!
!>        Complax data size
        integer(kind = kint) :: NFFT_c
!>        normalization parameter for FFTW (= 1 / Nfft)
        real(kind = kreal) :: aNfft
!>        real data for multiple Fourier transform
        real(kind = kreal), allocatable :: X_FFTW(:,:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), allocatable :: C_FFTW(:,:)
!>        flag for number of components for Fourier transform
        integer(kind = kint) :: iflag_fft_len =  -1
      end type working_FFTW
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_FFTW_t(Nsmp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nsmp, Nfft
      type(working_FFTW), intent(inout) :: WK
!
!
      WK%Nplan_sFFTW = Nsmp
      allocate(WK%plan_forward(Nsmp))
      allocate(WK%plan_backward(Nsmp))
!
      allocate(WK%istack_sFFTW(0:Nsmp))
      WK%istack_sFFTW(0:Nsmp) = 0
!
      WK%iflag_fft_len = Nfft*Nsmp
      WK%Nfft_c =        (Nfft+1)/2 + 1
      WK%aNfft = one / dble(Nfft)
      allocate( WK%X_FFTW(Nfft,Nsmp) )
      allocate( WK%C_FFTW(WK%Nfft_c,Nsmp) )
      WK%X_FFTW = 0.0d0
      WK%C_FFTW = 0.0d0
!
      end subroutine alloc_work_4_FFTW_t
!
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_FFTW_t(WK)
!
      type(working_FFTW), intent(inout) :: WK
!
      deallocate(WK%istack_sFFTW)
      deallocate(WK%X_FFTW, WK%C_FFTW)
      deallocate(WK%plan_forward, WK%plan_backward)
      WK%iflag_fft_len = 0
!
      end subroutine dealloc_work_4_FFTW_t
!
! ------------------------------------------------------------------
!
      end module t_single_FFTW_wrapper
