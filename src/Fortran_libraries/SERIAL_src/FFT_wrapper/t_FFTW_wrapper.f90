!>@file   t_FFTW_wrapper.f90
!!@brief  module t_FFTW_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_FFTW_type(Ncomp, Nfft, WK)
!!      subroutine finalize_FFTW_type(Ncomp, WK)
!!      subroutine verify_wk_FFTW_type(Ncomp, Nfft, WK)
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_forward_type(Nsmp, Nstacksmp, Ncomp, Nfft, X, WK)
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
!!      subroutine FFTW_backward_type(Nsmp, Nstacksmp, Ncomp, Nfft,     &
!!                X, WK)
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
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!!@n @param WK          Work structure for FFTW3
!
      module t_FFTW_wrapper
!
      use m_precision
      use m_constants
!
      use FFTW3_wrapper
!
      implicit none
!
!>      structure for working data for FFTW
      type working_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan), pointer :: plan_backward(:)
!>        plan ID for forward transform
        integer(kind = fftw_plan), pointer :: plan_forward(:)
!
!>      normalization parameter for FFTW (= 1 / Nfft)
        real(kind = kreal) :: aNfft
!>        real data for multiple Fourier transform
        real(kind = kreal), pointer :: X_FFTW(:,:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), pointer :: C_FFTW(:,:)
!>        flag for number of components for Fourier transform
        integer(kind = kint) :: iflag_fft_len =  -1
      end type working_FFTW
!
      private :: alloc_work_4_FFTW_t, dealloc_work_4_FFTW_t
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_FFTW_type(Ncomp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Ncomp, Nfft
!
      type(working_FFTW), intent(inout) :: WK
!
!
      call alloc_work_4_FFTW_t(Ncomp, Ncomp, Nfft, WK)
      call init_4_FFTW_smp(Ncomp, Nfft, WK%plan_forward,                &
     &      WK%plan_backward, WK%aNfft, WK%X_FFTW, WK%C_FFTW)
!
      end subroutine init_FFTW_type
!
! ------------------------------------------------------------------
!
      subroutine finalize_FFTW_type(Ncomp, WK)
!
      integer(kind = kint), intent(in) ::  Ncomp
!
      type(working_FFTW), intent(inout) :: WK
!
!
      call destroy_FFTW_smp(Ncomp, WK%plan_forward, WK%plan_backward)
      call dealloc_work_4_FFTW_t(WK)
!
      end subroutine finalize_FFTW_type
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_FFTW_type(Ncomp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Ncomp, Nfft
!
      type(working_FFTW), intent(inout) :: WK
!
!
      if(WK%iflag_fft_len .lt. 0) then
        call init_FFTW_type(Ncomp, Nfft, WK)
        return
      end if
!
      if( WK%iflag_fft_len .ne. Nfft*Ncomp) then
        call finalize_FFTW_type(Ncomp, WK)
        call init_FFTW_type(Ncomp, Nfft, WK)
      end if
!
      end subroutine verify_wk_FFTW_type
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FFTW_forward_type(Nsmp, Nstacksmp, Ncomp, Nfft, X, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      type(working_FFTW), intent(inout) :: WK
!
!
      call FFTW_forward_SMP(WK%plan_forward, Nsmp, Nstacksmp,           &
     &          Ncomp, Nfft, WK%aNfft, X, WK%X_FFTW, WK%C_FFTW)
!
      end subroutine FFTW_forward_type
!
! ------------------------------------------------------------------
!
      subroutine FFTW_backward_type(Nsmp, Nstacksmp, Ncomp, Nfft,       &
     &          X, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      type(working_FFTW), intent(inout) :: WK
!
!
      call FFTW_backward_SMP(WK%plan_backward, Nsmp, Nstacksmp,         &
     &    Ncomp, Nfft, X, WK%X_FFTW, WK%C_FFTW)
!
      end subroutine FFTW_backward_type
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_FFTW_t(Nplan, Ncomp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nplan, Ncomp, Nfft
      type(working_FFTW), intent(inout) :: WK
!
!
      allocate(WK%plan_forward(Nplan))
      allocate(WK%plan_backward(Nplan))
!
      WK%iflag_fft_len = Nfft*Ncomp
      allocate( WK%X_FFTW(Nfft,Ncomp) )
      allocate( WK%C_FFTW(Nfft/2+1,Ncomp) )
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
      deallocate(WK%X_FFTW, WK%C_FFTW)
      deallocate(WK%plan_forward, WK%plan_backward)
      WK%iflag_fft_len = 0
!
      end subroutine dealloc_work_4_FFTW_t
!
! ------------------------------------------------------------------
!
      end module t_FFTW_wrapper
