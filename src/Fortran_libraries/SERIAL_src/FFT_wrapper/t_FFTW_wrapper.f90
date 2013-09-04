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
!!      subroutine init_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!!      subroutine finalize_FFTW_type(Nsmp, WK)
!!      subroutine verify_wk_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_forward_type(Nsmp, Nstacksmp, M, Nfft, X, WK)
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
!!      subroutine FFTW_backward_type(Nsmp, Nstacksmp, M, Nfft, X, WK)
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
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!!@n @param WK          Work structure for FFTW3
!
      module t_FFTW_wrapper
!
      use m_precision
      use m_constants
!
      use FFTW_wrapper
!
      implicit none
!
!>      structure for working data for FFTW
      type working_FFTW
!>        Maximum nuber of components for each SMP process
        integer(kind = kint) :: Mmax_smp
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
      subroutine init_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTW), intent(inout) :: WK
!
      integer(kind = kint) :: ip
!
!
      WK%Mmax_smp = Nstacksmp(1)
      do ip = 1, Nsmp
        WK%Mmax_smp                                                     &
     &      = max(WK%Mmax_smp, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      call alloc_work_4_FFTW_t(Nsmp, Nfft, WK)
      call init_4_FFTW_smp(Nsmp, Nfft, WK%plan_forward,                 &
     &      WK%plan_backward, WK%aNfft, WK%X_FFTW, WK%C_FFTW)
!
      end subroutine init_FFTW_type
!
! ------------------------------------------------------------------
!
      subroutine finalize_FFTW_type(Nsmp, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp
!
      type(working_FFTW), intent(inout) :: WK
!
!
      call destroy_FFTW_smp(Nsmp, WK%plan_forward, WK%plan_backward)
      call dealloc_work_4_FFTW_t(WK)
!
      end subroutine finalize_FFTW_type
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTW), intent(inout) :: WK
!
      integer(kind = kint) :: ip
!
!
      if(WK%iflag_fft_len .lt. 0) then
        call init_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
        return
      end if
!
      WK%Mmax_smp = Nstacksmp(1)
      do ip = 1, Nsmp
        WK%Mmax_smp                                                     &
     &      = max(WK%Mmax_smp, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      if( WK%iflag_fft_len .ne. Nfft) then
        call destroy_FFTW_smp(Nsmp, WK%plan_forward, WK%plan_backward)
        call dealloc_work_4_FFTW_t(WK)
!
        call alloc_work_4_FFTW_t(Nsmp, Nfft, WK)
        call init_4_FFTW_smp(Nsmp, Nfft, WK%plan_forward,               &
     &      WK%plan_backward, WK%aNfft, WK%X_FFTW, WK%C_FFTW)
      end if
!
      end subroutine verify_wk_FFTW_type
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FFTW_forward_type(Nsmp, Nstacksmp, M, Nfft, X, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      type(working_FFTW), intent(inout) :: WK
!
!
      call FFTW_forward_SMP(WK%plan_forward, Nsmp, Nstacksmp,           &
     &          M, Nfft, WK%aNfft, X, WK%X_FFTW, WK%C_FFTW)
!
      end subroutine FFTW_forward_type
!
! ------------------------------------------------------------------
!
      subroutine FFTW_backward_type(Nsmp, Nstacksmp, M, Nfft, X, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      type(working_FFTW), intent(inout) :: WK
!
!
      call FFTW_backward_SMP(WK%plan_backward, Nsmp, Nstacksmp,         &
     &    M, Nfft, X, WK%X_FFTW, WK%C_FFTW)
!
      end subroutine FFTW_backward_type
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_FFTW_t(Nsmp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nsmp, Nfft
      type(working_FFTW), intent(inout) :: WK
!
!
      allocate(WK%plan_forward(Nsmp))
      allocate(WK%plan_backward(Nsmp))
!
      WK%iflag_fft_len = Nfft
      allocate( WK%X_FFTW(Nfft,Nsmp) )
      allocate( WK%C_FFTW(Nfft/2+1,Nsmp) )
      WK%X_FFTW = 0.0d0
      WK%C_FFTW = 0.0d0
!
      end subroutine alloc_work_4_FFTW_t
!
! ------------------------------------------------------------------
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
