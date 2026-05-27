!>@file   calypso_multi_FFTW3.f90
!!@brief  module calypso_multi_FFTW3
!!
!!@author H. Matsui
!!@date Programmed on Apr., 2013
!
!
!>@brief  Fourier transform with work structures for ISPACK
!!
!!@verbatim
!!   wrapper subroutine for initierize FFT by FFTW
!!      subroutine init_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WK)
!!      subroutine finalize_FFTW_mul_type(Nsmp, WK)
!!      subroutine verify_wk_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WK)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) ::  Nfft
!!        type(working_mul_FFTW), intent(inout) :: WK
!! ------------------------------------------------------------------
!!
!!      subroutine calypso_multi_pin_fwd_FFTW3(Nsmp, Nstacksmp,         &
!!     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!        type(working_mul_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine calypso_multi_pout_fwd_FFTW3(Nsmp, Nstacksmp,        &
!!     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!        real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
!!        type(working_mul_FFTW), intent(inout) :: WK
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine calypso_multi_pin_bwd_FFTW3(Nsmp, Nstacksmp,         &
!!     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!        type(working_mul_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine calypso_multi_pout_bwd_FFTW3(Nsmp, Nstacksmp,        &
!!     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!        type(working_mul_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
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
!
      module calypso_multi_FFTW3
!
      use m_precision
      use m_constants
      use t_multi_FFTW_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) ::  Nfft
!
      type(working_mul_FFTW), intent(inout) :: WK
!
!
      call alloc_mul_FFTW_plan_t(Nsmp, Nstacksmp(Nsmp), Nfft, WK)
      call init_4_FFTW_mul_smp                                          &
     &   (Nsmp, Nstacksmp, Nstacksmp(Nsmp), Nfft, WK%Nfft_c,            &
     &    WK%plan_fowd_mul, WK%plan_back_mul,                           &
     &    WK%X_FFTW_mul, WK%C_FFTW_mul)
!
      end subroutine init_FFTW_mul_type
!
! ------------------------------------------------------------------
!
      subroutine finalize_FFTW_mul_type(Nsmp, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp
!
      type(working_mul_FFTW), intent(inout) :: WK
!
!
      call destroy_FFTW_mul_smp                                         &
     &   (Nsmp, WK%plan_fowd_mul, WK%plan_back_mul)
      call dealloc_mul_FFTW_plan_t(WK)
!
      end subroutine finalize_FFTW_mul_type
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Nfft
!
      type(working_mul_FFTW), intent(inout) :: WK
!
!
      if(WK%iflag_fft_mul_len .lt. 0) then
        call init_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WK)
        return
      end if
!
      if( WK%iflag_fft_mul_len .ne. Nfft*Nstacksmp(Nsmp)) then
        call finalize_FFTW_mul_type(Nsmp, WK)
        call init_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WK)
      end if
!
      end subroutine verify_wk_FFTW_mul_type
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_multi_pin_fwd_FFTW3(Nsmp, Nstacksmp,           &
     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      use multi_pin_FFTW3_smp
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      type(working_mul_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call multi_pin_fwd_FFTW3_smp(WK%plan_fowd_mul, Nsmp, Nstacksmp,   &
     &    Ncomp, Nfft, WK%aNfft, WK%Nfft_c, X, WK%C_FFTW_mul,           &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_multi_pin_fwd_FFTW3
!
! ------------------------------------------------------------------
!
      subroutine calypso_multi_pin_bwd_FFTW3(Nsmp, Nstacksmp,           &
     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      use multi_pin_FFTW3_smp
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      type(working_mul_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call multi_pin_bwd_FFTW3_smp(WK%plan_back_mul, Nsmp, Nstacksmp,   &
     &    Ncomp, Nfft, WK%Nfft_c, X, WK%C_FFTW_mul,                     &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_multi_pin_bwd_FFTW3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_multi_pout_fwd_FFTW3(Nsmp, Nstacksmp,          &
     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      type(working_mul_FFTW), intent(inout) :: WK
!
!
      call multi_pout_fwd_FFTW3_smp(WK%plan_fowd_mul,                   &
     &    Nsmp, Nstacksmp, Ncomp, Nfft, WK%aNfft, WK%Nfft_c, X,         &
     &    WK%X_FFTW_mul, WK%C_FFTW_mul, elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_multi_pout_fwd_FFTW3
!
! ------------------------------------------------------------------
!
      subroutine calypso_multi_pout_bwd_FFTW3(Nsmp, Nstacksmp,          &
     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      type(working_mul_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call multi_pout_bwd_FFTW3_smp(WK%plan_back_mul,                   &
     &    Nsmp, Nstacksmp, Ncomp, Nfft, WK%Nfft_c, X,                   &
     &    WK%X_FFTW_mul, WK%C_FFTW_mul, elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_multi_pout_bwd_FFTW3
!
! ------------------------------------------------------------------
!
      end module calypso_multi_FFTW3
