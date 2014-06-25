!>@file   FFT_selector.f90
!!@brief  module FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!      module FFT_selector
!
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine initialize_FFT_select(my_rank, Nsmp, Nstacksmp, Nfft)
!!      subroutine finalize_FFT_select(Nsmp, Nstacksmp)
!!      subroutine verify_FFT_select(Nsmp, Nstacksmp, Nfft)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine forward_FFT_select(Nsmp, Nstacksmp, M, Nfft, X)
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine backward_FFT_select(Nsmp, Nstacksmp, M, Nfft, X)
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for backward FFT
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
      module FFT_selector
!
      use m_precision
      use m_machine_parameter
      use m_FFTPACK5_wrapper
!
#ifdef FFTW3
      use m_FFTW_wrapper
      use m_multi_FFTW_wrapper
#endif
!
      implicit none
!
!>      integer flag for undefined
      integer(kind = kint), parameter :: iflag_UNDEFINED_FFT =   0
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK =     1
!>      integer flag to use FFTW3
      integer(kind = kint), parameter :: iflag_FFTW =        2
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_SINGLE = 3
!>      integer flag to use ISPACK
      integer(kind = kint), parameter :: iflag_ISPACK =      4
!
      integer(kind = kint) :: iflag_FFT = iflag_UNDEFINED_FFT
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine initialize_FFT_select(my_rank, Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  my_rank, Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        if(my_rank .eq. 0) write(*,*) 'Use FFTW'
        call init_FFTW_mul(Nsmp, Nstacksmp, Nfft)
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(my_rank .eq. 0) write(*,*) 'Use single FFTW transforms'
        call init_4_FFTW(Nsmp, Nstacksmp, Nfft)
        return
      end if
#endif
      if(my_rank .eq. 0) write(*,*) 'Use FFTPACK'
      call init_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
!
      end subroutine initialize_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine finalize_FFT_select(Nsmp, Nstacksmp)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_FFTW_mul(Nsmp)
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .eq. 0) write(*,*) 'Finalize single FFTW'
        call finalize_4_FFTW(Nsmp, Nstacksmp)
        return
      end if
#endif
!
      if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
      call finalize_4_FFTPACK
!
      end subroutine finalize_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine verify_FFT_select(Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW'
        call verify_work_FFTW_mul(Nsmp, Nstacksmp, Nfft)
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .eq. 0) write(*,*) 'Use single FFTW transforms'
        call verify_work_4_FFTW(Nsmp, Nstacksmp, Nfft)
        return
      end if
#endif
!
      if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK'
      call verify_work_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
!
      end subroutine verify_FFT_select
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine forward_FFT_select(Nsmp, Nstacksmp, M, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        call FFTW_mul_forward(Nsmp, Nstacksmp, M, Nfft, X)
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call FFTW_forward(Nsmp, Nstacksmp, M, Nfft, X)
        return
      end if
#endif
!
      call CALYPSO_RFFTMF(Nsmp, Nstacksmp, M, Nfft, X)
!
      end subroutine forward_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine backward_FFT_select(Nsmp, Nstacksmp, M, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        call FFTW_mul_backward(Nsmp, Nstacksmp, M, Nfft, X)
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call FFTW_backward(Nsmp, Nstacksmp, M, Nfft, X)
        return
      end if
#endif
!
      call CALYPSO_RFFTMB(Nsmp, Nstacksmp, M, Nfft, X)
!
      end subroutine backward_FFT_select
!
! ------------------------------------------------------------------
!
      end module FFT_selector
