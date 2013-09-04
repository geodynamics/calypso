!>@file   t_FFTPACK5_wrapper.f90
!!@brief  module t_FFTPACK5_wrapper
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
!!      subroutine finalize_WK_FFTPACK_t(WK)
!!      subroutine verify_wk_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WK)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine CALYPSO_RFFTMF_t(Nsmp, Nstacksmp, M, Nfft, X, WK)
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
!!      subroutine CALYPSO_RFFTMB_t(Nsmp, Nstacksmp, M, Nfft, X, WK)
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
      module t_FFTPACK5_wrapper
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      structure for working data for FFTPACK5
      type working_FFTPACK
!>        Maximum nuber of components for each SMP process
        integer(kind = kint) :: Mmax_smp
!>        Data for multiple Fourier transform
        real(kind = 8), pointer :: X_FFTPACK5(:,:)
!
!>      Work area for FFTPACK
        integer(kind = kint) :: lsave_FFTPACK
!>        Work constatnts for FFTPACK
        real(kind = 8), pointer :: WSAVE_FFTPACK(:)
!>        Work area for FFTPACK
        real(kind = 8), pointer :: WORK_FFTPACK(:,:)
!>        flag for length of Fourier transform
        integer(kind = kint) :: iflag_fft_len =  -1
!>        flag for number of components for Fourier transform
        integer(kind = kint) :: iflag_fft_comp = -1
      end type working_FFTPACK
!
      private :: alloc_work_4_FFTPACK_t, alloc_const_4_FFTPACK_t
      private :: dealloc_work_4_FFTPACK_t, dealloc_const_FFTPACK_t
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_WK_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WK)
!
      use FFTPACK5_wrapper
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTPACK), intent(inout) :: WK
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
      call alloc_const_4_FFTPACK_t(Nfft, WK)
      call init_CALYPSO_FFTPACK(Nfft,                                   &
     &    WK%lsave_FFTPACK, WK%WSAVE_FFTPACK)
!
      call alloc_work_4_FFTPACK_t(Nsmp, Nfft, WK)
!
      end subroutine init_WK_FFTPACK_t
!
! ------------------------------------------------------------------
!
      subroutine finalize_WK_FFTPACK_t(WK)
!
      type(working_FFTPACK), intent(inout) :: WK
!
!
      call dealloc_const_FFTPACK_t(WK)
      call dealloc_work_4_FFTPACK_t(WK)
!
      end subroutine finalize_WK_FFTPACK_t
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WK)
!
      use FFTPACK5_wrapper
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTPACK), intent(inout) :: WK
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
      if(WK%iflag_fft_len .ne. Nfft) then
!
        if(WK%iflag_fft_len .lt. 0) then
          call alloc_const_4_FFTPACK_t(Nfft, WK)
        else if( Nfft .gt. WK%iflag_fft_comp ) then
          call dealloc_const_FFTPACK_t(WK)
          call alloc_const_4_FFTPACK_t(Nfft, WK)
        end if
!
        call init_CALYPSO_FFTPACK(Nfft,                                 &
     &    WK%lsave_FFTPACK, WK%WSAVE_FFTPACK)
      end if
!
      if(WK%iflag_fft_comp .lt. 0) then
        call alloc_work_4_FFTPACK_t(Nsmp, Nfft, WK)
      else if( (WK%Mmax_smp*Nfft) .gt. WK%iflag_fft_comp ) then
        call dealloc_work_4_FFTPACK_t(WK)
        call alloc_work_4_FFTPACK_t(Nsmp, Nfft, WK)
      end if
!
      end subroutine verify_wk_FFTPACK_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine CALYPSO_RFFTMF_t(Nsmp, Nstacksmp, M, Nfft, X, WK)
!
      use FFTPACK5_wrapper
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      type(working_FFTPACK), intent(inout) :: WK
!
!
      call CALYPSO_RFFTMF_SMP(Nsmp, Nstacksmp, M, Nfft, X,              &
     &    WK%X_FFTPACK5, WK%Mmax_smp, WK%lsave_FFTPACK,                 &
     &    WK%WSAVE_FFTPACK, WK%WORK_FFTPACK)
!
      end subroutine CALYPSO_RFFTMF_t
!
! ------------------------------------------------------------------
!
      subroutine CALYPSO_RFFTMB_t(Nsmp, Nstacksmp, M, Nfft, X, WK)
!
      use FFTPACK5_wrapper
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      type(working_FFTPACK), intent(inout) :: WK
!
!
      call CALYPSO_RFFTMB_SMP(Nsmp, Nstacksmp, M, Nfft, X,              &
     &    WK%X_FFTPACK5, WK%Mmax_smp, WK%lsave_FFTPACK,                 &
     &    WK%WSAVE_FFTPACK, WK%WORK_FFTPACK)
!
      end subroutine CALYPSO_RFFTMB_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_FFTPACK_t(Nsmp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nsmp, Nfft
      type(working_FFTPACK), intent(inout) :: WK
!
!
      WK%iflag_fft_comp = WK%Mmax_smp*Nfft
      allocate( WK%X_FFTPACK5(WK%iflag_fft_comp,Nsmp) )
      allocate( WK%WORK_FFTPACK(WK%iflag_fft_comp,Nsmp) )
      WK%WORK_FFTPACK = 0.0d0
!
      end subroutine alloc_work_4_FFTPACK_t
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_4_FFTPACK_t(nfft, WK)
!
      integer(kind = kint), intent(in) :: nfft
      type(working_FFTPACK), intent(inout) :: WK
!
!
      WK%iflag_fft_len = nfft
      WK%lsave_FFTPACK = Nfft                                           &
     &                + int ( log ( real(Nfft) ) / log(two) ) + ifour
      allocate(WK%WSAVE_FFTPACK(WK%lsave_FFTPACK) )
      WK%WSAVE_FFTPACK = 0.0d0
!
      end subroutine alloc_const_4_FFTPACK_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_FFTPACK_t(WK)
!
      type(working_FFTPACK), intent(inout) :: WK
!
!
      deallocate(WK%X_FFTPACK5, WK%WORK_FFTPACK)
      WK%iflag_fft_comp = 0
!
      end subroutine dealloc_work_4_FFTPACK_t
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_FFTPACK_t(WK)
!
      type(working_FFTPACK), intent(inout) :: WK
!
!
      deallocate( WK%WSAVE_FFTPACK )
      WK%iflag_fft_len = 0
!
      end subroutine dealloc_const_FFTPACK_t
!
! ------------------------------------------------------------------
!
      end module t_FFTPACK5_wrapper
