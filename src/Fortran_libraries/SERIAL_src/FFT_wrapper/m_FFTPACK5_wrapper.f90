!>@file   m_FFTPACK5_wrapper.f90
!!@brief  module m_FFTPACK5_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
!!      subroutine finalize_4_FFTPACK
!!      subroutine verify_work_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine CALYPSO_RFFTMF(Nsmp, Nstacksmp, M, Nfft, X)
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
!!      subroutine CALYPSO_RFFTMB(Nsmp, Nstacksmp, M, Nfft, X)
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
!
      module m_FFTPACK5_wrapper
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Maximum nuber of components for each SMP process
      integer(kind = kint) :: Mmax_smp
!>      Data for multiple Fourier transform
      real(kind = 8), allocatable :: X_FFTPACK5(:,:)
!
!>      Size of work constant for FFTPACK
      integer(kind = kint) :: lsave_FFTPACK
!>      Work constatnts for FFTPACK
      real(kind = 8), allocatable :: WSAVE_FFTPACK(:)
!>      Work area for FFTPACK
      real(kind = 8), allocatable :: WORK_FFTPACK(:,:)
!>      flag for length of Fourier transform
      integer(kind = kint) :: iflag_fft_len =  -1
!>      flag for number of components for Fourier transform
      integer(kind = kint) :: iflag_fft_comp = -1
!
      private :: X_FFTPACK5, Mmax_smp
      private :: lsave_FFTPACK, WSAVE_FFTPACK, WORK_FFTPACK
      private :: iflag_fft_len, iflag_fft_comp
!
      private :: allocate_work_4_FFTPACK, allocate_const_4_FFTPACK
      private :: deallocate_work_4_FFTPACK, deallocate_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
!
      use FFTPACK5_wrapper
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      integer(kind = kint) :: ip
!
!
      Mmax_smp = Nstacksmp(1)
      do ip = 1, Nsmp
        Mmax_smp = max(Mmax_smp, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      call allocate_const_4_FFTPACK(Nfft)
      call init_CALYPSO_FFTPACK(Nfft, lsave_FFTPACK, WSAVE_FFTPACK)
!
      call allocate_work_4_FFTPACK(Nsmp, Nfft)
!
      end subroutine init_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine finalize_4_FFTPACK
!
      use FFTPACK5_wrapper
!
!
      call deallocate_const_4_FFTPACK
      call deallocate_work_4_FFTPACK
!
      end subroutine finalize_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine verify_work_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
!
      use FFTPACK5_wrapper
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      integer(kind = kint) :: ip
!
!
      Mmax_smp = Nstacksmp(1)
      do ip = 1, Nsmp
        Mmax_smp = max(Mmax_smp, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      if( iflag_fft_len .ne. Nfft) then
!
        if( iflag_fft_len .lt. 0) then
          call allocate_const_4_FFTPACK(Nfft)
        else if( Nfft .gt. iflag_fft_comp ) then
          call deallocate_const_4_FFTPACK
          call allocate_const_4_FFTPACK(Nfft)
        end if
!
        call init_CALYPSO_FFTPACK(Nfft, lsave_FFTPACK, WSAVE_FFTPACK)
      end if
!
      if( iflag_fft_comp .lt. 0) then
        call allocate_work_4_FFTPACK(Nsmp, Nfft)
      else if( (Mmax_smp*Nfft) .gt. iflag_fft_comp ) then
        call deallocate_work_4_FFTPACK
        call allocate_work_4_FFTPACK(Nsmp, Nfft)
      end if
!
      end subroutine verify_work_4_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine CALYPSO_RFFTMF(Nsmp, Nstacksmp, M, Nfft, X)
!
      use FFTPACK5_wrapper
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
!
!
      call CALYPSO_RFFTMF_SMP(Nsmp, Nstacksmp, M, Nfft, X, X_FFTPACK5,  &
     &    Mmax_smp, lsave_FFTPACK, WSAVE_FFTPACK, WORK_FFTPACK)
!
      end subroutine CALYPSO_RFFTMF
!
! ------------------------------------------------------------------
!
      subroutine CALYPSO_RFFTMB(Nsmp, Nstacksmp, M, Nfft, X)
!
      use FFTPACK5_wrapper
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
!
      call CALYPSO_RFFTMB_SMP(Nsmp, Nstacksmp, M, Nfft, X, X_FFTPACK5,  &
     &    Mmax_smp, lsave_FFTPACK, WSAVE_FFTPACK, WORK_FFTPACK)
!
      end subroutine CALYPSO_RFFTMB
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine allocate_work_4_FFTPACK(Nsmp, Nfft)
!
      integer(kind = kint), intent(in) :: Nsmp, Nfft
!
!
      iflag_fft_comp = Mmax_smp*Nfft
      allocate( X_FFTPACK5(iflag_fft_comp,Nsmp) )
      allocate( WORK_FFTPACK(iflag_fft_comp,Nsmp) )
      WORK_FFTPACK = 0.0d0
!
      end subroutine allocate_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine allocate_const_4_FFTPACK(nfft)
!
      integer(kind = kint), intent(in) :: nfft
!
      iflag_fft_len = nfft
      lsave_FFTPACK = Nfft                                              &
     &                + int ( log ( real(Nfft) ) / log(two) ) + ifour
      allocate(WSAVE_FFTPACK(lsave_FFTPACK) )
      WSAVE_FFTPACK = 0.0d0
!
      end subroutine allocate_const_4_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine deallocate_work_4_FFTPACK
!
      deallocate(X_FFTPACK5, WORK_FFTPACK)
      iflag_fft_comp = 0
!
      end subroutine deallocate_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine deallocate_const_4_FFTPACK
!
      deallocate( WSAVE_FFTPACK )
      iflag_fft_len = 0
!
      end subroutine deallocate_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      end module m_FFTPACK5_wrapper
