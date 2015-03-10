!>@file   FFTPACK5_wrapper.f90
!!@brief  module FFTPACK5_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!
!>@brief  FFTPACK5 wrapper
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_CALYPSO_FFTPACK(Nfft, lSAVE, WSAVE)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine CALYPSO_RFFTMF_SMP(Nsmp, Nstacksmp, M, Nfft,         &
!!     &          X, X_FFTPACK5, Mmax_smp, lSAVE, WSAVE, WORK)
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
!!      subroutine CALYPSO_RFFTMB_SMP(Nsmp, Nstacksmp, M, Nfft,         &
!!     &          X, X_FFTPACK5, Mmax_smp, lSAVE, WSAVE, WORK)
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
!!
!!@n @param Mmax_smp    Maximum number of component for each SMP process
!!@n @param X_FFTPACK5(Mmax_smp*Nfft,Nsmp) 
!!                 Data for multiple Fourier transform
!!@n @param lSAVE                     Size of work constant for FFTPACK
!!@n @param WSAVE(lSAVE)              Work constatnts for FFTPACK
!!@n @param WORK(Mmax_smp*Nfft,Nsmp)  Work area for FFTPACK
!
      module FFTPACK5_wrapper
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
      subroutine init_CALYPSO_FFTPACK(Nfft, lSAVE, WSAVE)
!
      integer(kind = kint), intent(in) :: lSAVE
      integer(kind = kint), intent(in) ::  Nfft
      real(kind = 8), intent(in) :: WSAVE(lSAVE)
!
      integer(kind = kint) :: ierr
!
!
      call RFFTMI(Nfft, WSAVE, lSAVE, ierr)
!
      end subroutine init_CALYPSO_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine CALYPSO_RFFTMF_SMP(Nsmp, Nstacksmp, M, Nfft,           &
     &          X, X_FFTPACK5, Mmax_smp, lSAVE, WSAVE, WORK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
      integer(kind = kint), intent(in) :: lSAVE, Mmax_smp
      real(kind = 8), intent(in) :: WSAVE(lSAVE)
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      real(kind = 8), intent(inout) :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK(Mmax_smp*Nfft,Nsmp)
!
!
      integer(kind = kint) ::  i, j, ismp, ist, num, inum, nsize
      integer(kind = kint) :: inod_s, inod_c, ierr
!
!
!$omp parallel do private(i,j,ist,num,inum,nsize,inod_s,inod_c)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        nsize = num*Nfft
!
        do i = 1, Nfft
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (i-1) * num
            X_FFTPACK5(inod_c,ismp) = X(j,i)
          end do
        end do
!
        call RFFTMF(num, ione, Nfft, num, X_FFTPACK5(1,ismp), nsize,    &
     &      WSAVE, lSAVE, WORK(1,ismp), nsize, ierr)
!
        do inum = 1, num
          j = ist + inum
          inod_s = inum + (Nfft-1) * num
          X(j,1) = X_FFTPACK5(inum,ismp)
          X(j,2) = X_FFTPACK5(inod_s,ismp)
        end do
        do i = 1, (Nfft+1)/2 - 1
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-1) * num
            inod_s = inum + (2*i  ) * num
            X(j,2*i+1) = X_FFTPACK5(inod_c,ismp)
            X(j,2*i+2) = X_FFTPACK5(inod_s,ismp)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine CALYPSO_RFFTMF_SMP
!
! ------------------------------------------------------------------
!
      subroutine CALYPSO_RFFTMB_SMP(Nsmp, Nstacksmp, M, Nfft,           &
     &          X, X_FFTPACK5, Mmax_smp, lSAVE, WSAVE, WORK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
      integer(kind = kint), intent(in) :: lSAVE, Mmax_smp
      real(kind = 8), intent(in) :: WSAVE(lSAVE)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      real(kind = 8), intent(inout) :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint) ::  i, j, ismp, ist, inum, num, nsize
      integer(kind = kint) :: inod_s, inod_c, ierr
!
!
!$omp parallel do private(i,j,ist,num,inum,nsize,inod_s,inod_c)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        nsize = num*Nfft
!
!   normalization
        do inum = 1, num
          j = ist + inum
          inod_s = inum + (Nfft-1) * num
          X_FFTPACK5(inum,ismp) =   X(j,1)
          X_FFTPACK5(inod_s,ismp) = X(j,2)
        end do
        do i = 1, (Nfft+1)/2 - 1
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-1) * num
            inod_s = inum + (2*i  ) * num
            X_FFTPACK5(inod_c,ismp) = X(j,2*i+1)
            X_FFTPACK5(inod_s,ismp) = X(j,2*i+2)
          end do
        end do
!
        call RFFTMB (num, ione, Nfft, num, X_FFTPACK5(1,ismp), nsize,   &
     &      WSAVE, lSAVE, WORK(1,ismp), nsize, ierr)
!
        do i = 1, Nfft
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (i-1) * num
            X(j,i) = X_FFTPACK5(inod_c,ismp)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine CALYPSO_RFFTMB_SMP
!
! ------------------------------------------------------------------
!
      end module FFTPACK5_wrapper
