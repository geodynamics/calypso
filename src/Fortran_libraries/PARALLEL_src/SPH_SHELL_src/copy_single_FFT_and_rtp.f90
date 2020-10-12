!>@file   copy_single_FFT_and_rtp.f90
!!@brief  module copy_single_FFT_and_rtp
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2020
!
!>@brief  Data copy between rtp field and single FFT
!!
!!@verbatim
!!      subroutine sum_omp_elapsed_4_FFT(np_smp, time_omp, ave_time)
!!
!!      subroutine sel_copy_single_rtp_to_FFT                           &
!!     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r, X_rtp, X_fft)
!!      subroutine sel_copy_single_FFT_to_rtp                           &
!!     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r, X_fft, X_rtp)
!!
!!      subroutine sel_copy_comp_rtp_to_FFT                             &
!!     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r,              &
!!     &          ncomp_fwd, X_rtp, X_fft)
!!      subroutine sel_copy_comp_FFT_to_rtp                             &
!!     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r,              &
!!     &          ncomp_bwd, X_fft, X_rtp)
!!@endverbatim
!!
      module copy_single_FFT_and_rtp
!
      use m_precision
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sum_omp_elapsed_4_FFT(np_smp, time_omp, ave_time)
!
      integer(kind = kint), intent(in) :: np_smp
      real(kind = kreal), intent(inout) :: time_omp(np_smp,3)
      real(kind = kreal), intent(inout) :: ave_time(3)
!
      integer(kind = kint) :: ip
!
!
      do ip = 2, np_smp
        time_omp(1,1) = time_omp(1,1) + time_omp(ip,1)
        time_omp(1,2) = time_omp(1,2) + time_omp(ip,2)
        time_omp(1,3) = time_omp(1,3) + time_omp(ip,3)
      end do
      ave_time(1) = ave_time(1) + time_omp(1,1) / dble(np_smp)
      ave_time(2) = ave_time(2) + time_omp(1,2) / dble(np_smp)
      ave_time(3) = ave_time(3) + time_omp(1,3) / dble(np_smp)
!
      end subroutine sum_omp_elapsed_4_FFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_copy_single_rtp_to_FFT                             &
     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r, X_rtp, X_fft)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt, Nfft_r
      integer(kind = kint), intent(in) :: istep_phi
      real(kind = kreal), intent(in) :: X_rtp(nnod_rtp)
!
      real(kind = kreal), intent(inout) :: X_fft(Nfft_r)
!
      integer(kind = kint) :: ist, m
!
!
      if(istep_phi .eq. 1) then
        ist = (j-1) * Nfft_r
        X_fft(1:Nfft_r) = X_rtp(ist+1:ist+Nfft_r)
      else
        do m = 1, Nfft_r
          ist = j + (m-1) * nnod_rt
          X_fft(m) = X_rtp(ist)
        end do
      end if
!
      end subroutine sel_copy_single_rtp_to_FFT
!
! ------------------------------------------------------------------
!
      subroutine sel_copy_single_FFT_to_rtp                             &
     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r, X_fft, X_rtp)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt, Nfft_r
      integer(kind = kint), intent(in) :: istep_phi
      real(kind = kreal), intent(in) :: X_fft(Nfft_r)
      real(kind = kreal), intent(inout) :: X_rtp(nnod_rtp)
!
      integer(kind = kint) :: ist, m
!
!
      if(istep_phi .eq. 1) then
        ist = (j-1) * Nfft_r
        X_rtp(ist+1:ist+Nfft_r) = X_fft(1:Nfft_r)
      else
        do m = 1, Nfft_r
          ist = j + (m-1) * nnod_rt
          X_rtp(ist) = X_fft(m)
        end do
      end if
!
      end subroutine sel_copy_single_FFT_to_rtp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_copy_comp_rtp_to_FFT                               &
     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r,                &
     &          ncomp_fwd, X_rtp, X_fft)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt, Nfft_r
      integer(kind = kint), intent(in) :: istep_phi
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_rtp(nnod_rtp,ncomp_fwd)
!
      real(kind = kreal), intent(inout) :: X_fft(ncomp_fwd,Nfft_r)
!
      integer(kind = kint) :: ist, m
!
!
      if(istep_phi .eq. 1) then
        ist = (j-1) * Nfft_r
        do m = 1, Nfft_r
          X_fft(1:ncomp_fwd,m) = X_rtp(ist+m,1:ncomp_fwd)
        end do
      else
        do m = 1, Nfft_r
          ist = j + (m-1) * nnod_rt
          X_fft(1:ncomp_fwd,m) = X_rtp(ist,1:ncomp_fwd)
        end do
      end if
!
      end subroutine sel_copy_comp_rtp_to_FFT
!
! ------------------------------------------------------------------
!
      subroutine sel_copy_comp_FFT_to_rtp                               &
     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r,                &
     &          ncomp_bwd, X_fft, X_rtp)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt, Nfft_r
      integer(kind = kint), intent(in) :: istep_phi
      integer(kind = kint), intent(in) :: ncomp_bwd
      real(kind = kreal), intent(in) :: X_fft(ncomp_bwd,Nfft_r)
!
      real(kind = kreal), intent(inout) :: X_rtp(nnod_rtp,ncomp_bwd)
!
      integer(kind = kint) :: ist, m, nd
!
!
      if(istep_phi .eq. 1) then
        ist = (j-1) * Nfft_r
        do nd = 1, ncomp_bwd
          X_rtp(ist+1:ist+Nfft_r,nd) = X_fft(nd,1:Nfft_r)
        end do
      else
        do m = 1, Nfft_r
          ist = j + (m-1) * nnod_rt
          X_rtp(ist,1:ncomp_bwd) = X_fft(1:ncomp_bwd,m)
        end do
      end if
!
      end subroutine sel_copy_comp_FFT_to_rtp
!
! ------------------------------------------------------------------
!
      end module copy_single_FFT_and_rtp
