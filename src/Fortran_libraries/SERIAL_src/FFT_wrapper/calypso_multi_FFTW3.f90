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
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFTW plans
!! ------------------------------------------------------------------
!!      subroutine init_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WK)
!!      subroutine finalize_FFTW_mul_type(Nsmp, WK)
!!      subroutine verify_wk_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WK)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) ::  Nfft
!!        type(working_mul_FFTW), intent(inout) :: WK
!!
!!      subroutine init_pout_FFTW_mul_smp                               &
!!     &         (Nsmp, Nstacksmp, Ncomp, Nfft, Nfft_c,                 &
!!     &          plan_forward_smp, plan_backward_smp, istack_smp_FFTW, &
!!     &          X_FFTW, C_FFTW)
!!        integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Nfft, Nfft_c, Ncomp
!!        integer(kind = fftw_plan), intent(inout)                      &
!!     &                          :: plan_forward_smp(Nsmp)
!!        integer(kind = fftw_plan), intent(inout)                      &
!!     &                          :: plan_backward_smp(Nsmp)
!!        integer(kind = kint), intent(inout) :: istack_smp_FFTW(0:Nsmp)
!!        real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &                          :: C_FFTW(Nfft_c,Ncomp)
!!
!! ------------------------------------------------------------------
!!   wrapper subroutine for clear FFTW plans
!!        CAUTION!!  dfftw_destroy_plan oftern makes SEGMENTAION FAULT!!
!! ------------------------------------------------------------------
!!      subroutine destroy_FFTW_mul_smp                                 &
!!     &         (Nsmp, plan_backward_smp, plan_backward)
!!        integer(kind = kint), intent(in) ::  Nsmp
!!        integer(kind = fftw_plan), intent(in) :: plan_forward(Nsmp)
!!        integer(kind = fftw_plan), intent(in) :: plan_backward(Nsmp)
!!
!! ------------------------------------------------------------------
!! wrapper subroutine for forward Fourier transform by FFTW3
!! ------------------------------------------------------------------
!!      subroutine multi_fwd_FFTW3_smp                                  &
!!     &         (plan_forward_smp, Nsmp, Nstacksmp,                    &
!!     &          Ncomp_c, Nfft_r, X_FFTW, Nfft_c, C_FFTW)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp_c, Nfft_c, Nfft_r
!!        integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
!!        real(kind = kreal), intent(in) :: X_FFTW(Nfft_r,Ncomp_c)
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &                                  :: C_FFTW(Nfft_c,Ncomp_c)
!! ------------------------------------------------------------------
!!   a_{k} = \frac{2}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \cos (\frac{2\pi j k}{Nfft})]
!!   b_{k} = \frac{2}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \sin (\frac{2\pi j k}{Nfft})]
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \cos (\frac{2\pi j k}{Nfft})]
!! ------------------------------------------------------------------
!!
!! ------------------------------------------------------------------
!! wrapper subroutine for backward Fourier transform by FFTW3
!! ------------------------------------------------------------------
!!      subroutine multi_bwd_FFTW3_smp                                  &
!!     &         (plan_backward_smp, Nsmp, Nstacksmp,                   &
!!     &          Ncomp_c, Nfft_c, C_FFTW, Nfft_r, X_FFTW)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp_c, Nfft_c, Nfft_r
!!        integer(kind = fftw_plan), intent(in):: plan_backward_smp(Nsmp)
!!        complex(kind = fftw_complex), intent(in)                      &
!!     &                                  :: C_FFTW(Nfft_c,Ncomp_c)
!!        real(kind = kreal), intent(inout) :: X_FFTW(Nfft_r,Ncomp_c)
!!
!! ------------------------------------------------------------------
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
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
!!
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
!
      integer, parameter, private :: IONE_4 = 1
      integer, parameter, private :: inembed = 0
      integer, parameter, private :: istride = 1
!
      private :: init_pout_FFTW_mul_smp, destroy_FFTW_mul_smp
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
      call init_pout_FFTW_mul_smp                                       &
     &   (Nsmp, Nstacksmp, Nstacksmp(Nsmp), Nfft, WK%Nfft_c,            &
     &    WK%plan_mul_fwd, WK%plan_mul_bwd,                             &
     &    WK%istack_FFTW, WK%Mmax_smp, WK%X_FFTW_mul, WK%C_FFTW_mul)
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
     &   (Nsmp, WK%plan_mul_fwd, WK%plan_mul_bwd)
      call dealloc_mul_FFTW_plan_t(WK)
!
      end subroutine finalize_FFTW_mul_type
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
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
      if(WK%iflag_fft_mul_len .ne. Nfft*Nstacksmp(Nsmp)) then
        call finalize_FFTW_mul_type(Nsmp, WK)
        call init_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WK)
      end if
!
      end subroutine verify_wk_FFTW_mul_type
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine init_pout_FFTW_mul_smp                                 &
     &         (Nsmp, Nstacksmp, Ncomp, Nfft, Nfft_c,                   &
     &          plan_forward_smp, plan_backward_smp,                    &
     &          istack_smp_FFTW, Mmax_smp, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Nfft, Nfft_c, Ncomp
!
      integer(kind = fftw_plan), intent(inout)                          &
     &                          :: plan_forward_smp(Nsmp)
      integer(kind = fftw_plan), intent(inout)                          &
     &                          :: plan_backward_smp(Nsmp)
      integer(kind = kint_gl), intent(inout) :: istack_smp_FFTW(0:Nsmp)
      integer(kind = kint_gl), intent(inout) :: Mmax_smp
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &                          :: C_FFTW(Nfft_c,Ncomp)
!
      integer(kind = kint) :: ip, ist
      integer(kind = 4) :: Nfft4, howmany, idist_r, idist_c
!
!
      istack_smp_FFTW(0:Nsmp) = Nstacksmp(0:Nsmp)
      Mmax_smp = 0
      do ip = 1, Nsmp
        Mmax_smp = max(Mmax_smp, (Nstacksmp(ip) - Nstacksmp(ip-1)))
      end do
!
      Nfft4 = int(Nfft,KIND(Nfft4))
      do ip = 1, Nsmp
        ist =     Nstacksmp(ip-1) + 1
        howmany = int(Nstacksmp(ip  ) - Nstacksmp(ip-1), KIND(howmany))
        idist_r = int(Nfft, KIND(idist_r))
        idist_c = int(Nfft, KIND(idist_c))/2 + 1
!
        call dfftw_plan_many_dft_r2c                                    &
     &     (plan_forward_smp(ip), IONE_4, Nfft4, howmany,               &
     &      X_FFTW(1,ist), inembed, istride, idist_r,                   &
     &      C_FFTW(1,ist), inembed, istride, idist_c, FFTW_KEMO_EST)
        call dfftw_plan_many_dft_c2r                                    &
     &     (plan_backward_smp(ip), IONE_4, Nfft4, howmany,              &
     &      C_FFTW(1,ist), inembed, istride, idist_c,                   &
     &      X_FFTW(1,ist), inembed, istride, idist_r, FFTW_KEMO_EST)
      end do
!
      end subroutine init_pout_FFTW_mul_smp
!
! ------------------------------------------------------------------
!
      subroutine destroy_FFTW_mul_smp                                   &
     &          (Nsmp, plan_forward, plan_backward)
!
      integer(kind = kint), intent(in) ::  Nsmp
!
      integer(kind = fftw_plan), intent(in) :: plan_forward(Nsmp)
      integer(kind = fftw_plan), intent(in) :: plan_backward(Nsmp)
!
      integer(kind = kint) :: j
!
!
      do j = 1, Nsmp
        call dfftw_destroy_plan(plan_forward(j))
        call dfftw_destroy_plan(plan_backward(j))
      end do
      call dfftw_cleanup
!
      end subroutine destroy_FFTW_mul_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_fwd_FFTW3_smp                                    &
     &         (plan_forward_smp, Nsmp, Nstacksmp,                      &
     &          Ncomp_c, Nfft_r, X_FFTW, Nfft_c, C_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp_c, Nfft_c, Nfft_r
      integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
      real(kind = kreal), intent(in) :: X_FFTW(Nfft_r,Ncomp_c)
!
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(Nfft_c,Ncomp_c)
!
      integer(kind = kint) :: ip
      integer(kind = kint_gl) :: ist, num
!
!
!$omp do private(ip,ist,num)
      do ip = 1, Nsmp
        num = Nstacksmp(ip) - Nstacksmp(ip-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ip-1) - Nstacksmp(0)
        call dfftw_execute_dft_r2c(plan_forward_smp(ip),                &
     &                             X_FFTW(1,ist+1), C_FFTW(1,ist+1))
      end do
!$omp end do nowait
!
      end subroutine multi_fwd_FFTW3_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_bwd_FFTW3_smp                                    &
     &         (plan_backward_smp, Nsmp, Nstacksmp,                     &
     &          Ncomp_c, Nfft_c, C_FFTW, Nfft_r, X_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp_c, Nfft_c, Nfft_r
      integer(kind = fftw_plan), intent(in) :: plan_backward_smp(Nsmp)
      complex(kind = fftw_complex), intent(in)                          &
     &                                  :: C_FFTW(Nfft_c,Ncomp_c)
!
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft_r,Ncomp_c)
!
      integer(kind = kint) :: ip
      integer(kind = kint_gl) :: ist, num
!
!
!$omp do private(ip,ist,num)
      do ip = 1, Nsmp
        num = Nstacksmp(ip) - Nstacksmp(ip-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ip-1) - Nstacksmp(0)
        call dfftw_execute_dft_c2r(plan_backward_smp(ip),               &
     &                             C_FFTW(1,ist+1), X_FFTW(1,ist+1))
      end do
!$omp end do nowait
!
      end subroutine multi_bwd_FFTW3_smp
!
! ------------------------------------------------------------------
!
      end module calypso_multi_FFTW3
