!>@file   t_sph_component_FFTW.F90
!!@brief  module t_sph_component_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_component_FFTW                              &
!!     &         (sph_rtp, ncomp_bwd, ncomp_fwd, FFTW_c)
!!      subroutine finalize_sph_component_FFTW(FFTW_c)
!!      subroutine verify_sph_component_FFTW                            &
!!     &         (sph_rtp, ncomp_bwd, ncomp_fwd, FFTW_c)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_comp_fwd_FFTW_to_send(sph_rtp, comm_rtp,         &
!!     &          ncomp_fwd, n_WS, X_rtp, WS, FFTW_c)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
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
!!      subroutine sph_comp_back_FFTW_from_recv(sph_rtp, comm_rtp,      &
!!     &          ncomp_bwd, n_WR, WR, X_rtp, FFTW_c)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
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
!!@n @param X(Ncomp,Nfft,np_smp)  Data for Fourier transform
!
      module t_sph_component_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_fftw_parameters
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      implicit none
!
!>      Structure to use SNGLE FFTW
      type work_for_comp_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan), allocatable :: plan_bwd(:)
!>        plan ID for forward transform
        integer(kind = fftw_plan), allocatable :: plan_fwd(:)
!
!>        number of backward FFT
        integer(kind = kint_4b) :: howmany_bwd
!>        number of forward FFT
        integer(kind = kint_4b) :: howmany_fwd
!>        length of FFT for real
        integer(kind = kint) :: Nfft_r
!>        length of FFT for complex
        integer(kind = kint) :: Nfft_c
!>        normalization parameter for FFTW (= 1 / Nfft)
        real(kind = kreal) :: aNfft
!>        real data for multiple Fourier transform
        real(kind = kreal), allocatable :: X(:,:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), allocatable :: C(:,:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_comp_FFTW
!
      private :: alloc_comp_FFTW_plan, dealloc_comp_FFTW_plan
      private :: copy_comp_FFTW_from_recv, copy_comp_FFTW_to_send
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_component_FFTW                                &
     &         (sph_rtp, ncomp_bwd, ncomp_fwd, FFTW_c)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
      integer(kind = kint) :: ip
      integer(kind = 4) :: Nfft4
!
      integer, parameter :: IONE_4 =  1
      integer, parameter :: inembed = 0
!
      call alloc_comp_FFTW_plan                                         &
     &   (sph_rtp%nidx_rtp(3), ncomp_bwd, ncomp_fwd, FFTW_c)
!
      Nfft4 = int(sph_rtp%nidx_rtp(3))
      do ip = 1, np_smp
        call dfftw_plan_many_dft_r2c(FFTW_c%plan_fwd(ip),               &
     &      IONE_4, int(FFTW_c%Nfft_r), FFTW_c%howmany_fwd,             &
     &      FFTW_c%X(1,ip), inembed, FFTW_c%howmany_fwd, IONE_4,        &
     &      FFTW_c%C(1,ip), inembed, FFTW_c%howmany_fwd, IONE_4,        &
     &      FFTW_KEMO_EST)
        call dfftw_plan_many_dft_c2r(FFTW_c%plan_bwd(ip),               &
     &      IONE_4, int(FFTW_c%Nfft_r), FFTW_c%howmany_bwd,             &
     &      FFTW_c%C(1,ip), inembed, FFTW_c%howmany_bwd, IONE_4,        &
     &      FFTW_c%X(1,ip), inembed, FFTW_c%howmany_bwd, IONE_4,        &
     &      FFTW_KEMO_EST)
      end do
      FFTW_c%aNfft = one / dble(sph_rtp%nidx_rtp(3))
!
      allocate(FFTW_c%t_omp(np_smp,0:3))
      FFTW_c%t_omp = 0.0d0
!
      end subroutine init_sph_component_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_component_FFTW(FFTW_c)
!
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
      integer(kind = kint) :: j
!
!
      do j = 1, np_smp
        call dfftw_destroy_plan(FFTW_c%plan_fwd(j))
        call dfftw_destroy_plan(FFTW_c%plan_bwd(j))
        call dfftw_cleanup
      end do
!
      call dealloc_comp_FFTW_plan(FFTW_c)
      deallocate(FFTW_c%t_omp)
!
      end subroutine finalize_sph_component_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_component_FFTW                              &
     &         (sph_rtp, ncomp_bwd, ncomp_fwd, FFTW_c)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
!
      if(allocated(FFTW_c%X) .eqv. .false.) then
        call init_sph_component_FFTW                                    &
     &     (sph_rtp, ncomp_bwd, ncomp_fwd, FFTW_c)
        return
      end if
!
      if(     int(ncomp_bwd) .ne. FFTW_c%howmany_bwd                    &
     &   .or. int(ncomp_fwd) .ne. FFTW_c%howmany_fwd                    &
     &   .or. sph_rtp%nidx_rtp(3) .ne. FFTW_c%Nfft_r) then
        call finalize_sph_component_FFTW(FFTW_c)
        call init_sph_component_FFTW                                    &
     &    (sph_rtp, ncomp_bwd, ncomp_fwd, FFTW_c)
      end if
!
      end subroutine verify_sph_component_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_comp_fwd_FFTW_to_send(sph_rtp, comm_rtp,           &
     &          ncomp_fwd, n_WS, X_rtp, WS, FFTW_c)
!
      use copy_single_FFT_and_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
      integer(kind = kint) ::  j, ip, ist, ied
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        FFTW_c%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(j,ip,ist,ied)
      do ip = 1, np_smp
        ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
        ied = sph_rtp%istack_rtp_rt_smp(ip) 
        do j = ist, ied
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          call sel_copy_comp_rtp_to_FFT                                 &
     &       (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),                &
     &        sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),   &
     &        ncomp_fwd, X_rtp(1,1), FFTW_c%X(1,ip))
          if(iflag_FFT_time) FFTW_c%t_omp(ip,1) = FFTW_c%t_omp(ip,1)    &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
!
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          call dfftw_execute_dft_r2c(FFTW_c%plan_fwd(ip),               &
     &        FFTW_c%X(1,ip), FFTW_c%C(1,ip))
          if(iflag_FFT_time) FFTW_c%t_omp(ip,2) = FFTW_c%t_omp(ip,2)    &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
!
!   normalization
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          call copy_comp_FFTW_to_send                                   &
     &       (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp, comm_rtp%irev_sr, &
     &        ncomp_fwd, FFTW_c%Nfft_c, FFTW_c%C(1,ip), FFTW_c%aNfft,   &
     &        n_WS, WS)
          if(iflag_FFT_time) FFTW_c%t_omp(ip,3)= FFTW_c%t_omp(ip,3)     &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        call sum_omp_elapsed_4_FFT(np_smp, FFTW_c%t_omp(1,1),           &
     &      elps1%elapsed(ist_elapsed_FFT+4))
      end if
!
      end subroutine sph_comp_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_comp_back_FFTW_from_recv(sph_rtp, comm_rtp,        &
     &          ncomp_bwd, n_WR, WR, X_rtp, FFTW_c)
!
      use copy_single_FFT_and_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
      integer(kind = kint) :: j, ip, ist, ied
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        FFTW_c%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(j,ip,ist,ied)
      do ip = 1, np_smp
        ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
        ied = sph_rtp%istack_rtp_rt_smp(ip)
        do j = ist, ied
!
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          call copy_comp_FFTW_from_recv                                 &
     &       (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp, comm_rtp%irev_sr, &
     &        ncomp_bwd, n_WR, WR, FFTW_c%Nfft_c, FFTW_c%C(1,ip))
          if(iflag_FFT_time) FFTW_c%t_omp(ip,1) = FFTW_c%t_omp(ip,1)    &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
!
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          call dfftw_execute_dft_c2r(FFTW_c%plan_bwd(ip),               &
     &        FFTW_c%C(1,ip), FFTW_c%X(1,ip))
          if(iflag_FFT_time) FFTW_c%t_omp(ip,2)= FFTW_c%t_omp(ip,2)     &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
!
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          call sel_copy_comp_FFT_to_rtp                                 &
     &       (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),                &
     &        sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),   &
     &        ncomp_bwd, FFTW_c%X(1,ip), X_rtp(1,1))
          if(iflag_FFT_time) FFTW_c%t_omp(ip,3) = FFTW_c%t_omp(ip,3)    &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        call sum_omp_elapsed_4_FFT(np_smp, FFTW_c%t_omp(1,1),           &
     &      elps1%elapsed(ist_elapsed_FFT+1))
      end if
!
      end subroutine sph_comp_back_FFTW_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_comp_FFTW_plan                                   &
     &         (Nfft, ncomp_bwd, ncomp_fwd, FFTW_c)
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      integer(kind = kint), intent(in) :: Nfft
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
      integer(kind = kint) :: Ncomp
!
      Ncomp = max(ncomp_bwd, ncomp_fwd)
      FFTW_c%howmany_bwd = int(ncomp_bwd)
      FFTW_c%howmany_fwd = int(ncomp_fwd)
      FFTW_c%Nfft_r = Nfft
      FFTW_c%Nfft_c = Nfft/2 + 1
!
      allocate(FFTW_c%plan_fwd(np_smp))
      allocate(FFTW_c%plan_bwd(np_smp))
!
      allocate( FFTW_c%X(Ncomp*FFTW_c%Nfft_r,np_smp) )
      allocate( FFTW_c%C(Ncomp*FFTW_c%Nfft_c,np_smp) )
      FFTW_c%X = 0.0d0
      FFTW_c%C = 0.0d0
!
      end subroutine alloc_comp_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine dealloc_comp_FFTW_plan(FFTW_c)
!
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
!
      deallocate(FFTW_c%plan_fwd, FFTW_c%plan_bwd)
      deallocate(FFTW_c%X, FFTW_c%C)
!
      end subroutine dealloc_comp_FFTW_plan
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_comp_FFTW_to_send(j, nnod_rtp, istep_rtp,         &
     &          irev_sr_rtp, ncomp_fwd, Nfft_c, C_fft, aNfft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      integer(kind = kint), intent(in) :: Nfft_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = fftw_complex), intent(in)                          &
     &              :: C_fft(ncomp_fwd*Nfft_c)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: m, ms, j0_rtp, ic_rtp, is_rtp
      integer(kind = kint) :: ic_send, is_send
!
!
      j0_rtp = 1 + (j-1) * istep_rtp(1)
      ic_send = ncomp_fwd * (irev_sr_rtp(j0_rtp) - 1)
      WS(ic_send+1:ic_send+ncomp_fwd)                                   &
     &          = aNfft * real(C_fft(1:ncomp_fwd))
      do m = 2, Nfft_c-1
        ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
        is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
        ic_send = ncomp_fwd * (irev_sr_rtp(ic_rtp) - 1)
        is_send = ncomp_fwd * (irev_sr_rtp(is_rtp) - 1)
        ms =      ncomp_fwd * (m-1)
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &          = two*aNfft * real(C_fft(ms+1:ms+ncomp_fwd))
        WS(is_send+1:is_send+ncomp_fwd)                                 &
     &          = two*aNfft * real(C_fft(ms+1:ms+ncomp_fwd)*iu)
      end do 
      ic_rtp = j0_rtp + istep_rtp(3)
      ic_send = ncomp_fwd * (irev_sr_rtp(ic_rtp)-1)
      ms =      ncomp_fwd * (Nfft_c-1)
      WS(ic_send+1:ic_send+ncomp_fwd)                                   &
     &        = two*aNfft * real(C_fft(ms+1:ms+ncomp_fwd))
!
      end subroutine copy_comp_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_comp_FFTW_from_recv                               &
     &         (j, nnod_rtp, istep_rtp, irev_sr_rtp,                    &
     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: Nfft_c
      complex(kind = fftw_complex), intent(inout)                       &
     &                  :: C_fft(ncomp_bwd*Nfft_c)
!
      integer(kind = kint) :: m, ms, j0_rtp, ic_rtp, is_rtp
      integer(kind = kint) :: ic_recv, is_recv
!
!
      j0_rtp = 1 + (j-1) * istep_rtp(1)
      ic_recv = ncomp_bwd * (irev_sr_rtp(j0_rtp) - 1)
      C_fft(1:ncomp_bwd)                                                &
     &        = cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd), zero, kind(0d0))
      do m = 2, Nfft_c-1
        ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
        is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
        ic_recv = ncomp_bwd * (irev_sr_rtp(ic_rtp) - 1)
        is_recv = ncomp_bwd * (irev_sr_rtp(is_rtp) - 1)
        ms =      ncomp_bwd * (m - 1)
        C_fft(ms+1:ms+ncomp_bwd)                                        &
     &        = half*cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd),             &
     &                    -WR(is_recv+1:is_recv+ncomp_bwd),kind(0d0))
      end do
      ic_rtp = j0_rtp + istep_rtp(3)
      ic_recv = ncomp_bwd * (irev_sr_rtp(ic_rtp) - 1)
      ms =      ncomp_bwd * (Nfft_c - 1)
      C_fft(ms+1:ms+ncomp_bwd)                                          &
     &  = half * cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd),zero, kind(0d0))
!
      end subroutine copy_comp_FFTW_from_recv
!
! ------------------------------------------------------------------
!
      end module t_sph_component_FFTW
