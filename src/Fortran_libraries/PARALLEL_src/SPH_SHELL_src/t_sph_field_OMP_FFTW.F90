!>@file   t_sph_field_OMP_FFTW.F90
!!@brief  module t_sph_field_OMP_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_domain_OMP_FFTW(sph_rtp, comm_rtp,          &
!!     &                                    OFFTW_d, flag_fft)
!!      subroutine finalize_sph_domain_OMP_FFTW(OFFTW_d, flag_fft)
!!      subroutine verify_sph_domain_OMP_FFTW(sph_rtp, comm_rtp,        &
!!     &                                      OFFTW_d, flag_fft)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!!        logical, intent(inout) :: flag_fft
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_domain_fwd_OFFTW_to_send(sph_rtp, ncomp_fwd,     &
!!     &          n_WS, X_rtp, WS, OFFTW_d, flag_FFT)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        integer(kind = kint), intent(in) :: ncomp_fwd
!!        real(kind = kreal), intent(in)                                &
!!     &        :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!!        integer(kind = kint), intent(in) :: n_WS
!!        real (kind=kreal), intent(inout):: WS(n_WS)
!!        type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!!        logical, intent(inout) :: flag_FFT
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
!!      subroutine sph_domain_back_OFFTW_from_recv(sph_rtp, comm_rtp,   &
!!     &          ncomp_bwd, n_WR, WR, X_rtp, OFFTW_d, flag_FFT)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd
!!        integer(kind = kint), intent(in) :: n_WR
!!        real (kind=kreal), intent(in):: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!     &        :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!!        type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!!        logical, intent(inout) :: flag_FFT
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
!!@n @param Nstacksmp(0:np_smp)   End number for each SMP process
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!
      module t_sph_field_OMP_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_fftw_parameters
      use m_OMP_FFTW3_counter
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_sph_comm_table_from_FFTW
!
      implicit none
!
!>      Structure to use FFTW with OpenMP
      type work_for_domain_OMP_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan) :: plan_bwd
!>        plan ID for forward transform
        integer(kind = fftw_plan) :: plan_fwd
!
        type(comm_tbl_from_FFTW) :: comm_FFTW
!
!>        length of FFT for real
        integer(kind = kint) :: Nfft_r
!>        length of FFT for complex
        integer(kind = kint) :: Nfft_c
!>        normalization parameter for FFTW (= 1 / Nfft)
        real(kind = kreal) :: aNfft
!
!>        real data for multiple Fourier transform
        real(kind = kreal), allocatable :: X(:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), allocatable :: C(:)
!
!>        temporal area for ordering
        real(kind = kreal), allocatable :: v_tmp(:)
      end type work_for_domain_OMP_FFTW
!
      private :: alloc_domain_OMP_FFTW_plan
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_domain_OMP_FFTW(sph_rtp, comm_rtp,            &
     &                                    OFFTW_d, flag_fft)
!
      use set_comm_table_rtp_OMP_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
      logical, intent(inout) :: flag_fft
!
      integer(kind = 4) :: howmany
!
      integer, parameter :: IONE_4 = 1
      integer, parameter :: inembed = 0
!
!
!
      call alloc_domain_OMP_FFTW_plan                                   &
     &   (sph_rtp%nidx_rtp(3), sph_rtp%istack_rtp_rt_smp, OFFTW_d)
!
      howmany = int(sph_rtp%istack_rtp_rt_smp(np_smp))
!
      call check_init_OMP_FFTW()
!
      call dfftw_plan_many_dft_r2c                                      &
     &   (OFFTW_d%plan_fwd, IONE_4, int(OFFTW_d%Nfft_r), howmany,       &
     &    OFFTW_d%X(1), inembed, howmany, IONE_4,                       &
     &    OFFTW_d%C(1), inembed, howmany, IONE_4,                       &
     &    FFTW_KEMO_EST)
      call dfftw_plan_many_dft_c2r                                      &
     &   (OFFTW_d%plan_bwd, IONE_4, int(OFFTW_d%Nfft_r), howmany,       &
     &    OFFTW_d%C(1), inembed, howmany, IONE_4,                       &
     &    OFFTW_d%X(1), inembed, howmany, IONE_4,                       &
     &    FFTW_KEMO_EST)
      OFFTW_d%aNfft = one / dble(sph_rtp%nidx_rtp(3))
!
      call alloc_comm_table_sph_FFTW                                    &
     &   (comm_rtp%ntot_item_sr, OFFTW_d%comm_FFTW)
      call set_comm_item_pout_OMP_FFTW                                  &
     &   (sph_rtp%nnod_rtp, sph_rtp%istack_rtp_rt_smp(np_smp),          &
     &    comm_rtp%irev_sr, OFFTW_d%Nfft_c, OFFTW_d%aNfft,              &
     &    OFFTW_d%comm_FFTW)
      flag_fft = .TRUE.
!
      end subroutine init_sph_domain_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_domain_OMP_FFTW(OFFTW_d, flag_fft)
!
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
      logical, intent(inout) :: flag_fft
!
!
      call dealloc_comm_table_sph_FFTW(OFFTW_d%comm_FFTW)
      call dfftw_destroy_plan(OFFTW_d%plan_fwd)
      call dfftw_destroy_plan(OFFTW_d%plan_bwd)
      call dfftw_cleanup
      call check_clean_OMP_FFTW()
!
      call dealloc_domain_OMP_FFTW_plan(OFFTW_d)
      flag_fft = .TRUE.
!
      end subroutine finalize_sph_domain_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_domain_OMP_FFTW(sph_rtp, comm_rtp,          &
     &                                      OFFTW_d, flag_fft)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
      logical, intent(inout) :: flag_fft
!
!
      if(allocated(OFFTW_d%X) .eqv. .false.) then
        call init_sph_domain_OMP_FFTW(sph_rtp, comm_rtp,                &
     &                                OFFTW_d, flag_fft)
        return
      end if
!
      if(size(OFFTW_d%X) .ne. sph_rtp%nnod_rtp) then
        call finalize_sph_domain_OMP_FFTW(OFFTW_d, flag_fft)
        call init_sph_domain_OMP_FFTW(sph_rtp, comm_rtp,                &
     &                                OFFTW_d, flag_fft)
      end if
      flag_fft = .TRUE.
!
      end subroutine verify_sph_domain_OMP_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_domain_fwd_OFFTW_to_send(sph_rtp, ncomp_fwd,       &
     &          n_WS, X_rtp, WS, OFFTW_d, flag_FFT)
!
      use copy_field_smp
      use set_comm_table_rtp_OMP_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &        :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
      logical, intent(inout) :: flag_FFT
!
      integer(kind = kint) ::  nd
!
!
      flag_FFT = .TRUE.
      do nd = 1, ncomp_fwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
        call copy_nod_scalar_smp(sph_rtp%nnod_rtp, X_rtp(1,nd),         &
     &                           OFFTW_d%X(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
        call dfftw_execute_dft_r2c(OFFTW_d%plan_fwd,                    &
      &                            OFFTW_d%X, OFFTW_d%C)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
!   normalization
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
          call copy_1comp_rtp_FFTW_to_send                             &
     &       (nd, sph_rtp%istack_rtp_rt_smp(np_smp), OFFTW_d%Nfft_c,   &
     &        ncomp_fwd, OFFTW_d%C, OFFTW_d%comm_FFTW, n_WS, WS)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
      end do
!
      end subroutine sph_domain_fwd_OFFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_domain_back_OFFTW_from_recv(sph_rtp, comm_rtp,     &
     &          ncomp_bwd, n_WR, WR, X_rtp, OFFTW_d, flag_FFT)
!
      use copy_field_smp
      use set_comm_table_rtp_OMP_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &        :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
      logical, intent(inout) :: flag_FFT
!
      integer(kind = kint) :: nd
!
!
      flag_FFT = .TRUE.
      do nd = 1, ncomp_bwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
        call set_OMP_FFTW_comp_from_recv                                &
     &     (nd, sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nnod_rtp,    &
     &      ncomp_bwd, n_WR, comm_rtp%irev_sr, WR, OFFTW_d%Nfft_c,      &
     &      OFFTW_d%C(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
        call dfftw_execute_dft_c2r(OFFTW_d%plan_bwd,                    &
     &                             OFFTW_d%C, OFFTW_d%X)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
        call copy_nod_scalar_smp(sph_rtp%nnod_rtp, OFFTW_d%X(1),        &
     &                           X_rtp(1,nd))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
      end do
!
      end subroutine sph_domain_back_OFFTW_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_domain_OMP_FFTW_plan                             &
     &         (Nfft, irt_rtp_smp_stack, OFFTW_d)
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!
      integer(kind = kint_gl) :: nnod_rt
!
!
      OFFTW_d%Nfft_r = Nfft
      OFFTW_d%Nfft_c = Nfft/2 + 1
!
      nnod_rt = irt_rtp_smp_stack(np_smp)
      allocate(OFFTW_d%X(OFFTW_d%Nfft_r*nnod_rt))
      allocate(OFFTW_d%C(OFFTW_d%Nfft_c*nnod_rt))
      OFFTW_d%X = 0.0d0
      OFFTW_d%C = 0.0d0
!
      end subroutine alloc_domain_OMP_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine dealloc_domain_OMP_FFTW_plan(OFFTW_d)
!
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!
!
      deallocate(OFFTW_d%X, OFFTW_d%C)
!
      end subroutine dealloc_domain_OMP_FFTW_plan
!
! ------------------------------------------------------------------
!
      end module t_sph_field_OMP_FFTW
