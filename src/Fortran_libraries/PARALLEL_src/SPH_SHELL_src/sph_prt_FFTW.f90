!>@file   sph_prt_FFTW.F90
!!@brief  module sph_prt_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_prt_FFTW(sph_rtp, comm_rtp,                     &
!!     &                         ncomp_bwd, ncomp_fwd, FFTW_f)
!!      subroutine verify_prt_FFTW(sph_rtp, comm_rtp,                   &
!!     &                           ncomp_bwd, ncomp_fwd, FFTW_f)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine prt_fwd_FFTW_to_send                                 &
!!     &         (sph_rtp, comm_rtp, ncomp_fwd, n_WS, X_rtp, WS, FFTW_f)
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
!!      subroutine prt_back_FFTW_from_recv                              &
!!     &         (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, X_rtp, FFTW_f)
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
!!@n @param Nstacksmp(0:np_smp)   End number for each SMP process
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!
      module sph_prt_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_fftw_parameters
      use m_elapsed_labels_SPH_TRNS
!
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_sph_comm_table_from_FFTW
      use t_sph_field_FFTW
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_prt_FFTW(sph_rtp, comm_rtp,                       &
     &                         ncomp_bwd, ncomp_fwd, FFTW_f)
!
      use set_comm_table_prt_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) :: howmany, ist_r, ist_c
      integer(kind = kint) :: ip
      integer(kind = 4) :: idist_r, idist_c
!
      integer, parameter :: IONE_4 = 1
      integer, parameter :: inembed = 0
      integer, parameter :: istride = 1
!
!
      call alloc_whole_FFTW_plan                                        &
     &  (sph_rtp%nidx_rtp(3), sph_rtp%istack_rtp_rt_smp,                &
     &   ncomp_bwd, ncomp_fwd, FFTW_f)
!
      do ip = 1, np_smp
        howmany = sph_rtp%istack_rtp_rt_smp(ip  )                       &
     &           - sph_rtp%istack_rtp_rt_smp(ip-1)
        FFTW_f%howmany_bwd = ncomp_bwd * howmany
        FFTW_f%howmany_fwd = ncomp_fwd * howmany
!
        idist_r = int(FFTW_f%Nfft_r)
        idist_c = int(FFTW_f%Nfft_c)
        ist_r = FFTW_f%Nfft_r * sph_rtp%istack_rtp_rt_smp(ip-1)
        ist_c = FFTW_f%Nfft_c * sph_rtp%istack_rtp_rt_smp(ip-1)
!
        call dfftw_plan_many_dft_r2c(FFTW_f%plan_fwd(ip),               &
     &      IONE_4, int(FFTW_f%Nfft_r), int(FFTW_f%howmany_fwd),        &
     &      FFTW_f%X(ist_r+1), inembed, istride, idist_r,               &
     &      FFTW_f%C(ist_c+1), inembed, istride, idist_c,               &
     &      FFTW_KEMO_EST)
        call dfftw_plan_many_dft_c2r(FFTW_f%plan_bwd(ip),               &
     &      IONE_4, int(FFTW_f%Nfft_r), int(FFTW_f%howmany_bwd),        &
     &      FFTW_f%C(ist_c+1), inembed, istride, idist_c,               &
     &      FFTW_f%X(ist_r+1), inembed, istride, idist_r,               &
     &      FFTW_KEMO_EST)
      end do
      FFTW_f%aNfft = one / dble(sph_rtp%nidx_rtp(3))
!
      call alloc_comm_table_sph_FFTW                                    &
     &   (comm_rtp%ntot_item_sr, FFTW_f%comm_sph_FFTW)
      call set_comm_item_prt_4_FFTW                                     &
     &   (sph_rtp%nnod_rtp, sph_rtp%istep_rtp,                          &
     &    sph_rtp%istack_rtp_rt_smp(np_smp),                            &
     &    comm_rtp%ntot_item_sr, comm_rtp%irev_sr,                      &
     &    FFTW_f%Nfft_c, FFTW_f%aNfft, FFTW_f%comm_sph_FFTW)
!
      end subroutine init_prt_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_prt_FFTW(sph_rtp, comm_rtp,                     &
     &                           ncomp_bwd, ncomp_fwd, FFTW_f)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
!
      if(allocated(FFTW_f%X) .eqv. .false.) then
        call init_prt_FFTW(sph_rtp, comm_rtp,                           &
     &                     ncomp_bwd, ncomp_fwd, FFTW_f)
        return
      end if
!
      if(size(FFTW_f%X) .ne. sph_rtp%nnod_rtp) then
        call finalize_sph_field_FFTW(FFTW_f)
        call init_prt_FFTW(sph_rtp, comm_rtp,                           &
     &                     ncomp_bwd, ncomp_fwd, FFTW_f)
      end if
!
      end subroutine verify_prt_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine prt_fwd_FFTW_to_send                                   &
     &         (sph_rtp, comm_rtp, ncomp_fwd, n_WS, X_rtp, WS, FFTW_f)
!
      use set_comm_table_prt_FFTW
      use copy_rtp_data_to_FFTPACK
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
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) :: ist_r, ist_c, ntot
      integer(kind = kint) :: ip
!
!
      ntot = sph_rtp%nnod_rtp * ncomp_fwd
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
      call copy_FFTPACK_to_prt_comp(ntot, X_rtp(1,1), FFTW_f%X(1))
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
!$omp parallel do  private(ip,ist_r,ist_c)
      do ip = 1, np_smp
        ist_r = FFTW_f%Nfft_r * ncomp_fwd                               &
     &         * sph_rtp%istack_rtp_rt_smp(ip-1)
        ist_c = FFTW_f%Nfft_c * ncomp_fwd                               &
     &         * sph_rtp%istack_rtp_rt_smp(ip-1)
        call dfftw_execute_dft_r2c(FFTW_f%plan_fwd(ip),                 &
     &      FFTW_f%X(ist_r+1), FFTW_f%C(ist_c+1))
      end do
!$omp end parallel do
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
!      call copy_prt_field_FFTW_to_send                                 &
!     &   (sph_rtp%nnod_rtp, comm_rtp%irev_sr,                          &
!     &    sph_rtp%istack_rtp_rt_smp(np_smp), ncomp_fwd,                &
!     &    FFTW_f%Nfft_c, FFTW_f%aNfft, FFTW_f%C(1), n_WS, WS)
      call copy_all_prt_FFTW_to_send                                    &
     &   (sph_rtp%istack_rtp_rt_smp(np_smp),  ncomp_fwd, FFTW_f%Nfft_c, &
     &    FFTW_f%C(1), FFTW_f%comm_sph_FFTW, n_WS, WS)
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
!
      end subroutine prt_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine prt_back_FFTW_from_recv                                &
     &         (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, X_rtp, FFTW_f)
!
      use set_comm_table_prt_FFTW
      use copy_rtp_data_to_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) :: ist_r, ist_c, ntot
      integer(kind = kint) :: ip
!
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
      call copy_FFTW_field_from_recv                                    &
     &   (sph_rtp%nnod_rtp, sph_rtp%istep_rtp,                          &
     &    sph_rtp%istack_rtp_rt_smp(np_smp), comm_rtp%irev_sr,          &
     &    ncomp_bwd, n_WR, WR, FFTW_f%Nfft_c, FFTW_f%C(1))
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
!$omp parallel do private(ip,ist_r,ist_c)
      do ip = 1, np_smp
        ist_r = FFTW_f%Nfft_r * ncomp_bwd                               &
     &         * sph_rtp%istack_rtp_rt_smp(ip-1)
        ist_c = FFTW_f%Nfft_c * ncomp_bwd                               &
     &         * sph_rtp%istack_rtp_rt_smp(ip-1)
        call dfftw_execute_dft_c2r(FFTW_f%plan_bwd(ip),                 &
     &      FFTW_f%C(ist_c+1), FFTW_f%X(ist_r+1))
      end do
!$omp end parallel do
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
      ntot = sph_rtp%nnod_rtp * ncomp_bwd
      call copy_FFTPACK_to_prt_comp(ntot, FFTW_f%X(1), X_rtp(1,1))
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
!
      end subroutine prt_back_FFTW_from_recv
!
! ------------------------------------------------------------------
!
      end module sph_prt_FFTW
