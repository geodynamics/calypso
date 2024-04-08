!>@file   sph_rtp_domain_FFTPACK5.f90
!!@brief  module sph_rtp_domain_FFTPACK5
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_rtp_domain_FFTPACK5                             &
!!     &         (sph_rtp, comm_rtp, fftpack_d)
!!      subroutine verify_rtp_domain_FFTPACK5                           &
!!     &         (sph_rtp, comm_rtp, fftpack_d)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine rtp_domain_RFFTMF_to_send                            &
!!     &         (sph_rtp, ncomp_fwd, n_WS, X_rtp, WS, fftpack_d)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
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
!!      subroutine rtp_domain_RFFTMB_from_recv(sph_rtp, comm_rtp,       &
!!     &          ncomp_bwd, n_WR, WR, X_rtp, fftpack_d)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
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
      module sph_rtp_domain_FFTPACK5
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_sph_comm_table_from_FFT
      use t_sph_domain_FFTPACK5
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_rtp_domain_FFTPACK5                               &
     &         (sph_rtp, comm_rtp, fftpack_d)
!
      use set_comm_table_rtp_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
!
      call init_sph_domain_FFTPACK5(sph_rtp, fftpack_d)
!
      call alloc_comm_table_sph_FFT                                     &
     &   (comm_rtp%ntot_item_sr, fftpack_d%comm_sph_FFTPACK)
      call set_comm_item_rtp_4_FFTPACK(sph_rtp%nnod_rtp,                &
     &    sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                  &
     &    comm_rtp%ntot_item_sr, comm_rtp%irev_sr,                      &
     &    fftpack_d%comm_sph_FFTPACK)
!
      end subroutine init_rtp_domain_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine verify_rtp_domain_FFTPACK5                             &
     &         (sph_rtp, comm_rtp, fftpack_d)
!
      use set_comm_table_rtp_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
!
      if(fftpack_d%iflag_fft_len .ne. sph_rtp%nidx_rtp(3)) then
        call dealloc_comm_table_sph_FFT(fftpack_d%comm_sph_FFTPACK)
        call alloc_comm_table_sph_FFT                                   &
     &     (comm_rtp%ntot_item_sr, fftpack_d%comm_sph_FFTPACK)
        call set_comm_item_rtp_4_FFTPACK(sph_rtp%nnod_rtp,              &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                &
     &      comm_rtp%ntot_item_sr, comm_rtp%irev_sr,                    &
     &      fftpack_d%comm_sph_FFTPACK)
      end if
!
      call verify_sph_domain_FFTPACK5(sph_rtp, fftpack_d)
!
      end subroutine verify_rtp_domain_FFTPACK5
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine rtp_domain_RFFTMF_to_send                              &
     &         (sph_rtp, ncomp_fwd, n_WS, X_rtp, WS, fftpack_d)
!
      use copy_rtp_data_to_FFTPACK
      use set_comm_table_rtp_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
      integer(kind = kint) :: ip, num, nsize, nd
      integer(kind = kint) :: ist_fft, ierr
!
!
      do nd = 1, ncomp_fwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
        call copy_FFTPACK_from_rtp_comp                                 &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, X_rtp(1,nd), fftpack_d%X)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
!$omp parallel do private(num,nsize,ist_fft)
        do ip = 1, np_smp
          num = sph_rtp%istack_rtp_rt_smp(ip)                           &
     &         - sph_rtp%istack_rtp_rt_smp(ip-1)
          nsize = num * sph_rtp%nidx_rtp(3)
          ist_fft = sph_rtp%istack_rtp_rt_smp(ip-1)                     &
     &             * sph_rtp%nidx_rtp(3)
          call RFFTMF(num, ione, sph_rtp%nidx_rtp(3), num,              &
     &        fftpack_d%X(ist_fft+1), nsize,                            &
     &        fftpack_d%WSV, fftpack_d%NSV,                             &
     &        fftpack_d%WK(ist_fft+1), nsize, ierr)
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
        call copy_1comp_rtp_FFT_to_send_smp                             &
     &     (nd, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                     &
     &      sph_rtp%istack_rtp_rt_smp, ncomp_fwd,                       &
     &      fftpack_d%X(1), fftpack_d%comm_sph_FFTPACK, n_WS, WS)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
      end do
!
      end subroutine rtp_domain_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine rtp_domain_RFFTMB_from_recv(sph_rtp, comm_rtp,         &
     &          ncomp_bwd, n_WR, WR, X_rtp, fftpack_d)
!
      use copy_rtp_data_to_FFTPACK
      use set_comm_table_rtp_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
      integer(kind = kint) :: ip, num, nsize, nd
      integer(kind = kint) :: ierr, ist_fft
!
!
      do nd = 1, ncomp_bwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
        call copy_FFTPACK_comp_from_recv(nd, sph_rtp%nnod_rtp,          &
     &      sph_rtp%nidx_rtp, sph_rtp%istep_rtp,                        &
     &      sph_rtp%istack_rtp_rt_smp, ncomp_bwd,                       &
     &      comm_rtp%irev_sr, n_WR, WR, fftpack_d%X(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
!$omp parallel do private(ip,num,nsize,ist_fft)
        do ip = 1, np_smp
          num = sph_rtp%istack_rtp_rt_smp(ip)                           &
     &         - sph_rtp%istack_rtp_rt_smp(ip-1)
          nsize = num * sph_rtp%nidx_rtp(3)
          ist_fft = sph_rtp%istack_rtp_rt_smp(ip-1)                     &
     &             * sph_rtp%nidx_rtp(3)
          call RFFTMB(num, ione, sph_rtp%nidx_rtp(3), num,              &
     &        fftpack_d%X(ist_fft+1), nsize,                            &
     &        fftpack_d%WSV, fftpack_d%NSV,                             &
     &        fftpack_d%WK(ist_fft+1), nsize, ierr)
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
        call copy_FFTPACK_to_rtp_comp(sph_rtp%nnod_rtp,                 &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                &
     &      fftpack_d%X, X_rtp(1,nd))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
      end do
!
      end subroutine rtp_domain_RFFTMB_from_recv
!
! ------------------------------------------------------------------
!
      end module sph_rtp_domain_FFTPACK5
