!>@file   t_sph_FFT_selector.F90
!!@brief  module t_sph_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine init_sph_FFT_select(id_rank, iflag_FFT_in,           &
!!     &         sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!      subroutine finalize_sph_FFT_select(sph_rtp, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!      subroutine verify_sph_FFT_select                                &
!!     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine fwd_FFT_select_to_send(sph_rtp, comm_rtp, ncomp_fwd, &
!!     &                                  n_WS, v_rtp, WS, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
!!
!!   a_{k} = \frac{2}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \cos (\frac{2\pi j k}{Nfft})]
!!   b_{k} = \frac{2}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \sin (\frac{2\pi j k}{Nfft})]
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \cos (\frac{2\pi j k}{Nfft})]
!!
!! ------------------------------------------------------------------
!!
!!      subroutine back_FFT_select_from_recv                            &
!!     &        (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
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
!!@n @param id_rank     Procdess ID
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!
      module t_sph_FFT_selector
!
      use m_precision
      use m_machine_parameter
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use m_FFT_selector
!
#ifdef FFTW3
      use t_sph_FFTW_selector
#endif
!
      use t_sph_FFTPACK_selector
!
      implicit none
!
!>      Structure for work area of FFTs
      type work_for_FFTs
!>        Integer flag for FFT type
        integer(kind = kint) :: iflag_FFT
!
!>        Structure to use FFTPACK
        type(works_sph_FFTPACK) :: WKs_FFTPACK
!
#ifdef FFTW3
!>        Structure to use FFTW
        type(works_sph_FFTW) :: WKs_FFTW
#endif
      end type work_for_FFTs
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_FFT_select(id_rank, iflag_FFT_in,             &
     &         sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs)
!
      use sph_rtp_FFTPACK_selector
      use sph_prt_FFTPACK_selector
!
#ifdef FFTW3
      use sph_prt_FFTW_selector
      use sph_rtp_FFTW_selector
#endif
!
      integer, intent(in) :: id_rank
      integer(kind = kint) :: iflag_FFT_in
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      logical :: flag_FFT
      integer(kind = kint) :: iflag_sph_FFT, iflag_size
!
!
      WK_FFTs%iflag_FFT = iflag_FFT_in
      iflag_size =    mod(WK_FFTs%iflag_FFT,10)
      iflag_sph_FFT = WK_FFTs%iflag_FFT - iflag_size
!
      flag_fft = .FALSE.
!
#ifdef FFTW3
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call sel_init_prt_FFTW_smp(id_rank, iflag_sph_FFT, iflag_size,  &
     &      sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                    &
     &      WK_FFTs%WKs_FFTW, flag_FFT)
      else
        call sel_init_rtp_FFTW_smp(id_rank, iflag_sph_FFT, iflag_size,  &
     &      sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                    &
     &      WK_FFTs%WKs_FFTW, flag_FFT)
      end if
      if(flag_fft) return
#endif
!
      if(iflag_sph_FFT .eq. iflag_FFTPACK) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_init_prt_FFTPACK(id_rank, iflag_size,                &
     &        sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                  &
     &        WK_FFTs%WKs_FFTPACK, flag_FFT)
        else
          call sel_init_rtp_FFTPACK(id_rank, iflag_size,                &
     &        sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                  &
     &        WK_FFTs%WKs_FFTPACK, flag_FFT)
        end if
!
      else
        if(id_rank .eq. 0) write(*,*) 'Use single FFTPACK'
        call init_sph_single_FFTPACK5(sph_rtp,                          &
     &      WK_FFTs%WKs_FFTPACK%sph_sgl_FFTPACK, flag_fft)
      end if
!
      end subroutine init_sph_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_FFT_select(sph_rtp, WK_FFTs)
!
#ifdef FFTW3
      use sph_prt_FFTW_selector
#endif
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      logical :: flag_FFT
      integer(kind = kint) :: iflag_sph_FFT, iflag_size
!
!
      iflag_size =    mod(WK_FFTs%iflag_FFT,10)
      iflag_sph_FFT = WK_FFTs%iflag_FFT - iflag_size
      flag_fft = .FALSE.
!
#ifdef FFTW3
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call sel_finalize_prt_FFTW(iflag_sph_FFT, iflag_size,           &
     &                             WK_FFTs%WKs_FFTW, flag_FFT)
      else
        call sel_finalize_rtp_FFTW(iflag_sph_FFT, iflag_size,           &
     &                             WK_FFTs%WKs_FFTW, flag_FFT)
      end if
      if(flag_fft) return
#endif
!
      if(iflag_sph_FFT .eq. iflag_FFTPACK) then
        call sel_finalize_sph_FFTPACK(iflag_size, WK_FFTs%WKs_FFTPACK,  &
     &                                flag_FFT)
!
      else
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTPACK'
        call finalize_sph_single_FFTPACK5                               &
     &     (WK_FFTs%WKs_FFTPACK%sph_sgl_FFTPACK, flag_fft)
      end if
!
      end subroutine finalize_sph_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_FFT_select                                  &
     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs)
!
      use sph_rtp_FFTPACK_selector
      use sph_prt_FFTPACK_selector
!
#ifdef FFTW3
      use sph_prt_FFTW_selector
      use sph_rtp_FFTW_selector
#endif
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      logical :: flag_FFT
      integer(kind = kint) :: iflag_sph_FFT, iflag_size
!
!
      iflag_size =    mod(WK_FFTs%iflag_FFT,10)
      iflag_sph_FFT = WK_FFTs%iflag_FFT - iflag_size
!
      flag_fft = .FALSE.
!
#ifdef FFTW3
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call sel_verify_prt_FFTW_smp(iflag_sph_FFT, iflag_size,         &
     &      sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                    &
     &      WK_FFTs%WKs_FFTW, flag_fft)
      else
        call sel_verify_rtp_FFTW_smp(iflag_sph_FFT, iflag_size,         &
     &      sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                    &
     &      WK_FFTs%WKs_FFTW, flag_fft)
      end if
      if(flag_fft) return
#endif
!
      if(iflag_sph_FFT .eq. iflag_FFTPACK) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_verify_prt_FFTPACK                                   &
     &       (iflag_size, sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,      &
     &        WK_FFTs%WKs_FFTPACK, flag_FFT)
        else
          call sel_verify_rtp_FFTPACK                                   &
     &       (iflag_size, sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,      &
     &        WK_FFTs%WKs_FFTPACK, flag_FFT)
        end if
!
      else
        if(iflag_debug .gt. 0) write(*,*) 'Use single FFTPACK'
        call verify_sph_single_FFTPACK5                                 &
     &     (sph_rtp, WK_FFTs%WKs_FFTPACK%sph_sgl_FFTPACK, flag_fft)
      end if
!
      end subroutine verify_sph_FFT_select
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine fwd_FFT_select_to_send(sph_rtp, comm_rtp, ncomp_fwd,   &
     &                                  n_WS, v_rtp, WS, WK_FFTs)
!
      use calypso_mpi
      use sph_rtp_FFTPACK_selector
      use sph_prt_FFTPACK_selector
!
#ifdef FFTW3
      use sph_prt_FFTW_selector
      use sph_rtp_FFTW_selector
#endif
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
      real (kind=kreal), intent(in):: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      logical :: flag_FFT
      integer(kind = kint) :: iflag_sph_FFT, iflag_size
!
!
      iflag_size =    mod(WK_FFTs%iflag_FFT,10)
      iflag_sph_FFT = WK_FFTs%iflag_FFT - iflag_size
!
      flag_fft = .FALSE.
!
#ifdef FFTW3
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call sel_prt_fwd_FFTW_to_send(iflag_sph_FFT, iflag_size,        &
     &      sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),      &
     &      WK_FFTs%WKs_FFTW, flag_FFT)
      else
        call sel_rtp_fwd_FFTW_to_send(iflag_sph_FFT, iflag_size,        &
     &      sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),      &
     &      WK_FFTs%WKs_FFTW, flag_FFT)
      end if
      if(flag_fft) return
#endif
!
      if(iflag_sph_FFT .eq. iflag_FFTPACK) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_prt_fwd_FFTPACK_to_send(iflag_size,                  &
     &        sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),    &
     &        WK_FFTs%WKs_FFTPACK, flag_FFT)
        else
          call sel_rtp_fwd_FFTPACK_to_send(iflag_size,                  &
     &        sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),    &
     &        WK_FFTs%WKs_FFTPACK, flag_FFT)
        end if
!
      else
        call sph_single_RFFTMF_to_send                                  &
     &     (sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),      &
     &      WK_FFTs%WKs_FFTPACK%sph_sgl_FFTPACK, flag_FFT)
      end if
!
      end subroutine fwd_FFT_select_to_send
!
! ------------------------------------------------------------------
!
      subroutine back_FFT_select_from_recv                              &
     &        (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp, WK_FFTs)
!
      use sph_rtp_FFTPACK_selector
      use sph_prt_FFTPACK_selector
!
#ifdef FFTW3
      use sph_prt_FFTW_selector
      use sph_rtp_FFTW_selector
#endif
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
      real (kind=kreal), intent(inout) :: WR(n_WR)
      real (kind=kreal), intent(inout)                                  &
     &                  :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      logical :: flag_FFT
      integer(kind = kint) :: iflag_sph_FFT, iflag_size
!
!
      iflag_size =    mod(WK_FFTs%iflag_FFT,10)
      iflag_sph_FFT = WK_FFTs%iflag_FFT - iflag_size
!
      flag_fft = .FALSE.
!
#ifdef FFTW3
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call sel_prt_bwd_FFTW_from_recv(iflag_sph_FFT, iflag_size,      &
     &      sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WK_FFTs%WKs_FFTW, flag_FFT)
      else
        call sel_rtp_bwd_FFTW_from_recv(iflag_sph_FFT, iflag_size,      &
     &      sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WK_FFTs%WKs_FFTW, flag_FFT)
      end if
      if(flag_fft) return
#endif
!
      if(iflag_sph_FFT .eq. iflag_FFTPACK) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_prt_bwd_FFTPACK_from_recv(iflag_size,                &
     &        sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WK_FFTs%WKs_FFTPACK, flag_FFT)
        else
          call sel_rtp_bwd_FFTPACK_from_recv(iflag_size,                &
     &        sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WK_FFTs%WKs_FFTPACK, flag_FFT)
        end if
!
      else
        call sph_single_RFFTMB_from_recv                                &
     &     (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WK_FFTs%WKs_FFTPACK%sph_sgl_FFTPACK, flag_FFT)
      end if
!
      end subroutine back_FFT_select_from_recv
!
! ------------------------------------------------------------------
!
      end module t_sph_FFT_selector
