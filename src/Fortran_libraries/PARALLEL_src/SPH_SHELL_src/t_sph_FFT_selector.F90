!>@file   t_sph_FFT_selector.F90
!!@brief  module t_sph_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine init_sph_FFT_select(my_rank, sph_rtp, ncomp, WK_FFTs)
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!      subroutine finalize_sph_FFT_select(WK_FFTs)
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!      subroutine verify_sph_FFT_select(sph_rtp, ncomp, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine fwd_FFT_select_to_send                               &
!!     &         (sph_rtp, comm_rtp, ncomp, n_WS, v_rtp, WS, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
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
!!      subroutine back_FFT_select_from_recv                            &
!!     &         (sph_rtp, comm_rtp, ncomp, n_WR, WR, v_rtp, WK_FFTs)
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
!!@n @param my_rank     Procdess ID
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
      use m_FFT_selector
      use t_sph_FFTPACK5
!
#ifdef FFTW3
      use t_sph_single_FFTW
      use t_sph_field_FFTW
#endif
!
      implicit none
!
!>      Structure for work area of FFTs
      type work_for_FFTs
!>        Structure to use FFTPACK
        type(work_for_fftpack) :: sph_FFTPACK
!
#ifdef FFTW3
!>        Structure to use FFTW
        type(work_for_sgl_FFTW) :: sph_fld_FFTW
!>        Structure to use FFTW for each component
        type(work_for_sgl_FFTW) :: sph_sgl_FFTW
#endif
      end type work_for_FFTs
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_FFT_select(my_rank, sph_rtp, ncomp, WK_FFTs)
!
      use t_spheric_rtp_data
!
      integer(kind = kint), intent(in) ::  my_rank, ncomp
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW                                      &
     &   .or. iflag_FFT .eq. iflag_FFTW_FIELD) then
        if(my_rank .eq. 0) write(*,*) 'Use FFTW'
        call init_sph_field_FFTW                                        &
     &     (sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                &
     &      WK_FFTs%sph_fld_FFTW)
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(my_rank .eq. 0) write(*,*) 'Use single transform in FFTW'
        call init_sph_single_FFTW                                       &
     &     (sph_rtp%nidx_rtp, WK_FFTs%sph_sgl_FFTW)
        return
      end if
#endif
!
        if(my_rank .eq. 0) write(*,*) 'Use FFTPACK'
        call init_sph_FFTPACK5                                          &
     &     (sph_rtp%nidx_rtp, sph_rtp%maxirt_rtp_smp, ncomp,            &
     &      WK_FFTs%sph_FFTPACK)
!
      end subroutine init_sph_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_FFT_select(WK_FFTs)
!
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW                                      &
     &   .or. iflag_FFT .eq. iflag_FFTW_FIELD) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_sph_field_FFTW(WK_FFTs%sph_fld_FFTW)
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTW'
        call finalize_sph_single_FFTW(WK_FFTs%sph_sgl_FFTW)
        return
      end if
#endif
!
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
        call finalize_sph_FFTPACK5(WK_FFTs%sph_FFTPACK)
!
      end subroutine finalize_sph_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_FFT_select(sph_rtp, ncomp, WK_FFTs)
!
      use t_spheric_rtp_data
!
      integer(kind = kint), intent(in) ::  ncomp
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
!
!
#ifdef FFTW3
      if(     iflag_FFT .eq. iflag_FFTW                                 &
     &   .or. iflag_FFT .eq. iflag_FFTW_FIELD) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW'
        call verify_sph_field_FFTW                                      &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, WK_FFTs%sph_fld_FFTW)
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use single FFTW'
        call verify_sph_single_FFTW                                     &
     &     (sph_rtp%nidx_rtp, WK_FFTs%sph_sgl_FFTW)
        return
      end if
#endif
!
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK'
        call verify_sph_FFTPACK5                                        &
     &     (sph_rtp%nidx_rtp, sph_rtp%maxirt_rtp_smp, ncomp,            &
     &      WK_FFTs%sph_FFTPACK)
!
      end subroutine verify_sph_FFT_select
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine fwd_FFT_select_to_send                                 &
     &         (sph_rtp, comm_rtp, ncomp, n_WS, v_rtp, WS, WK_FFTs)
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp, n_WS
      real (kind=kreal), intent(in):: v_rtp(sph_rtp%nnod_rtp,ncomp)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
#ifdef FFTW3
      if(     iflag_FFT .eq. iflag_FFTW                                 &
     &   .or. iflag_FFT .eq. iflag_FFTW_FIELD) then
        call sph_field_fwd_FFTW_to_send                                 &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, ncomp, n_WS, comm_rtp%irev_sr,   &
     &      v_rtp(1,1), WS(1), WK_FFTs%sph_fld_FFTW)
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call sph_single_fwd_FFTW_to_send                                &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, ncomp, n_WS, comm_rtp%irev_sr,   &
     &      v_rtp(1,1), WS(1), WK_FFTs%sph_sgl_FFTW)
        return
      end if
#endif
!
        call sph_RFFTMF_to_send                                         &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, ncomp, n_WS, comm_rtp%irev_sr,   &
     &      v_rtp(1,1), WS(1), WK_FFTs%sph_FFTPACK)
!
      end subroutine fwd_FFT_select_to_send
!
! ------------------------------------------------------------------
!
      subroutine back_FFT_select_from_recv                              &
     &         (sph_rtp, comm_rtp, ncomp, n_WR, WR, v_rtp, WK_FFTs)
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp, n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: v_rtp(sph_rtp%nnod_rtp,ncomp)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
#ifdef FFTW3
      if(     iflag_FFT .eq. iflag_FFTW                                 &
     &   .or. iflag_FFT .eq. iflag_FFTW_FIELD) then
        call sph_field_back_FFTW_from_recv                              &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, ncomp, n_WR, comm_rtp%irev_sr,   &
     &      WR(1), v_rtp(1,1), WK_FFTs%sph_fld_FFTW)
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call sph_single_back_FFTW_from_recv                             &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, ncomp, n_WR, comm_rtp%irev_sr,   &
     &      WR(1), v_rtp(1,1), WK_FFTs%sph_sgl_FFTW)
        return
      end if
#endif
!
        call sph_RFFTMB_from_recv                                       &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, ncomp, n_WR, comm_rtp%irev_sr,   &
     &      WR, v_rtp(1,1), WK_FFTs%sph_FFTPACK)
!
      end subroutine back_FFT_select_from_recv
!
! ------------------------------------------------------------------
!
      end module t_sph_FFT_selector
