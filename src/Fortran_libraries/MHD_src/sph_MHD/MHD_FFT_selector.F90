!>@file   MHD_FFT_selector.F90
!!@brief  module MHD_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine init_MHD_FFT_select                                  &
!!     &         (my_rank, ncomp, ncomp_fwd, ncomp_bwd)
!!      subroutine finalize_MHD_FFT_select
!!      subroutine verify_MHD_FFT_select(ncomp, ncomp_fwd, ncomp_bwd)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine fwd_MHD_FFT_sel_from_recv                            &
!!     &         (ncomp_fwd, n_WS, frc_rtp, WS)
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
!!      subroutine back_MHD_FFT_sel_from_recv                           &
!!     &         (ncomp_bwd, n_WR, WR, fld_rtp)
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
      module MHD_FFT_selector
!
      use m_precision
      use m_machine_parameter
      use m_sph_FFTPACK5
      use m_FFT_selector
!
#ifdef FFTW3
      use m_sph_single_FFTW
      use m_sph_field_FFTW
      use m_MHD_multi_FFTW
#endif
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_MHD_FFT_select                                    &
     &         (my_rank, ncomp, ncomp_fwd, ncomp_bwd)
!
      integer(kind = kint), intent(in) ::  my_rank
      integer(kind = kint), intent(in) ::  ncomp, ncomp_fwd, ncomp_bwd
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        if(my_rank .eq. 0) write(*,*) 'Use FFTW'
        call init_MHD_multi_FFTW(ncomp, ncomp_fwd, ncomp_bwd)
        if(my_rank .eq. 0) write(*,*) 'Use FFTW for each field'
        call init_sph_field_FFTW
        return
      else if(iflag_FFT .eq. iflag_FFTW_FIELD) then
        if(my_rank .eq. 0) write(*,*) 'Use FFTW for each field'
        call init_sph_field_FFTW
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(my_rank .eq. 0) write(*,*) 'Use single transform in FFTW'
        call init_sph_single_FFTW
        return
      end if
#endif
!
        if(my_rank .eq. 0) write(*,*) 'Use FFTPACK'
        call init_sph_FFTPACK5(ncomp)
!
      end subroutine init_MHD_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine finalize_MHD_FFT_select
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_MHD_multi_FFTW
        if(iflag_debug .gt. 0) write(*,*) 'Finalize fields FFTW'
        call finalize_sph_field_FFTW
        return
      else if(iflag_FFT .eq. iflag_FFTW_FIELD) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize fields FFTW'
        call finalize_sph_field_FFTW
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTW'
        call finalize_sph_single_FFTW
        return
      end if
#endif
!
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
        call finalize_sph_FFTPACK5
!
      end subroutine finalize_MHD_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine verify_MHD_FFT_select(ncomp, ncomp_fwd, ncomp_bwd)
!
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) ::  ncomp, ncomp_fwd, ncomp_bwd
      integer(kind = kint) :: Nstacksmp(0:np_smp)
!
!
      Nstacksmp(0:np_smp) = ncomp*irt_rtp_smp_stack(0:np_smp)
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW'
        call verify_MHD_multi_FFTW(ncomp, ncomp_fwd, ncomp_bwd)
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW for wach field'
        call verify_sph_field_FFTW
        return
      else if(iflag_FFT .eq. iflag_FFTW_FIELD) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW for wach field'
        call verify_sph_field_FFTW
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                                'Use single transforms in FFTW'
        call verify_sph_single_FFTW
        return
      end if
#endif
!
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK'
        call verify_sph_FFTPACK5(ncomp)
!
      end subroutine verify_MHD_FFT_select
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine fwd_MHD_FFT_sel_from_recv                              &
     &         (ncomp_fwd, n_WS, frc_rtp, WS)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
      real (kind=kreal), intent(inout):: frc_rtp(nnod_rtp,ncomp_fwd)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        call MHD_multi_fwd_FFTW_to_send                                 &
     &     (ncomp_fwd, n_WS, irev_sr_rtp, frc_rtp(1,1), WS(1))
        return
      else if(iflag_FFT .eq. iflag_FFTW_FIELD) then
        call sph_field_fwd_FFTW_to_send                                 &
     &     (ncomp_fwd, n_WS, irev_sr_rtp, frc_rtp(1,1), WS(1))
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call sph_single_fwd_FFTW_to_send                                &
     &     (ncomp_fwd, n_WS, irev_sr_rtp, frc_rtp(1,1), WS(1))
        return
      end if
#endif
!
        call sph_RFFTMF_to_send                                         &
     &     (ncomp_fwd, n_WS, irev_sr_rtp, frc_rtp(1,1), WS(1))
!
      end subroutine fwd_MHD_FFT_sel_from_recv
!
! ------------------------------------------------------------------
!
      subroutine back_MHD_FFT_sel_from_recv                             &
     &         (ncomp_bwd, n_WR, WR, fld_rtp)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
      real(kind=kreal), intent(inout):: WR(n_WR)
      real(kind=kreal), intent(inout):: fld_rtp(nnod_rtp,ncomp_bwd)
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        call MHD_multi_back_FFTW_from_recv                             &
     &    (ncomp_bwd, n_WR, irev_sr_rtp, WR(1), fld_rtp(1,1))
        return
      else if(iflag_FFT .eq. iflag_FFTW_FIELD) then
        call sph_field_back_FFTW_from_recv                              &
     &    (ncomp_bwd, n_WR, irev_sr_rtp, WR(1), fld_rtp(1,1))
        return
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call sph_single_back_FFTW_from_recv                             &
     &    (ncomp_bwd, n_WR, irev_sr_rtp, WR(1), fld_rtp(1,1))
        return
      end if
#endif
!
        call sph_RFFTMB_from_recv                                       &
     &     (ncomp_bwd, n_WR, irev_sr_rtp, WR, fld_rtp(1,1))
!
      end subroutine back_MHD_FFT_sel_from_recv
!
! ------------------------------------------------------------------
!
      end module MHD_FFT_selector
