!>@file   sph_rtp_FFTW_selector.F90
!!@brief  module sph_rtp_FFTW_selector
!!
!!@author H. Matsui
!!@date Programmed in MAy, 2026
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine sel_init_rtp_FFTW_smp                                &
!!     &         (id_rank, iflag_sph_FFT, iflag_size, sph_rtp, comm_rtp,&
!!     &          ncomp_bwd, ncomp_fwd, WKs_FFTW, flag_FFT)
!!      subroutine sel_verify_rtp_FFTW_smp(iflag_sph_FFT, iflag_size,   &
!!     &          sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,              &
!!     &          WKs_FFTW, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
!!        integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(works_sph_FFTW), intent(inout) :: WKs_FFTW
!!        logical, intent(inout) :: flag_FFT
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for FFTW with OpoenMP
!! ------------------------------------------------------------------
!!
!!      subroutine sel_rtp_fwd_FFTW_to_send(iflag_sph_FFT, iflag_size,  &
!!     &          sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp, WS,        &
!!     &          WKs_FFTW, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
!!        real(kind = kreal), intent(in)                                &
!!     &           :: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!!        real(kind = kreal), intent(inout) :: WS(n_WS)
!!        type(works_sph_FFTW), intent(inout) :: WKs_FFTW
!!        logical, intent(inout) :: flag_FFT
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
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
!!      subroutine sel_rtp_bwd_FFTW_from_recv(iflag_sph_FFT, iflag_size,&
!!     &          sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp,        &
!!     &          WKs_FFTW, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!!        type(works_sph_FFTW), intent(inout) :: WKs_FFTW
!!        logical, intent(inout) :: flag_FFT
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
      module sph_rtp_FFTW_selector
!
      use m_precision
      use m_machine_parameter
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use m_FFT_selector

      use t_sph_FFTW_selector
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_init_rtp_FFTW_smp                                  &
     &         (id_rank, iflag_sph_FFT, iflag_size, sph_rtp, comm_rtp,  &
     &          ncomp_bwd, ncomp_fwd, WKs_FFTW, flag_FFT)
!
      use sph_rtp_FFTW
      use sph_rtp_domain_FFTW
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(works_sph_FFTW), intent(inout) :: WKs_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        if(iflag_size .eq. iflag_once_fft) then
            if(id_rank .eq. 0) write(*,*) 'Use rtp FFTW'
            call init_rtp_FFTW_smp(sph_rtp, comm_rtp,                   &
     &          ncomp_bwd, ncomp_fwd, WKs_FFTW%sph_fld_FFTW, flag_fft)
        else if(iflag_size .eq. iflag_domain_once) then
          if(id_rank .eq. 0) write(*,*) 'Use rtp FFTW for domain'
            call init_rtp_field_FFTW(sph_rtp, comm_rtp,                 &
     &                               WKs_FFTW%sph_fld_FFTW, flag_fft)
        else if(iflag_size .eq. iflag_component_once) then
          if(id_rank .eq. 0) write(*,*) 'Use FFTW for all compontnent'
          call init_sph_component_FFTW(sph_rtp, ncomp_bwd, ncomp_fwd,   &
     &        WKs_FFTW%sph_comp_FFTW, flag_fft)
        else if(iflag_size .eq. iflag_single_fft) then
          if(id_rank .eq. 0) write(*,*) 'Use single transform in FFTW'
          call init_sph_single_FFTW(sph_rtp,                            &
     &                              WKs_FFTW%sph_sgl_FFTW, flag_fft)
        end if
!
#ifdef OMP_FFTW3
      else if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if     (iflag_size .eq. iflag_once_fft) then
          if(id_rank .eq. 0) write(*,*) 'Use at once rtp OpenMP FFTW'
          call init_rtp_OMP_FFTW(sph_rtp, comm_rtp, &
     &        ncomp_bwd, ncomp_fwd, WKs_FFTW%sph_OMP_FFTW, flag_fft)
        else if(iflag_size .eq. iflag_domain_once) then
          if(id_rank .eq. 0) write(*,*)                                 &
     &                       'Use rtp OpenMP FFTW for domain'
          call init_sph_domain_OMP_FFTW(sph_rtp, comm_rtp,              &
     &        WKs_FFTW%sph_domain_OMP_FFTW, flag_fft)
        end if
#endif
      end if
!
      end subroutine sel_init_rtp_FFTW_smp
!
! ------------------------------------------------------------------
!
      subroutine sel_verify_rtp_FFTW_smp(iflag_sph_FFT, iflag_size,     &
     &          sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                &
     &          WKs_FFTW, flag_FFT)
!
      use sph_rtp_FFTW
      use sph_rtp_domain_FFTW
!
      integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(works_sph_FFTW), intent(inout) :: WKs_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        if     (iflag_size .eq. iflag_once_fft) then
          if(iflag_debug .gt. 0) write(*,*) 'Use prt FFTW'
          call verify_rtp_FFTW_smp(sph_rtp, comm_rtp,                   &
     &        ncomp_bwd, ncomp_fwd, WKs_FFTW%sph_fld_FFTW, flag_fft)
        else if(iflag_size .eq. iflag_domain_once) then
          if(iflag_debug .gt. 0) write(*,*) 'Use rtp FFTW for field'
          call verify_rtp_field_FFTW(sph_rtp, comm_rtp,                 &
     &                               WKs_FFTW%sph_fld_FFTW, flag_fft)
        else if(iflag_size .eq. iflag_component_once) then
          if(iflag_debug .gt. 0) write(*,*) 'Use FFTW for all comp.'
          call verify_sph_component_FFTW(sph_rtp, ncomp_bwd, ncomp_fwd, &
     &        WKs_FFTW%sph_comp_FFTW, flag_fft)
        else if(iflag_size .eq. iflag_single_fft) then
          if(iflag_debug .gt. 0) write(*,*) 'Use single FFTW'
          call verify_sph_single_FFTW(sph_rtp,                          &
     &                                WKs_FFTW%sph_sgl_FFTW, flag_fft)
        end if
!
#ifdef OMP_FFTW3
      else if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if     (iflag_size .eq. iflag_once_fft) then
          if(iflag_debug .gt. 0) write(*,*) 'Use at once OpenMP FFTW'
          call verify_rtp_OMP_FFTW(sph_rtp, comm_rtp,                   &
     &        ncomp_bwd, ncomp_fwd, WKs_FFTW%sph_OMP_FFTW, flag_fft)
        else if(iflag_size .eq. iflag_domain_once) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                    'Use OpenMP FFTW for domain'
          call verify_sph_domain_OMP_FFTW(sph_rtp, comm_rtp,            &
     &        WKs_FFTW%sph_domain_OMP_FFTW, flag_fft)
        end if
#endif
      end if
!
      end subroutine sel_verify_rtp_FFTW_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_rtp_fwd_FFTW_to_send(iflag_sph_FFT, iflag_size,    &
     &          sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp, WS,          &
     &          WKs_FFTW, flag_FFT)
!
      use sph_rtp_FFTW
      use sph_rtp_domain_FFTW
!
      integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
      real(kind = kreal), intent(in)                                    &
     &           :: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      real(kind = kreal), intent(inout) :: WS(n_WS)
      type(works_sph_FFTW), intent(inout) :: WKs_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        if     (iflag_size .eq. iflag_once_fft) then
          call rtp_fwd_FFTW_smp_to_send                                 &
     &       (sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),              &
     &        WKs_FFTW%sph_fld_FFTW, flag_FFT)
        else if(iflag_size .eq. iflag_domain_once) then
          call rtp_field_fwd_FFTW_to_send                               &
     &       (sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),    &
     &        WKs_FFTW%sph_fld_FFTW, flag_FFT)
        else if(iflag_size .eq. iflag_component_once) then
          call sph_comp_fwd_FFTW_to_send                                &
     &       (sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),    &
     &        WKs_FFTW%sph_comp_FFTW, flag_FFT)
        else if(iflag_size .eq. iflag_single_fft) then
          call sph_single_fwd_FFTW_to_send                              &
     &       (sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),    &
     &        WKs_FFTW%sph_sgl_FFTW, flag_FFT)
        end if
!
#ifdef OMP_FFTW3
      else if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if     (iflag_size .eq. iflag_once_fft) then
          call rtp_fwd_OMP_FFTW_from_recv                               &
     &       (sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),              &
     &        WKs_FFTW%sph_OMP_FFTW, flag_FFT)
        else if(iflag_size .eq. iflag_domain_once) then
          call sph_domain_fwd_OFFTW_to_send                             &
     &       (sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),              &
     &        WKs_FFTW%sph_domain_OMP_FFTW, flag_FFT)
        end if
#endif
      end if
!
      end subroutine sel_rtp_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sel_rtp_bwd_FFTW_from_recv(iflag_sph_FFT, iflag_size,  &
     &          sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp,          &
     &          WKs_FFTW, flag_FFT)
!
      use sph_rtp_FFTW
      use sph_rtp_domain_FFTW
!
      integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(works_sph_FFTW), intent(inout) :: WKs_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        if     (iflag_size .eq. iflag_once_fft) then
          call rtp_back_FFTW_smp_from_recv                              &
     &       (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WKs_FFTW%sph_fld_FFTW, flag_FFT)
        else if(iflag_size .eq. iflag_domain_once) then
          call rtp_field_back_FFTW_from_recv                            &
     &       (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WKs_FFTW%sph_fld_FFTW, flag_FFT)
        else if(iflag_size .eq. iflag_component_once) then
          call sph_comp_back_FFTW_from_recv                             &
     &       (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WKs_FFTW%sph_comp_FFTW, flag_FFT)
        else if(iflag_size .eq. iflag_single_fft) then
          call sph_single_back_FFTW_from_recv                           &
     &       (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WKs_FFTW%sph_sgl_FFTW, flag_FFT)
        end if
!
#ifdef OMP_FFTW3
      else if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if(iflag_size .eq. iflag_once_fft) then
          call rtp_back_OMP_FFTW_from_recv                              &
     &       (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WKs_FFTW%sph_OMP_FFTW, flag_FFT)
        else if(iflag_size .eq. iflag_domain_once) then
          call sph_domain_back_OFFTW_from_recv                          &
     &       (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WKs_FFTW%sph_domain_OMP_FFTW, flag_FFT)
        end if
#endif
      end if
!
      end subroutine sel_rtp_bwd_FFTW_from_recv
!
! ------------------------------------------------------------------
!
      end module sph_rtp_FFTW_selector
