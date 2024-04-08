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
      use t_sph_component_FFTPACK5
      use t_sph_domain_FFTPACK5
      use t_sph_single_FFTPACK5
      use sph_rtp_domain_FFTPACK5
      use sph_prt_domain_FFTPACK5
      use sph_rtp_FFTPACK5
      use sph_prt_FFTPACK5
!
#ifdef FFTW3
      use t_sph_single_FFTW
      use t_sph_field_FFTW
      use t_sph_component_FFTW
      use sph_rtp_domain_FFTW
      use sph_prt_domain_FFTW
      use sph_rtp_FFTW
      use sph_prt_FFTW
#endif
!
      implicit none
!
!>      Structure for work area of FFTs
      type work_for_FFTs
!>        Integer flag for FFT type
        integer(kind = kint) :: iflag_FFT
!
!>        Structure to use FFTPACK
        type(work_for_fftpack) :: sph_FFTPACK
!>        Structure to use single FFTPACK
        type(work_for_sgl_fftpack) :: sph_sgl_FFTPACK
!>        Structure to use single FFTPACK
        type(work_for_comp_fftpack) :: sph_comp_FFTPACK
!>        Structure to use single FFTPACK
        type(work_for_domain_fftpack) :: sph_domain_FFTPACK
!
#ifdef FFTW3
!>        Structure to use FFTW
        type(work_for_field_FFTW) :: sph_fld_FFTW
!>        Structure to use FFTW for each component
        type(work_for_sgl_FFTW) :: sph_sgl_FFTW
!>        Structure to use FFTW for each component
        type(work_for_comp_FFTW) :: sph_comp_FFTW
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
      use t_spheric_rtp_data
!
      integer, intent(in) :: id_rank
      integer(kind = kint) :: iflag_FFT_in
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
      WK_FFTs%iflag_FFT = iflag_FFT_in
      if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_SINGLE) then
        if(id_rank .eq. 0) write(*,*) 'Use single FFTPACK'
        call init_sph_single_FFTPACK5(sph_rtp, WK_FFTs%sph_sgl_FFTPACK)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_COMPONENT) then
        if(id_rank .eq. 0) write(*,*) 'Use FFTPACK for all comp'
        call init_sph_comp_FFTPACK5                                     &
     &     (sph_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs%sph_comp_FFTPACK)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(id_rank .eq. 0) write(*,*) 'Use prt FFTPACK for domaikn'
          call init_prt_domain_FFTPACK5(sph_rtp, comm_rtp,              &
     &                                  WK_FFTs%sph_domain_FFTPACK)
        else
          if(id_rank .eq. 0) write(*,*) 'Use rtp FFTPACK for domaikn'
          call init_rtp_domain_FFTPACK5(sph_rtp, comm_rtp,              &
     &                                  WK_FFTs%sph_domain_FFTPACK)
        end if
!
#ifdef FFTW3
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(id_rank .eq. 0) write(*,*) 'Use prt FFTW'
          call init_prt_FFTW(sph_rtp, comm_rtp,                         &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_fld_FFTW)
        else
          if(id_rank .eq. 0) write(*,*) 'Use rtp FFTW'
          call init_rtp_FFTW(sph_rtp, comm_rtp,                         &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_fld_FFTW)
        end if
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(id_rank .eq. 0) write(*,*) 'Use prt FFTW for domain'
          call init_prt_field_FFTW                                      &
     &       (sph_rtp, comm_rtp, WK_FFTs%sph_fld_FFTW)
        else
          if(id_rank .eq. 0) write(*,*) 'Use rtp FFTW for domain'
          call init_rtp_field_FFTW                                      &
     &       (sph_rtp, comm_rtp, WK_FFTs%sph_fld_FFTW)
        end if
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(id_rank .eq. 0) write(*,*) 'Use single transform in FFTW'
        call init_sph_single_FFTW(sph_rtp, WK_FFTs%sph_sgl_FFTW)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_COMPONENT) then
        if(id_rank .eq. 0) write(*,*) 'Use FFTW for all compontnent'
        call init_sph_component_FFTW                                    &
     &     (sph_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs%sph_comp_FFTW)
#endif
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(id_rank .eq. 0) write(*,*) 'Use prt FFTPACK'
          call init_prt_FFTPACK5(sph_rtp, comm_rtp,                     &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_FFTPACK)
        else
          if(id_rank .eq. 0) write(*,*) 'Use rtp FFTPACK'
          call init_rtp_FFTPACK5(sph_rtp, comm_rtp,                     &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_FFTPACK)
        end if
      end if
!
      end subroutine init_sph_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_FFT_select(sph_rtp, WK_FFTs)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
      if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTPACK'
        call finalize_sph_single_FFTPACK5(WK_FFTs%sph_sgl_FFTPACK)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_COMPONENT) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                     'Finalize FFTPACK for all comp'
        call finalize_sph_comp_FFTPACK5(WK_FFTs%sph_comp_FFTPACK)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_DOMAIN) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                     'Finalize FFTPACK for domain'
        call finalize_sph_domain_FFTPACK5(WK_FFTs%sph_domain_FFTPACK)
!
!
#ifdef FFTW3
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_sph_field_FFTW(WK_FFTs%sph_fld_FFTW)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_DOMAIN) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW for domain'
        call finalize_sph_field_FFTW(WK_FFTs%sph_fld_FFTW)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTW'
        call finalize_sph_single_FFTW(WK_FFTs%sph_sgl_FFTW)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_COMPONENT) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW for all comps'
        call finalize_sph_component_FFTW(WK_FFTs%sph_comp_FFTW)
#endif
      else
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
        call finalize_sph_FFTPACK5(WK_FFTs%sph_FFTPACK)
      end if
!
      end subroutine finalize_sph_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_FFT_select                                  &
     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs)
!
      use t_spheric_rtp_data
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
      if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use single FFTPACK'
        call verify_sph_single_FFTPACK5                                 &
     &     (sph_rtp, WK_FFTs%sph_sgl_FFTPACK)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_COMPONENT) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK for component'
        call verify_sph_comp_FFTPACK5                                   &
     &     (sph_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs%sph_comp_FFTPACK)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                         'Use prt FFTPACK for domain'
          call verify_prt_domain_FFTPACK5(sph_rtp, comm_rtp,            &
     &                                    WK_FFTs%sph_domain_FFTPACK)
        else
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                         'Use rtp FFTPACK for domain'
          call verify_rtp_domain_FFTPACK5(sph_rtp, comm_rtp,            &
     &                                    WK_FFTs%sph_domain_FFTPACK)
        end if
!
#ifdef FFTW3
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(iflag_debug .gt. 0) write(*,*) 'Use prt FFTW'
          call verify_prt_FFTW(sph_rtp, comm_rtp,                       &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_fld_FFTW)
        else
          if(iflag_debug .gt. 0) write(*,*) 'Use rtp FFTW'
          call verify_rtp_FFTW(sph_rtp, comm_rtp,                       &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_fld_FFTW)
        end if
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(iflag_debug .gt. 0) write(*,*) 'Use prt FFTW for field'
          call verify_prt_field_FFTW                                    &
     &       (sph_rtp, comm_rtp, WK_FFTs%sph_fld_FFTW)
        else
          if(iflag_debug .gt. 0) write(*,*) 'Use rtp FFTW for field'
          call verify_rtp_field_FFTW                                    &
     &       (sph_rtp, comm_rtp, WK_FFTs%sph_fld_FFTW)
        end if
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use single FFTW'
        call verify_sph_single_FFTW(sph_rtp, WK_FFTs%sph_sgl_FFTW)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_COMPONENT) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW for all comp。'
        call verify_sph_component_FFTW                                  &
     &     (sph_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs%sph_comp_FFTW)
#endif
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(iflag_debug .gt. 0) write(*,*) 'Use prt FFTPACK'
          call verify_prt_FFTPACK5(sph_rtp, comm_rtp,                   &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_FFTPACK)
        else
          if(iflag_debug .gt. 0) write(*,*) 'Use rtp FFTPACK'
          call verify_rtp_FFTPACK5(sph_rtp, comm_rtp,                   &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_FFTPACK)
        end if
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
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
      real (kind=kreal), intent(in):: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
      if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_SINGLE) then
        call sph_single_RFFTMF_to_send(sph_rtp, comm_rtp, ncomp_fwd,    &
     &      n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_sgl_FFTPACK)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_COMPONENT) then
        call sph_comp_RFFTMF_to_send(sph_rtp, comm_rtp, ncomp_fwd,      &
     &      n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_comp_FFTPACK)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_domain_RFFTMF_to_send(sph_rtp, ncomp_fwd,            &
     &        n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_domain_FFTPACK)
        else
          call rtp_domain_RFFTMF_to_send(sph_rtp, ncomp_fwd,            &
     &        n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_domain_FFTPACK)
        end if
!
#ifdef FFTW3
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_fwd_FFTW_to_send(sph_rtp, comm_rtp,                  &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_fld_FFTW)
        else
          call rtp_fwd_FFTW_to_send(sph_rtp, comm_rtp,                  &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_fld_FFTW)
        end if
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_field_fwd_FFTW_to_send(sph_rtp, comm_rtp,            &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_fld_FFTW)
        else
          call rtp_field_fwd_FFTW_to_send(sph_rtp, comm_rtp,            &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_fld_FFTW)
        end if
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call sph_single_fwd_FFTW_to_send(sph_rtp, comm_rtp,             &
     &      ncomp_fwd, n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_sgl_FFTW)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_COMPONENT) then
        call sph_comp_fwd_FFTW_to_send(sph_rtp, comm_rtp, ncomp_fwd,    &
     &      n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_comp_FFTW)
#endif
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_RFFTMF_to_send(sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), &
     &                            WS(1), WK_FFTs%sph_FFTPACK)
        else
          call rtp_RFFTMF_to_send(sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), &
     &                            WS(1), WK_FFTs%sph_FFTPACK)
        end if
      end if
!
      end subroutine fwd_FFT_select_to_send
!
! ------------------------------------------------------------------
!
      subroutine back_FFT_select_from_recv                              &
     &        (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp, WK_FFTs)
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout)                                  &
     &                  :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
      if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_SINGLE) then
        call sph_single_RFFTMB_from_recv(sph_rtp, comm_rtp, ncomp_bwd,  &
     &      n_WR, WR, v_rtp(1,1), WK_FFTs%sph_sgl_FFTPACK)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_COMPONENT) then
        call sph_comp_RFFTMB_from_recv(sph_rtp, comm_rtp, ncomp_bwd,    &
     &      n_WR, WR, v_rtp(1,1), WK_FFTs%sph_comp_FFTPACK)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_domain_RFFTMB_from_recv                              &
     &       (sph_rtp, comm_rtp, ncomp_bwd,                             &
     &        n_WR, WR, v_rtp(1,1), WK_FFTs%sph_domain_FFTPACK)
        else
          call rtp_domain_RFFTMB_from_recv                              &
     &       (sph_rtp, comm_rtp, ncomp_bwd,                             &
     &        n_WR, WR, v_rtp(1,1), WK_FFTs%sph_domain_FFTPACK)
        end if
!
#ifdef FFTW3
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_back_FFTW_from_recv(sph_rtp, comm_rtp,               &
     &        ncomp_bwd, n_WR, WR(1), v_rtp(1,1), WK_FFTs%sph_fld_FFTW)
        else
          call rtp_back_FFTW_from_recv(sph_rtp, comm_rtp,               &
     &        ncomp_bwd, n_WR, WR(1), v_rtp(1,1), WK_FFTs%sph_fld_FFTW)
        end if
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_field_back_FFTW_from_recv(sph_rtp, comm_rtp,         &
     &        ncomp_bwd, n_WR, WR(1), v_rtp(1,1), WK_FFTs%sph_fld_FFTW)
        else
          call rtp_field_back_FFTW_from_recv(sph_rtp, comm_rtp,         &
     &        ncomp_bwd, n_WR, WR(1), v_rtp(1,1), WK_FFTs%sph_fld_FFTW)
        end if
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call sph_single_back_FFTW_from_recv(sph_rtp, comm_rtp,          &
     &      ncomp_bwd, n_WR, WR(1), v_rtp(1,1), WK_FFTs%sph_sgl_FFTW)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTW_COMPONENT) then
        call sph_comp_back_FFTW_from_recv(sph_rtp, comm_rtp, ncomp_bwd, &
     &      n_WR, WR(1), v_rtp(1,1), WK_FFTs%sph_comp_FFTW)
#endif
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_RFFTMB_from_recv(sph_rtp, comm_rtp, ncomp_bwd, n_WR, &
     &        WR, v_rtp(1,1), WK_FFTs%sph_FFTPACK)
        else
          call rtp_RFFTMB_from_recv(sph_rtp, comm_rtp, ncomp_bwd, n_WR, &
     &        WR, v_rtp(1,1), WK_FFTs%sph_FFTPACK)
        end if
      end if
!
      end subroutine back_FFT_select_from_recv
!
! ------------------------------------------------------------------
!
      end module t_sph_FFT_selector
