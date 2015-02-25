!>@file   sph_trans_w_coriols.f90
!!@brief  module sph_trans_w_coriols
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform
!!       including Coriolis terms
!!
!!@verbatim
!!      subroutine sph_b_trans_w_coriolis(ncomp_trans, nvector, nscalar,&
!!     &          n_WS, n_WR, WS, WR, fld_rtp)
!!      subroutine sph_f_trans_w_coriolis(ncomp_trans, nvector, nscalar,&
!!     &          frc_rtp, n_WS, n_WR, WS, WR)
!!
!!      subroutine sph_b_trans_licv(ncomp_trans, n_WR, WR)
!!      subroutine sph_f_trans_licv(ncomp_trans, n_WS, WS)
!!
!!   input /outpt arrays for single vector
!!      radial component:      v_rtp(i_rtp,1)
!!      elevetional component: v_rtp(i_rtp,2)
!!      azimuthal component:   v_rtp(i_rtp,3)
!!
!!      Poloidal component:          WR(3*i_rj-2)
!!      diff. of Poloidal component: WR(3*i_rj-1)
!!      Toroidal component:          WR(3*i_rj  )
!!@endverbatim
!!
!!@param ncomp_trans Number of components for transform
!!@param nvector     Number of vectors for transform
!!@param nscalar     Number of scalars for transform
!!
!!@param n_WS Number of components for send buffrt
!!@param n_WR Number of components for recieve buffer
!!@param WS Send buffer
!!@param WR Recieve buffer
!!@param fld_rtp Field data to make nonlinear terms
!!@param frc_rtp Nonlinear terms
!
      module sph_trans_w_coriols
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_machine_parameter
      use m_work_4_sph_trans
      use MHD_FFT_selector
      use legendre_transform_select
      use spherical_SRs_N
      use const_coriolis_sph_rlm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_w_coriolis(ncomp_trans, nvector, nscalar,  &
     &          n_WS, n_WR, WS, WR, fld_rtp)
!
      integer(kind = kint), intent(in) :: ncomp_trans, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
      real (kind=kreal), intent(inout):: fld_rtp(nnod_rtp,ncomp_trans)
!
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      if(iflag_debug .gt. 0) write(*,*) 'calypso_sph_comm_rj_2_rlm_N'
      call calypso_sph_comm_rj_2_rlm_N(ncomp_trans)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'sum_coriolis_rlm'
      call sum_coriolis_rlm(ncomp_trans, n_WR, WR)
      call end_eleps_time(13)
!
      call start_eleps_time(22)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &    'sel_backward_legendre_trans', ncomp_trans, nvector, nscalar
      call sel_backward_legendre_trans                                  &
     &   (ncomp_trans, nvector, nscalar, n_WR, n_WS, WR, WS)
      call end_eleps_time(22)
!
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(19)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &      'calypso_sph_comm_rtm_2_rtp_N'
      call calypso_sph_comm_rtm_2_rtp_N(ncomp_trans)
      call end_eleps_time(19)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!
      call start_eleps_time(24)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &    'back_MHD_FFT_sel_from_recv', ncomp_trans, nvector, nscalar
      call back_MHD_FFT_sel_from_recv(ncomp_trans, n_WR, WR, fld_rtp)
      call end_eleps_time(24)
!
      if(iflag_debug .gt. 0) write(*,*) 'finish_send_recv_rtm_2_rtp'
      call finish_send_recv_rtm_2_rtp
!
      end subroutine sph_b_trans_w_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_w_coriolis(ncomp_trans, nvector, nscalar,  &
     &          frc_rtp, n_WS, n_WR, WS, WR)
!
      integer(kind = kint), intent(in) :: ncomp_trans, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real (kind=kreal), intent(inout):: frc_rtp(nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
!
!
      call start_eleps_time(24)
      call fwd_MHD_FFT_sel_from_recv(ncomp_trans, n_WS, frc_rtp, WS)
      call end_eleps_time(24)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(20)
      call calypso_sph_comm_rtp_2_rtm_N(ncomp_trans)
      call end_eleps_time(20)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(23)
      if(iflag_debug .gt. 0) write(*,*) 'sel_forward_legendre_trans'
      call sel_forward_legendre_trans                                   &
     &   (ncomp_trans, nvector, nscalar, n_WR, n_WS, WR, WS)
      call end_eleps_time(23)
!
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'copy_coriolis_terms_rlm'
      call copy_coriolis_terms_rlm(ncomp_trans, n_WS, WS)
      call end_eleps_time(13)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call calypso_sph_comm_rlm_2_rj_N(ncomp_trans)
      call finish_send_recv_rlm_2_rj
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      end subroutine sph_f_trans_w_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_licv(ncomp_trans, n_WR, WR)
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call calypso_sph_comm_rj_2_rlm_N(ncomp_trans)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'sum_coriolis_rlm'
      call sum_coriolis_rlm(ncomp_trans, n_WR, WR)
      call end_eleps_time(13)
!
      call finish_send_recv_rj_2_rlm
!
      end subroutine sph_b_trans_licv
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_licv(ncomp_trans, n_WS, WS)
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'copy_coriolis_terms_rlm'
      call copy_coriolis_terms_rlm(ncomp_trans, n_WS, WS)
      call end_eleps_time(24)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call calypso_sph_comm_rlm_2_rj_N(ncomp_trans)
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call finish_send_recv_rlm_2_rj
!
      end subroutine sph_f_trans_licv
!
! -----------------------------------------------------------------------
!
      end module sph_trans_w_coriols
