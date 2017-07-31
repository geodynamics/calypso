!>@file   t_sph_transforms.f90
!!@brief  module t_sph_transforms
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform for vector
!!       and gradient of scalar
!!
!!@verbatim
!!      subroutine sph_backward_transforms                              &
!!     &        (ncomp_trans, nvector, nscalar, sph, comms_sph, trans_p,&
!!     &         n_WS, n_WR, WS, WR, v_rtp, WK_sph)
!!      subroutine sph_b_trans_w_poles                                  &
!!     &        (ncomp_trans, nvector, nscalar, sph, comms_sph, trans_p,&
!!     &         n_WS, n_WR, WS, WR, v_rtp, v_pl_local, v_pole, WK_sph)
!!      subroutine pole_b_transform                                     &
!!     &        (ncomp_trans, nvector, nscalar, sph, comms_sph, trans_p,&
!!     &         n_WS, n_WR, WS, WR, v_pl_local, v_pole)
!!      subroutine sph_forward_transforms                               &
!!     &        (ncomp_trans, nvector, nscalar, sph, comms_sph, trans_p,&
!!     &         v_rtp, n_WS, n_WR, WS, WR, WK_sph)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!
!!   input /outpt arrays for single field
!!
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
!
      module t_sph_transforms
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_machine_parameter
      use spherical_SRs_N
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_legendre_trans_select
      use t_sph_FFT_selector
!
      implicit none
!
!>      Work structures for various spherical harmonics trasform
      type spherical_trns_works
!>        Work structures for various Legendre trasform
        type(legendre_trns_works) :: WK_leg
!>        Structure for work area of FFTs
        type(work_for_FFTs) :: WK_FFTs
      end type spherical_trns_works
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_backward_transforms                                &
     &        (ncomp_trans, nvector, nscalar, sph, comms_sph, trans_p,  &
     &         n_WS, n_WR, WS, WR, v_rtp, WK_sph)
!
      use pole_sph_transform
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: nvector, nscalar
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                :: v_rtp(sph%sph_rtp%nnod_rtp,ncomp_trans)
      type(spherical_trns_works), intent(inout) :: WK_sph
!
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rj, comms_sph%comm_rlm)
      call finish_send_recv_sph(comms_sph%comm_rj)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(22)
      call sel_backward_legendre_trans                                  &
     &   (ncomp_trans, nvector, nscalar, sph%sph_rlm, sph%sph_rtm,      &
     &    comms_sph%comm_rlm, comms_sph%comm_rtm,                       &
     &    trans_p%leg, trans_p%idx_trns,                                &
     &    n_WR, n_WS, WR, WS, WK_sph%WK_leg)
      call end_eleps_time(22)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(19)
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rtm, comms_sph%comm_rtp)
      call end_eleps_time(19)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(24)
      call back_FFT_select_from_recv(sph%sph_rtp, comms_sph%comm_rtp,   &
     &    ncomp_trans, n_WR, WR, v_rtp, WK_sph%WK_FFTs)
      call finish_send_recv_sph(comms_sph%comm_rtm)
      call end_eleps_time(24)
!
      end subroutine sph_backward_transforms
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_w_poles                                    &
     &        (ncomp_trans, nvector, nscalar, sph, comms_sph, trans_p,  &
     &         n_WS, n_WR, WS, WR, v_rtp, v_pl_local, v_pole, WK_sph)
!
      use pole_sph_transform
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: nvector, nscalar
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                :: v_rtp(sph%sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pl_local(sph%sph_rtp%nnod_pole,ncomp_trans)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pole(sph%sph_rtp%nnod_pole,ncomp_trans)
      type(spherical_trns_works), intent(inout) :: WK_sph
!
      integer(kind = kint) :: ncomp_pole
!
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rj, comms_sph%comm_rlm)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(22)
      call calypso_mpi_barrier
      if(iflag_debug .gt. 0) write(*,*) 'pole_backward_transforms'
      call pole_backward_transforms(ncomp_trans, nvector, nscalar,      &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rtm, sph%sph_rlm,        &
     &    comms_sph%comm_rlm, trans_p%leg, n_WR, WR, v_pl_local)
      call finish_send_recv_sph(comms_sph%comm_rj)
!
!
      call calypso_mpi_barrier
      if(iflag_debug .gt. 0) write(*,*) 'sel_backward_legendre_trans'
      call sel_backward_legendre_trans                                  &
     &   (ncomp_trans, nvector, nscalar, sph%sph_rlm, sph%sph_rtm,      &
     &    comms_sph%comm_rlm, comms_sph%comm_rtm,                       &
     &    trans_p%leg, trans_p%idx_trns,                                &
     &    n_WR, n_WS, WR, WS, WK_sph%WK_leg)
      call end_eleps_time(22)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(19)
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rtm, comms_sph%comm_rtp)
      call end_eleps_time(19)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(24)
      call back_FFT_select_from_recv(sph%sph_rtp, comms_sph%comm_rtp,   &
     &    ncomp_trans, n_WR, WR, v_rtp, WK_sph%WK_FFTs)
      call end_eleps_time(24)
!
      call finish_send_recv_sph(comms_sph%comm_rtm)
!
      call calypso_mpi_barrier
      if(iflag_debug .gt. 0) write(*,*) 'v_pole'
      v_pole(1:sph%sph_rtp%nnod_pole,1:ncomp_trans) = zero
      ncomp_pole = ncomp_trans * sph%sph_rtp%nnod_pole
      call MPI_allreduce(v_pl_local, v_pole, ncomp_pole,                &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sph_b_trans_w_poles
!
! -----------------------------------------------------------------------
!
      subroutine pole_b_transform                                       &
     &        (ncomp_trans, nvector, nscalar, sph, comms_sph, trans_p,  &
     &         n_WS, n_WR, WS, WR, v_pl_local, v_pole)
!
      use pole_sph_transform
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: nvector, nscalar
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pl_local(sph%sph_rtp%nnod_pole,ncomp_trans)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pole(sph%sph_rtp%nnod_pole,ncomp_trans)
!
      integer(kind = kint) :: ncomp_pole
!
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rj, comms_sph%comm_rlm)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(22)
      call pole_backward_transforms(ncomp_trans, nvector, nscalar,      &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rtm, sph%sph_rlm,        &
     &    comms_sph%comm_rlm, trans_p%leg, n_WR, WR, v_pl_local)
      call finish_send_recv_sph(comms_sph%comm_rj)
      call end_eleps_time(22)
!
      v_pole(1:sph%sph_rtp%nnod_pole,1:ncomp_trans) = zero
      ncomp_pole = ncomp_trans * sph%sph_rtp%nnod_pole
      call MPI_allreduce(v_pl_local, v_pole, ncomp_pole,                &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine pole_b_transform
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_forward_transforms                                 &
     &        (ncomp_trans, nvector, nscalar, sph, comms_sph, trans_p,  &
     &         v_rtp, n_WS, n_WR, WS, WR, WK_sph)
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: nvector, nscalar
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(in)                                    &
     &                   :: v_rtp(sph%sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
      type(spherical_trns_works), intent(inout) :: WK_sph
!
!
      call start_eleps_time(24)
      call fwd_FFT_select_to_send(sph%sph_rtp, comms_sph%comm_rtp,      &
     &    ncomp_trans, n_WS, v_rtp, WS, WK_sph%WK_FFTs)
      call end_eleps_time(24)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(20)
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call finish_send_recv_sph(comms_sph%comm_rtp)
      call end_eleps_time(20)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(23)
      if(iflag_debug .gt. 0) write(*,*) 'sel_forward_legendre_trans'
      call sel_forward_legendre_trans                                   &
     &   (ncomp_trans, nvector, nscalar, sph%sph_rtm, sph%sph_rlm,      &
     &    comms_sph%comm_rtm, comms_sph%comm_rlm,                       &
     &    trans_p%leg, trans_p%idx_trns,                                &
     &    n_WR, n_WS, WR, WS, WK_sph%WK_leg)
      call end_eleps_time(23)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rlm, comms_sph%comm_rj)
      call finish_send_recv_sph(comms_sph%comm_rlm)
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      end subroutine sph_forward_transforms
!
! -----------------------------------------------------------------------
!
      end module t_sph_transforms
