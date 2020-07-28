!>@file   init_spherical_SRs.f90
!!@brief  module init_spherical_SRs
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communications 
!!@n      for spherical harmonics transform
!!
!!@verbatim
!!      subroutine init_sph_send_recv_N(NB, sph, comms_sph, iflag_recv)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!@endverbatim
!!
!!
!!@n @param  NB    Number of components for communication
!!@n @param  X_rtp(NB*nnod_rtp)  @f$ f(r,\theta,\phi) @f$
!!@n               (Order, X_rtp(i_comp,inod))
!
!
      module init_spherical_SRs
!
      use m_precision
!
      use m_constants
      use m_solver_SR
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans_spin
!
      implicit none
!
      type(leg_trns_spin_work), save, private :: WK0_spin
!
      private :: all_sph_send_recv_N, all_sph_SR_core_N
      private :: check_spherical_SRs_N, check_calypso_sph_buffer_N
      private :: sel_sph_import_table
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_sph_send_recv_N(NB, sph, comms_sph, iflag_recv)
!
      use calypso_mpi
!
      use m_sph_communicators
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: NB
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      integer(kind = kint), intent(inout) :: iflag_recv
!
      real(kind = kreal), allocatable :: X_rtp(:), X_rj(:)
!
!
      call alloc_work_sph_trans                                         &
     &   (NB, sph%sph_rtm%nnod_rtm, sph%sph_rlm%nnod_rlm, WK0_spin)
      allocate(X_rj(NB * sph%sph_rj%nnod_rj))
      allocate(X_rtp(NB * sph%sph_rtp%nnod_rtp))
      X_rj = 0.0d0
      X_rtp = 0.0d0
!
      call check_spherical_SRs_N                                        &
     &   (NB, comms_sph%comm_rtp, comms_sph%comm_rtm,                   &
     &        comms_sph%comm_rlm, comms_sph%comm_rj)
!
      call check_calypso_sph_buffer_N(NB, comms_sph)
      call sel_sph_import_table(NB, comms_sph,                          &
     &    sph%sph_rtp%nnod_rtp, sph%sph_rtm%nnod_rtm,                   &
     &    sph%sph_rlm%nnod_rlm, sph%sph_rj%nnod_rj,                     &
     &    X_rtp, WK0_spin%vr_rtm_wk, WK0_spin%sp_rlm_wk, X_rj,          &
     &    iflag_recv)
!
      deallocate(X_rj, X_rtp)
      call dealloc_work_sph_trans(WK0_spin)
!
      end subroutine init_sph_send_recv_N
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_import_table(NB, comms_sph,                    &
     &          nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,                  &
     &          X_rtp, X_rtm, X_rlm, X_rj, iflag_recv)
!
      use calypso_mpi
      use calypso_mpi_real
      use transfer_to_long_integers
      use select_copy_from_recv
!
      use m_sph_communicators
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rtm
      integer(kind = kint), intent(in) :: nnod_rlm, nnod_rj
      type(sph_comm_tables), intent(in) :: comms_sph
!
      real (kind=kreal), intent(inout) :: X_rtp(NB*nnod_rtp)
      real (kind=kreal), intent(inout) :: X_rtm(NB*nnod_rtm)
      real (kind=kreal), intent(inout) :: X_rlm(NB*nnod_rlm)
      real (kind=kreal), intent(inout)::  X_rj(NB*nnod_rj)
      integer(kind = kint), intent(inout) :: iflag_recv
!
      real(kind = kreal) :: starttime, endtime(0:2)
      real(kind = kreal) :: etime_item_import(0:1) = 0.0d0
!
!
      call check_spherical_SRs_N                                        &
     &   (NB, comms_sph%comm_rtp, comms_sph%comm_rtm,                   &
     &        comms_sph%comm_rlm, comms_sph%comm_rj)
!
      if(iflag_recv .ne. iflag_import_UNDEFINED) return
!
      starttime = MPI_WTIME()
      call all_sph_send_recv_N(iflag_import_rev, NB, comms_sph,         &
     &    nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,                        &
     &    X_rtp, X_rtm, X_rlm, X_rj)
!
      if(my_rank .eq. 0) write(*,*) 'test  send_recv with reg. import'
      starttime = MPI_WTIME()
      call all_sph_send_recv_N(iflag_import_item, NB, comms_sph,        &
     &    nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,                        &
     &    X_rtp, X_rtm, X_rlm, X_rj)
      endtime(0) = MPI_WTIME() - starttime
!
      if(my_rank .eq. 0) write(*,*) 'test  send_recv with rev. import'
      starttime = MPI_WTIME()
      call all_sph_send_recv_N(iflag_import_rev, NB, comms_sph,         &
     &    nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,                        &
     &    X_rtp, X_rtm, X_rlm, X_rj)
      endtime(1) = MPI_WTIME() - starttime
!
      endtime(1) = MPI_WTIME() - starttime
      call calypso_mpi_allreduce_real(endtime(0), etime_item_import(0), &
     &                                cast_long(2), MPI_SUM)
      etime_item_import(0:1) = etime_item_import(0:1) / dble(nprocs)
!
      if(etime_item_import(1) .le. etime_item_import(0)) then
        iflag_recv = iflag_import_rev
      else
        iflag_recv = iflag_import_item
      end if
!
      if(my_rank .ne. 0) return
        write(*,*) '0: Time by reg. import list: ',etime_item_import(0)
        write(*,*) '1: Time by rev. import list: ',etime_item_import(1)
!
      end subroutine sel_sph_import_table
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine all_sph_send_recv_N(iflag_recv, NB, comms_sph,         &
     &          nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,                  &
     &          X_rtp, X_rtm, X_rlm, X_rj)
!
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rtm
      integer(kind = kint), intent(in) :: nnod_rlm, nnod_rj
      type(sph_comm_tables), intent(in) :: comms_sph
!
      real (kind=kreal), intent(inout) :: X_rtp(NB*nnod_rtp)
      real (kind=kreal), intent(inout) :: X_rtm(NB*nnod_rtm)
      real (kind=kreal), intent(inout) :: X_rlm(NB*nnod_rlm)
      real (kind=kreal), intent(inout)::  X_rj(NB*nnod_rj)
!
!
      call send_recv_sph_trans_N(iflag_recv, NB, nnod_rj, nnod_rlm,     &
     &    comms_sph%comm_rj,  comms_sph%comm_rlm, X_rj, X_rlm)
      call send_recv_sph_trans_N(iflag_recv, NB, nnod_rlm, nnod_rj,     &
     &    comms_sph%comm_rlm, comms_sph%comm_rj, X_rlm, X_rj)
      call send_recv_sph_trans_N(iflag_recv, NB, nnod_rtp, nnod_rtm,    &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm, X_rtp, X_rtm)
      call send_recv_sph_trans_N(iflag_recv, NB, nnod_rtm, nnod_rtp,    &
     &    comms_sph%comm_rtm, comms_sph%comm_rtp, X_rtm, X_rtp)
!
      end subroutine all_sph_send_recv_N
!
! ----------------------------------------------------------------------
!
      subroutine all_sph_SR_core_N(NB, comms_sph)
!
      use spherical_SRs_N
!
      integer (kind=kint), intent(in) :: NB
      type(sph_comm_tables), intent(in) :: comms_sph
!
!
      call check_calypso_sph_buffer_N(NB, comms_sph)
!
      call calypso_sph_comm_N                                           &
     &   (NB, comms_sph%comm_rj, comms_sph%comm_rlm)
      call finish_send_recv_sph(comms_sph%comm_rj)
      call calypso_sph_comm_N                                           &
     &   (NB, comms_sph%comm_rlm, comms_sph%comm_rj)
      call finish_send_recv_sph(comms_sph%comm_rlm)
      call calypso_sph_comm_N                                           &
     &   (NB, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call finish_send_recv_sph(comms_sph%comm_rtp)
      call calypso_sph_comm_N                                           &
     &   (NB, comms_sph%comm_rtm, comms_sph%comm_rtp)
      call finish_send_recv_sph(comms_sph%comm_rtm)
!
      end subroutine all_sph_SR_core_N
!
! ----------------------------------------------------------------------
!
      subroutine check_spherical_SRs_N                                  &
     &         (NB, comm_rtp, comm_rtm, comm_rlm, comm_rj)
!
      use calypso_mpi
      use calypso_SR_core
!
      integer (kind=kint), intent(in) :: NB
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
!
!
      if(my_rank .eq. 0) write(*,*) 'check rtp -> rtm'
      call check_calypso_SR_stack                                       &
     &   (NB, comm_rtp%nneib_domain, comm_rtp%iflag_self,               &
     &    comm_rtp%istack_sr, comm_rtm%nneib_domain,                    &
     &    comm_rtm%iflag_self, comm_rtm%istack_sr, SR_sig1, SR_r1)
      if(my_rank .eq. 0) write(*,*) 'check rtm -> rtp'
      call check_calypso_SR_stack                                       &
     &   (NB, comm_rtm%nneib_domain, comm_rtm%iflag_self,               &
     &    comm_rtm%istack_sr, comm_rtp%nneib_domain,                    &
     &    comm_rtp%iflag_self, comm_rtp%istack_sr, SR_sig1, SR_r1)
      if(my_rank .eq. 0) write(*,*) 'check rj -> rlm'
      call check_calypso_SR_stack                                       &
     &    (NB, comm_rj%nneib_domain, comm_rj%iflag_self,                &
     &     comm_rj%istack_sr, comm_rlm%nneib_domain,                    &
     &     comm_rlm%iflag_self, comm_rlm%istack_sr, SR_sig1, SR_r1)
      if(my_rank .eq. 0) write(*,*) 'check rlm -> rj'
      call check_calypso_SR_stack                                       &
     &   (NB, comm_rlm%nneib_domain, comm_rlm%iflag_self,               &
     &    comm_rlm%istack_sr, comm_rj%nneib_domain,                     &
     &    comm_rj%iflag_self, comm_rj%istack_sr, SR_sig1, SR_r1)
!
      end subroutine check_spherical_SRs_N
!
! ------------------------------------------------------------------
!
      subroutine check_calypso_sph_buffer_N(NB, comms_sph)
!
      use t_sph_trans_comm_tbl
      use m_solver_SR
      use spherical_SRs_N
     use sel_spherical_SRs
!
      integer (kind=kint), intent(in) :: NB
      type(sph_comm_tables), intent(in) :: comms_sph
!
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (NB, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (NB, comms_sph%comm_rtm, comms_sph%comm_rtp)
      call check_calypso_sph_comm_buf_N                                 &
     &   (NB, comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (NB, comms_sph%comm_rlm, comms_sph%comm_rj)
!
      end subroutine check_calypso_sph_buffer_N
!
!-----------------------------------------------------------------------
!
      end module init_spherical_SRs
