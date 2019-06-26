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
!!      subroutine init_sph_send_recv_N(NB, sph, comms_sph)
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
      use m_sel_spherical_SRs
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
      private :: sel_sph_import_table, sel_sph_comm_routine
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_sph_send_recv_N(NB, sph, comms_sph)
!
      use calypso_mpi
!
      use m_sph_communicators
      use spherical_SRs_N
!
      integer (kind=kint), intent(in) :: NB
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
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
     &    X_rtp, WK0_spin%vr_rtm_wk, WK0_spin%sp_rlm_wk, X_rj)
!
      deallocate(X_rj, X_rtp)
      call dealloc_work_sph_trans(WK0_spin)
!
      if(my_rank .eq. 0) then
        write(*,'(a,i4)', advance='no')                                 &
     &   'Communication mode for sph. transform: ', iflag_sph_SRN
        if(iflag_sph_SRN .eq. iflag_import_item) then
          write(*,'(3a)') ' (', trim(hd_import_item), ') '
        else if(iflag_sph_SRN .eq. iflag_import_rev) then
          write(*,'(3a)') ' (', trim(hd_import_rev), ') '
        end if
      end if
!
      iflag_sph_commN = iflag_send_recv
!      call sel_sph_comm_routine(NB, comms_sph)
      if(my_rank .eq. 0) then
        write(*,'(a,i4)', advance='no')                                 &
     &   'Selected communication routine: ', iflag_sph_commN
        if(iflag_sph_commN .eq. iflag_send_recv) then
          write(*,'(3a)') ' (', trim(hd_sendrecv), ') '
        else if(iflag_sph_commN .eq. iflag_alltoallv) then
          write(*,'(3a)') ' (', trim(hd_all2allv), ') '
        end if
      end if
!
      end subroutine init_sph_send_recv_N
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_import_table(NB, comms_sph,                    &
     &          nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,                  &
     &          X_rtp, X_rtm, X_rlm, X_rj)
!
      use calypso_mpi
!
      use m_sph_communicators
      use m_solver_SR
!
      integer (kind=kint), intent(in) :: NB
      integer (kind=kint), intent(in) :: nnod_rtp, nnod_rtm
      integer (kind=kint), intent(in) :: nnod_rlm, nnod_rj
      type(sph_comm_tables), intent(in) :: comms_sph
!
      real (kind=kreal), intent(inout) :: X_rtp(NB*nnod_rtp)
      real (kind=kreal), intent(inout) :: X_rtm(NB*nnod_rtm)
      real (kind=kreal), intent(inout) :: X_rlm(NB*nnod_rlm)
      real (kind=kreal), intent(inout)::  X_rj(NB*nnod_rj)
!
      real(kind = kreal) :: starttime, endtime(0:2)
      real(kind = kreal) :: etime_item_import(0:1) = 0.0d0
!
!
      call check_spherical_SRs_N                                        &
     &   (NB, comms_sph%comm_rtp, comms_sph%comm_rtm,                   &
     &        comms_sph%comm_rlm, comms_sph%comm_rj)
!
      if(iflag_sph_SRN .ne. iflag_import_UNDEFINED) return
!
      if(my_rank .eq. 0) write(*,*) 'test  send_recv with reg. import'
      iflag_sph_SRN = iflag_import_item
      starttime = MPI_WTIME()
      call all_sph_send_recv_N(NB, comms_sph,                           &
     &    nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,                        &
     &    X_rtp, X_rtm, X_rlm, X_rj)
      endtime(0) = MPI_WTIME() - starttime
!
      if(my_rank .eq. 0) write(*,*) 'test  send_recv with rev. import'
      iflag_sph_SRN = iflag_import_rev
      starttime = MPI_WTIME()
      call all_sph_send_recv_N(NB, comms_sph,                           &
     &    nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,                        &
     &    X_rtp, X_rtm, X_rlm, X_rj)
      endtime(1) = MPI_WTIME() - starttime
!
      endtime(1) = MPI_WTIME() - starttime
      call MPI_allREDUCE (endtime(0), etime_item_import(0), 2,          &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      etime_item_import(0:1) = etime_item_import(0:1) / dble(nprocs)
!
      if(etime_item_import(1) .le. etime_item_import(0)) then
        iflag_sph_SRN = iflag_import_rev
      else
        iflag_sph_SRN = iflag_import_item
      end if
!
      if(my_rank .ne. 0) return
        write(*,*) '0: Time by reg. import list: ',etime_item_import(0)
        write(*,*) '1: Time by rev. import list: ',etime_item_import(1)
!
      end subroutine sel_sph_import_table
!
! ----------------------------------------------------------------------
!
      subroutine sel_sph_comm_routine(NB, comms_sph)
!
      use m_sph_communicators
      use calypso_mpi
      use set_from_recv_buf_rev
!
      integer (kind=kint), intent(in) :: NB
      type(sph_comm_tables), intent(in) :: comms_sph
!
      real(kind = kreal) :: starttime, endtime(0:2)
      real(kind = kreal) :: etime_send_recv(0:2) =   0.0d0
      real(kind = kreal) :: etime_shortest
!
      integer (kind=kint) :: i
!
!
      if(iflag_sph_commN .ne. iflag_SR_UNDEFINED) return
!
      endtime(0:2) = 0.0d0
!
      iflag_sph_commN = iflag_send_recv
      starttime = MPI_WTIME()
      call all_sph_SR_core_N(NB, comms_sph)
      endtime(0) = MPI_WTIME() - starttime
!
      iflag_sph_commN = iflag_alltoallv
      starttime = MPI_WTIME()
      call all_sph_SR_core_N(NB, comms_sph)
      endtime(1) = MPI_WTIME() - starttime
!
      call MPI_allREDUCE (endtime(0), etime_send_recv(0), 3,            &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      etime_shortest = etime_send_recv(0)
      iflag_sph_commN = iflag_send_recv
      do i = 1, 1
        if(etime_send_recv(i) .le. etime_shortest                       &
     &          .and. etime_send_recv(i) .gt. 0.0) then
          etime_shortest = etime_send_recv(i)
          iflag_sph_commN = i
        end if
      end do
!
      if(my_rank .gt. 0) return
        write(*,*) '0: Time by MPI_ISEND_IRECV: ', etime_send_recv(0)
        write(*,*) '1: Time by MPI_AllToAllV: ',   etime_send_recv(1)
!
      end subroutine sel_sph_comm_routine
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine all_sph_send_recv_N(NB, comms_sph,                     &
     &          nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,                  &
     &          X_rtp, X_rtm, X_rlm, X_rj)
!
      use spherical_SRs_N
!
      integer (kind=kint), intent(in) :: NB
      integer (kind=kint), intent(in) :: nnod_rtp, nnod_rtm
      integer (kind=kint), intent(in) :: nnod_rlm, nnod_rj
      type(sph_comm_tables), intent(in) :: comms_sph
!
      real (kind=kreal), intent(inout) :: X_rtp(NB*nnod_rtp)
      real (kind=kreal), intent(inout) :: X_rtm(NB*nnod_rtm)
      real (kind=kreal), intent(inout) :: X_rlm(NB*nnod_rlm)
      real (kind=kreal), intent(inout)::  X_rj(NB*nnod_rj)
!
!
      call send_recv_sph_trans_N(NB, nnod_rj, nnod_rlm,                 &
     &    comms_sph%comm_rj,  comms_sph%comm_rlm, X_rj, X_rlm)
      call send_recv_sph_trans_N(NB, nnod_rlm, nnod_rj,                 &
     &    comms_sph%comm_rlm, comms_sph%comm_rj, X_rlm, X_rj)
      call send_recv_sph_trans_N(NB, nnod_rtp, nnod_rtm,                &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm, X_rtp, X_rtm)
      call send_recv_sph_trans_N(NB, nnod_rtm, nnod_rtp,                &
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
      use select_calypso_SR
!
      integer (kind=kint), intent(in) :: NB
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
!
!
      if(my_rank .eq. 0) write(*,*) 'check rtp -> rtm'
      call check_calypso_send_recv_N                                    &
     &   (NB, comm_rtp%nneib_domain, comm_rtp%iflag_self,               &
     &    comm_rtp%istack_sr, comm_rtm%nneib_domain,                    &
     &    comm_rtm%iflag_self, comm_rtm%istack_sr)
      if(my_rank .eq. 0) write(*,*) 'check rtm -> rtp'
      call check_calypso_send_recv_N                                    &
     &   (NB, comm_rtm%nneib_domain, comm_rtm%iflag_self,               &
     &    comm_rtm%istack_sr, comm_rtp%nneib_domain,                    &
     &    comm_rtp%iflag_self, comm_rtp%istack_sr)
      if(my_rank .eq. 0) write(*,*) 'check rj -> rlm'
      call check_calypso_send_recv_N                                    &
     &    (NB, comm_rj%nneib_domain, comm_rj%iflag_self,                &
     &     comm_rj%istack_sr, comm_rlm%nneib_domain,                    &
     &     comm_rlm%iflag_self, comm_rlm%istack_sr)
      if(my_rank .eq. 0) write(*,*) 'check rlm -> rj'
      call check_calypso_send_recv_N                                    &
     &   (NB, comm_rlm%nneib_domain, comm_rlm%iflag_self,               &
     &    comm_rlm%istack_sr, comm_rj%nneib_domain,                     &
     &    comm_rj%iflag_self, comm_rj%istack_sr)
!
      end subroutine check_spherical_SRs_N
!
! ------------------------------------------------------------------
!
      subroutine check_calypso_sph_buffer_N(NB, comms_sph)
!
      use t_sph_trans_comm_tbl
      use m_sel_spherical_SRs
      use m_solver_SR
      use spherical_SRs_N
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
