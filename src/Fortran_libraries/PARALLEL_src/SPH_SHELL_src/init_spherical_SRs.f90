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
!!      subroutine init_sph_send_recv_N(NB)
!!      subroutine check_spherical_SRs_N(NB)
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
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_solver_SR
!
      use m_sel_spherical_SRs
!
      implicit none
!
      private :: all_sph_send_recv_N, all_sph_SR_core_N
      private :: check_spherical_SRs_N
      private :: sel_sph_import_table, sel_sph_comm_routine
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_sph_send_recv_N(NB)
!
      use calypso_mpi
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_sph_communicators
      use m_work_4_sph_trans_spin
      use spherical_SRs_N
!
      integer (kind=kint), intent(in) :: NB
!
      real(kind = kreal), allocatable :: X_rtp(:), X_rj(:)
!
      call allocate_work_sph_trans(NB)
      allocate(X_rj(NB*nnod_rj))
      allocate(X_rtp(NB*nnod_rtp))
      X_rj = 0.0d0
      X_rtp = 0.0d0
!
      call check_spherical_SRs_N(NB)
!
      call buffer_size_sph_send_recv(NB)
      call sel_sph_import_table(NB, X_rtp, vr_rtm_wk, sp_rlm_wk, X_rj)
!
      deallocate(X_rj, X_rtp)
      call deallocate_work_sph_trans
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
!      call sel_sph_comm_routine(NB)
      if(my_rank .eq. 0) then
        write(*,'(a,i4)', advance='no')                                 &
     &   'Selected communication routine: ', iflag_sph_commN
        if(iflag_sph_commN .eq. iflag_send_recv) then
          write(*,'(3a)') ' (', trim(hd_sendrecv), ') '
        else if(iflag_sph_commN .eq. iflag_alltoallv) then
          write(*,'(3a)') ' (', trim(hd_all2allv), ') '
        else if(iflag_sph_commN .eq. iflag_alltoall) then
          write(*,'(3a)') ' (', trim(hd_all2all), ') '
        end if
      end if
!
      end subroutine init_sph_send_recv_N
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_import_table(NB, X_rtp, X_rtm, X_rlm, X_rj)
!
      use calypso_mpi
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_sph_communicators
      use m_solver_SR
!
      integer (kind=kint), intent(in) :: NB
      real (kind=kreal), intent(inout) :: X_rtp(NB*nnod_rtp)
      real (kind=kreal), intent(inout) :: X_rtm(NB*nnod_rtm)
      real (kind=kreal), intent(inout) :: X_rlm(NB*nnod_rlm)
      real (kind=kreal), intent(inout)::  X_rj(NB*nnod_rj)
!
      real(kind = kreal) :: starttime, endtime(0:2)
      real(kind = kreal) :: etime_item_import(0:1) = 0.0d0
!
!
      call check_spherical_SRs_N(NB)
!
      if(iflag_sph_SRN .ne. iflag_import_UNDEFINED) return
!
      if(my_rank .eq. 0) write(*,*) 'test  send_recv with reg. import'
      iflag_sph_SRN = iflag_import_item
      starttime = MPI_WTIME()
      call all_sph_send_recv_N(NB, X_rtp, X_rtm, X_rlm, X_rj)
      endtime(0) = MPI_WTIME() - starttime
!
      if(my_rank .eq. 0) write(*,*) 'test  send_recv with rev. import'
      iflag_sph_SRN = iflag_import_rev
      starttime = MPI_WTIME()
      call all_sph_send_recv_N(NB, X_rtp, X_rtm, X_rlm, X_rj)
      endtime(1) = MPI_WTIME() - starttime
!
      endtime(1) = MPI_WTIME() - starttime
      call MPI_allREDUCE (endtime(0), etime_item_import(0), itwo,       &
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
      subroutine sel_sph_comm_routine(NB)
!
      use m_sph_communicators
      use calypso_mpi
!      use set_all2all_buffer
      use set_from_recv_buf_rev
!
      integer (kind=kint), intent(in) :: NB
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
!      call set_rev_all2all_import_tbl(nnod_rtp, nmax_sr_rtp,           &
!     &    nneib_domain_rtp, istack_sr_rtp, item_sr_rtp, irev_sr_rtp)
!      call set_rev_all2all_import_tbl(nnod_rtm, nmax_sr_rtp,           &
!     &    nneib_domain_rtm, istack_sr_rtm, item_sr_rtm, irev_sr_rtm)
!      call set_rev_all2all_import_tbl(nnod_rlm, nmax_sr_rj,            &
!     &    nneib_domain_rlm, istack_sr_rlm, item_sr_rlm, irev_sr_rlm)
!      call set_rev_all2all_import_tbl(nnod_rj, nmax_sr_rj,             &
!     &    nneib_domain_rj,  istack_sr_rj,  item_sr_rj,  irev_sr_rj)
!
      endtime(0:2) = 0.0d0
      iflag_sph_commN = iflag_alltoall
      starttime = MPI_WTIME()
      call all_sph_SR_core_N(NB)
      endtime(2) = MPI_WTIME() - starttime
!
      call set_reverse_import_table(nnod_rtp, ntot_item_sr_rtp,         &
     &    item_sr_rtp, irev_sr_rtp)
      call set_reverse_import_table(nnod_rtm, ntot_item_sr_rtm,         &
     &    item_sr_rtm, irev_sr_rtm)
      call set_reverse_import_table(nnod_rlm, ntot_item_sr_rlm,         &
     &    item_sr_rlm, irev_sr_rlm)
      call set_reverse_import_table(nnod_rj, ntot_item_sr_rj,           &
     &    item_sr_rj, irev_sr_rj)
!
      iflag_sph_commN = iflag_send_recv
      starttime = MPI_WTIME()
      call all_sph_SR_core_N(NB)
      endtime(0) = MPI_WTIME() - starttime
!
      iflag_sph_commN = iflag_alltoallv
      starttime = MPI_WTIME()
      call all_sph_SR_core_N(NB)
      endtime(1) = MPI_WTIME() - starttime
!
      call MPI_allREDUCE (endtime(0), etime_send_recv(0), ithree,       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      etime_shortest = etime_send_recv(0)
      iflag_sph_commN = iflag_send_recv
      do i = 1, 2
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
        write(*,*) '2: Time by MPI_AllToAll:  ',   etime_send_recv(2)
!
      end subroutine sel_sph_comm_routine
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine all_sph_send_recv_N(NB, X_rtp, X_rtm, X_rlm, X_rj)
!
      use spherical_SRs_N
!
      integer (kind=kint), intent(in) :: NB
      real (kind=kreal), intent(inout) :: X_rtp(NB*nnod_rtp)
      real (kind=kreal), intent(inout) :: X_rtm(NB*nnod_rtm)
      real (kind=kreal), intent(inout) :: X_rlm(NB*nnod_rlm)
      real (kind=kreal), intent(inout)::  X_rj(NB*nnod_rj)
!
!
      call send_recv_rj_2_rlm_N(NB, X_rj, X_rlm)
      call send_recv_rlm_2_rj_N(NB, X_rlm, X_rj)
      call send_recv_rtp_2_rtm_N(NB, X_rtp, X_rtm)
      call send_recv_rtm_2_rtp_N(NB, X_rtm, X_rtp)
!
      end subroutine all_sph_send_recv_N
!
! ----------------------------------------------------------------------
!
      subroutine all_sph_SR_core_N(NB)
!
      use spherical_SRs_N
!
      integer (kind=kint), intent(in) :: NB
!
!
      call check_calypso_rj_2_rlm_buf_N(NB)
      call check_calypso_rlm_2_rj_buf_N(NB)
      call check_calypso_rtp_2_rtm_buf_N(NB)
      call check_calypso_rtm_2_rtp_buf_N(NB)
!
      call calypso_sph_comm_rj_2_rlm_N(NB)
      call finish_send_recv_rj_2_rlm
      call calypso_sph_comm_rlm_2_rj_N(NB)
      call finish_send_recv_rlm_2_rj
      call calypso_sph_comm_rtp_2_rtm_N(NB)
      call finish_send_recv_rtp_2_rtm
      call calypso_sph_comm_rtm_2_rtp_N(NB)
      call finish_send_recv_rtm_2_rtp
!
      end subroutine all_sph_SR_core_N
!
! ----------------------------------------------------------------------
!
      subroutine check_spherical_SRs_N(NB)
!
      use calypso_mpi
      use m_spheric_parameter
      use select_calypso_SR
!
      integer (kind=kint), intent(in) :: NB
!
!
      call calypso_MPI_Barrier
      if(my_rank .eq. 0) write(*,*) 'check rtp -> rtm'
      call check_calypso_send_recv_N                                    &
     &         (NB, nneib_domain_rtp, iflag_self_rtp, istack_sr_rtp,    &
     &              nneib_domain_rtm, iflag_self_rtm, istack_sr_rtm)
      call calypso_MPI_Barrier
      if(my_rank .eq. 0) write(*,*) 'check rtm -> rtp'
      call check_calypso_send_recv_N                                    &
     &         (NB, nneib_domain_rtm, iflag_self_rtm, istack_sr_rtm,    &
     &              nneib_domain_rtp, iflag_self_rtp, istack_sr_rtp)
      call calypso_MPI_Barrier
      if(my_rank .eq. 0) write(*,*) 'check rj -> rlm'
      call check_calypso_send_recv_N                                    &
     &         (NB, nneib_domain_rj, iflag_self_rj, istack_sr_rj,       &
     &              nneib_domain_rlm, iflag_self_rlm, istack_sr_rlm)
      call calypso_MPI_Barrier
      if(my_rank .eq. 0) write(*,*) 'check rlm -> rj'
      call check_calypso_send_recv_N                                    &
     &         (NB, nneib_domain_rlm, iflag_self_rlm, istack_sr_rlm,    &
     &              nneib_domain_rj, iflag_self_rj, istack_sr_rj)
!
      end subroutine check_spherical_SRs_N
!
! ------------------------------------------------------------------
!
      end module init_spherical_SRs
