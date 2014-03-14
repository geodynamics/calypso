!>@file   spherical_SRs_int.f90
!!@brief  module spherical_SRs_int
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Integer data communications
!!@n      for spherical harmonics transform
!!
!!@verbatim
!!      subroutine init_sph_send_recv_int(iX_rtp, iX_rtm, iX_rlm, iX_rj)
!!
!!      subroutine send_recv_rtp_2_rtm_int(iX_rtp, iX_rtm)
!!      subroutine send_recv_rtm_2_rtp_int(iX_rtm, iX_rtp)
!!      subroutine send_recv_rj_2_rlm_int(iX_rj, iX_rlm)
!!      subroutine send_recv_rlm_2_rj_int(iX_rlm, iX_rj)
!!@endverbatim
!!
!!@n @param  iX_rtp(nnod_rtp)  @f$ f(r,\theta,\phi) @f$
!!@n @param  iX_rtm(nnod_rtm)  @f$ f(r,\theta,m) @f$
!!@n @param  iX_rlm(nnod_rlm)  @f$ f(r,l,m) @f$
!!@n @param  iX_rj(nnod_rj)    @f$ f(r,j) @f$
!
      module spherical_SRs_int
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      use select_calypso_SR
!
      implicit none
!
!>      Data communication mode for integer
      integer(kind = kint) :: iflag_sph_SR_int = iflag_import_item
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_sph_send_recv_int(iX_rtp, iX_rtm, iX_rlm, iX_rj)
!
      use calypso_mpi
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      use m_solver_SR
!
      integer (kind=kint), intent(inout):: iX_rtp(nnod_rtp)
      integer (kind=kint), intent(inout):: iX_rtm(nnod_rtm)
      integer (kind=kint), intent(inout):: iX_rlm(nnod_rlm)
      integer (kind=kint), intent(inout):: iX_rj(nnod_rj)
!
      integer (kind=kint) :: nneib_max_send, nneib_max_recv
      integer (kind=kint) :: nnod_max_send,  nnod_max_recv
!
      real(kind = kreal) :: stime, etime
      real(kind = kreal) :: etime_item_import, etime_irev_import
!
!
      nneib_max_send = nneib_domain_rtp
      nneib_max_recv = nneib_domain_rtm
      nnod_max_send =  ntot_item_sr_rtp
      nnod_max_recv =  ntot_item_sr_rtm
!
      nneib_max_send = max(nneib_max_send,nneib_domain_rtm)
      nneib_max_recv = max(nneib_max_recv,nneib_domain_rtp)
      nnod_max_send =  max(nnod_max_send,ntot_item_sr_rtm)
      nnod_max_recv =  max(nnod_max_recv,ntot_item_sr_rtp)
!
      nneib_max_send = max(nneib_max_send,nneib_domain_rj)
      nneib_max_recv = max(nneib_max_recv,nneib_domain_rlm)
      nnod_max_send =  max(nnod_max_send,ntot_item_sr_rj)
      nnod_max_recv =  max(nnod_max_recv,ntot_item_sr_rlm)
!
      nneib_max_send = max(nneib_max_send,nneib_domain_rlm)
      nneib_max_recv = max(nneib_max_recv,nneib_domain_rj)
      nnod_max_send =  max(nnod_max_send,ntot_item_sr_rlm)
      nnod_max_recv =  max(nnod_max_recv,ntot_item_sr_rj)
!
      call resize_iwork_sph_SR(nneib_max_send, nneib_max_recv,          &
     &    ntot_item_sr_rtp, ntot_item_sr_rtm)
!
!
!
      iflag_sph_SR_int = iflag_import_item
      stime = MPI_WTIME()
      call send_recv_rtp_2_rtm_int(iX_rtp, iX_rtm)
      call send_recv_rtm_2_rtp_int(iX_rtm, iX_rtp)
      call send_recv_rj_2_rlm_int(iX_rj, iX_rlm)
      call send_recv_rlm_2_rj_int(iX_rlm, iX_rj)
!
      etime = MPI_WTIME() - stime
      call MPI_allREDUCE (etime, etime_item_import, ione,               &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      iflag_sph_SR_int = iflag_import_rev
      stime = MPI_WTIME()
      call send_recv_rtp_2_rtm_int(iX_rtp, iX_rtm)
      call send_recv_rtm_2_rtp_int(iX_rtm, iX_rtp)
      call send_recv_rj_2_rlm_int(iX_rj, iX_rlm)
      call send_recv_rlm_2_rj_int(iX_rlm, iX_rj)
!
      etime = MPI_WTIME() - stime
      call MPI_allREDUCE (etime, etime_irev_import, ione,               &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if(etime_irev_import .le. etime_item_import) then
        iflag_sph_SR_int = iflag_import_rev
      end if
!
      if(my_rank .eq. 0) then
        write(*,*)                                                      &
     &     'SPH_sr_flag, time_by_item_inport, time_by_irev_inport'
        write(*,*) iflag_sph_SR_int,                                    &
     &            iflag_import_item, iflag_import_rev
      end if
!
      end subroutine init_sph_send_recv_int
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine send_recv_rtp_2_rtm_int(iX_rtp, iX_rtm)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      integer (kind=kint), intent(in)::    iX_rtp(nnod_rtp)
      integer (kind=kint), intent(inout):: iX_rtm(nnod_rtm)
!
!
      call sel_calypso_send_recv_int                                    &
     &             (iflag_sph_SR_int, nnod_rtp, nnod_rtm,               &
     &              nneib_domain_rtp, iflag_self_rtp,                   &
     &              id_domain_rtp, istack_sr_rtp, item_sr_rtp,          &
     &              nneib_domain_rtm, iflag_self_rtm,                   &
     &              id_domain_rtm, istack_sr_rtm, item_sr_rtm,          &
     &              irev_sr_rtm, iX_rtp, iX_rtm)
!
      end subroutine send_recv_rtp_2_rtm_int
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_rtm_2_rtp_int(iX_rtm, iX_rtp)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      integer (kind=kint), intent(in)::    iX_rtm(nnod_rtm)
      integer (kind=kint), intent(inout):: iX_rtp(nnod_rtp)
!
!
      call sel_calypso_send_recv_int                                    &
     &             (iflag_sph_SR_int, nnod_rtm, nnod_rtp,               &
     &              nneib_domain_rtm, iflag_self_rtm,                   &
     &              id_domain_rtm, istack_sr_rtm, item_sr_rtm,          &
     &              nneib_domain_rtp, iflag_self_rtp,                   &
     &              id_domain_rtp, istack_sr_rtp, item_sr_rtp,          &
     &              irev_sr_rtp, iX_rtm, iX_rtp)
!
      end subroutine send_recv_rtm_2_rtp_int
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_rj_2_rlm_int(iX_rj, iX_rlm)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      integer (kind=kint), intent(in)::    iX_rj(nnod_rj)
      integer (kind=kint), intent(inout):: iX_rlm(nnod_rlm)
!
!
      call sel_calypso_send_recv_int                                    &
     &             (iflag_sph_SR_int, nnod_rj, nnod_rlm,                &
     &              nneib_domain_rj, iflag_self_rj,                     &
     &              id_domain_rj, istack_sr_rj, item_sr_rj,             &
     &              nneib_domain_rlm, iflag_self_rlm,                   &
     &              id_domain_rlm, istack_sr_rlm, item_sr_rlm,          &
     &              irev_sr_rlm, iX_rj, iX_rlm)
!
      end subroutine send_recv_rj_2_rlm_int
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_rlm_2_rj_int(iX_rlm, iX_rj)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      integer (kind=kint), intent(in)::    iX_rlm(nnod_rlm)
      integer (kind=kint), intent(inout):: iX_rj(nnod_rj)
!
!
      call sel_calypso_send_recv_int                                    &
     &             (iflag_sph_SR_int, nnod_rlm, nnod_rj,                &
     &              nneib_domain_rlm, iflag_self_rlm,                   &
     &              id_domain_rlm, istack_sr_rlm, item_sr_rlm,          &
     &              nneib_domain_rj, iflag_self_rj,                     &
     &              id_domain_rj, istack_sr_rj, item_sr_rj,             &
     &              irev_sr_rj, iX_rlm, iX_rj)
!
      end subroutine send_recv_rlm_2_rj_int
!
! ----------------------------------------------------------------------
!
      end module spherical_SRs_int
