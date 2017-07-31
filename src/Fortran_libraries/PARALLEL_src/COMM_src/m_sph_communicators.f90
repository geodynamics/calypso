!>@file   m_sph_communicators.f90
!!@brief  module m_sph_communicators
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!
!> @brief MPI communicator for spherical transforms
!!
!!@verbatim
!!      subroutine split_rtp_comms(nneib_domain_rtp, id_domain_rtp,     &
!!     &          nneib_domain_rj)
!!      subroutine calypso_RTP_MPI_abort(code, message)
!!      subroutine calypso_RJ_MPI_abort(code, message)
!!
!!      subroutine calypso_MPI_RTP_barrier
!!      subroutine calypso_MPI_RJ_barrier
!!@endverbatim
!!
!!@n @param  nneib_domain_rtp    Number of commucation for f(r,t,p)
!!@n @param  id_domain_rtp       process list of commucation for f(r,t,p)
!!@n @param  nneib_domain_rj     Number of commucation for f(r,j)
!
      module m_sph_communicators
!
      use m_precision
      use calypso_mpi
!
!
      implicit none
!
!
!>     MPI communicator for f(r,t,p)  data communication
      integer :: CALYPSO_RTP_COMM
!>      process ID for f(r,t,p) data communication (start from 0)
      integer(kind=kint) :: my_rank_rtp
!>      number of processes for f(r,t,m) data communication
      integer(kind=kint) :: nprocs_rtp
!
!>     MPI communicator for f(r,j)  data communication
      integer :: CALYPSO_RJ_COMM
!>      process ID for f(r,j) data communication (start from 0)
      integer(kind=kint) :: my_rank_rj
!>      number of processes for f(r,j) data communication
      integer(kind=kint) :: nprocs_rj
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine split_rtp_comms(nneib_domain_rtp, id_domain_rtp,       &
     &          nneib_domain_rj)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: nneib_domain_rtp
      integer(kind = kint), intent(in)                                  &
     &                     :: id_domain_rtp(nneib_domain_rtp)
      integer(kind = kint), intent(in) :: nneib_domain_rj
!
      integer(kind = kint) :: isum_rtp, isum_rj
      integer :: icolor_rj_rlm, icolor_rtp_rtm
      integer :: nprocs_rtp4, nprocs_rj4
      integer :: my_rank4, my_rank_rtp4, my_rank_rj4
!
!
      if(nneib_domain_rtp .gt. 1) then
        if(abs(id_domain_rtp(2) - id_domain_rtp(1)) .eq. 1) then
          icolor_rj_rlm =  int(mod(my_rank,nneib_domain_rtp))
          icolor_rtp_rtm                                                &
     &      = int((my_rank - mod(my_rank,nneib_domain_rtp))             &
     &                    / nneib_domain_rtp)
        else
          icolor_rtp_rtm =  int(mod(my_rank,nneib_domain_rj))
          icolor_rj_rlm                                                 &
     &      =  int((my_rank - mod(my_rank,nneib_domain_rj))             &
     &                    / nneib_domain_rj)
        end if
      else
        icolor_rtp_rtm = 0
        icolor_rj_rlm =  int(my_rank)
      end if
!
      my_rank4 = int(my_rank)
      call MPI_COMM_SPLIT(CALYPSO_COMM, icolor_rtp_rtm, my_rank4,       &
     &    CALYPSO_RTP_COMM, ierr_MPI)
      call MPI_COMM_SIZE(CALYPSO_RTP_COMM, nprocs_rtp4, ierr_MPI)
      call MPI_COMM_RANK(CALYPSO_RTP_COMM, my_rank_rtp4, ierr_MPI)
      nprocs_rtp = nprocs_rtp4
      my_rank_rtp = my_rank_rtp4
!
      call MPI_COMM_SPLIT(CALYPSO_COMM, icolor_rj_rlm, my_rank4,        &
     &    CALYPSO_RJ_COMM, ierr_MPI)
      call MPI_COMM_SIZE(CALYPSO_RJ_COMM, nprocs_rj4, ierr_MPI)
      call MPI_COMM_RANK(CALYPSO_RJ_COMM, my_rank_rj4, ierr_MPI)
      nprocs_rj = nprocs_rj4
      my_rank_rj = my_rank_rj4
!
      if(i_debug .eq. 0) return
!
      write(*,'(a,6i16)') 'my_rank, RTP_COMM, np_rtp, rank_rtp',        &
     &           my_rank, CALYPSO_RTP_COMM, nprocs_rtp, my_rank_rtp
      write(*,'(a,6i16)') 'my_rank, RJ_COMM,  np_rj,  rank_rj ',        &
     &          my_rank, CALYPSO_RJ_COMM, nprocs_rj, my_rank_rj
!
      CALL MPI_ALLREDUCE(my_rank,isum_rtp, 1, CALYPSO_INTEGER,MPI_SUM,  &
     &     CALYPSO_RTP_COMM,ierr_MPI)
      CALL MPI_ALLREDUCE(my_rank,isum_rj,  1, CALYPSO_INTEGER,MPI_SUM,  &
     &     CALYPSO_RJ_COMM,ierr_MPI)
      write(*,'(a,6i16)') 'SUM_test', my_rank, isum_rtp, isum_rj
!
      end subroutine split_rtp_comms
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_RTP_MPI_abort(code, message)
!
      integer,       intent(in)  ::  code
      character(len=*), intent(in)  ::  message
!
!
      write(*,*) ' ///// abnormal termination ///// ', code,            &
     &                                            ' ', message
!
      call  MPI_ABORT(CALYPSO_RTP_COMM, 998, ierr_MPI)
!
      end subroutine calypso_RTP_MPI_abort
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_RJ_MPI_abort(code, message)
!
      integer,       intent(in)  ::  code
      character(len=*), intent(in)  ::  message
!
!
      write(*,*) ' ///// abnormal termination ///// ', code,            &
     &                                            ' ', message
!
      call  MPI_ABORT(CALYPSO_RJ_COMM, 997, ierr_MPI)
!
      end subroutine calypso_RJ_MPI_abort
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_RTP_barrier
!
!
      call MPI_BARRIER(CALYPSO_RTP_COMM, ierr_MPI)
!
      end subroutine  calypso_MPI_RTP_barrier
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_RJ_barrier
!
!
      call MPI_BARRIER(CALYPSO_RJ_COMM, ierr_MPI)
!
      end subroutine  calypso_MPI_RJ_barrier
!
!  ---------------------------------------------------------------------
!
      end module m_sph_communicators
