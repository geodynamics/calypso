!>@file   m_sel_spherical_SRs.f90
!!@brief  module m_sel_spherical_SRs
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2014
!
!>@brief  Data communication selector for spherical transform
!!
!!@verbatim
!!      subroutine set_import_table_ctl(import_ctl)
!!      subroutine set_sph_comm_routine_ctl(send_recv_ctl)
!!
!!      subroutine finish_sph_send_recv(npe_send, isend_self)
!!      subroutine check_calypso_sph_buf_N(NB, nmax_sr,                 &
!!     &          npe_send, istack_send, npe_recv, istack_recv)
!!      subroutine sel_calypso_sph_comm_N(NB, nmax_sr,                  &
!!     &                  npe_send, isend_self, id_pe_send, istack_send,&
!!     &                  npe_recv, irecv_self, id_pe_recv, istack_recv,&
!!     &                  CALYPSO_SUB_COMM)
!!
!!      subroutine sel_calypso_to_send_N(NB, nnod_org, n_WS, nmax_sr,   &
!!     &                    npe_send, istack_send, inod_export,         &
!!     &                    X_org, WS)
!!
!!      subroutine sel_calypso_to_send_vector(NB, nnod_org, n_WS,       &
!!     &                    nmax_sr, npe_send, istack_send, inod_export,&
!!     &                    ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!!      subroutine sel_calypso_to_send_scalar(NB, nnod_org, n_WS,       &
!!     &                    nmax_sr, npe_send, istack_send, inod_export,&
!!     &                    ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!!      subroutine sel_calypso_to_send_tensor(NB, nnod_org, n_WS,       &
!!     &                    nmax_sr, npe_send, istack_send, inod_export,&
!!     &                    ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!!
!!      subroutine sel_sph_vector_from_recv(NB, nnod_new, n_WR, nmax_sr,&
!!     &                    npe_recv, istack_recv, inod_import,         &
!!     &                    irev_import, ncomp_X, i_fld_X, i_fld_WR,    &
!!     &                    WR, d_new)
!!      subroutine sel_sph_scalar_from_recv(NB, nnod_new, n_WR, nmax_sr,&
!!     &                    npe_recv, istack_recv, inod_import,         &
!!     &                    irev_import, ncomp_X, i_fld_X, i_fld_WR,    &
!!     &                    WR, d_new)
!!      subroutine sel_sph_tensor_from_recv(NB, nnod_new, n_WR, nmax_sr,&
!!     &                    npe_recv, istack_recv, inod_import,         &
!!     &                    irev_import, ncomp_X, i_fld_X, i_fld_WR,    &
!!     &                    WR, d_new)
!!
!!      subroutine sel_calypso_from_recv_N(NB, nnod_new, n_WR, nmax_sr, &
!!     &                    npe_recv, istack_recv, inod_import,         &
!!     &                    irev_import, WR, X_new)
!!@endverbatim
!
      module m_sel_spherical_SRs
!
      use m_precision
      use m_work_time
      use select_copy_from_recv
!
      implicit none
!
!>      Character flag to use import table
      character(len = kchara), parameter                                &
     &                       :: hd_import_item = 'regular_table'
!>      Character flag to use reverse import table
      character(len = kchara), parameter                                &
     &                       :: hd_import_rev =  'reversed_table'
!
!>      Character flag to use FFTW3
      character(len = kchara), parameter                                &
     &                       :: hd_sendrecv = 'SEND_RECV'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter                                &
     &                       :: hd_all2allv =  'AllToAllv'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter                                &
     &                       :: hd_all2all =  'AllToAll'
!
!
!
!>      Undefined flag
      integer(kind = kint), parameter :: iflag_SR_UNDEFINED = -1
!>      Integer flag to use MPI_Isend and MPI_IRecv
      integer(kind = kint), parameter :: iflag_send_recv = 0
!>      Integer flag to use MPI_AllToAllv
      integer(kind = kint), parameter :: iflag_alltoallv = 1
!>      Integer flag to use MPI_AllToAll
      integer(kind = kint), parameter :: iflag_alltoall =  2
!
!
!>      Data communication mode for arbitrary size data
      integer(kind = kint) :: iflag_sph_SRN =   iflag_import_UNDEFINED
!
!>      Data communication mode for arbitrary size data
      integer(kind = kint) :: iflag_sph_commN = iflag_SR_UNDEFINED
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_import_table_ctl(import_ctl)
!
      use m_solver_SR
      use skip_comment_f
      use calypso_solver_SR
!
      character(len = kchara), intent(in) :: import_ctl
!
!
      if(cmp_no_case(import_ctl, hd_import_item)) then
        iflag_sph_SRN = iflag_import_item
      else if(cmp_no_case(import_ctl, hd_import_rev)) then
        iflag_sph_SRN = iflag_import_rev
      else
        iflag_sph_SRN = iflag_import_UNDEFINED
      end if
!
      end subroutine set_import_table_ctl
!
! ------------------------------------------------------------------
!
      subroutine set_sph_comm_routine_ctl(send_recv_ctl)
!
      use m_solver_SR
      use skip_comment_f
!
      character(len = kchara), intent(in) :: send_recv_ctl
!
!
      if(cmp_no_case(send_recv_ctl, hd_sendrecv)     ) then
        iflag_sph_commN = iflag_send_recv
!      else if(cmp_no_case(send_recv_ctl, hd_all2allv)) then
!        iflag_sph_commN = iflag_alltoallv
!      else if(cmp_no_case(send_recv_ctl, hd_all2all) ) then
!        iflag_sph_commN = iflag_alltoall
      else
        iflag_sph_commN = iflag_SR_UNDEFINED
      end if
!
      end subroutine set_sph_comm_routine_ctl
!
!-----------------------------------------------------------------------
!
      subroutine finish_sph_send_recv(npe_send, isend_self)
!
      use m_solver_SR
      use select_calypso_SR
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
!
!
      if(iflag_sph_commN .ne. iflag_send_recv) return
      call finish_calypso_send_recv(npe_send, isend_self)
!
      end subroutine finish_sph_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_calypso_sph_buf_N(NB, nmax_sr,                   &
     &          npe_send, istack_send, npe_recv, istack_recv)
!
      use m_solver_SR
      use set_to_send_buffer
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nmax_sr
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      integer(kind = kint) :: nitem
!
!
!      if     (iflag_sph_commN .eq. iflag_alltoall) then
!        nitem = npe_send*nmax_sr
!        call resize_work_sph_SR(NB, npe_send, npe_recv, nitem, nitem)
!      else
        call resize_work_sph_SR(NB, npe_send, npe_recv,                 &
     &      istack_send(npe_send), istack_recv(npe_recv))
!      end if
!
      end subroutine check_calypso_sph_buf_N
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_sph_comm_N(NB, nmax_sr,                    &
     &                  npe_send, isend_self, id_pe_send, istack_send,  &
     &                  npe_recv, irecv_self, id_pe_recv, istack_recv,  &
     &                  CALYPSO_SUB_COMM)
!
      use m_solver_SR
      use calypso_SR_core
!      use calypso_AlltoAll_core
      use set_from_recv_buf_rev
!      use set_all2all_buffer
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: NB, nmax_sr
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
!
      call start_eleps_time(37)
!      if     (iflag_sph_commN .eq. iflag_alltoall) then
!        call calypso_AllToAll_Ncore(NB, nmax_sr, CALYPSO_SUB_COMM)
!        call clear_addtional_AllToAll_recv(NB, nmax_sr, npe_recv, WR)
!      else if(iflag_sph_commN .eq. iflag_alltoallv) then
!        call calypso_AllToAllv_Ncore                                    &
!     &     (NB, npe_send, istack_send, istack_recv, CALYPSO_SUB_COMM)
!        call clear_addtional_SR_recv(NB, istack_recv(npe_recv), WR)
!      else
        call calypso_send_recv_core                                     &
     &         (NB, npe_send, isend_self, id_pe_send, istack_send,      &
     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
        call clear_addtional_SR_recv(NB, istack_recv(npe_recv), WR)
!      end if
      call end_eleps_time(37)
!
      end subroutine sel_calypso_sph_comm_N
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_to_send_N(NB, nnod_org, n_WS, nmax_sr,     &
     &                    npe_send, istack_send, inod_export,           &
     &                    X_org, WS)
!
!      use set_all2all_buffer
      use set_to_send_buffer
!
      integer(kind = kint), intent(in) :: NB, nnod_org
      integer(kind = kint), intent(in) :: nmax_sr, n_WS
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call start_eleps_time(36)
!      if(    iflag_sph_commN .eq. iflag_alltoall) then
!        call set_to_all2all_buf_N(NB, nnod_org, nmax_sr, npe_send,     &
!     &    istack_send, inod_export, X_org, WS(1))
!      else
        call set_to_send_buf_N(NB, nnod_org, istack_send(npe_send),     &
     &      inod_export, X_org, WS(1))
!      end if
      call end_eleps_time(36)
!
      end subroutine sel_calypso_to_send_N
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_to_send_vector(NB, nnod_org, n_WS,         &
     &                    nmax_sr, npe_send, istack_send, inod_export,  &
     &                    ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!
!      use sph_field_to_all2all
      use field_to_send_buffer
!
      integer(kind = kint), intent(in) :: NB, i_fld_WS, nmax_sr, n_WS
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_org
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
      real (kind=kreal), intent(in)::    d_org(nnod_org,NB)
!
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call start_eleps_time(36)
!      if(    iflag_sph_commN .eq. iflag_alltoall) then
!        call set_to_all2all_buf_vector(NB, nnod_org, nmax_sr, npe_send,&
!     &      istack_send, inod_export, ncomp_X, i_fld_X, i_fld_WS,      &
!     &      d_org, WS(1))
!      else
        call set_to_send_buf_vector(NB, nnod_org,                       &
     &      istack_send(npe_send), inod_export, ncomp_X,                &
     &      i_fld_X, i_fld_WS, d_org, WS(1))
!      end if
      call end_eleps_time(36)
!
      end subroutine sel_calypso_to_send_vector
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_to_send_scalar(NB, nnod_org, n_WS,         &
     &                    nmax_sr, npe_send, istack_send, inod_export,  &
     &                    ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!
!      use sph_field_to_all2all
      use field_to_send_buffer
!
      integer(kind = kint), intent(in) :: NB, i_fld_WS, nmax_sr, n_WS
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_org
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
      real (kind=kreal), intent(in)::    d_org(nnod_org,NB)
!
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call start_eleps_time(36)
!      if(    iflag_sph_commN .eq. iflag_alltoall) then
!        call set_to_all2all_buf_scalar(NB, nnod_org, nmax_sr, npe_send,&
!     &      istack_send, inod_export, ncomp_X, i_fld_X, i_fld_WS,      &
!     &      d_org, WS(1))
!      else
        call set_to_send_buf_scalar(NB, nnod_org,                       &
     &      istack_send(npe_send), inod_export, ncomp_X,                &
     &      i_fld_X, i_fld_WS, d_org, WS(1))
!      end if
      call end_eleps_time(36)
!
      end subroutine sel_calypso_to_send_scalar
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_to_send_tensor(NB, nnod_org, n_WS,         &
     &                    nmax_sr, npe_send, istack_send, inod_export,  &
     &                    ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!
!      use sph_field_to_all2all
      use field_to_send_buffer
!
      integer(kind = kint), intent(in) :: NB, i_fld_WS, nmax_sr, n_WS
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_org
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
      real (kind=kreal), intent(in)::    d_org(nnod_org,NB)
!
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call start_eleps_time(36)
!      if(    iflag_sph_commN .eq. iflag_alltoall) then
!        call set_to_all2all_buf_tensor(NB, nnod_org, nmax_sr, npe_send,&
!     &      istack_send, inod_export, ncomp_X, i_fld_X, i_fld_WS,      &
!     &      d_org, WS(1))
!      else
        call set_to_send_buf_tensor(NB, nnod_org,                       &
     &      istack_send(npe_send), inod_export, ncomp_X,                &
     &      i_fld_X, i_fld_WS, d_org, WS(1))
!      end if
      call end_eleps_time(36)
!
      end subroutine sel_calypso_to_send_tensor
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_sph_vector_from_recv(NB, nnod_new, n_WR, nmax_sr,  &
     &                    npe_recv, istack_recv, inod_import,           &
     &                    irev_import, ncomp_X, i_fld_X, i_fld_WR,      &
     &                    WR, d_new)
!
      use field_to_send_buffer
!      use sph_field_to_all2all
!
      integer(kind = kint), intent(in) :: nnod_new, ncomp_X, i_fld_X
      integer(kind = kint), intent(in) :: NB, nmax_sr, n_WR, i_fld_WR
!
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout) :: WR(n_WR)
!
      real (kind=kreal), intent(inout):: d_new(nnod_new,ncomp_X)
!
!
      call start_eleps_time(38)
!      if(     iflag_sph_commN .eq. iflag_alltoall                      &
!     &  .and. iflag_sph_SRN .eq. iflag_import_item) then
!        call set_from_all2all_buf_vector(NB, nnod_new, nmax_sr,        &
!     &      npe_recv, istack_recv, inod_import,                        &
!     &      ncomp_X, i_fld_X, i_fld_WR, WR(1), d_new)
!      else if(iflag_sph_commN .eq. iflag_alltoall) then
!        call set_from_all2all_buf_rev_vect(NB, nnod_new, nmax_sr,      &
!     &      npe_recv, irev_import, ncomp_X, i_fld_X, i_fld_WR,         &
!     &      WR(1), d_new)
      if(iflag_sph_SRN .eq. iflag_import_item) then
        call set_from_recv_buf_vector(NB, nnod_new,                     &
     &      istack_recv(npe_recv), inod_import,                         &
     &      ncomp_X, i_fld_X, i_fld_WR, WR(1), d_new)
      else
        call set_from_recv_buf_rev_vector(NB, nnod_new,                 &
     &      istack_recv(npe_recv), irev_import,                         &
     &      ncomp_X, i_fld_X, i_fld_WR, WR(1), d_new)
      end if
      call end_eleps_time(38)
!
      end subroutine sel_sph_vector_from_recv
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_scalar_from_recv(NB, nnod_new, n_WR, nmax_sr,  &
     &                    npe_recv, istack_recv, inod_import,           &
     &                    irev_import, ncomp_X, i_fld_X, i_fld_WR,      &
     &                    WR, d_new)
!
      use field_to_send_buffer
!      use sph_field_to_all2all
!
      integer(kind = kint), intent(in) :: nnod_new, ncomp_X, i_fld_X
      integer(kind = kint), intent(in) :: NB, nmax_sr, n_WR, i_fld_WR
!
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout) :: WR(n_WR)
!
      real (kind=kreal), intent(inout):: d_new(nnod_new,ncomp_X)
!
!
      call start_eleps_time(38)
!      if(     iflag_sph_commN .eq. iflag_alltoall                      &
!     &  .and. iflag_sph_SRN .eq. iflag_import_item) then
!        call set_from_all2all_buf_scalar(NB, nnod_new, nmax_sr,        &
!     &      npe_recv, istack_recv, inod_import,                        &
!     &      ncomp_X, i_fld_X, i_fld_WR, WR(1), d_new)
!      else if(iflag_sph_commN .eq. iflag_alltoall) then
!        call set_from_all2all_buf_rev_scl(NB, nnod_new, nmax_sr,       &
!     &      npe_recv, irev_import, ncomp_X, i_fld_X, i_fld_WR,         &
!     &      WR(1), d_new)
      if(iflag_sph_SRN .eq. iflag_import_item) then
        call set_from_recv_buf_scalar(NB, nnod_new,                     &
     &      istack_recv(npe_recv), inod_import,                         &
     &      ncomp_X, i_fld_X, i_fld_WR, WR(1), d_new)
      else
        call set_from_recv_buf_rev_scalar(NB, nnod_new,                 &
     &      istack_recv(npe_recv), irev_import,                         &
     &      ncomp_X, i_fld_X, i_fld_WR, WR(1), d_new)
      end if
      call end_eleps_time(38)
!
      end subroutine sel_sph_scalar_from_recv
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_tensor_from_recv(NB, nnod_new, n_WR, nmax_sr,  &
     &                    npe_recv, istack_recv, inod_import,           &
     &                    irev_import, ncomp_X, i_fld_X, i_fld_WR,      &
     &                    WR, d_new)
!
      use field_to_send_buffer
!      use sph_field_to_all2all
!
      integer(kind = kint), intent(in) :: nnod_new, ncomp_X, i_fld_X
      integer(kind = kint), intent(in) :: NB, nmax_sr, n_WR, i_fld_WR
!
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout) :: WR(n_WR)
!
      real (kind=kreal), intent(inout):: d_new(nnod_new,ncomp_X)
!
!
      call start_eleps_time(38)
!      if(     iflag_sph_commN .eq. iflag_alltoall                      &
!     &  .and. iflag_sph_SRN .eq. iflag_import_item) then
!        call set_from_all2all_buf_tensor(NB, nnod_new, nmax_sr,        &
!     &      npe_recv, istack_recv, inod_import,                        &
!     &      ncomp_X, i_fld_X, i_fld_WR, WR(1), d_new)
!      else if(iflag_sph_commN .eq. iflag_alltoall) then
!        call set_from_all2all_buf_rev_tsr(NB, nnod_new, nmax_sr,       &
!     &      npe_recv, irev_import, ncomp_X, i_fld_X, i_fld_WR,         &
!     &      WR(1), d_new)
      if(iflag_sph_SRN .eq. iflag_import_item) then
        call set_from_recv_buf_tensor(NB, nnod_new,                     &
     &      istack_recv(npe_recv), inod_import,                         &
     &      ncomp_X, i_fld_X, i_fld_WR, WR(1), d_new)
      else
        call set_from_recv_buf_rev_tensor(NB, nnod_new,                 &
     &      istack_recv(npe_recv), irev_import,                         &
     &      ncomp_X, i_fld_X, i_fld_WR, WR(1), d_new)
      end if
      call end_eleps_time(38)
!
      end subroutine sel_sph_tensor_from_recv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_from_recv_N(NB, nnod_new, n_WR, nmax_sr,   &
     &                    npe_recv, istack_recv, inod_import,           &
     &                    irev_import, WR, X_new)
!
!      use set_all2all_buffer
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: NB, nnod_new
      integer(kind = kint), intent(in) :: nmax_sr, n_WR
!
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout) :: WR(n_WR)
!
      real (kind=kreal), intent(inout) :: X_new(NB*nnod_new)
!
!
      call start_eleps_time(38)
!      if(     iflag_sph_commN .eq. iflag_alltoall                      &
!     &  .and. iflag_sph_SRN .eq. iflag_import_item) then
!        call set_from_all2all_buf_N(NB, nnod_new, nmax_sr,             &
!     &      npe_recv, istack_recv, inod_import, WR(1), X_new)
!      else if(iflag_sph_commN .eq. iflag_alltoall) then
!        call set_from_all2all_rev_N(NB, nnod_new, nmax_sr,             &
!     &      npe_recv, irev_import, WR(1), X_new)
      if(iflag_sph_SRN .eq. iflag_import_item) then
        call set_from_recv_buf_N(NB, nnod_new,                          &
     &      istack_recv(npe_recv), inod_import, WR(1), X_new)
      else
        call set_from_recv_buf_rev_N(NB, nnod_new,                      &
     &      istack_recv(npe_recv), irev_import, WR(1), X_new)
      end if
      call end_eleps_time(38)
!
      end subroutine sel_calypso_from_recv_N
!
!-----------------------------------------------------------------------
!
      end module m_sel_spherical_SRs
