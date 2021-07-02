!>@file   psf_global_nod_id.f90
!!@brief  module psf_global_nod_id
!!
!!@date  Programmed by H.Matsui in June, 2006
!
!>@brief Set global node ID list for sectioning
!!
!!@verbatim
!!      subroutine psf_global_nod_id_on_edge(edge_comm,                 &
!!     &          numedge, istack_internod, psf_list, SR_sig, SR_il)
!!        type(communication_table), intent(in) :: edge_comm
!!        type(sectioning_list), intent(inout) :: psf_list
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine psf_global_nod_id_on_node(nod_comm,                  &
!!     &          numnod, istack_internod, id_n_on_n, SR_sig, SR_il)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!@endverbatim
!
      module psf_global_nod_id
!
      use calypso_mpi
      use t_comm_table
      use t_solver_SR
      use t_solver_SR_int8
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine psf_global_nod_id_on_edge(edge_comm,                   &
     &          numedge, istack_internod, psf_list, SR_sig, SR_il)
!
      use t_psf_geometry_list
      use solver_SR_type
!
      type(communication_table), intent(in) :: edge_comm
      integer(kind = kint), intent(in) :: numedge
      integer(kind = kint_gl), intent(in) :: istack_internod(0:nprocs)
!
      type(sectioning_list), intent(inout) :: psf_list
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint) :: iedge, icou
!
!
      icou = 0
      do iedge = 1, numedge
        if(psf_list%id_n_on_e(iedge) .gt. 0) then
          psf_list%id_n_on_e(iedge) = psf_list%id_n_on_e(iedge)         &
     &                               + istack_internod(my_rank)
          icou = icou + 1
        else
          psf_list%id_n_on_e(iedge) = 0
        end if
      end do
!
      call SOLVER_SEND_RECV_int8_type                                   &
     &   (numedge, edge_comm, SR_sig, SR_il, psf_list%id_n_on_e)
!
      end subroutine psf_global_nod_id_on_edge
!
!  ---------------------------------------------------------------------
!
      subroutine psf_global_nod_id_on_node(nod_comm,                    &
     &          numnod, istack_internod, id_n_on_n, SR_sig, SR_il)
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: nod_comm
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint_gl), intent(in) :: istack_internod(0:nprocs)
!
      integer(kind = kint_gl), intent(inout) :: id_n_on_n(numnod)
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint) :: inod
!
!
      do inod = 1, numnod
        if(id_n_on_n(inod) .gt. 0) then
          id_n_on_n(inod) = id_n_on_n(inod)                             &
     &                     + istack_internod(my_rank)
        end if
      end do
      write(*,*) 'istack_internod', istack_internod
!
      call SOLVER_SEND_RECV_int8_type(numnod, nod_comm,                 &
     &                                SR_sig, SR_il, id_n_on_n)
!
      end subroutine psf_global_nod_id_on_node
!
!  ---------------------------------------------------------------------
!
      end module psf_global_nod_id
