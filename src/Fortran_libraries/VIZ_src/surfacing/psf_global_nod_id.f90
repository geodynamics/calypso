!>@file   psf_global_nod_id.f90
!!@brief  module psf_global_nod_id
!!
!!@date  Programmed by H.Matsui in June, 2006
!
!>@brief Set global node ID list for sectioning
!!
!!@verbatim
!!      subroutine psf_global_nod_id_on_edge                            &
!!     &         (edge_comm, numedge, istack_internod, id_n_on_e)
!!      subroutine psf_global_nod_id_on_node                            &
!!     &         (nod_comm, numnod, istack_internod, id_n_on_n)
!!@endverbatim
!
      module psf_global_nod_id
!
      use calypso_mpi
      use t_comm_table
      use solver_SR_type
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine psf_global_nod_id_on_edge                              &
     &         (edge_comm, numedge, istack_internod, id_n_on_e)
!
      type(communication_table), intent(in) :: edge_comm
      integer(kind = kint), intent(in) :: numedge
      integer(kind = kint_gl), intent(in) :: istack_internod(0:nprocs)
!
      integer(kind = kint_gl), intent(inout) :: id_n_on_e(numedge)
!
      integer(kind = kint) :: iedge, icou
!
!
      icou = 0
      do iedge = 1, numedge
        if(id_n_on_e(iedge) .gt. 0) then
          id_n_on_e(iedge) = id_n_on_e(iedge)                           &
     &                      + istack_internod(my_rank)
          icou = icou + 1
        else
          id_n_on_e(iedge) = 0
        end if
      end do
!
      call SOLVER_SEND_RECV_int8_type(numedge, edge_comm, id_n_on_e)
!
      end subroutine psf_global_nod_id_on_edge
!
!  ---------------------------------------------------------------------
!
      subroutine psf_global_nod_id_on_node                              &
     &         (nod_comm, numnod, istack_internod, id_n_on_n)
!
      type(communication_table), intent(in) :: nod_comm
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint_gl), intent(in) :: istack_internod(0:nprocs)
!
      integer(kind = kint_gl), intent(inout) :: id_n_on_n(numnod)
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
      call SOLVER_SEND_RECV_int8_type(numnod,  nod_comm,  id_n_on_n)
!
      end subroutine psf_global_nod_id_on_node
!
!  ---------------------------------------------------------------------
!
      end module psf_global_nod_id
