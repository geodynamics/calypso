!>@file   psf_global_nod_id.f90
!!@brief  module psf_global_nod_id
!!
!!@date  Programmed by H.Matsui in June, 2006
!
!>@brief Set global node ID list for sectioning
!!
!!@verbatim
!!      subroutine psf_global_nod_id_on_edge                            &
!!     &         (numedge, istack_internod, psf_list)
!!      subroutine psf_global_nod_id_on_node                            &
!!     &         (nod_comm, numnod, istack_internod, id_n_on_n)
!!      subroutine const_edge_comm_table_4_psf                          &
!!     &         (node, nod_comm, edge, psf_list)
!!@endverbatim
!
      module psf_global_nod_id
!
      use calypso_mpi
      use t_comm_table
      use m_machine_parameter
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
     &         (numedge, istack_internod, psf_list)
!
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: numedge
      integer(kind = kint_gl), intent(in) :: istack_internod(0:nprocs)
!
      type(sectioning_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: iedge, inum, i, ishift
!
!
      do i = 1, psf_list%internod_on_edge
        psf_list%id_global_psf(i) = i + istack_internod(my_rank)
      end do
!
      call SOLVER_SEND_RECV_int8_type(psf_list%totalnod_on_edge,        &
     &    psf_list%edge_comm_4_psf, psf_list%id_global_psf)
!
!
!$omp parallel workshare
      psf_list%id_n_on_e(1:numedge) = 0
!$omp end parallel workshare
!$omp parallel do private(inum,iedge)
      do inum = 1, psf_list%internod_on_edge
        iedge = psf_list%iedge_int_nod(inum)
        psf_list%id_n_on_e(iedge) = psf_list%id_global_psf(inum)
      end do
!$omp end parallel do
!
      ishift = psf_list%internod_on_edge
!$omp parallel do private(inum,iedge)
      do inum = 1, psf_list%externod_on_edge
        iedge = psf_list%iedge_ext_nod(inum)
        psf_list%id_n_on_e(iedge) = psf_list%id_global_psf(inum+ishift)
      end do
!$omp end parallel do
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
!  ---------------------------------------------------------------------
!
      subroutine const_edge_comm_table_4_psf                            &
     &         (node, nod_comm, edge, psf_list)
!
      use m_geometry_constants
      use t_geometry_data
      use t_edge_data
      use t_psf_geometry_list
      use t_belonged_element_4_node
      use const_element_comm_tables
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: nod_comm
      type(sectioning_list), intent(inout) :: psf_list
!
      type(edge_data) :: edge_4_psf
      type(belonged_table) :: belongs
!
      integer(kind = kint) :: i, iedge, i1, i2, j
!
!
      edge_4_psf%numedge =       psf_list%totalnod_on_edge
      edge_4_psf%internal_edge = psf_list%internod_on_edge
      edge_4_psf%nnod_4_edge = num_linear_edge
!
      call alloc_edge_connect(edge_4_psf, ione)
      call alloc_edge_geometory(edge_4_psf)
!
!$omp parallel do private(i,iedge,i1,i2)
      do i = 1, psf_list%internod_on_edge
        iedge = psf_list%iedge_int_nod(i)
        i1 = edge%ie_edge(iedge,1)
        i2 = edge%ie_edge(iedge,2)
        edge_4_psf%ie_edge(i,1) = i1
        edge_4_psf%ie_edge(i,2) = i2
        edge_4_psf%x_edge(i,1) = half*(node%xx(i1,1) + node%xx(i2,1))
        edge_4_psf%x_edge(i,2) = half*(node%xx(i1,2) + node%xx(i2,2))
        edge_4_psf%x_edge(i,3) = half*(node%xx(i1,3) + node%xx(i2,3))
        edge_4_psf%interior_edge(i) = 1
      end do
!$omp end parallel do
!
!$omp parallel do private(i,iedge,i1,i2, j)
      do i = 1, psf_list%externod_on_edge
        iedge = psf_list%iedge_ext_nod(i)
        i1 = edge%ie_edge(iedge,1)
        i2 = edge%ie_edge(iedge,2)
        j = i + psf_list%internod_on_edge
        edge_4_psf%ie_edge(j,1) = i1
        edge_4_psf%ie_edge(j,2) = i2
        edge_4_psf%x_edge(j,1) = half*(node%xx(i1,1) + node%xx(i2,1))
        edge_4_psf%x_edge(j,2) = half*(node%xx(i1,2) + node%xx(i2,2))
        edge_4_psf%x_edge(j,3) = half*(node%xx(i1,3) + node%xx(i2,3))
        edge_4_psf%interior_edge(j) = 0
      end do
!$omp end parallel do
!
      call const_edge_comm_table(node, nod_comm, belongs,               &
     &    psf_list%edge_comm_4_psf, edge_4_psf)
!
      call dealloc_edge_geometory(edge_4_psf)
      call dealloc_edge_connect(edge_4_psf)
!
      end subroutine const_edge_comm_table_4_psf
!
!  ---------------------------------------------------------------------
!
      end module psf_global_nod_id
