!>@file   const_surface_comm_table.f90
!!@brief  module const_surface_comm_table
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine const_surf_comm_table                                &
!!     &         (node, nod_comm, surf_comm, surf)
!!      subroutine dealloc_surf_comm_table(surf_comm, surf)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: surf_comm
!!        type(surface_data), intent(inout) :: surf
!!
!!      subroutine surf_send_recv_test                                  &
!!     &         (node, surf, surf_comm, surf_check)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(communication_table), intent(in) :: surf_comm
!!        type(work_for_comm_check), intent(inout) :: surf_check
!!@endverbatim
!
      module const_surface_comm_table
!
      use m_precision
      use calypso_mpi
      use t_next_node_ele_4_node
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_comm_table
      use t_failed_export_list
!
      use m_machine_parameter
      use m_geometry_constants
!
      implicit none
!
      character(len=kchara), parameter :: txt_surf = 'surface'
!
      private :: txt_surf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table                                  &
     &         (node, nod_comm, surf_comm, surf)
!
      use t_para_double_numbering
      use t_element_double_number
      use t_const_comm_table
      use set_ele_id_4_node_type
      use const_global_element_ids
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(surface_data), intent(inout) :: surf
      type(communication_table), intent(inout) :: surf_comm
!
      type(node_ele_double_number) :: inod_dbl
      type(element_double_number) :: isurf_dbl
      type(element_around_node) :: neib_surf
      type(failed_table) :: fail_tbl_s
!
      integer(kind = kint) :: internal_num = 0
      integer(kind = kint_gl), allocatable :: istack_inersurf(:)
!
!
      call alloc_interior_surf(surf)
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl)
!
      call alloc_ele_double_number(surf%numsurf, isurf_dbl)
      call find_belonged_pe_4_surf(my_rank, inod_dbl,                   &
     &    surf%numsurf, surf%nnod_4_surf, surf%ie_surf,                 &
     &    internal_num, surf%interior_surf, isurf_dbl)
!
      call set_surf_id_4_node(node, surf, neib_surf)
!
      call alloc_failed_export(0, fail_tbl_s)
      call const_comm_table_by_connenct                                 &
     &   (txt_surf, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &    surf%x_surf, node, nod_comm, inod_dbl, isurf_dbl,             &
     &    neib_surf, surf_comm, fail_tbl_s)
      call dealloc_iele_belonged(neib_surf)
      call dealloc_failed_export(fail_tbl_s)
!
      allocate(istack_inersurf(0:nprocs))
      istack_inersurf(0:nprocs) = 0
!
      call count_number_of_node_stack(internal_num, istack_inersurf)
      call set_global_ele_id                                            &
     &   (txt_surf, surf%numsurf, istack_inersurf,                      &
     &    surf%interior_surf, surf_comm, surf%isurf_global)
      deallocate(istack_inersurf)
!
      call calypso_mpi_barrier
      call check_element_position                                       &
     &   (txt_surf, node%numnod, node%inod_global, surf%numsurf,        &
     &    surf%nnod_4_surf, surf%ie_surf, surf%isurf_global,            &
     &    surf%x_surf, inod_dbl, isurf_dbl, surf_comm)
      call dealloc_ele_double_number(isurf_dbl)
      call dealloc_double_numbering(inod_dbl)
!
      end subroutine const_surf_comm_table
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_comm_table(surf_comm, surf)
!
      type(communication_table), intent(inout) :: surf_comm
      type(surface_data), intent(inout) :: surf
!
      call dealloc_comm_table(surf_comm)
      call dealloc_interior_surf(surf)
!
      end subroutine dealloc_surf_comm_table
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine surf_send_recv_test                                    &
     &         (surf, surf_comm, surf_check)
!
      use t_work_for_comm_check
      use m_solver_SR
      use diff_geometory_comm_test
      use nod_phys_send_recv
      use solver_SR_type
      use mesh_send_recv_check
!
      type(surface_data), intent(in) :: surf
      type(communication_table), intent(in) :: surf_comm
      type(work_for_comm_check), intent(inout) :: surf_check
!
!
      call alloc_geom_4_comm_test(surf%numsurf, surf_check)
      call set_element_4_comm_test(surf%numsurf, surf%interior_surf,    &
     &                             surf%x_surf, surf_check%xx_test)
      call SOLVER_SEND_RECV_3_type(surf%numsurf, surf_comm,             &
     &                             SR_sig1, SR_r1, surf_check%xx_test)
!
      call ele_send_recv_check                                          &
     &   (surf%numsurf, surf%isurf_global, surf%x_surf, surf_check)
!
      if(i_debug .gt. 0)  write(*,*) my_rank,                           &
     &     'Failed communication for surface', surf_check%num_diff
      call collect_failed_comm(surf_check)
      if(my_rank .eq. 0) write(*,*) my_rank,                            &
     &   'Total Failed communication for surface',                      &
     &    surf_check%istack_diff_pe(nprocs)
!
      end subroutine surf_send_recv_test
!
! ----------------------------------------------------------------------
!
      end module const_surface_comm_table
