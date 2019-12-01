!> @file  add_comm_table_in_node_grp.f90
!!      module add_comm_table_in_node_grp
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Update group data for sleeve extender
!!
!!@verbatim
!!      subroutine add_comm_tbl_in_node_grp_mesh(num_pe, mesh, group)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!      subroutine add_comm_table_in_node_group                         &
!!     &         (num_pe, new_comm, old_nod_grp, new_nod_grp)
!!        type(communication_table), intent(in) :: new_comm
!!        type(group_data), intent(in) :: old_nod_grp
!!        type(group_data), intent(inout) :: new_nod_grp
!!@endverbatim
!
      module add_comm_table_in_node_grp
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_comm_table
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine add_comm_tbl_in_node_grp_mesh(num_pe, mesh, group)
!
      use copy_mesh_structures
!
      integer, intent(in) :: num_pe
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
      type(group_data) :: new_nod_grp
!
!
      call add_comm_table_in_node_group(num_pe,                         &
     &    mesh%nod_comm, group%nod_grp, new_nod_grp)
!
      call dealloc_group(group%nod_grp)
      call copy_group_data(new_nod_grp, group%nod_grp)
      call dealloc_group(new_nod_grp)
!
      end subroutine add_comm_tbl_in_node_grp_mesh
!
!------------------------------------------------------------------
!
      subroutine add_comm_table_in_node_group                           &
     &         (num_pe, new_comm, old_nod_grp, new_nod_grp)
!
      use set_parallel_file_name
!
      integer, intent(in) :: num_pe
      type(communication_table), intent(in) :: new_comm
      type(group_data), intent(in) :: old_nod_grp
      type(group_data), intent(inout) :: new_nod_grp
!
      character(len=kchara), parameter :: import_head = 'import'
      character(len=kchara), parameter :: export_head = 'export'
      integer(kind = kint) :: n_import(num_pe)
      integer(kind = kint) :: n_export(num_pe)
!
      integer(kind = kint) :: i, igrp, inum, ist, jst
      integer :: ip
!
!
      new_nod_grp%num_grp = old_nod_grp%num_grp + 2*num_pe
      new_nod_grp%num_item = old_nod_grp%num_item                       &
     &                    + new_comm%ntot_import + new_comm%ntot_export
!
      call alloc_group_num(new_nod_grp)
!
!
      if (new_nod_grp%num_grp .gt. 0) then
        new_nod_grp%grp_name(1:old_nod_grp%num_grp)                     &
     &          = old_nod_grp%grp_name(1:old_nod_grp%num_grp)
        new_nod_grp%istack_grp(0:old_nod_grp%num_grp)                   &
     &          = old_nod_grp%istack_grp(0:old_nod_grp%num_grp)
      end if
!
      do ip = 1, num_pe
        igrp = old_nod_grp%num_grp + ip
        new_nod_grp%grp_name(igrp)                                      &
     &        = add_process_id((ip-1), import_head)
        igrp = old_nod_grp%num_grp + num_pe + ip
        new_nod_grp%grp_name(igrp)                                      &
     &        = add_process_id((ip-1), export_head)
      end do
!
      n_import = 0
      n_export = 0
      do i = 1, new_comm%num_neib
        ip = new_comm%id_neib(i) + 1
        n_import(ip) = new_comm%istack_import(i)                        &
     &                - new_comm%istack_import(i-1)
        n_export(ip) = new_comm%istack_export(i)                        &
     &                - new_comm%istack_export(i-1)
      end do
!
      do ip = 1, num_pe
        igrp = old_nod_grp%num_grp + ip
        new_nod_grp%istack_grp(igrp) = new_nod_grp%istack_grp(igrp-1)   &
     &                                + n_import(ip)
      end do
      do ip = 1, num_pe
        igrp = old_nod_grp%num_grp + num_pe + ip
        new_nod_grp%istack_grp(igrp) = new_nod_grp%istack_grp(igrp-1)   &
     &                                + n_export(ip)
      end do
!
      call alloc_group_item(new_nod_grp)
!
!
      if (old_nod_grp%num_item .gt. 0) then
        new_nod_grp%item_grp(1:old_nod_grp%num_item)                    &
     &          = old_nod_grp%item_grp(1:old_nod_grp%num_item)
      end if
!
      do i = 1, new_comm%num_neib
        ip = new_comm%id_neib(i) + 1
        ist = new_comm%istack_import(i-1)
        jst = new_nod_grp%istack_grp(old_nod_grp%num_grp+ip-1)
        do inum = 1, n_import(ip)
          new_nod_grp%item_grp(jst+inum) = new_comm%item_import(ist+inum)
        end do
!
        ist = new_comm%istack_export(i-1)
        jst = new_nod_grp%istack_grp(old_nod_grp%num_grp+num_pe+ip-1)
        do inum = 1, n_export(ip)
          new_nod_grp%item_grp(jst+inum) = new_comm%item_export(ist+inum)
        end do
      end do
!
      end subroutine add_comm_table_in_node_group
!
!  ---------------------------------------------------------------------
!
      end module add_comm_table_in_node_grp
