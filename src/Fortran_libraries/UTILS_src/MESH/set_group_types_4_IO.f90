!set_group_types_4_IO.f90
!     module set_group_types_4_IO
!
!      written by H. Matsui on Dec., 2008
!
!      subroutine set_grp_data_type_from_IO(group)
!      subroutine set_grp_data_type_to_IO(group)
!        type(mesh_groups), intent(inout) :: group
!
      module set_group_types_4_IO
!
      use m_precision
!
      implicit  none
!
!      private :: set_node_grp_type_to_IO
!      private :: set_ele_grp_type_to_IO
!      private :: set_surface_grp_type_to_IO
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_grp_data_type_from_IO(group)
!
      use  t_mesh_data
!
      type(mesh_groups), intent(inout) :: group
!
!
      call set_nod_grp_type_from_IO(group%nod_grp)
      call set_ele_grp_type_from_IO(group%ele_grp)
      call set_surf_grp_type_from_IO(group%surf_grp)
!
      end subroutine set_grp_data_type_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_grp_data_type_to_IO(group)
!
      use  t_mesh_data
!
      type(mesh_groups), intent(inout) :: group
!
!
      call set_node_grp_type_to_IO(group%nod_grp)
      call set_ele_grp_type_to_IO(group%ele_grp)
      call set_surface_grp_type_to_IO(group%surf_grp)
!
      end subroutine set_grp_data_type_to_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_nod_grp_type_from_IO(nod_grp)
!
      use t_group_data
      use m_read_boundary_data
!
      type(group_data), intent(inout) :: nod_grp
      integer(kind = kint) :: igrp
!
!
      nod_grp%num_grp = num_bc_dummy
      call allocate_grp_type_num(nod_grp)
!
      do igrp = 1, nod_grp%num_grp
        nod_grp%grp_name(igrp) =   bc_name_dummy(igrp)
        nod_grp%istack_grp(igrp) = bc_istack_dummy(igrp)
        nod_grp%nitem_grp(igrp) =  bc_istack_dummy(igrp)                &
     &                             - bc_istack_dummy(igrp-1)
      end do
!
      nod_grp%num_item = num_nod_bc_dummy
      call allocate_grp_type_item(nod_grp)
!
      if (nod_grp%num_item .gt. 0) then
        nod_grp%item_grp(1:nod_grp%num_item)                            &
     &        = bc_item_dummy(1:nod_grp%num_item)
      end if
!
      call deallocate_bc_item_dummy
!
      end subroutine set_nod_grp_type_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_ele_grp_type_from_IO(ele_grp)
!
      use t_group_data
      use m_read_boundary_data
!
      type(group_data), intent(inout) :: ele_grp
      integer(kind = kint) :: igrp
!
!
      ele_grp%num_grp = num_mat_dummy
      call allocate_grp_type_num(ele_grp)
!
      do igrp = 1, ele_grp%num_grp
        ele_grp%grp_name(igrp) =   mat_name_dummy(igrp)
        ele_grp%istack_grp(igrp) = mat_istack_dummy(igrp)
        ele_grp%nitem_grp(igrp) =  mat_istack_dummy(igrp)               &
     &                             - mat_istack_dummy(igrp-1)
      end do
!
      ele_grp%num_item = num_mat_bc_dummy
      call allocate_grp_type_item(ele_grp)
!
      if (ele_grp%num_item .gt. 0) then
        ele_grp%item_grp(1:ele_grp%num_item)                            &
     &        = mat_item_dummy(1:ele_grp%num_item)
      end if
!
      call deallocate_bc_ele_item_dummy
!
      end subroutine set_ele_grp_type_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grp_type_from_IO(sf_grp)
!
      use t_group_data
      use m_read_boundary_data
!
      type(surface_group_data), intent(inout) :: sf_grp
      integer(kind = kint) :: igrp
!
!
      sf_grp%num_grp = num_surf_dummy
      call allocate_sf_grp_type_num(sf_grp)
!
      do igrp = 1, sf_grp%num_grp
        sf_grp%grp_name(igrp) =   surf_name_dummy(igrp)
        sf_grp%istack_grp(igrp) = surf_istack_dummy(igrp)
        sf_grp%nitem_grp(igrp) =  surf_istack_dummy(igrp)               &
     &                            - surf_istack_dummy(igrp-1)
      end do
!
      sf_grp%num_item = num_surf_bc_dummy
      call allocate_sf_grp_type_item(sf_grp)
!
      if (sf_grp%num_item .gt. 0) then
        sf_grp%item_sf_grp(1,1:sf_grp%num_item)                         &
     &        = surf_item_dummy(1:sf_grp%num_item,1)
        sf_grp%item_sf_grp(2,1:sf_grp%num_item)                         &
     &        = surf_item_dummy(1:sf_grp%num_item,2)
      end if
!
      call deallocate_bc_sf_item_dummy
!
      end subroutine set_surf_grp_type_from_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_node_grp_type_to_IO(nod_grp)
!
      use t_group_data
      use m_read_boundary_data
!
      type(group_data), intent(inout) :: nod_grp
      integer(kind = kint) :: igrp
!
!
      num_bc_dummy =     nod_grp%num_grp
      num_nod_bc_dummy = nod_grp%num_item
      call allocate_bc_stack_dummy
!
      bc_istack_dummy(0) = nod_grp%istack_grp(0)
      do igrp = 1, nod_grp%num_grp
        bc_name_dummy(igrp) =   nod_grp%grp_name(igrp)
        bc_istack_dummy(igrp) = nod_grp%istack_grp(igrp)
      end do
!
      call allocate_bc_item_dummy
      bc_item_dummy(1:nod_grp%num_item)                                 &
     &    = nod_grp%item_grp(1:nod_grp%num_item)
!
      end subroutine set_node_grp_type_to_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_ele_grp_type_to_IO(ele_grp)
!
      use t_group_data
      use m_read_boundary_data
!
      type(group_data), intent(inout) :: ele_grp
      integer(kind = kint) :: igrp
!
!
      num_mat_dummy =    ele_grp%num_grp
      num_mat_bc_dummy = ele_grp%num_item
      call allocate_bc_ele_stack_dummy
!
      mat_istack_dummy(0) = ele_grp%istack_grp(0)
      do igrp = 1, ele_grp%num_grp
        mat_name_dummy(igrp) = ele_grp%grp_name(igrp)
        mat_istack_dummy(igrp) = ele_grp%istack_grp(igrp)
      end do
!
      call allocate_bc_ele_item_dummy
      mat_item_dummy(1:ele_grp%num_item)                                &
     &        = ele_grp%item_grp(1:ele_grp%num_item)
!
      end subroutine set_ele_grp_type_to_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_surface_grp_type_to_IO(sf_grp)
!
      use t_group_data
      use m_read_boundary_data
!
      type(surface_group_data), intent(inout) :: sf_grp
      integer(kind = kint) :: igrp
!
!
      num_surf_dummy =    sf_grp%num_grp
      num_surf_bc_dummy = sf_grp%num_item
      call allocate_bc_sf_stack_dummy
!
      surf_istack_dummy(0) = sf_grp%istack_grp(0)
      do igrp = 1, sf_grp%num_grp
        surf_name_dummy(igrp) = sf_grp%grp_name(igrp)
        surf_istack_dummy(igrp) = sf_grp%istack_grp(igrp)
      end do
!
      call allocate_bc_sf_item_dummy
      surf_item_dummy(1:sf_grp%num_item,1)                              &
     &      = sf_grp%item_sf_grp(1,1:sf_grp%num_item)
      surf_item_dummy(1:sf_grp%num_item,2)                              &
     &      = sf_grp%item_sf_grp(2,1:sf_grp%num_item)
!
      end subroutine set_surface_grp_type_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_group_types_4_IO
