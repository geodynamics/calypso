!>@file   copy_group_data_from_type.f90
!!@brief  module copy_group_data_from_type
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!
!>@brief Copy group data from structure to 1st mesh modules
!!
!!@verbatim
!!      subroutine group_data_from_type(group)
!!      subroutine compare_group_type_vs_1st(my_rank, group)
!!        type(mesh_groups), intent(inout) :: group
!!@endverbatim
!
      module copy_group_data_from_type
!
      use m_precision
!
      implicit  none
!
      private ::  node_group_from_type, element_group_from_type
      private ::  surface_group_from_type
      private ::  compare_nod_grp_type_vs_1st
      private ::  compare_ele_grp_type_vs_1st
      private ::  compare_surf_grp_type_vs_1st
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine group_data_from_type(group)
!
      use t_mesh_data
!
      type(mesh_groups), intent(inout) :: group
!
!
      call node_group_from_type(group%nod_grp)
      call element_group_from_type(group%ele_grp)
      call surface_group_from_type(group%surf_grp)
!
      call dealloc_groups_data(group)
!
      end subroutine group_data_from_type
!
!-----------------------------------------------------------------------
!
      subroutine compare_group_type_vs_1st(my_rank, group)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_groups), intent(in) :: group
!
!
      call compare_nod_grp_type_vs_1st(my_rank, group%nod_grp)
      call compare_ele_grp_type_vs_1st(my_rank, group%ele_grp)
      call compare_surf_grp_type_vs_1st(my_rank, group%surf_grp)
!
      end subroutine compare_group_type_vs_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine node_group_from_type(nod_grp)
!
      use m_node_group
      use t_group_data
!
      type(group_data), intent(inout) :: nod_grp
!
!
      num_bc = nod_grp%num_grp
      if (num_bc .gt. 0) then
        num_nod_bc = nod_grp%num_item
        call allocate_boundary_data
!
        bc_name(1:num_bc) =     nod_grp%grp_name(1:num_bc)
        bc_istack(0:num_bc) =   nod_grp%istack_grp(0:num_bc)
        bc_item(1:num_nod_bc) = nod_grp%item_grp(1:num_nod_bc)
      end if
!
      end subroutine node_group_from_type
!
!-----------------------------------------------------------------------
!
      subroutine element_group_from_type(ele_grp)
!
      use m_element_group
      use t_group_data
!
      type(group_data), intent(inout) :: ele_grp
!
!
      num_mat =     ele_grp%num_grp
      if (num_mat .gt. 0) then
        num_mat_bc = ele_grp%num_item
        call allocate_material_data
!
        mat_name(1:num_mat) =    ele_grp%grp_name(1:num_mat)
        mat_istack(0:num_mat) =  ele_grp%istack_grp(0:num_mat)
        mat_item(1:num_mat_bc) = ele_grp%item_grp(1:num_mat_bc)
      end if
!
      end subroutine element_group_from_type
!
!-----------------------------------------------------------------------
!
      subroutine surface_group_from_type(sf_grp)
!
      use m_surface_group
      use t_group_data
!
      type(surface_group_data), intent(inout) :: sf_grp
!
!
      num_surf = sf_grp%num_grp
      if (num_surf .gt. 0) then
        num_surf_bc = sf_grp%num_item
        call allocate_surface_data
!
        surf_name(1:num_surf) =    sf_grp%grp_name(1:num_surf)
        surf_istack(0:num_surf) =  sf_grp%istack_grp(0:num_surf)
        surf_item(1,1:num_surf_bc)= sf_grp%item_sf_grp(1,1:num_surf_bc)
        surf_item(2,1:num_surf_bc)= sf_grp%item_sf_grp(2,1:num_surf_bc)
      end if
!
      end subroutine surface_group_from_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine compare_nod_grp_type_vs_1st(my_rank, nod_grp)
!
      use m_node_group
      use t_group_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(group_data), intent(in) :: nod_grp
!
      integer(kind = kint) :: i
!
!
      if(nod_grp%num_grp .ne. num_bc) write(*,*) 'num_bc',              &
     &     my_rank, nod_grp%num_grp, num_bc
      if(nod_grp%num_item .ne. num_nod_bc) write(*,*)                   &
     &     'num_nod_bc', my_rank, nod_grp%num_item, num_nod_bc
      do i = 1, num_bc
        if(nod_grp%grp_name(i) .ne. bc_name(i))                         &
     &       write(*,*) 'bc_name(i)', my_rank, i,                       &
     &       nod_grp%grp_name(i), bc_name(i)
        if(nod_grp%istack_grp(i) .ne. bc_istack(i))                     &
     &       write(*,*) 'bc_istack(i)', my_rank, i,                     &
     &       nod_grp%istack_grp(i), bc_istack(i)
      end do
      do i = 1, num_nod_bc
        if(nod_grp%item_grp(i) .ne. bc_item(i))                         &
     &       write(*,*) 'bc_item(i)', my_rank, i,                       &
     &       nod_grp%item_grp(i), bc_item(i)
      end do
!
      end subroutine compare_nod_grp_type_vs_1st
!
!-----------------------------------------------------------------------
!
      subroutine compare_ele_grp_type_vs_1st(my_rank, ele_grp)
!
      use m_element_group
      use t_group_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint) :: i
!
      if(ele_grp%num_grp .ne. num_mat) write(*,*) 'num_mat',            &
     &     my_rank, ele_grp%num_grp, num_mat
      if(ele_grp%num_item .ne. num_mat_bc) write(*,*)                   &
     &     'num_mat_bc', my_rank, ele_grp%num_item, num_mat_bc
      do i = 1, num_mat
        if(ele_grp%grp_name(i) .ne. mat_name(i))                        &
     &       write(*,*) 'mat_name(i)', my_rank, i,                      &
     &       ele_grp%grp_name(i), mat_name(i)
        if(ele_grp%istack_grp(i) .ne. mat_istack(i))                    &
     &       write(*,*) 'mat_istack(i)', my_rank, i,                    &
     &       ele_grp%istack_grp(i), mat_istack(i)
      end do
      do i = 1, num_mat_bc
        if(ele_grp%item_grp(i) .ne. mat_item(i))                        &
     &       write(*,*) 'mat_item(i)', my_rank, i,                      &
     &       ele_grp%item_grp(i), mat_item(i)
      end do
!
      end subroutine compare_ele_grp_type_vs_1st
!
!-----------------------------------------------------------------------
!
      subroutine compare_surf_grp_type_vs_1st(my_rank, sf_grp)
!
      use m_surface_group
      use t_group_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(surface_group_data), intent(in) :: sf_grp
!
      integer(kind = kint) :: i
!
!
      if(sf_grp%num_grp .ne. num_surf) write(*,*) 'num_surf',           &
     &     my_rank, sf_grp%num_grp, num_surf
      if(sf_grp%num_item .ne. num_surf_bc) write(*,*)                   &
     &     'num_surf_bc', my_rank, sf_grp%num_item, num_surf_bc
      do i = 1, num_surf
        if(sf_grp%grp_name(i) .ne. surf_name(i))                        &
     &       write(*,*) 'surf_name(i)', my_rank, i,                     &
     &       sf_grp%grp_name(i), surf_name(i)
        if(sf_grp%istack_grp(i) .ne. surf_istack(i))                    &
     &       write(*,*) 'surf_istack(i)', my_rank, i,                   &
     &       sf_grp%istack_grp(i), surf_istack(i)
      end do
      do i = 1, num_surf_bc
        if(sf_grp%item_sf_grp(1,i) .ne. surf_item(1,i)                  &
     &   .or. sf_grp%item_sf_grp(2,i) .ne. surf_item(2,i))              &
     &       write(*,*) 'surf_item(:,i)', my_rank, i,                   &
     &       sf_grp%item_sf_grp(1,i), surf_item(1,i),                   &
     &       sf_grp%item_sf_grp(2,i), surf_item(2,i)
      end do
!
      end subroutine compare_surf_grp_type_vs_1st
!
!-----------------------------------------------------------------------
!
      end module copy_group_data_from_type
