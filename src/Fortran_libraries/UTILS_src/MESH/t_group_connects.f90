!
!     module t_group_connects
!
!> @brief Structure of connectivity data for group items
!
!     Writteg by H.Matsui on Dec., 2008
!
!      subroutine alloc_surf_item_sf_grp_type(nitem_grp, sf_grp_data)
!      subroutine alloc_num_other_grp(num_grp, e_grp)
!      subroutine alloc_item_other_grp(e_grp)
!
!      subroutine dealloc_surf_item_sf_grp_type(sf_grp_data)
!      subroutine dealloc_num_other_grp(e_grp)
!      subroutine dealloc_item_other_grp(e_grp)
!
!      subroutine unlink_surf_item_sf_grp_type(sf_grp_data)
!      subroutine unlink_num_other_grp(e_grp)
!      subroutine unlink_item_other_grp(e_grp)
!
!      subroutine link_ele_grp_connect_type(tbls_org, tbls_ele_new)
!      subroutine link_surf_grp_connect_type(tbls_org, tbls_sf_new)
!
      module t_group_connects
!
      use m_precision
!
      implicit  none
!
!>   Structure of connectivities for groups
      type group_connect_data
        integer(kind=kint) :: ntot_e_grp
!<   total number of connectivity for group
        integer(kind=kint), pointer :: nitem_e_grp(:)
!<   number of item for each group
        integer(kind=kint), pointer :: istack_e_grp(:)
!<   end number of item for each group
!
        integer(kind=kint), pointer :: item_e_grp(:)
!<   local item ID for group
      end type group_connect_data
!
!
!>   Structure of connectivities for element group
      type element_group_table
        type(group_connect_data) :: surf
!<   local surface connectivity for element group
        type(group_connect_data) :: edge
!<   local edge connectivity for element group
        type(group_connect_data) :: node
!<   local node connectivity for element group
      end type element_group_table
!
!
!>   Structure of connectivities for surface group
      type surface_group_table
        integer(kind=kint), pointer :: isurf_grp(:)
!<   local surface ID for surface group
        integer(kind=kint), pointer :: isurf_grp_n(:)
!<   local surface ID for opposite side of surface group
!
        type(group_connect_data) :: edge
!<   local edge connectivity for surface group
        type(group_connect_data) :: node
!<   local node connectivity for surface group
      end type surface_group_table
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_item_sf_grp_type(nitem_grp, sf_grp_data)
!
      integer(kind = kint), intent(in) :: nitem_grp
      type(surface_group_table), intent(inout) :: sf_grp_data
!
      allocate(sf_grp_data%isurf_grp(nitem_grp)  )
      allocate(sf_grp_data%isurf_grp_n(nitem_grp))
!
      if(nitem_grp .gt. 0) then
        sf_grp_data%isurf_grp =   0
        sf_grp_data%isurf_grp_n = 0
      end if
!
      end subroutine alloc_surf_item_sf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine alloc_num_other_grp(num_grp, e_grp)
!
      integer(kind = kint), intent(in) :: num_grp
      type(group_connect_data), intent(inout) :: e_grp
!
!
      allocate(e_grp%nitem_e_grp(num_grp))
      allocate(e_grp%istack_e_grp(0:num_grp))
!
      if (num_grp .gt. 0) e_grp%nitem_e_grp = 0
      e_grp%istack_e_grp = 0
!
      end subroutine alloc_num_other_grp
!
!-----------------------------------------------------------------------
!
      subroutine alloc_item_other_grp(e_grp)
!
      type(group_connect_data), intent(inout) :: e_grp
!
      allocate(e_grp%item_e_grp(e_grp%ntot_e_grp))
      if (e_grp%ntot_e_grp .gt. 0) e_grp%item_e_grp = 0
!
      end subroutine alloc_item_other_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_item_sf_grp_type(sf_grp_data)
!
      type(surface_group_table), intent(inout) :: sf_grp_data
!
      deallocate(sf_grp_data%isurf_grp  )
      deallocate(sf_grp_data%isurf_grp_n)
!
      end subroutine dealloc_surf_item_sf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_num_other_grp(e_grp)
!
      type(group_connect_data), intent(inout) :: e_grp
!
!
      deallocate(e_grp%nitem_e_grp )
      deallocate(e_grp%istack_e_grp)
!
      end subroutine dealloc_num_other_grp
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_item_other_grp(e_grp)
!
      type(group_connect_data), intent(inout) :: e_grp
!
      deallocate(e_grp%item_e_grp)
!
      end subroutine dealloc_item_other_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine unlink_surf_item_sf_grp_type(sf_grp_data)
!
      type(surface_group_table), intent(inout) :: sf_grp_data
!
      nullify(sf_grp_data%isurf_grp  )
      nullify(sf_grp_data%isurf_grp_n)
!
      end subroutine unlink_surf_item_sf_grp_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine unlink_num_other_grp(e_grp)
!
      type(group_connect_data), intent(inout) :: e_grp
!
!
      nullify(e_grp%nitem_e_grp )
      nullify(e_grp%istack_e_grp)
!
      end subroutine unlink_num_other_grp
!
!-----------------------------------------------------------------------
!
      subroutine unlink_item_other_grp(e_grp)
!
      type(group_connect_data), intent(inout) :: e_grp
!
      nullify(e_grp%item_e_grp)
!
      end subroutine unlink_item_other_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_ele_grp_connect_type(tbls_org, tbls_ele_new)
!
      type(element_group_table), intent(in) :: tbls_org
      type(element_group_table), intent(inout) :: tbls_ele_new
!
!
      tbls_ele_new%surf%ntot_e_grp = tbls_org%surf%ntot_e_grp
      tbls_ele_new%edge%ntot_e_grp = tbls_org%edge%ntot_e_grp
      tbls_ele_new%node%ntot_e_grp = tbls_org%node%ntot_e_grp
!
      tbls_ele_new%surf%nitem_e_grp =>  tbls_org%surf%nitem_e_grp
      tbls_ele_new%surf%istack_e_grp => tbls_org%surf%istack_e_grp
      tbls_ele_new%surf%item_e_grp =>   tbls_org%surf%item_e_grp
!
      tbls_ele_new%edge%nitem_e_grp =>  tbls_org%edge%nitem_e_grp
      tbls_ele_new%edge%istack_e_grp => tbls_org%edge%istack_e_grp
      tbls_ele_new%edge%item_e_grp =>   tbls_org%edge%item_e_grp
!
      tbls_ele_new%node%nitem_e_grp =>   tbls_org%node%nitem_e_grp
      tbls_ele_new%node%istack_e_grp =>  tbls_org%node%istack_e_grp
      tbls_ele_new%node%item_e_grp =>    tbls_org%node%item_e_grp
!
      end subroutine link_ele_grp_connect_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_surf_grp_connect_type(tbls_org, tbls_sf_new)
!
      type(surface_group_table), intent(in) :: tbls_org
      type(surface_group_table), intent(inout) :: tbls_sf_new
!
!
      tbls_sf_new%edge%ntot_e_grp = tbls_org%edge%ntot_e_grp
      tbls_sf_new%node%ntot_e_grp = tbls_org%node%ntot_e_grp
!
      tbls_sf_new%isurf_grp =>          tbls_org%isurf_grp
      tbls_sf_new%isurf_grp_n =>        tbls_org%isurf_grp_n
!
      tbls_sf_new%edge%nitem_e_grp =>  tbls_org%edge%nitem_e_grp
      tbls_sf_new%edge%istack_e_grp => tbls_org%edge%istack_e_grp
      tbls_sf_new%edge%item_e_grp =>   tbls_org%edge%item_e_grp
!
      tbls_sf_new%node%nitem_e_grp =>   tbls_org%node%nitem_e_grp
      tbls_sf_new%node%istack_e_grp =>  tbls_org%node%istack_e_grp
      tbls_sf_new%node%item_e_grp =>    tbls_org%node%item_e_grp
!
      end subroutine link_surf_grp_connect_type
!
!  ---------------------------------------------------------------------
!
      end module t_group_connects
