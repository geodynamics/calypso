!>@file   set_connects_4_surf_group.f90
!!@brief  module set_connects_4_surf_group
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2010
!
!> @brief Construct connectivities for surface group
!!
!!@verbatim
!!      subroutine set_node_4_surf_group(nod, ele, surf, sf_grp, sf_nod)
!!        type(node_data),    intent(in) ::       nod
!!        type(element_data), intent(in) ::       ele
!!        type(surface_data), intent(in) ::       surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(inout) :: sf_nod
!!
!!      subroutine cal_surf_normal_at_nod(node, ele, surf, sf_grp,      &
!!     &          sf_grp_v, sf_nod)
!!        type(node_data),              intent(in) :: node
!!        type(element_data),           intent(in) :: ele
!!        type(surface_data),           intent(in) :: surf
!!        type(surface_group_data),     intent(in) :: sf_grp
!!        type(surface_group_normals), intent(in) :: sf_grp_v
!!        type(surface_node_grp_data), intent(inout) :: sf_nod
!!
!!      subroutine empty_surface_node_grp_type(sf_grp, sf_nod)
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(inout) :: sf_nod
!!@endverbatim
!
      module set_connects_4_surf_group
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_node_4_surf_group(nod, ele, surf, sf_grp, sf_nod)
!
      use m_machine_parameter
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_surface_group_connect
      use set_surface_node
      use set_smp_4_group_types
      use cal_minmax_and_stacks
!
      type(node_data),    intent(in) ::       nod
      type(element_data), intent(in) ::       ele
      type(surface_data), intent(in) ::       surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      call allocate_make_4_surf_nod_grp(nod%numnod)
!
      call alloc_num_surf_grp_nod(sf_grp%num_grp, sf_nod)
!
      call count_surf_nod_grp_stack(np_smp, nod%istack_nod_smp,         &
     &    ele%numele, ele%nnod_4_ele, ele%ie, surf%nnod_4_surf,         &
     &    surf%node_on_sf, sf_grp%num_grp, sf_grp%num_item,             &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp,                        &
     &    sf_nod%ntot_node_sf_grp, sf_nod%nnod_sf_grp,                  &
     &    sf_nod%inod_stack_sf_grp)
!
!
      call alloc_num_surf_grp_nod_smp(sf_grp%num_grp_smp, sf_nod)
!
      call set_group_size_4_smp(np_smp,                                 &
     &    sf_grp%num_grp, sf_nod%inod_stack_sf_grp,                     &
     &    sf_nod%istack_surf_nod_smp, sf_nod%max_sf_nod_4_smp)
!
!
      call alloc_item_surf_grp_nod(sf_nod)
      if (sf_nod%ntot_node_sf_grp .gt. 0) then
        call set_surf_nod_grp_item(nod%numnod, ele%numele,              &
     &      ele%nnod_4_ele, ele%ie, surf%nnod_4_surf,                   &
     &      surf%node_on_sf, surf%node_on_sf_n,                         &
     &      sf_grp%num_grp, sf_grp%num_item,                            &
     &      sf_grp%istack_grp, sf_grp%item_sf_grp,                      &
     &      sf_nod%ntot_node_sf_grp, sf_nod%inod_stack_sf_grp,          &
     &      sf_nod%inod_surf_grp, sf_nod%surf_node_n,                   &
     &      sf_nod%num_sf_4_nod)
      end if
!
      call deallocate_make_4_surf_nod_grp
!
      end subroutine set_node_4_surf_group
!
!-----------------------------------------------------------------------
!
      subroutine cal_surf_normal_at_nod(node, ele, surf, sf_grp,        &
     &          sf_grp_v, sf_nod)
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_normals
      use t_surface_group_connect
      use set_norm_nod_4_surf_grp
!
      type(node_data),              intent(in) :: node
      type(element_data),           intent(in) :: ele
      type(surface_data),           intent(in) :: surf
      type(surface_group_data),     intent(in) :: sf_grp
      type(surface_group_normals), intent(in) :: sf_grp_v
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      call allocate_work_norm_nod(node%numnod)
      call alloc_vect_surf_grp_nod(sf_nod)
!
      call cal_surf_grp_norm_node(ele%numele, ele%nnod_4_ele,           &
     &    surf%nnod_4_surf, surf%node_on_sf, ele%ie,                    &
     &    sf_grp%num_grp, sf_grp%num_item, sf_grp%istack_grp,           &
     &    sf_grp%item_sf_grp, sf_grp_v%vnorm_sf_grp,                    &
     &    sf_grp_v%a_area_sf_grp, sf_nod%ntot_node_sf_grp,              &
     &    sf_nod%inod_stack_sf_grp, sf_nod%inod_surf_grp,               &
     &    sf_nod%surf_norm_nod, sf_nod%coef_sf_nod)
!
      call deallocate_work_norm_nod
!
      end subroutine cal_surf_normal_at_nod
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine empty_surface_node_grp_type(sf_grp, sf_nod)
!
      use t_group_data
      use t_surface_group_connect
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      sf_nod%ntot_node_sf_grp = 0
      call alloc_num_surf_grp_nod(sf_grp%num_grp, sf_nod)
!
      sf_nod%ntot_node_sf_grp = 0
      call alloc_item_surf_grp_nod(sf_nod)
!
      end subroutine empty_surface_node_grp_type
!
!-----------------------------------------------------------------------
!
      end module set_connects_4_surf_group
