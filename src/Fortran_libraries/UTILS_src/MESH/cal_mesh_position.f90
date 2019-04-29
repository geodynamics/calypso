!cal_mesh_position.f90
!      module cal_mesh_position
!
!      Written by H. Matsui on July, 2006
!
!      subroutine set_spherical_position(nod)
!        type(node_data), intent(inout) :: nod
!      subroutine set_center_of_element(nod, ele)
!        type(node_data),    intent(in) :: nod
!        type(element_data), intent(inout) :: ele
!      subroutine set_center_of_surface(nod, surf)
!        type(node_data),    intent(in) :: nod
!        type(surface_data), intent(inout) :: surf
!      subroutine set_center_of_edge(nod, edge)
!        type(node_data), intent(in) :: nod
!        type(edge_data), intent(inout) :: edge
!      subroutine find_subdomain_position_range(nod)
!        type(node_data), intent(in) :: nod
!
      module cal_mesh_position
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_spherical_position(nod)
!
      use t_geometry_data
      use coordinate_converter
!
      type(node_data), intent(inout) :: nod
!
!
       call position_2_sph(nod%numnod, nod%xx,                          &
     &     nod%rr, nod%theta, nod%phi, nod%a_r, nod%ss, nod%a_s)
!
      end subroutine set_spherical_position
!
! ----------------------------------------------------------------------
!
      subroutine set_center_of_element(nod, ele)
!
      use m_geometry_constants
      use t_geometry_data
      use set_element_position
      use coordinate_converter
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(inout) :: ele
!
!
      if (ele%nnod_4_ele .eq. num_t_quad) then
        call set_quad_ele_position(nod%numnod, ele%numele, ele%ie,      &
     &      nod%xx, ele%x_ele)
      else if (ele%nnod_4_ele .eq. num_t_linear) then
        call set_linear_ele_position(nod%numnod, ele%numele, ele%ie,    &
     &      nod%xx, ele%x_ele)
      else if (ele%nnod_4_ele .eq. num_t_lag) then
        call set_lag_ele_position(nod%numnod, ele%numele, ele%ie,       &
     &      nod%xx, ele%x_ele)
      end if
!
      call position_2_sph( ele%numele, ele%x_ele,                       &
     &    ele%r_ele, ele%theta_ele,   ele%phi_ele,                      &
     &    ele%ar_ele, ele%s_ele, ele%as_ele)
!
      end subroutine set_center_of_element
!
! ----------------------------------------------------------------------
!
      subroutine set_center_of_surface(nod, surf)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use set_surface_position
      use coordinate_converter
!
      type(node_data),    intent(in) :: nod
      type(surface_data), intent(inout) :: surf
!
!
      if (surf%nnod_4_surf .eq. num_quad_sf) then
        call set_quad_surf_position(nod%numnod,                         &
     &      surf%numsurf, surf%ie_surf, nod%xx, surf%x_surf)
      else if (surf%nnod_4_surf .eq. num_linear_sf) then
        call set_linear_surf_position(nod%numnod,                       &
     &      surf%numsurf, surf%ie_surf, nod%xx, surf%x_surf)
      else if (surf%nnod_4_surf .eq. num_lag_sf) then
        call set_lag_surf_position(nod%numnod,                          &
     &      surf%numsurf, surf%ie_surf, nod%xx, surf%x_surf)
      end if
!
      call position_2_sph(surf%numsurf, surf%x_surf,                    &
     &    surf%r_surf, surf%theta_surf, surf%phi_surf,                  &
     &    surf%ar_surf, surf%s_surf, surf%as_surf)
!
      end subroutine set_center_of_surface
!
! ----------------------------------------------------------------------
!
      subroutine set_center_of_edge(nod, edge)
!
      use m_geometry_constants
      use t_geometry_data
      use t_edge_data
      use set_edge_position
      use coordinate_converter
!
      type(node_data), intent(in) :: nod
      type(edge_data), intent(inout) :: edge
!
!
      if (edge%nnod_4_edge .eq. num_quad_edge) then
        call set_quad_edge_position(nod%numnod,                         &
     &      edge%numedge, edge%ie_edge, nod%xx, edge%x_edge )
      else if (edge%nnod_4_edge .eq. num_linear_edge) then
        call set_linear_edge_position(nod%numnod,                       &
     &      edge%numedge, edge%ie_edge, nod%xx, edge%x_edge )
      end if
!
      call position_2_sph(edge%numedge, edge%x_edge,                    &
     &    edge%r_edge, edge%theta_edge, edge%phi_edge,                  &
     &    edge%ar_edge, edge%s_edge, edge%as_edge)
!
      end subroutine set_center_of_edge
!
! ----------------------------------------------------------------------
!
      subroutine find_subdomain_position_range(nod)
!
      use t_geometry_data
!
      type(node_data), intent(inout) :: nod
!
!  Evaluate range in local domain
      nod%xyz_max_lc = maxval(nod%xx,DIM=1)
      nod%xyz_min_lc = minval(nod%xx,DIM=1)
!
      end subroutine find_subdomain_position_range
!
! ----------------------------------------------------------------------
!
      end module cal_mesh_position
