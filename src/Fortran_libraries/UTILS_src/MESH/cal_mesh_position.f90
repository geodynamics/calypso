!cal_mesh_position.f90
!      module cal_mesh_position
!
!      Written by H. Matsui on July, 2006
!
!      subroutine set_spherical_position
!      subroutine set_center_of_element
!
!      subroutine set_center_of_surface
!      subroutine set_center_of_edge
!
      module cal_mesh_position
!
      use m_precision
!
      use  m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_spherical_position
!
      use m_geometry_parameter
      use m_geometry_data
      use coordinate_converter
!
       call position_2_sph( numnod, xx, radius, colatitude,             &
     &       longitude, a_radius, s_cylinder, a_s_cylinder)
!
      end subroutine set_spherical_position
!
! ----------------------------------------------------------------------
!
      subroutine set_center_of_element
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use set_element_position
      use coordinate_converter
!
      if (nnod_4_ele .eq. num_t_quad) then
        call set_quad_ele_position(numnod, numele, ie, xx,              &
     &          x_ele)
      else if (nnod_4_ele .eq. num_t_linear) then
        call set_linear_ele_position(numnod, numele, ie, xx,            &
     &          x_ele)
      else if (nnod_4_ele .eq. num_t_lag) then
        call set_lag_ele_position(numnod, numele, ie, xx,               &
     &          x_ele)
      end if
!
      call position_2_sph( numele, x_ele, r_ele, theta_ele,             &
     &       phi_ele, ar_ele, s_ele, as_ele)
!
      end subroutine set_center_of_element
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_center_of_surface
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_geometry_data
      use set_surface_position
      use coordinate_converter
!
      if (nnod_4_surf .eq. num_quad_sf) then
        call set_quad_surf_position(numnod, numsurf, ie_surf, xx,       &
     &          x_surf)
      else if (nnod_4_surf .eq. num_linear_sf) then
        call set_linear_surf_position(numnod, numsurf, ie_surf, xx,     &
     &          x_surf)
      else if (nnod_4_surf .eq. num_lag_sf) then
        call set_lag_surf_position(numnod, numsurf, ie_surf, xx,        &
     &          x_surf)
      end if
!
      call position_2_sph(numsurf, x_surf, r_surf, theta_surf,          &
     &    phi_surf, ar_surf, s_surf, as_surf)
!
      end subroutine set_center_of_surface
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_center_of_edge
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_edge_geometry_data
      use set_edge_position
      use coordinate_converter
!
      if (nnod_4_edge .eq. num_quad_edge) then
        call set_quad_edge_position(numnod, numedge, ie_edge, xx,       &
     &          x_edge )
      else if (nnod_4_edge .eq. num_linear_edge) then
        call set_linear_edge_position(numnod, numedge, ie_edge, xx,     &
     &          x_edge )
      end if
!
      call position_2_sph(numedge, x_edge, r_edge, theta_edge,          &
     &    phi_edge, ar_edge, s_edge, as_edge)
!
      end subroutine set_center_of_edge
!
! ----------------------------------------------------------------------
!
      end module cal_mesh_position
