!>@file   m_geometry_data.f90
!!@brief  module m_geometry_data
!!
!!@author H. Okuda and H. Matsui
!!@date Programmed in 2001
!!@date Modified in 2008
!!@date Modified in Aug., 2015
!
!> @brief geometry data for FEM mesh
!!   including node and element position, all connectivities
!!
!
      module   m_geometry_data
!
      use m_precision
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
!>  structure for node data (position)
      type(node_data), save :: node1
!    node1%istack_internod
!
!>  structure for element data (position and connectivity)
      type(element_data), save :: ele1
!    ele1%istack_interele
!
!>      structure of surface data (geometry and connectivity)
      type(surface_data), save :: surf1
!  surf1%x_surf
!
!>     Structure for edge data
      type(edge_data), save :: edge1
!  edge1%istack_numedge
!
      end module m_geometry_data
