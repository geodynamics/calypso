!m_mesh_data.f90
!      module m_mesh_data
!
!      Written by H. Matsui on July, 2006
!      Modified by H. Matsui on June, 2007
!
      module m_mesh_data
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
!
      implicit none
!
!>     Structure for grid data
!>        (position, connectivity, and communication)
      type(mesh_geometry), save :: mesh1
!>     Structure for group data
      type(mesh_groups), save ::   group1
!
!>     Structure for element data (communication)
      type(element_geometry), save :: ele_mesh1
!
      end module m_mesh_data
