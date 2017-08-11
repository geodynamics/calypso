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
!>     Structure for FEM mesh data
!!        (position, connectivity, communication, and groups)
      type(mesh_data), save :: femmesh1
!>     Structure for element data (communication)
      type(element_geometry), save :: ele_mesh1
!
      end module m_mesh_data
