!t_FEM_mesh_field_data.f90
!      module t_FEM_mesh_field_data
!
!      Written by H. Matsui on July, 2006
!      Modified by H. Matsui on June, 2007
!
!>@file   t_FEM_mesh_field_data.f90
!!@brief  module t_FEM_mesh_field_data
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in July, 2006
!!@n    Mmodified by H. Matsui in June, 2007
!!@n    Mmodified by H. Matsui in Sep., 2017
!
!> @brief Strcture for FEM mesh and nodal field
!!
      module t_FEM_mesh_field_data
!
      use m_precision
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
!
      implicit none
!
!>      Structure of FEM mesh and field structures
      type FEM_mesh_field_data
!>       label   for simulation
        character(len=kchara)   :: label_sim
!
!>       Structure for FEM mesh data
!!         (position, connectivity, communication, and groups)
        type(mesh_data) :: geofem
!>       Structure for element data (communication)
       type(element_geometry) :: ele_mesh
!
!
!>       Structure for nodal field data
        type(phys_data) :: field
!>       address for nodal fields
        type(phys_address) :: iphys
      end type FEM_mesh_field_data
!
      end module t_FEM_mesh_field_data
