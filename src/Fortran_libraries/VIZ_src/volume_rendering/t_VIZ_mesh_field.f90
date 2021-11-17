!>@file   t_VIZ_mesh_field.f90
!!@brief  module t_VIZ_mesh_field
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Data structuresa for visualizers
!!
!!@verbatim
!!      subroutine link_jacobians_4_viz(next_tbl, jacobians, VIZ_DAT)
!!      subroutine unlink_jacobians_4_viz(VIZ_DAT)
!!        type(mesh_data), intent(inout), target :: geofem
!!        type(next_nod_ele_table), intent(in), target :: next_tbl
!!        type(jacobians_type), intent(in), target :: jacobians
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!@endverbatim
!
      module t_VIZ_mesh_field
!
      use m_precision
      use m_machine_parameter
!
      use t_comm_table
      use t_phys_data
      use t_next_node_ele_4_node
      use t_shape_functions
      use t_jacobians
      use t_VIZ_step_parameter
!
      implicit none
!
!
!>      Structure of data for visualization
      type VIZ_mesh_field
!!>        Structure of shape function for PVR and fieldline
!        type(shape_finctions_at_points) :: spfs
!>        Stracture for Jacobians
        type(jacobians_type) :: jacobians_v
!>        Structure of included element list for each node
        type(next_nod_ele_table) :: next_tbl_v
!
!!>        Structure of shape function for PVR and fieldline
!        type(shape_finctions_at_points) :: spfs
!>        Stracture for Jacobians
        type(jacobians_type), pointer :: jacobians
!>        Structure of neighboring list for each node
        type(next_nod_ele_table), pointer :: next_tbl
!
!>        Structure of element communication table
        type(communication_table) :: ele_comm
!>        Structure of edge communication table
        type(communication_table) :: edge_comm
      end type VIZ_mesh_field
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine link_jacobians_4_viz(next_tbl, jacobians, VIZ_DAT)
!
      type(next_nod_ele_table), intent(in), target :: next_tbl
      type(jacobians_type), intent(in), target :: jacobians
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      VIZ_DAT%next_tbl =>  next_tbl
      VIZ_DAT%jacobians => jacobians
!
      end subroutine link_jacobians_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine unlink_jacobians_4_viz(VIZ_DAT)
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      nullify(VIZ_DAT%jacobians, VIZ_DAT%next_tbl)
!
      end subroutine unlink_jacobians_4_viz
!
! ----------------------------------------------------------------------
!
      end module t_VIZ_mesh_field
