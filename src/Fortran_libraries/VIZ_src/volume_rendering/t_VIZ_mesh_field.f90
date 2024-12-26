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
!!      subroutine init_mesh_data_for_vizs(elps_VIZ, viz_step, mesh,    &
!!     &                                   VIZ_DAT, m_SR)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!        type(mesh_SR), intent(inout) :: m_SR
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
      use t_para_double_numbering
      use t_paralell_surface_indices
      use t_elapsed_labels_4_VIZ
!
      implicit none
!
!>      Structure of data for visualization
      type VIZ_mesh_field
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
!>        Double numbering for surface
        type(paralell_surface_indices) :: para_surf
!
!
!>        Structure of edge communication table
        type(communication_table) :: edge_comm
!>        Structure of edge communication table
        type(communication_table) :: surf_comm
!
!>        Double numbering for node
        type(node_ele_double_number) :: inod_dbl
!>        Double numbering for element
        type(node_ele_double_number) :: iele_dbl
!>        Double numbering for surface
        type(node_ele_double_number) :: isurf_dbl
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
! ----------------------------------------------------------------------
!
      subroutine init_mesh_data_for_vizs(elps_VIZ, viz_step, mesh,      &
     &                                   VIZ_DAT, m_SR)
!
      use m_work_time
      use t_elapsed_labels_4_VIZ
      use t_work_time
      use int_volume_of_domain
      use set_element_id_4_node
      use parallel_FEM_mesh_init
      use const_element_comm_tables
      use const_surface_comm_table
      use set_normal_vectors
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
!
      type(mesh_geometry), intent(inout) :: mesh
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: iflag
!
      iflag = viz_step%FLINE_t%increment + viz_step%LIC_t%increment     &
     &       + viz_step%TRACER_t%increment
      if(iflag .gt. 0) then
!  -----  Construct Element communication table
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+16)
        if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
        call const_ele_comm_table                                       &
     &     (mesh%node, mesh%nod_comm, mesh%ele,                         &
     &      VIZ_DAT%ele_comm, m_SR)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+16)
      end if
!
      iflag = viz_step%FLINE_t%increment + viz_step%TRACER_t%increment
      if(iflag .gt. 0) then
!  -----  Construct Surface communication table
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+16)
        if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
        call const_surf_comm_table(mesh%node, mesh%nod_comm,            &
     &                             VIZ_DAT%surf_comm, mesh%surf, m_SR)
!
        call alloc_double_numbering(mesh%node%numnod,                   &
     &                              VIZ_DAT%inod_dbl)
        call alloc_double_numbering(mesh%ele%numele,                    &
     &                              VIZ_DAT%iele_dbl)
        call set_node_ele_double_address                                &
     &     (mesh%node, mesh%ele, mesh%nod_comm,                         &
     &      VIZ_DAT%ele_comm, VIZ_DAT%inod_dbl, VIZ_DAT%iele_dbl,       &
     &      m_SR%SR_sig, m_SR%SR_i)
!
        call alloc_double_numbering(mesh%surf%numsurf,                  &
     &                              VIZ_DAT%isurf_dbl)
        call set_ele_double_numbering                                   &
     &     (mesh%surf%numsurf, mesh%surf%ie_surf(1,1),                  &
     &      VIZ_DAT%surf_comm, VIZ_DAT%inod_dbl, VIZ_DAT%isurf_dbl,     &
     &      m_SR%SR_sig, m_SR%SR_i)
!
        call init_para_surf_indices                                     &
     &     (mesh, VIZ_DAT%ele_comm, VIZ_DAT%surf_comm,                  &
     &      VIZ_DAT%iele_dbl, VIZ_DAT%isurf_dbl, VIZ_DAT%para_surf,     &
     &      m_SR)
!
        call dealloc_double_numbering(VIZ_DAT%isurf_dbl)
        call dealloc_double_numbering(VIZ_DAT%iele_dbl)
        call dealloc_double_numbering(VIZ_DAT%inod_dbl)
        call dealloc_comm_table(VIZ_DAT%surf_comm)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+16)
      end if
!
!  -----  Construct Edge communication table
      iflag = viz_step%PSF_t%increment + viz_step%ISO_t%increment       &
     &       + viz_step%MAP_t%increment
      if(iflag .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+17)
        if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
        call const_edge_comm_table(mesh%node, mesh%nod_comm,            &
     &                             VIZ_DAT%edge_comm, mesh%edge, m_SR)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+17)
      end if
      call calypso_mpi_barrier
!
      end subroutine init_mesh_data_for_vizs
!
! ----------------------------------------------------------------------
!
      end module t_VIZ_mesh_field
