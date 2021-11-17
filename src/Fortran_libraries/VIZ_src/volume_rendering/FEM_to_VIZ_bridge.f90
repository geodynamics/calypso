!>@file   FEM_to_VIZ_bridge.f90
!!@brief  module FEM_to_VIZ_bridge
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Data structuresa for visualizers
!!
!!@verbatim
!!      subroutine init_FEM_to_VIZ_bridge                               &
!!     &         (viz_step, geofem, VIZ_DAT, m_SR)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine init_FEM_MHD_to_VIZ_bridge                           &
!!     &         (viz_step, next_tbl, jacobians, geofem, VIZ_DAT, m_SR)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(next_nod_ele_table), intent(in), target :: next_tbl
!!        type(jacobians_type), intent(in), target :: jacobians
!!        type(mesh_data), intent(inout) :: geofem
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module FEM_to_VIZ_bridge
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_comm_table
      use t_next_node_ele_4_node
      use t_shape_functions
      use t_jacobians
      use t_VIZ_step_parameter
      use t_VIZ_mesh_field
      use t_mesh_SR
      use t_work_time
      use m_elapsed_labels_4_VIZ
!
      implicit none
!
      private :: normals_and_jacobians_4_VIZ
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_FEM_to_VIZ_bridge                                 &
     &         (viz_step, geofem, VIZ_DAT, m_SR)
!
      use m_work_time
      use parallel_FEM_mesh_init
!
      type(VIZ_step_params), intent(in) :: viz_step
!
      type(mesh_data), intent(inout) :: geofem
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call FEM_mesh_initialization(geofem%mesh, geofem%group,           &
     &                             m_SR%SR_sig, m_SR%SR_i)
!
      if(iflag_debug.gt.0) write(*,*) 'normals_and_jacobians_VIZ_pre'
      call link_jacobians_4_viz                                         &
     &   (VIZ_DAT%next_tbl_v, VIZ_DAT%jacobians_v, VIZ_DAT)
      if(iflag_debug.gt.0) write(*,*) 'normals_and_jacobians_4_VIZ'
      call normals_and_jacobians_4_VIZ                                  &
     &   (viz_step, geofem, VIZ_DAT%ele_comm, VIZ_DAT%edge_comm,        &
     &    VIZ_DAT%next_tbl, VIZ_DAT%jacobians, m_SR)
!
      end subroutine init_FEM_to_VIZ_bridge
!
! ----------------------------------------------------------------------
!
      subroutine init_FEM_MHD_to_VIZ_bridge                             &
     &         (viz_step, next_tbl, jacobians, geofem, VIZ_DAT, m_SR)
!
      use m_work_time
      use const_element_comm_tables
      use parallel_FEM_mesh_init
!
      use int_volume_of_domain
      use set_element_id_4_node
      use parallel_FEM_mesh_init
      use const_element_comm_tables
      use set_normal_vectors
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(next_nod_ele_table), intent(in), target :: next_tbl
      type(jacobians_type), intent(in), target :: jacobians
!
      type(mesh_data), intent(inout) :: geofem
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: iflag
!
!
      call link_jacobians_4_viz(next_tbl, jacobians, VIZ_DAT)
!
!  -----  Construct Element communication table
      if(viz_step%LIC_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+12)
        if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
        call const_ele_comm_table                                       &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, geofem%mesh%ele,    &
     &      VIZ_DAT%ele_comm, m_SR)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+12)
      end if
!
!  -----  Construct Edge communication table
      iflag = viz_step%PSF_t%increment + viz_step%ISO_t%increment
      if(iflag .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+13)
        if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
        call const_edge_comm_table                                      &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, VIZ_DAT%edge_comm,  &
     &      geofem%mesh%edge, m_SR)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+13)
      end if
      call calypso_mpi_barrier
!
      end subroutine init_FEM_MHD_to_VIZ_bridge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine normals_and_jacobians_4_VIZ(viz_step, geofem,          &
     &          ele_comm, edge_comm, next_tbl, jacobians, m_SR)
!
      use t_fem_gauss_int_coefs
      use int_volume_of_domain
      use set_element_id_4_node
      use set_normal_vectors
      use const_element_comm_tables
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(inout) :: geofem
      type(communication_table), intent(inout) :: ele_comm
      type(communication_table), intent(inout) :: edge_comm
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(jacobians_type), intent(inout) :: jacobians
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: iflag
      type(shape_finctions_at_points) :: spfs
!
!
!  -----  Const Neighboring information
      iflag = viz_step%FLINE_t%increment + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
        call set_belonged_ele_and_next_nod                              &
     &     (geofem%mesh, next_tbl%neib_ele, next_tbl%neib_nod)
      end if
!
!  -----  Construct Element communication table
      if(viz_step%LIC_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+12)
        if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
        call const_ele_comm_table                                       &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, geofem%mesh%ele,    &
     &      ele_comm, m_SR)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+12)
      end if
!
!  -----  Construct Edge communication table
      iflag = viz_step%PSF_t%increment + viz_step%ISO_t%increment
      if(iflag .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+13)
        if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
        call const_edge_comm_table                                      &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, edge_comm,          &
     &      geofem%mesh%edge, m_SR)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+13)
      end if
!
      iflag = viz_step%PVR_t%increment + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'jacobian_and_element_volume'
!        call sel_max_int_point_by_etype                                &
!     &     (geofem%mesh%ele%nnod_4_ele, jacobians%g_FEM)
        call set_max_integration_points(ione, jacobians%g_FEM)
        call jacobian_and_element_volume(my_rank, nprocs,               &
     &      geofem%mesh, geofem%group, spfs, jacobians)
        if (iflag_debug.eq.1) write(*,*) 'surf_jacobian_sf_grp_normal'
        call surf_jacobian_sf_grp_normal(my_rank, nprocs,               &
     &      geofem%mesh, geofem%group, spfs, jacobians)
      end if
!
      end subroutine normals_and_jacobians_4_VIZ
!
! ----------------------------------------------------------------------
!
      end module FEM_to_VIZ_bridge
