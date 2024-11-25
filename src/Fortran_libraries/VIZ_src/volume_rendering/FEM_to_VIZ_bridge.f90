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
!!     &         (elps_VIZ, viz_step, geofem, VIZ_DAT, m_SR)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine init_FEM_MHD_to_VIZ_bridge                           &
!!     &         (elps_VIZ, viz_step, next_tbl, jacobians,              &
!!     &          geofem, VIZ_DAT, m_SR)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
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
      use m_work_time
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
      use t_elapsed_labels_4_VIZ
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
     &         (elps_VIZ, viz_step, geofem, VIZ_DAT, m_SR)
!
      use parallel_FEM_mesh_init
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
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
!
      if(iflag_debug.gt.0) write(*,*) 'normals_and_jacobians_4_VIZ'
      call normals_and_jacobians_4_VIZ(elps_VIZ, viz_step, geofem,      &
     &    VIZ_DAT%next_tbl, VIZ_DAT%jacobians)
!
      call init_mesh_data_for_vizs(elps_VIZ, viz_step, geofem%mesh,     &
     &                             VIZ_DAT, m_SR)
!
      end subroutine init_FEM_to_VIZ_bridge
!
! ----------------------------------------------------------------------
!
      subroutine init_FEM_MHD_to_VIZ_bridge                             &
     &         (elps_VIZ, viz_step, next_tbl, jacobians,                &
     &          geofem, VIZ_DAT, m_SR)
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
      type(next_nod_ele_table), intent(in), target :: next_tbl
      type(jacobians_type), intent(in), target :: jacobians
!
      type(mesh_data), intent(inout) :: geofem
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call link_jacobians_4_viz(next_tbl, jacobians, VIZ_DAT)
      call init_mesh_data_for_vizs(elps_VIZ, viz_step, geofem%mesh,     &
     &                             VIZ_DAT, m_SR)
!
      end subroutine init_FEM_MHD_to_VIZ_bridge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine normals_and_jacobians_4_VIZ                            &
     &         (elps_VIZ, viz_step, geofem,  next_tbl, jacobians)
!
      use t_fem_gauss_int_coefs
      use int_volume_of_domain
      use set_element_id_4_node
      use set_normal_vectors
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(inout) :: geofem
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(jacobians_type), intent(inout) :: jacobians
!
      integer(kind = kint) :: iflag
      type(shape_finctions_at_points) :: spfs
!
!
!  -----  Const Neighboring information
      if(viz_step%LIC_t%increment .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+16)
        if(iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
        call set_belonged_ele_and_next_nod                              &
     &     (geofem%mesh, next_tbl%neib_ele, next_tbl%neib_nod)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+16)
      end if
!
      iflag = viz_step%PVR_t%increment + viz_step%LIC_t%increment       &
     &     + viz_step%FLINE_t%increment + viz_step%TRACER_t%increment
      if(iflag .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+16)
        if(iflag_debug.gt.0) write(*,*) 'jacobian_and_element_volume'
!        call sel_max_int_point_by_etype                                &
!     &     (geofem%mesh%ele%nnod_4_ele, jacobians%g_FEM)
        call set_max_integration_points(ione, jacobians%g_FEM)
        call jacobian_and_element_volume(my_rank, nprocs,               &
     &      geofem%mesh, geofem%group, spfs, jacobians)
        if (iflag_debug.eq.1) write(*,*) 'surf_jacobian_sf_grp_normal'
        call surf_jacobian_sf_grp_normal(my_rank, nprocs,               &
     &      geofem%mesh, geofem%group, spfs, jacobians)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+16)
      end if
!
      end subroutine normals_and_jacobians_4_VIZ
!
! ----------------------------------------------------------------------
!
      end module FEM_to_VIZ_bridge
