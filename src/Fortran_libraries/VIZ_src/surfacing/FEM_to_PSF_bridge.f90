!>@file   FEM_to_PSF_bridge.f90
!!@brief  module FEM_to_PSF_bridge
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Data structuresa for sectioning module
!!
!!@verbatim
!!      subroutine init_FEM_to_PSF_bridge                               &
!!     &         (elps_SECT, viz_step, geofem, edge_comm, m_SR)
!!        type(elapsed_labels_4_SECTIONS), intent(in) :: elps_SECT
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(communication_table), intent(inout) :: edge_comm
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module FEM_to_PSF_bridge
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_phys_data
      use t_comm_table
      use t_VIZ_step_parameter
      use t_mesh_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_FEM_to_PSF_bridge                                 &
     &         (elps_SECT, viz_step, geofem, edge_comm, m_SR)
!
      use t_elapsed_labels_4_SECTIONS
      use parallel_FEM_mesh_init
      use const_element_comm_tables
!
      type(elapsed_labels_4_SECTIONS), intent(in) :: elps_SECT
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(inout) :: geofem
      type(communication_table), intent(inout) :: edge_comm
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: iflag
!
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(geofem%mesh, geofem%group,           &
     &                             m_SR%SR_sig, m_SR%SR_i)
!
!     --------------------- init for sectioning
!
      iflag = viz_step%PSF_t%increment + viz_step%ISO_t%increment
      if(iflag .gt. 0) then
        if(elps_SECT%flag_elapsed_S)                                    &
     &           call start_elapsed_time(elps_SECT%ist_elapsed_S+6)
        if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
        call const_edge_comm_table                                      &
     &     (geofem%mesh%node, geofem%mesh%nod_comm,                     &
     &      edge_comm, geofem%mesh%edge, m_SR)
        if(elps_SECT%flag_elapsed_S)                                    &
     &           call end_elapsed_time(elps_SECT%ist_elapsed_S+6)
      end if
!
      end subroutine init_FEM_to_PSF_bridge
!
! ----------------------------------------------------------------------
!
      end module FEM_to_PSF_bridge
