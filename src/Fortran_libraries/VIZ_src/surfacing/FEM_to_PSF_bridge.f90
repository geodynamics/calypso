!>@file   FEM_to_PSF_bridge.f90
!!@brief  module FEM_to_PSF_bridge
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Data structuresa for sectioning module
!!
!!@verbatim
!!      subroutine init_FEM_to_PSF_bridge(viz_step, geofem, PSF_DAT)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(PSF_mesh_field), intent(inout) :: PSF_DAT
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
!
      implicit none
!
      type PSF_mesh_field
!>        Structure of edge communication table
        type(communication_table) :: edge_comm
!>        Structure of global edge data
        type(global_edge_data) :: edge_gl
      end type PSF_mesh_field
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_FEM_to_PSF_bridge(viz_step, geofem, PSF_DAT)
!
      use parallel_FEM_mesh_init
      use const_element_comm_tables
      use const_edge_comm_table
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(inout) :: geofem
      type(PSF_mesh_field), intent(inout) :: PSF_DAT
!
      integer(kind = kint) :: iflag
!
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(geofem%mesh, geofem%group)
!
!     --------------------- init for sectioning
!
      iflag = viz_step%PSF_t%increment + viz_step%ISO_t%increment
      if(iflag .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*) 's_const_edge_comm_table'
        call s_const_edge_comm_table                                    &
     &     (geofem%mesh%node, geofem%mesh%nod_comm,                     &
     &      geofem%mesh%edge, PSF_DAT%edge_comm, PSF_DAT%edge_gl)
      end if
!
      end subroutine init_FEM_to_PSF_bridge
!
! ----------------------------------------------------------------------
!
      end module FEM_to_PSF_bridge
