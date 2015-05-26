!>@file   FEM_analyzer_sph_MHD.f90
!!@brief  module FEM_analyzer_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine FEM_initialize_sph_MHD
!!      subroutine FEM_analyze_sph_MHD(i_step                           &
!!     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!!      subroutine FEM_finalize
!!
!!      subroutine SPH_to_FEM_bridge_MHD
!!      subroutine FEM_to_SPH_bridge
!!@endverbatim
!!
!!@n @param  i_step       Current time step
!!@n @param  istep_psf    Time step increment for cross sectioning
!!@n @param  istep_iso    Time step increment for iso surfaces
!!@n @param  istep_pvr    Time step increment for volume rendering
!!@n @param  istep_fline  Time step increment for field line generation
!!@n @param  visval       Return flag to call visualization routines
!
      module FEM_analyzer_sph_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_work_time
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_sph_MHD
!
      use m_array_for_send_recv
      use m_t_step_parameter
      use m_surface_geometry_data
      use m_edge_geometry_data
      use m_node_phys_address
      use m_cal_max_indices
!
      use const_mesh_info
      use nodal_vector_send_recv
      use output_ucd_file_control
      use range_data_IO
      use node_monitor_IO
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_local_node_id_4_monitor'
      call set_local_node_id_4_monitor
!
!  -----    construct geometry informations
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
      call deallocate_surface_geometry
      call deallocate_edge_geometry
!
!  -------------------------------
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_nod_field_data'
      call initialize_nod_field_data
!
!  connect grid data to volume output
!
      if(i_step_output_ucd.gt.0) call allocate_phys_range
!
      if(iflag_debug .gt. 0) write(*,*) 'output_grd_file_4_snapshot'
      call output_grd_file_4_snapshot
!
      end subroutine FEM_initialize_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_MHD(i_step,                            &
     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      use set_exit_flag_4_visualizer
      use output_viz_file_control
      use output_ucd_file_control
!
      integer (kind =kint), intent(in) :: i_step
!
      integer (kind =kint), intent(inout) :: visval
      integer(kind = kint), intent(inout) :: istep_psf, istep_iso
      integer(kind = kint), intent(inout) :: istep_pvr, istep_fline
!
      integer (kind =kint) :: iflag
!
!
      visval = 1
      call set_lead_physical_values_flag(iflag)
!
      if(iflag .eq. 0) then
!*  ----------   Count steps for visualization
!*
        call set_flag_to_visualization(i_step,                          &
     &        istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!*
!*  -----------  Output volume data --------------
!*
        call s_output_ucd_file_control
      end if
!
      end subroutine FEM_analyze_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_MHD
!
      use output_viz_file_control
      use lead_pole_data_4_sph_mhd
      use nod_phys_send_recv
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use coordinate_convert_4_sph
!
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
      if(iflag .ne. 0) return
!*
!*  -----------  data transfer to FEM array --------------
!*
      call copy_forces_to_snapshot_rtp
      call copy_snap_vec_fld_from_trans
      call copy_snap_vec_fld_to_trans
!
      call overwrite_nodal_sph_2_xyz
!
!*  ----------- transform field at pole and center --------------
!*
      call lead_pole_fields_4_sph_mhd
!
      call phys_send_recv_all
!
      end subroutine SPH_to_FEM_bridge_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!      subroutine FEM_to_SPH_bridge
!
!
!      end subroutine FEM_to_SPH_bridge
!
!-----------------------------------------------------------------------
!
      subroutine FEM_finalize
!
      use m_t_step_parameter
      use m_cal_max_indices
      use output_parallel_ucd_file
!
!
     if(i_step_output_ucd.gt.0) then
       call deallocate_phys_range
       call finalize_ucd_file_output
     end if
!
      end subroutine FEM_finalize
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_MHD
