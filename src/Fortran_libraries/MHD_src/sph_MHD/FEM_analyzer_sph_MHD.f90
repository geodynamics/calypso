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
!!      subroutine FEM_initialize
!!      subroutine FEM_analyze(i_step                                   &
!!     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!!      subroutine FEM_finalize
!!
!!      subroutine SPH_to_FEM_init_MHD
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
      subroutine FEM_initialize
!
      use m_geometry_parameter
      use m_array_for_send_recv
      use m_t_step_parameter
      use m_surface_geometry_data
      use m_edge_geometry_data
      use m_node_phys_address
      use m_cal_max_indices
!
      use load_mesh_data
      use const_mesh_info
      use nodal_vector_send_recv
      use output_ucd_file_control
      use range_data_IO
      use node_monitor_IO
!
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!  --  load FEM mesh data
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call start_eleps_time(4)
      call input_mesh(my_rank)
      call end_eleps_time(4)
!
! ---------------------------------
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
!
      end subroutine FEM_initialize
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze(i_step,                                    &
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
      end subroutine FEM_analyze
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_init_MHD
!
      use pole_sph_transform
!
!
      call init_pole_transform
!
      end subroutine SPH_to_FEM_init_MHD
!
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_MHD
!
      use output_viz_file_control
      use lead_pole_data_4_sph_mhd
      use cvt_nod_data_to_sph_data
      use nod_phys_send_recv
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
      if(iflag .ne. 0) return
!*
!*  ----------- transform field at pole and center --------------
!*
      call lead_pole_fields_4_sph_mhd
!*
!*  -----------  data transfer to FEM array --------------
!*
      call copy_nod_scalar_from_sph_data
      call cvt_xyz_from_sph_vec_sph_data
      call cvt_sph_to_xyz_tensor_data
!
      call phys_send_recv_4_viz
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
