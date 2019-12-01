!t_SPH_MHD_zonal_mean_viz.f90
!      module t_SPH_MHD_zonal_mean_viz
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine init_zonal_mean_sections                             &
!!     &         (fem, nod_fld, zm_ctls, zmeans)
!!        type(mesh_data), intent(in) :: fem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!!        type(sph_zonal_mean_sectioning), intent(inout) :: zmeans
!!      subroutine SPH_MHD_zmean_sections(viz_step, time_d,             &
!!     &          sph, fem, WK, nod_fld, zmeans)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(sph_grids), intent(in) :: sph
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: fem
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sph_zonal_mean_sectioning), intent(inout) :: zmeans
!!
!!      subroutine SPH_MHD_zonal_mean_section(viz_step, time_d,         &
!!     &          sph, fem, nod_fld, zm_psf)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(sph_grids), intent(in) :: sph
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: fem
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sectioning_module), intent(inout) :: zm_psf
!
      module t_SPH_MHD_zonal_mean_viz
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_VIZ
      use calypso_mpi
!
      use t_VIZ_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_spheric_parameter
      use t_sph_trans_arrays_MHD
      use t_cross_section
!
      implicit  none
!
!>      Structures of zonal mean controls
      type sph_zonal_mean_sectioning
!>        Structures of zonal mean sectioning controls
        type(sectioning_module) :: zm_psf
!>        Structures of zonal RMS sectioning controls
        type(sectioning_module) :: zrms_psf
      end type sph_zonal_mean_sectioning
!
      private :: SPH_MHD_zonal_RMS_section
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_zonal_mean_sections                               &
     &         (fem, nod_fld, zm_ctls, zmeans)
!
      use t_control_data_dynamo_vizs
!
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: nod_fld
!
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
      type(sph_zonal_mean_sectioning), intent(inout) :: zmeans
!
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+1)
      call SECTIONING_initialize                                        &
     &   (fem, nod_fld, zm_ctls%zm_psf_ctls, zmeans%zm_psf)
      call SECTIONING_initialize                                        &
     &   (fem, nod_fld, zm_ctls%zRMS_psf_ctls, zmeans%zrms_psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+1)
!
      end subroutine init_zonal_mean_sections
!
!  ---------------------------------------------------------------------
!
      subroutine SPH_MHD_zmean_sections(viz_step, time_d,               &
     &          sph, fem, WK, nod_fld, zmeans)
!
      use FEM_analyzer_sph_MHD
      use nod_phys_send_recv
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: fem
      type(works_4_sph_trans_MHD), intent(in) :: WK
!
      type(phys_data), intent(inout) :: nod_fld
      type(sph_zonal_mean_sectioning), intent(inout) :: zmeans
!
!
      call SPH_MHD_zonal_mean_section(viz_step, time_d,                 &
     &    sph, fem, nod_fld, zmeans%zm_psf)
      call SPH_MHD_zonal_RMS_section(viz_step, time_d,                  &
     &    sph, fem, WK, nod_fld, zmeans%zrms_psf)
!
      end subroutine SPH_MHD_zmean_sections
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SPH_MHD_zonal_mean_section(viz_step, time_d,           &
     &          sph, fem, nod_fld, zm_psf)
!
      use m_elapsed_labels_4_VIZ
      use sph_rtp_zonal_rms_data
      use nod_phys_send_recv
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: fem
!
      type(phys_data), intent(inout) :: nod_fld
      type(sectioning_module), intent(inout) :: zm_psf
!
!
      if(viz_step%PSF_t%istep_file.le.0) return
      if(zm_psf%num_psf .le. 0) return
!
      if (iflag_debug.gt.0) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field                                     &
     &   (sph%sph_rtp, fem%mesh%node, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(fem%mesh, nod_fld)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+6)
      if (iflag_debug.gt.0) write(*,*) 'SECTIONING_visualize zmean'
      call SECTIONING_visualize(viz_step%PSF_t%istep_file, time_d,      &
     &    fem, nod_fld, zm_psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+6)
!
      end subroutine SPH_MHD_zonal_mean_section
!
!  ---------------------------------------------------------------------
!
      subroutine SPH_MHD_zonal_RMS_section(viz_step, time_d,            &
     &          sph, fem, WK, nod_fld, zrms_psf)
!
      use m_elapsed_labels_4_VIZ
      use FEM_analyzer_sph_MHD
      use sph_rtp_zonal_rms_data
      use nod_phys_send_recv
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: fem
      type(works_4_sph_trans_MHD), intent(in) :: WK
!
      type(phys_data), intent(inout) :: nod_fld
      type(sectioning_module), intent(inout) :: zrms_psf
!
!
      if(viz_step%PSF_t%istep_file.le.0) return
      if(zrms_psf%num_psf .le. 0) return
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_MHD'
      call SPH_to_FEM_bridge_MHD(sph, WK, fem%mesh, nod_fld)
      call zonal_rms_all_rtp_field                                      &
     &   (sph%sph_rtp, fem%mesh%node, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(fem%mesh, nod_fld)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+6)
      if (iflag_debug.gt.0) write(*,*) 'SECTIONING_visualize RMS'
      call SECTIONING_visualize(viz_step%PSF_t%istep_file, time_d,      &
     &    fem, nod_fld, zrms_psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+6)
!
      end subroutine SPH_MHD_zonal_RMS_section
!
!  ---------------------------------------------------------------------
!
      end module t_SPH_MHD_zonal_mean_viz
