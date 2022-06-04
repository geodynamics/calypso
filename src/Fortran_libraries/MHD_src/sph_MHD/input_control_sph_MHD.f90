!>@file   input_control_sph_MHD.f90
!!@brief  module input_control_sph_MHD
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_SPH_MHD_psf(MHD_files, DMHD_ctl,       &
!!     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!!      subroutine input_control_4_SPH_MHD_nosnap(MHD_files, DMHD_ctl,  &
!!     &          MHD_step, SPH_model, SPH_WK, SPH_MHD)
!!
!!      subroutine input_control_4_SPH_make_init(MHD_files, DMHD_ctl,   &
!!     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!!@endverbatim
!
!
      module input_control_sph_MHD
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_const_spherical_grid
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_FEM_mesh_field_data
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_flex_delta_t_data
      use t_work_SPH_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_MHD_psf(MHD_files, DMHD_ctl,         &
     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!
      use t_ctl_data_MHD
      use m_error_IDs
!
      use set_control_sph_mhd
      use sph_file_IO_select
      use set_control_4_SPH_to_FEM
      use parallel_load_data_4_sph
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%model_ctl,           &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%nmtr_ctl, DMHD_ctl%psph_ctl,     &
     &    MHD_files, SPH_model%bc_IO, SPH_model%refs, MHD_step,         &
     &    SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_WK%trans_p,         &
     &    SPH_WK%trns_WK, SPH_MHD)
!
      call set_control_SPH_MHD_w_viz(DMHD_ctl%model_ctl,                &
     &    DMHD_ctl%psph_ctl, DMHD_ctl%smonitor_ctl, DMHD_ctl%zm_ctls,   &
     &    SPH_model%MHD_prop, SPH_MHD%sph, SPH_MHD%fld, FEM_dat%field,  &
     &    SPH_WK%monitor)
!
!  Load spherical shell table
      if (iflag_debug.eq.1) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (MHD_files%FEM_mesh_flags, MHD_files%sph_file_param,           &
     &    SPH_MHD, FEM_dat%geofem, MHD_files%mesh_file_IO)
!
      call dealloc_sph_mhd_ctl_data(DMHD_ctl)
!
      call sph_boundary_IO_control                                      &
     &   (SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_model%bc_IO)
!
      end subroutine input_control_SPH_MHD_psf
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_MHD_nosnap(MHD_files, DMHD_ctl,    &
     &          MHD_step, SPH_model, SPH_WK, SPH_MHD)
!
      use t_ctl_data_MHD
      use set_control_sph_mhd
      use parallel_load_data_4_sph
      use set_control_4_SPH_to_FEM
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%model_ctl,           &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%nmtr_ctl, DMHD_ctl%psph_ctl,     &
     &    MHD_files, SPH_model%bc_IO, SPH_model%refs, MHD_step,         &
     &    SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_WK%trans_p,         &
     &    SPH_WK%trns_WK, SPH_MHD)
!
      call set_control_SPH_MHD_noviz                                    &
     &   (DMHD_ctl%model_ctl, DMHD_ctl%smonitor_ctl,                    &
     &    SPH_model%MHD_prop, SPH_MHD%fld, SPH_WK%monitor)
!
      if (iflag_debug.eq.1) write(*,*) 'check_and_make_SPH_mesh'
      call check_and_make_SPH_mesh(MHD_files%sph_file_param, SPH_MHD)
!
      call dealloc_sph_mhd_ctl_data(DMHD_ctl)
!
      call sph_boundary_IO_control                                      &
     &    (SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_model%bc_IO)
!
      end subroutine input_control_4_SPH_MHD_nosnap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_make_init(MHD_files, DMHD_ctl,     &
     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!
      use t_ctl_data_MHD
      use set_control_sph_mhd
      use parallel_load_data_4_sph
      use set_control_4_SPH_to_FEM
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%model_ctl,           &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%nmtr_ctl, DMHD_ctl%psph_ctl,     &
     &    MHD_files, SPH_model%bc_IO, SPH_model%refs, MHD_step,         &
     &    SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_WK%trans_p,         &
     &    SPH_WK%trns_WK, SPH_MHD)
!
      call set_control_SPH_MHD_w_viz(DMHD_ctl%model_ctl,                &
     &    DMHD_ctl%psph_ctl, DMHD_ctl%smonitor_ctl, DMHD_ctl%zm_ctls,   &
     &    SPH_model%MHD_prop, SPH_MHD%sph, SPH_MHD%fld, FEM_dat%field,  &
     &    SPH_WK%monitor)
!
!  Load spherical shell table
      if (iflag_debug.eq.1) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (MHD_files%FEM_mesh_flags, MHD_files%sph_file_param,           &
     &    SPH_MHD, FEM_dat%geofem, MHD_files%mesh_file_IO)
!
      call dealloc_sph_mhd_ctl_data(DMHD_ctl)
!
      call sph_boundary_IO_control                                      &
     &   (SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_model%bc_IO)
!
      end subroutine input_control_4_SPH_make_init
!
! ----------------------------------------------------------------------
!
      end module input_control_sph_MHD
