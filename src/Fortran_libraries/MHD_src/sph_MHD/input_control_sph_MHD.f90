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
!!     &          MHD_step, SPH_model, WK, monitor, SPH_MHD, FEM_dat)
!!      subroutine input_control_4_SPH_MHD_nosnap(MHD_files, DMHD_ctl,  &
!!     &          MHD_step, SPH_model, WK, monitor, SPH_MHD)
!!
!!      subroutine input_control_4_SPH_make_init(MHD_files, DMHD_ctl,   &
!!     &          MHD_step, SPH_model, WK, monitor, SPH_MHD, FEM_dat)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
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
      use t_sph_trans_arrays_MHD
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_select_make_SPH_mesh
      use t_flex_delta_t_data
      use t_sph_mhd_monitor_data_IO
!
      implicit none
!
!>      Structure to construct grid
      type(sph_grid_maker_in_sim), save :: sph_maker1
!
      private :: sph_maker1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_MHD_psf(MHD_files, DMHD_ctl,         &
     &          MHD_step, SPH_model, WK, monitor, SPH_MHD, FEM_dat)
!
      use t_ctl_data_MHD
      use m_error_IDs
!
      use set_control_sph_mhd
      use sph_file_IO_select
      use set_control_4_SPH_to_FEM
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%Dmodel_ctl,          &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%smonitor_ctl, DMHD_ctl%nmtr_ctl, &
     &    DMHD_ctl%psph_ctl, sph_maker1%sph_tmp, SPH_MHD%fld,           &
     &    MHD_files, SPH_model%bc_IO, MHD_step, SPH_model%MHD_prop,     &
     &    SPH_model%MHD_BC, WK%WK_sph, sph_maker1%gen_sph, monitor)
!
      call s_set_control_4_SPH_to_FEM(DMHD_ctl%psph_ctl%spctl,          &
     &    SPH_MHD%sph, SPH_MHD%fld, FEM_dat%field)
!
!
      call select_make_SPH_mesh(DMHD_ctl%psph_ctl%iflag_sph_shell,      &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups, sph_maker1,       &
     &    FEM_dat%geofem, FEM_dat%ele_mesh, MHD_files)
!
      call sph_boundary_IO_control                                      &
     &   (SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_model%bc_IO)
!
      end subroutine input_control_SPH_MHD_psf
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_MHD_nosnap(MHD_files, DMHD_ctl,    &
     &          MHD_step, SPH_model, WK, monitor, SPH_MHD)
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
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%Dmodel_ctl,          &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%smonitor_ctl, DMHD_ctl%nmtr_ctl, &
     &    DMHD_ctl%psph_ctl, sph_maker1%sph_tmp, SPH_MHD%fld,           &
     &    MHD_files, SPH_model%bc_IO, MHD_step, SPH_model%MHD_prop,     &
     &    SPH_model%MHD_BC, WK%WK_sph, sph_maker1%gen_sph, monitor)
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh                                           &
     &   (SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
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
     &          MHD_step, SPH_model, WK, monitor, SPH_MHD, FEM_dat)
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
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%Dmodel_ctl,          &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%smonitor_ctl, DMHD_ctl%nmtr_ctl, &
     &    DMHD_ctl%psph_ctl, sph_maker1%sph_tmp, SPH_MHD%fld,           &
     &    MHD_files, SPH_model%bc_IO, MHD_step, SPH_model%MHD_prop,     &
     &    SPH_model%MHD_BC, WK%WK_sph, sph_maker1%gen_sph, monitor)
!
      call select_make_SPH_mesh(DMHD_ctl%psph_ctl%iflag_sph_shell,      &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups, sph_maker1,       &
     &    FEM_dat%geofem, FEM_dat%ele_mesh, MHD_files)
!
      call sph_boundary_IO_control                                      &
     &   (SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_model%bc_IO)
!
      end subroutine input_control_4_SPH_make_init
!
! ----------------------------------------------------------------------
!
      end module input_control_sph_MHD
