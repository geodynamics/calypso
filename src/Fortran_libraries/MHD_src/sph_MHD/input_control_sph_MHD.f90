!>@file   input_control_sph_MHD.f90
!!@brief  module input_control_sph_MHD
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_SPH_MHD_psf(MHD_files, bc_IO, DMHD_ctl,&
!!     &          sph, comms_sph, sph_grps, rj_fld, nod_fld, pwr,       &
!!     &          flex_p, MHD_step, MHD_prop, MHD_BC, WK,               &
!!     &          mesh, group, ele_mesh)
!!      subroutine input_control_4_SPH_MHD_nosnap(MHD_files, bc_IO,     &
!!     &          DMHD_ctl, sph, comms_sph, sph_grps, rj_fld, pwr,      &
!!     &          flex_p, MHD_step, MHD_prop, MHD_BC, WK)
!!
!!      subroutine input_control_4_SPH_make_init                        &
!!     &         (MHD_files, bc_IO, DMHD_ctl, sph, comms_sph, sph_grps, &
!!     &          rj_fld, pwr, flex_p, MHD_step, mesh, group, ele_mesh, &
!!     &          MHD_prop, MHD_BC, WK)
!!      subroutine input_control_SPH_dynamobench                        &
!!     &          (MHD_files, bc_IO, DMHD_ctl, sph, comms_sph, sph_grps,&
!!     &           rj_fld, nod_fld, pwr, flex_p, MHD_step,              &
!!     &           MHD_prop, MHD_BC, WK, cdat)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!!        type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!        type(construct_spherical_grid), intent(inout) :: gen_sph1
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(sph_filters_type), intent(inout) :: sph_filters(1)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!        type(circle_fld_maker), intent(inout) :: cdat
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
      use t_control_parameter
      use t_const_spherical_grid
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_spheric_parameter
      use t_mesh_data
      use t_phys_data
      use t_spheric_mesh
      use t_group_data
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_sph_trans_arrays_MHD
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_select_make_SPH_mesh
      use t_flex_delta_t_data
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
      subroutine input_control_SPH_MHD_psf(MHD_files, bc_IO, DMHD_ctl,  &
     &          sph, comms_sph, sph_grps, rj_fld, nod_fld, pwr,         &
     &          flex_p, MHD_step, MHD_prop, MHD_BC, WK,                 &
     &          mesh, group, ele_mesh)
!
      use t_ctl_data_MHD
      use m_error_IDs
!
      use set_control_sph_mhd
      use sph_file_IO_select
      use set_control_4_SPH_to_FEM
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
      type(sph_mean_squares), intent(inout) :: pwr
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%Dmodel_ctl,          &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%smonitor_ctl,                    &
     &    DMHD_ctl%nmtr_ctl, DMHD_ctl%psph_ctl, sph_maker1%sph_tmp,     &
     &    rj_fld, MHD_files, bc_IO, pwr, flex_p, MHD_step, MHD_prop,    &
     &    MHD_BC, WK%WK_sph, sph_maker1%gen_sph)
!
      call s_set_control_4_SPH_to_FEM                                   &
     &   (DMHD_ctl%psph_ctl%spctl, sph%sph_params, rj_fld, nod_fld)
!
!
      call select_make_SPH_mesh(DMHD_ctl%psph_ctl%iflag_sph_shell,      &
     &    sph, comms_sph, sph_grps, sph_maker1,                         &
     &    mesh, group, ele_mesh, MHD_files%mesh_file_IO)
!
      call sph_boundary_IO_control(MHD_prop, MHD_BC, bc_IO)
!
      end subroutine input_control_SPH_MHD_psf
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_MHD_nosnap(MHD_files, bc_IO,       &
     &          DMHD_ctl, sph, comms_sph, sph_grps, rj_fld, pwr,        &
     &          flex_p, MHD_step, MHD_prop, MHD_BC, WK)
!
      use t_ctl_data_MHD
      use set_control_sph_mhd
      use parallel_load_data_4_sph
      use set_control_4_SPH_to_FEM
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mean_squares), intent(inout) :: pwr
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%Dmodel_ctl,          &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%smonitor_ctl,                    &
     &    DMHD_ctl%nmtr_ctl, DMHD_ctl%psph_ctl, sph_maker1%sph_tmp,     &
     &    rj_fld, MHD_files, bc_IO, pwr, flex_p, MHD_step, MHD_prop,    &
     &    MHD_BC, WK%WK_sph, sph_maker1%gen_sph)
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh(sph, comms_sph, sph_grps)
!
      call sph_boundary_IO_control(MHD_prop, MHD_BC, bc_IO)
!
      end subroutine input_control_4_SPH_MHD_nosnap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_make_init                          &
     &         (MHD_files, bc_IO, DMHD_ctl, sph, comms_sph, sph_grps,   &
     &          rj_fld, pwr, flex_p, MHD_step, mesh, group, ele_mesh,   &
     &          MHD_prop, MHD_BC, WK)
!
      use t_ctl_data_MHD
      use set_control_sph_mhd
      use parallel_load_data_4_sph
      use set_control_4_SPH_to_FEM
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mean_squares), intent(inout) :: pwr
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%Dmodel_ctl,          &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%smonitor_ctl,                    &
     &    DMHD_ctl%nmtr_ctl, DMHD_ctl%psph_ctl, sph_maker1%sph_tmp,     &
     &    rj_fld, MHD_files, bc_IO, pwr, flex_p, MHD_step, MHD_prop,    &
     &    MHD_BC, WK%WK_sph, sph_maker1%gen_sph)
!
      call select_make_SPH_mesh(DMHD_ctl%psph_ctl%iflag_sph_shell,      &
     &    sph, comms_sph, sph_grps, sph_maker1,                         &
     &    mesh, group, ele_mesh, MHD_files%mesh_file_IO)
!
      call sph_boundary_IO_control(MHD_prop, MHD_BC, bc_IO)
!
      end subroutine input_control_4_SPH_make_init
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_dynamobench                          &
     &          (MHD_files, bc_IO, DMHD_ctl, sph, comms_sph, sph_grps,  &
     &           rj_fld, nod_fld, pwr, flex_p, MHD_step,                &
     &           MHD_prop, MHD_BC, WK, cdat)
!
      use t_ctl_data_MHD
      use t_field_on_circle
      use set_control_sph_mhd
      use set_control_sph_data_MHD
      use parallel_load_data_4_sph
      use set_control_4_SPH_to_FEM
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
      type(sph_mean_squares), intent(inout) :: pwr
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(circle_fld_maker), intent(inout) :: cdat
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%Dmodel_ctl,          &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%smonitor_ctl,                    &
     &    DMHD_ctl%nmtr_ctl, DMHD_ctl%psph_ctl, sph_maker1%sph_tmp,     &
     &    rj_fld, MHD_files, bc_IO, pwr, flex_p, MHD_step, MHD_prop,    &
     &    MHD_BC, WK%WK_sph, sph_maker1%gen_sph)
!
      call s_set_control_4_SPH_to_FEM                                   &
     &   (DMHD_ctl%psph_ctl%spctl, sph%sph_params, rj_fld, nod_fld)
      call set_ctl_params_dynamobench                                   &
     &   (DMHD_ctl%Dmodel_ctl%fld_ctl%field_ctl,                        &
     &    DMHD_ctl%smonitor_ctl%meq_ctl, cdat%circle, cdat%d_circle)
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh(sph, comms_sph, sph_grps)
!
      end subroutine input_control_SPH_dynamobench
!
! ----------------------------------------------------------------------
!
      end module input_control_sph_MHD
