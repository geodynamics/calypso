!>@file   init_radial_infos_sph_mhd.f90
!!@brief  module init_radial_infos_sph_mhd
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n    Modified in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!!      subroutine init_r_infos_sph_mhd_evo(bc_IO, sph_grps, MHD_BC,    &
!!     &          ipol, sph, r_2nd, omega_sph, MHD_prop, sph_MHD_bc)
!!      subroutine init_r_infos_sph_mhd(bc_IO, sph_grps, MHD_BC, sph,   &
!!     &                                MHD_prop, omega_sph, sph_MHD_bc)
!!      subroutine init_reference_fields(sph, ipol, r_2nd,              &
!!     &          refs, rj_fld, MHD_prop, sph_MHD_bc)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(inout) :: r_2nd
!!        type(sph_rotation), intent(inout) :: omega_sph
!!        type(radial_reference_field), intent(inout) :: refs
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@n @param r_hot        radius at highest temperature point
!!@n @param r_cold       radius at lowest temperature point
!!@n @param temp_hot     temperature at highest temperature point
!!@n @param temp_cold    temperature at lowest temperature point
!!@n @param rotate(3)    rotation vector
!
      module init_radial_infos_sph_mhd
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_spheric_constants
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_parameter
      use t_spheric_group
      use t_poloidal_rotation
      use t_radial_reference_field
      use t_fdm_coefs
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_boundary_data_sph_MHD
      use t_phys_address
      use t_phys_data
      use t_work_4_sph_trans
      use t_physical_property
!
      implicit none
!
      private :: set_delta_r_4_sph_mhd
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine init_r_infos_sph_mhd_evo(bc_IO, sph_grps, MHD_BC,      &
     &          ipol, sph, r_2nd, omega_sph, MHD_prop, sph_MHD_bc)
!
      use second_fdm_node_coefs
      use material_property
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_group_data), intent(in) :: sph_grps
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(phys_address), intent(in) :: ipol
      type(sph_grids), intent(in) :: sph
!
      type(fdm_matrices), intent(inout) :: r_2nd
      type(sph_rotation), intent(inout) :: omega_sph
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
!
      call init_r_infos_sph_mhd(bc_IO, sph_grps, MHD_BC, sph, MHD_prop, &
     &                          omega_sph, sph_MHD_bc)
!
      if (iflag_debug.gt.0) write(*,*) 'const_second_fdm_coefs'
      call const_second_fdm_coefs(sph%sph_params, sph%sph_rj, r_2nd)
!
      if(iflag_debug.gt.0) write(*,*)' set_material_property'
      call set_material_property                                        &
     &   (sph%sph_params%radius_CMB, sph%sph_params%radius_ICB,         &
     &    ipol, MHD_prop)
!
      end subroutine init_r_infos_sph_mhd_evo
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine init_r_infos_sph_mhd(bc_IO, sph_grps, MHD_BC, sph,     &
     &                                MHD_prop, omega_sph, sph_MHD_bc)
!
      use set_bc_sph_mhd
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_group_data), intent(in) :: sph_grps
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(sph_rotation), intent(inout) :: omega_sph
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_delta_r_4_sph_mhd'
      call set_delta_r_4_sph_mhd(sph%sph_params, sph%sph_rj)
!
!*  ----------  rotation of earth  ---------------
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &                write(*,*) 'set_rot_earth_4_sph'
      call set_rot_earth_4_sph(sph%sph_rlm, sph%sph_rj,                 &
     &    MHD_prop%fl_prop, omega_sph)
!
!*  ---------- boundary conditions  ---------------
      if(iflag_debug.gt.0) write(*,*) 's_set_bc_sph_mhd'
      call s_set_bc_sph_mhd                                             &
     &   (bc_IO, sph%sph_params, sph%sph_rj, sph_grps%radial_rj_grp,    &
     &    MHD_prop, MHD_BC, sph_MHD_bc)
!
      end subroutine init_r_infos_sph_mhd
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_delta_r_4_sph_mhd(sph_params, sph_rj)
!
      use set_radius_func_noequi
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_shell_parameters), intent(in) :: sph_params
!
!   Choose radial grid mode
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_dr_for_nonequi'
      call allocate_dr_rj_noequi(sph_rj%nidx_rj(1))
      call set_dr_for_nonequi(sph_params%nlayer_CMB,                    &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r)
!*
      end subroutine set_delta_r_4_sph_mhd
!
!  -------------------------------------------------------------------
!
      subroutine init_reference_fields(sph, ipol, r_2nd,                &
     &          refs, rj_fld, MHD_prop, sph_MHD_bc)
!
      use sph_mhd_rst_IO_control
      use reference_sources_from_d_rj
      use init_reference_scalar
      use init_external_magne_sph
      use radial_reference_field_IO
      use m_base_field_labels
!
      type(phys_address), intent(in) :: ipol
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
!
      type(radial_reference_field), intent(inout) :: refs
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
      type(phys_data), intent(inout) :: rj_fld
!
      character(len=kchara), parameter                                  &
     &            :: tmat_name = 'reference_Temperature'
      character(len=kchara), parameter                                  &
     &            :: cmat_name = 'reference_Composition'
      logical :: flag_write_ref
!
!
      flag_write_ref = .FALSE.
      call init_reft_rj_data(sph%sph_rj, ipol, refs)
      call cal_ref_sources_from_d_rj(sph, ipol, rj_fld, refs)
      call load_sph_reference_fields(refs)
      call overwrite_sources_by_reference(sph%sph_rj, refs%iref_base,   &
     &    ipol%base, refs%ref_field, rj_fld)
!
!
      call s_init_reference_scalar                                      &
     &   (MHD_prop%takepito_T, sph%sph_params, sph%sph_rj,              &
     &    r_2nd, MHD_prop%ht_prop, sph_MHD_bc%sph_bc_T,                 &
     &    sph_MHD_bc%fdm2_center, tmat_name, MHD_prop%ref_param_T,      &
     &    refs%iref_radius, temperature%name,                           &
     &    refs%iref_base%i_temp, refs%iref_grad%i_grad_temp,            &
     &    refs%iref_base%i_heat_source, refs%r_itp, refs%ref_fld_IO,    &
     &    refs%ref_field, sph_MHD_bc%bcs_T, flag_write_ref)
!
      call s_init_reference_scalar                                      &
     &   (MHD_prop%takepito_C, sph%sph_params, sph%sph_rj,              &
     &    r_2nd, MHD_prop%cp_prop, sph_MHD_bc%sph_bc_C,                 &
     &    sph_MHD_bc%fdm2_center, cmat_name, MHD_prop%ref_param_C,      &
     &    refs%iref_radius, composition%name,                           &
     &    refs%iref_base%i_light, refs%iref_grad%i_grad_composit,       &
     &    refs%iref_base%i_light_source, refs%r_itp, refs%ref_fld_IO,   &
     &    refs%ref_field, sph_MHD_bc%bcs_C, flag_write_ref)
!
      call init_sph_contant_ext_magne(MHD_prop%cd_prop, sph%sph_rj,     &
     &    refs%iref_cmp, ipol%base, refs%ref_field, rj_fld,             &
     &    flag_write_ref)
!
      call calypso_mpi_barrier
!
      if(flag_write_ref .eqv. .FALSE.) return
      call set_default_reference_file_name(refs)
      call output_reference_field(refs)
!
      end subroutine init_reference_fields
!
!  -------------------------------------------------------------------
!
      end module init_radial_infos_sph_mhd
