!>@file   cal_nonlinear.f90
!!@brief  module cal_nonlinear
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine nonlinear(r_2nd, SPH_model, trans_p, WK, SPH_MHD)
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!      subroutine nonlinear_by_pseudo_sph                              &
!!     &         (sph, comms_sph, omega_sph, r_2nd, MHD_prop,           &
!!     &          sph_MHD_bc, trans_p, gt_cor, trns_MHD, WK_sph,        &
!!     &          cor_rlm, ipol, itor, rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!!        type(phys_address), intent(in) :: ipol, itor
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine licv_exp(ref_temp, ref_comp, MHD_prop, sph_MHD_bc,   &
!!     &          sph, comms_sph, omega_sph, trans_p, ipol, itor,       &
!!     &          WK, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol, itor
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(reference_temperature), intent(in) :: ref_temp
!!        type(reference_temperature), intent(in) :: ref_comp
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!@endverbatim
!
!
      module cal_nonlinear
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_physical_property
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_poloidal_rotation
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_radial_reference_temp
      use t_sph_transforms
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
!
      implicit none
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!*
      subroutine nonlinear(r_2nd, SPH_model, trans_p, WK, SPH_MHD)
!
      use cal_inner_core_rotation
!
      use cal_nonlinear_sph_MHD
      use sum_rotation_of_forces
!
      use m_work_time
      use m_elapsed_labels_4_MHD
!
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SPH_MHD_model_data), intent(in) :: SPH_model
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if (iflag_debug.eq.1) write(*,*) 'nonlinear_by_pseudo_sph'
      call nonlinear_by_pseudo_sph                                      &
     &   (SPH_MHD%sph, SPH_MHD%comms, SPH_model%omega_sph,              &
     &    r_2nd, SPH_model%MHD_prop, SPH_model%sph_MHD_bc, trans_p,     &
     &    WK%gt_cor, WK%trns_MHD, WK%WK_sph, WK%cor_rlm,                &
     &    SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
!
!   ----  Lead advection of reference field
      call add_ref_advect_sph_MHD                                       &
     &   (SPH_MHD%sph%sph_rj, SPH_model%sph_MHD_bc, SPH_model%MHD_prop, &
     &    trans_p%leg, SPH_model%ref_temp, SPH_model%ref_comp,          &
     &    SPH_MHD%ipol, SPH_MHD%fld)
!
!*  ----  copy coriolis term for inner core rotation
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+8)
      call copy_icore_rot_to_tor_coriolis                               &
     &   (SPH_model%sph_MHD_bc%sph_bc_U, SPH_MHD%sph%sph_rj,            &
     &    SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+8)
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_forces_to_explicit'
      call sum_forces_to_explicit                                       &
     &   (SPH_MHD%sph%sph_rj, SPH_model%MHD_prop%fl_prop,               &
     &    SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
!
      end subroutine nonlinear
!*
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_by_pseudo_sph                                &
     &         (sph, comms_sph, omega_sph, r_2nd, MHD_prop,             &
     &          sph_MHD_bc, trans_p, gt_cor, trns_MHD, WK_sph,          &
     &          cor_rlm, ipol, itor, rj_fld)
!
      use sph_transforms_4_MHD
      use cal_nonlinear_sph_MHD
      use cal_sph_field_by_rotation
!
      use m_elapsed_labels_4_MHD
      use m_work_time
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_rotation), intent(in) :: omega_sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
      type(phys_address), intent(in) :: ipol, itor
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(phys_data), intent(inout) :: rj_fld
!
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+9)
      if (iflag_debug.ge.1) write(*,*) 'sph_back_trans_4_MHD'
      call sph_back_trans_4_MHD                                         &
     &   (sph, comms_sph, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,        &
     &    omega_sph, trans_p, gt_cor, rj_fld, trns_MHD%b_trns,          &
     &    trns_MHD%backward, WK_sph, trns_MHD%mul_FFTW, cor_rlm)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+9)
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+10)
      if (iflag_debug.ge.1) write(*,*) 'nonlinear_terms_in_rtp'
      call nonlinear_terms_in_rtp(sph%sph_rtp, MHD_prop, trans_p%leg,   &
     &    trns_MHD%b_trns, trns_MHD%f_trns, trns_MHD%backward,          &
     &    trns_MHD%forward)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+10)
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+11)
      if (iflag_debug.ge.1) write(*,*) 'sph_forward_trans_4_MHD'
      call sph_forward_trans_4_MHD(sph, comms_sph, MHD_prop%fl_prop,    &
     &    trans_p, cor_rlm, trns_MHD%f_trns, trns_MHD%forward,          &
     &    WK_sph, trns_MHD%mul_FFTW, rj_fld)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+11)
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+12)
      if (iflag_debug.ge.1) write(*,*) 'rot_momentum_eq_exp_sph'
      call rot_momentum_eq_exp_sph(sph%sph_rj, r_2nd, sph_MHD_bc,       &
     &    trans_p%leg, ipol, itor, rj_fld)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+12)
!
      end subroutine nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!*
      subroutine licv_exp(ref_temp, ref_comp, MHD_prop, sph_MHD_bc,     &
     &          sph, comms_sph, omega_sph, trans_p, ipol, itor,         &
     &          WK, rj_fld)
!
      use m_phys_constants
      use sph_transforms_4_MHD
      use copy_nodal_fields
      use cal_nonlinear_sph_MHD
      use sum_rotation_of_forces
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol, itor
      type(reference_temperature), intent(in) :: ref_temp, ref_comp
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: rj_fld
!
!*  ----  copy velocity for coriolis term ------------------
!*
      if(iflag_debug.eq.1) write(*,*) 'sph_transform_4_licv'
      if(MHD_prop%fl_prop%iflag_4_coriolis .ne. id_turn_OFF) then
        call sph_transform_4_licv                                       &
     &     (sph%sph_rlm, comms_sph%comm_rlm, comms_sph%comm_rj,         &
     &      MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, omega_sph,           &
     &      trans_p%leg, WK%gt_cor, WK%trns_MHD,                        &
     &      rj_fld, WK%cor_rlm)
      end if
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if(ipol%i_h_advect .gt. 0) then
        call clear_field_data(rj_fld, n_scalar, ipol%i_h_advect)
      end if
      if(ipol%i_c_advect .gt. 0) then
        call clear_field_data(rj_fld, n_scalar, ipol%i_c_advect)
      end if
      if(ipol%i_forces .gt. 0) then
        call clear_field_data(rj_fld, n_vector, ipol%i_forces)
      end if
!
!
      call add_ref_advect_sph_MHD                                       &
     &   (sph%sph_rj, sph_MHD_bc, MHD_prop,                             &
     &    trans_p%leg, ref_temp, ref_comp, ipol, rj_fld)
!
      call licv_forces_to_explicit                                      &
     &   (sph%sph_rj, MHD_prop%fl_prop, ipol, itor, rj_fld)
!
!
      end subroutine licv_exp
!*
!*   ------------------------------------------------------------------
!
      end module cal_nonlinear
