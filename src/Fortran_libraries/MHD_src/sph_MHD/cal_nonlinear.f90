!>@file   cal_nonlinear.f90
!!@brief  module cal_nonlinear
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine nonlinear(sph, comms_sph, omega_sph, r_2nd,          &
!!     &          MHD_prop, sph_MHD_bc, trans_p, ref_temp, ref_comp,    &
!!     &          ipol, itor, WK, rj_fld)
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
      use t_control_parameter
      use t_physical_property
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_poloidal_rotation
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_radial_reference_temp
      use t_sph_transforms
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_data_sph_MHD
!
      implicit none
!
      private :: nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine nonlinear(sph, comms_sph, omega_sph, r_2nd,            &
     &          MHD_prop, sph_MHD_bc, trans_p, ref_temp, ref_comp,      &
     &          ipol, itor, WK, rj_fld)
!
      use cal_inner_core_rotation
!
      use cal_nonlinear_sph_MHD
      use sum_rotation_of_forces
!
      use m_work_time
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol, itor
      type(reference_temperature), intent(in) :: ref_temp, ref_comp
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: rj_fld
!
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if (iflag_debug.eq.1) write(*,*) 'nonlinear_by_pseudo_sph'
      call nonlinear_by_pseudo_sph(sph, comms_sph, omega_sph,           &
     &    r_2nd, MHD_prop, sph_MHD_bc, trans_p, WK%gt_cor,              &
     &    WK%trns_MHD, WK%WK_sph, WK%MHD_mul_FFTW, WK%cor_rlm,          &
     &    ipol, itor, rj_fld)
!
!   ----  Lead advection of reference field
      call add_ref_advect_sph_MHD                                       &
     &   (sph%sph_rj, sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,         &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    trans_p%leg, ref_temp, ref_comp, ipol, rj_fld)
!
!*  ----  copy coriolis term for inner core rotation
!*
      call start_elapsed_time(13)
      if(sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call copy_icore_rot_to_tor_coriolis                             &
     &     (sph_MHD_bc%sph_bc_U%kr_in, sph%sph_rj%idx_rj_degree_one,    &
     &      sph%sph_rj%nidx_rj(2), ipol, itor,                          &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      call end_elapsed_time(13)
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_forces_to_explicit'
      call sum_forces_to_explicit                                       &
     &   (sph%sph_rj, MHD_prop%fl_prop, ipol, itor, rj_fld)
!
      end subroutine nonlinear
!*
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_by_pseudo_sph(sph, comms_sph, omega_sph,     &
     &          r_2nd, MHD_prop, sph_MHD_bc, trans_p, gt_cor, trns_MHD, &
     &          WK_sph, MHD_mul_FFTW, cor_rlm, ipol, itor, rj_fld)
!
      use sph_transforms_4_MHD
      use cal_nonlinear_sph_MHD
      use cal_sph_field_by_rotation
!
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
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(phys_data), intent(inout) :: rj_fld
!
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      call start_elapsed_time(14)
      if (iflag_debug.ge.1) write(*,*) 'sph_back_trans_4_MHD'
      call sph_back_trans_4_MHD                                         &
     &   (sph, comms_sph, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,        &
     &    omega_sph, trans_p, gt_cor, ipol, rj_fld, trns_MHD,           &
     &    WK_sph, MHD_mul_FFTW, cor_rlm)
      call end_elapsed_time(14)
!
      call start_elapsed_time(15)
      if (iflag_debug.ge.1) write(*,*) 'nonlinear_terms_in_rtp'
      call nonlinear_terms_in_rtp                                       &
     &   (sph%sph_rtp, MHD_prop%fl_prop, MHD_prop%cd_prop,              &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    trns_MHD%b_trns, trns_MHD%f_trns,                             &
     &    trns_MHD%ncomp_rj_2_rtp, trns_MHD%ncomp_rtp_2_rj,             &
     &    trns_MHD%fld_rtp, trns_MHD%frc_rtp)
!
      call start_elapsed_time(16)
      if (iflag_debug.ge.1) write(*,*) 'sph_forward_trans_4_MHD'
      call sph_forward_trans_4_MHD                                      &
     &   (sph, comms_sph, MHD_prop%fl_prop, trans_p, cor_rlm,           &
     &    ipol, trns_MHD, WK_sph, MHD_mul_FFTW, rj_fld)
      call end_elapsed_time(16)
!
      call start_elapsed_time(17)
      if (iflag_debug.ge.1) write(*,*) 'rot_momentum_eq_exp_sph'
      call rot_momentum_eq_exp_sph(sph%sph_rj, r_2nd, sph_MHD_bc,       &
     &    trans_p%leg, ipol, itor, rj_fld)
      call end_elapsed_time(17)
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
     &      ipol, rj_fld, WK%cor_rlm)
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
     &   (sph%sph_rj, sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,         &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
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
