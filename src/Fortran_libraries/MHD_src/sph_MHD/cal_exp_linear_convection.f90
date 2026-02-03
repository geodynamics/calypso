!>@file   cal_exp_linear_convection.f90
!!@brief  module cal_exp_linear_convection
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine licv_exp(refs, MHD_prop, sph_MHD_bc,                 &
!!     &          sph, comms_sph, omega_sph, trans_p, ipol, WK,         &
!!     &          rj_fld, SR_sig, SR_r)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(radial_reference_field), intent(in) :: refs
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
!
      module cal_exp_linear_convection
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_physical_property
      use t_SPH_mesh_field_data
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_poloidal_rotation
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_radial_reference_field
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_solver_SR
!
      implicit none
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!*
      subroutine licv_exp(refs, MHD_prop, sph_MHD_bc,                   &
     &          sph, comms_sph, omega_sph, trans_p, ipol, WK,           &
     &          rj_fld, SR_sig, SR_r)
!
      use m_phys_constants
      use cal_self_buoyancies_sph
      use rot_self_buoyancies_sph
      use sph_transforms_4_MHD
      use copy_nodal_fields
      use cal_nonlinear_sph_MHD
      use sum_rotation_of_forces
      use add_sph_ref_scalar_advect
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(radial_reference_field), intent(in) :: refs
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
!   ----  lead rotation of buoyancies
        call sel_buoyancies_sph_MHD                                     &
     &     (sph%sph_rj, trans_p%leg, ipol%forces,                       &
     &      MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,                      &
     &      ipol%base%i_temp, ipol%base%i_light, rj_fld)
!
!   ----  lead rotation of buoyancies
        if(iflag_debug.gt.0) write(*,*) 'sel_rot_buoyancy_sph_MHD'
        call sel_rot_buoyancy_sph_MHD                                   &
     &     (sph%sph_rj, ipol%base, ipol%rot_forces,                     &
     &      MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, rj_fld)
      end if
!*
!*  ----  copy velocity for coriolis term ------------------
      if(iflag_debug.eq.1) write(*,*) 'sph_transform_4_licv'
      if(MHD_prop%fl_prop%flag_coriolis) then
        call sph_transform_4_licv                                       &
     &     (sph%sph_rlm, comms_sph%comm_rlm, comms_sph%comm_rj,         &
     &      MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, omega_sph, trans_p,  &
     &      WK%gt_cor, WK%trns_MHD, rj_fld, WK%cor_rlm, SR_sig, SR_r)
      end if
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if(ipol%forces%i_h_advect .gt. 0) then
        call clear_field_data(rj_fld, n_scalar, ipol%forces%i_h_advect)
      end if
      if(ipol%forces%i_c_advect .gt. 0) then
        call clear_field_data(rj_fld, n_scalar, ipol%forces%i_c_advect)
      end if
      if(ipol%exp_work%i_forces .gt. 0) then
        call clear_field_data(rj_fld, n_vector, ipol%exp_work%i_forces)
      end if
!
      call add_ref_advect_sph_MHD(sph%sph_rj, trans_p%leg,              &
     &    sph_MHD_bc, MHD_prop, refs%iref_grad, refs%ref_field,         &
     &    ipol%base, ipol%forces, rj_fld)
!
!      call licv_forces_to_explicit(MHD_prop%fl_prop,                   &
!     &    ipol%exp_work, ipol%forces, rj_fld)

        call licv_forces_to_explicit(MHD_prop%fl_prop,                  &
     &      ipol%exp_work, ipol%rot_forces, rj_fld)
!
!
      end subroutine licv_exp
!*
!*   ------------------------------------------------------------------
!
      end module cal_exp_linear_convection
