!>@file   lead_fields_4_sph_mhd.f90
!!@brief  module lead_fields_4_sph_mhd
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine s_lead_fields_4_sph_mhd                              &
!!     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, &
!!     &          ipol, sph_MHD_mat, WK, rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine cal_sph_enegy_fluxes                                 &
!!     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, &
!!     &          ipol, trns_MHD, trns_snap,  WK_sph, rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine gradients_of_vectors_sph                             &
!!     &         (sph, comms_sph, r_2nd, sph_MHD_bc, trans_p,           &
!!     &          ipol, trns_MHD, trns_tmp, WK_sph, rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_tmp
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module lead_fields_4_sph_mhd
!
      use m_precision
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_addresses_sph_transform
      use t_sph_trans_arrays_MHD
      use t_sph_matrices
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_transforms
      use t_boundary_data_sph_MHD
      use t_radial_matrices_sph_MHD
!
      implicit none
!
      private :: pressure_4_sph_mhd
      private :: enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_lead_fields_4_sph_mhd                                &
     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p,   &
     &          ipol, sph_MHD_mat, WK, rj_fld)
!
      use sph_transforms_4_MHD
      use cal_buoyancies_sph_MHD
      use cal_energy_flux_rtp
      use swap_phi_order_4_sph_trans
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_buoyancies_sph_MHD                                       &
     &   (sph%sph_rj, trans_p%leg, ipol, MHD_prop%fl_prop,              &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    sph_MHD_bc%sph_bc_U, rj_fld)
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call pressure_4_sph_mhd                                         &
     &     (sph%sph_rj, MHD_prop, sph_MHD_bc, r_2nd,                    &
     &      trans_p%leg, sph_MHD_mat%band_p_poisson, ipol, rj_fld)
      end if
!
      call lead_fields_by_sph_trans                                     &
     &   (sph, comms_sph, MHD_prop, trans_p, WK, rj_fld)
!
      call gradients_of_vectors_sph                                     &
     &   (sph, comms_sph, r_2nd, sph_MHD_bc, trans_p,                   &
     &    ipol, WK%trns_MHD, WK%trns_tmp, WK%WK_sph, rj_fld)
      call enegy_fluxes_4_sph_mhd                                       &
     &   (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol,   &
     &    WK%trns_MHD, WK%trns_snap, WK%WK_sph, rj_fld)
!
      end subroutine s_lead_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine lead_fields_by_sph_trans                               &
     &         (sph, comms_sph, MHD_prop, trans_p, WK, rj_fld)
!
      use sph_transforms_4_MHD
      use cal_buoyancies_sph_MHD
      use cal_energy_flux_rtp
      use swap_phi_order_4_sph_trans
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: rj_fld
!
!
      call swap_phi_from_MHD_trans(sph%sph_rtp, WK%trns_MHD)
!
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center)  &
     &      then
        if (iflag_debug.eq.1) write(*,*) 'sph_pole_trans_4_MHD'
        call sph_pole_trans_4_MHD                                       &
     &     (sph, comms_sph, trans_p, rj_fld, WK%trns_MHD%backward)
!
        if (iflag_debug.eq.1) write(*,*) 'cal_nonlinear_pole_MHD'
        call cal_nonlinear_pole_MHD                                     &
     &     (sph%sph_rtp, MHD_prop%fl_prop, MHD_prop%cd_prop,            &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop,                         &
     &      WK%trns_MHD%f_trns, WK%trns_MHD%b_trns,                     &
     &      WK%trns_MHD%backward, WK%trns_MHD%forward)
      end if
!
      end subroutine lead_fields_by_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine pressure_4_sph_mhd(sph_rj, MHD_prop, sph_MHD_bc,       &
     &          r_2nd, leg, band_p_poisson, ipol, rj_fld)
!
      use cal_sol_sph_fluid_crank
!
      use cal_sph_field_by_rotation
      use const_radial_forces_on_bc
      use cal_div_of_forces
      use const_sph_radial_grad
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(band_matrices_type), intent(in) :: band_p_poisson
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_div_of_forces_sph_2'
      call cal_div_of_forces_sph_2(sph_rj, r_2nd, MHD_prop, sph_MHD_bc, &
     &    leg%g_sph_rj, ipol, rj_fld)
!
      call s_const_radial_forces_on_bc                                  &
     &   (sph_rj, leg%g_sph_rj, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,  &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C, ipol, rj_fld)
!
      call sum_div_of_forces(MHD_prop%fl_prop, ipol, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_pressure_by_div_v'
      call cal_sol_pressure_by_div_v                                    &
     &   (sph_rj, sph_MHD_bc%sph_bc_U, band_p_poisson, ipol, rj_fld)
!
      if(ipol%i_press_grad .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'const_pressure_gradient'
        call const_pressure_gradient                                    &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U,                         &
     &      leg%g_sph_rj, MHD_prop%fl_prop%coef_press,                  &
     &      ipol%i_press, ipol%i_press_grad, rj_fld)
      end if
!
      end subroutine pressure_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine enegy_fluxes_4_sph_mhd                                 &
     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc,            &
     &          trans_p, ipol, trns_MHD, trns_snap, WK_sph, rj_fld)
!
      use sph_transforms_snapshot
      use cal_energy_flux_rtp
      use cal_energy_flux_rj
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_enegy_fluxes                                         &
     &   (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p,         &
     &    ipol, trns_MHD, trns_snap,  WK_sph, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'sph_forward_trans_snapshot_MHD'
      call sph_forward_trans_snapshot_MHD                               &
     &   (sph, comms_sph, trans_p, trns_snap%forward, WK_sph, rj_fld)
!
      end subroutine enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_sph_enegy_fluxes                                   &
     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p,   &
     &          ipol, trns_MHD, trns_snap,  WK_sph, rj_fld)
!
      use sph_transforms_snapshot
      use cal_energy_flux_rtp
      use cal_energy_flux_rj
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
!
!
!      Evaluate fields for output in spectrum space
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rj'
      call s_cal_energy_flux_rj                                         &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc, ipol, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_snapshot_MHD'
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p,         &
     &    rj_fld, trns_snap%backward, WK_sph)
!
!      Evaluate fields for output in grid space
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rtp'
      call s_cal_energy_flux_rtp                                        &
     &   (sph%sph_rtp, MHD_prop%fl_prop, MHD_prop%cd_prop,              &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C, trans_p%leg,      &
     &    trns_MHD%f_trns, trns_snap%b_trns, trns_snap%f_trns,          &
     &    trns_MHD%forward, trns_snap%backward, trns_snap%forward)
!
      end subroutine cal_sph_enegy_fluxes
!
! ----------------------------------------------------------------------
!
      subroutine gradients_of_vectors_sph                               &
     &         (sph, comms_sph, r_2nd, sph_MHD_bc, trans_p,             &
     &          ipol, trns_MHD, trns_tmp, WK_sph, rj_fld)
!
      use sph_transforms_snapshot
      use sph_poynting_flux_smp
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_vectors_rtp_4_grad'
      call copy_vectors_rtp_4_grad                                      &
     &   (sph, trns_MHD%b_trns, trns_tmp%f_trns,                        &
     &    trns_MHD%backward, trns_tmp%forward)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_snapshot_MHD'
      call sph_forward_trans_snapshot_MHD                               &
     &   (sph, comms_sph, trans_p, trns_tmp%forward, WK_sph, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_grad_of_velocities_sph'
      call cal_grad_of_velocities_sph(sph%sph_rj, r_2nd,                &
     &    sph_MHD_bc%sph_bc_U, trans_p%leg%g_sph_rj, ipol, rj_fld)
!
      end subroutine gradients_of_vectors_sph
!
! ----------------------------------------------------------------------
!
      end module lead_fields_4_sph_mhd
