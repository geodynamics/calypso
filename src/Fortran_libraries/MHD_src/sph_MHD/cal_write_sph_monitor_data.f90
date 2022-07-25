!>@file   cal_write_sph_monitor_data.f90
!!@brief  module cal_write_sph_monitor_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine init_rms_sph_mhd_control(MHD_prop, sph_MHD_bc,       &
!!     &          r_2nd, SPH_MHD, MHD_mats, monitor, SR_sig)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(MHD_radial_matrices), intent(inout) :: MHD_mats
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine output_rms_sph_mhd_control(time_d, SPH_MHD, MHD_prop,&
!!     &          sph_MHD_bc, r_2nd, leg, MHD_mats, monitor, SR_sig)
!!        type(time_data), intent(in) :: time_d
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!
!!      subroutine output_sph_mean_square_files                         &
!!     &         (ene_labels, time_d, sph_params, sph_rj, pwr)
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!
!!      subroutine cal_write_no_heat_sourse_Nu                          &
!!     &         (is_scalar, is_source, is_grad_s, time_d, sph, sc_prop,&
!!     &          sph_bc_S, sph_bc_U, bcs_S, fdm2_center, r_2nd,        &
!!     &          band_s00_poisson_fixS, rj_fld, Nusselt)
!!      subroutine cal_write_dipolarity(time_d, sph_params,             &
!!     &          ipol, rj_fld, pwr, dip)
!!      subroutine cal_write_typical_scale(time_d, rj_fld, pwr, tsl)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_U/
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_S
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(band_matrix_type), intent(in) :: band_s00_poisson_fixS
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!        type(picked_spectrum_data), intent(inout) :: pick_coef
!!        type(picked_spectrum_data), intent(inout) :: gauss_coef
!!        type(nusselt_number_data), intent(inout) :: Nusselt
!!        type(dipolarity_data), intent(inout) :: dip
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(typical_scale_data), intent(inout) :: tsl
!!@endverbatim
!
      module cal_write_sph_monitor_data
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_SPH_mesh_field_data
      use t_sph_mhd_monitor_data_IO
      use t_spheric_parameter
      use t_phys_data
      use t_boundary_data_sph_MHD
      use t_boundary_sph_spectr
      use t_schmidt_poly_on_rtm
      use t_time_data
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_pickup_sph_spectr_data
      use t_no_heat_Nusselt
      use t_CMB_dipolarity
      use t_sph_typical_scales
      use t_energy_label_parameters
      use t_fdm_coefs
      use t_physical_property
      use t_radial_matrices_sph_MHD
      use t_sph_matrix
!
      private :: cal_sph_monitor_data
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine init_rms_sph_mhd_control(MHD_prop, sph_MHD_bc,         &
     &          r_2nd, SPH_MHD, MHD_mats, monitor, SR_sig)
!
      use t_solver_SR
      use cal_heat_source_Nu
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(fdm_matrices), intent(in) :: r_2nd
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(MHD_radial_matrices), intent(inout) :: MHD_mats
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
      character(len=kchara) :: mat_name
!
!
     if(monitor%heat_Nusselt%iflag_Nusselt .eq. iflag_source_Nu) then
        write(mat_name,'(a)') 'Diffusive_Temperature'
        call init_poisson_matrix_for_Nu                                 &
     &     (mat_name, SPH_MHD%sph, r_2nd, MHD_prop%ht_prop,             &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%fdm2_center,                &
     &      MHD_mats%band_T00_poisson_fixT, monitor%heat_Nusselt)
      end if
!
     if(monitor%comp_Nusselt%iflag_Nusselt .eq. iflag_source_Nu) then
        write(mat_name,'(a)') 'Diffusive_Composition'
        call init_poisson_matrix_for_Nu                                 &
     &     (mat_name, SPH_MHD%sph, r_2nd, MHD_prop%cp_prop,             &
     &      sph_MHD_bc%sph_bc_C, sph_MHD_bc%fdm2_center,                &
     &      MHD_mats%band_C00_poisson_fixC, monitor%comp_Nusselt)
      end if
!
      call open_sph_vol_rms_file_mhd                                    &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld, monitor, SR_sig)
!
      end subroutine init_rms_sph_mhd_control
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine output_rms_sph_mhd_control(time_d, SPH_MHD, MHD_prop,  &
     &          sph_MHD_bc, r_2nd, leg, MHD_mats, monitor, SR_sig)
!
      use t_solver_SR
      use t_time_data
!
      type(time_data), intent(in) :: time_d
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(MHD_radial_matrices), intent(in) :: MHD_mats
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      call cal_sph_monitor_data                                         &
     &   (SPH_MHD%sph, MHD_prop, sph_MHD_bc, r_2nd, leg,                &
     &    MHD_mats, SPH_MHD%ipol, SPH_MHD%fld, monitor)
!
      call output_sph_monitor_data                                      &
     &   (time_d, SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj,           &
     &    SPH_MHD%ipol, SPH_MHD%fld, monitor, SR_sig)
!
      end subroutine output_rms_sph_mhd_control
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine cal_sph_monitor_data(sph, MHD_prop, sph_MHD_bc,        &
     &          r_2nd, leg, MHD_mats, ipol, rj_fld, monitor)
!
      use cal_rms_fields_by_sph
      use pickup_sph_spectr_data
      use pickup_gauss_coefficients
      use cal_heat_source_Nu
!
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(MHD_radial_matrices), intent(in) :: MHD_mats
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell(sph%sph_params, sph%sph_rj,          &
     &    ipol, rj_fld, leg%g_sph_rj, monitor%pwr, monitor%WK_pwr)
!
       if(monitor%heat_Nusselt%iflag_Nusselt .ne. 0) then
        call sel_Nusselt_routine(ipol%base%i_temp,                      &
     &      ipol%base%i_heat_source, ipol%grad_fld%i_grad_temp,         &
     &      sph, r_2nd, MHD_prop%ht_prop,                               &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_T, &
     &      sph_MHD_bc%fdm2_center, MHD_mats%band_T00_poisson_fixT,     &
     &      rj_fld, monitor%heat_Nusselt)
      end if
!
      if(monitor%comp_Nusselt%iflag_Nusselt .ne. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'sel_Nusselt_routine'
        call sel_Nusselt_routine(ipol%base%i_light,                     &
     &      ipol%base%i_light_source, ipol%grad_fld%i_grad_composit,    &
     &      sph, r_2nd, MHD_prop%cp_prop,                               &
     &      sph_MHD_bc%sph_bc_C, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_C, &
     &      sph_MHD_bc%fdm2_center, MHD_mats%band_C00_poisson_fixC,     &
     &      rj_fld, monitor%comp_Nusselt)
      end if
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_CMB_dipolarity'
      call cal_CMB_dipolarity(my_rank, rj_fld,                          &
     &                        monitor%pwr, monitor%dip)
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_typical_scales'
      call cal_typical_scales(rj_fld, monitor%pwr, monitor%tsl)
!
      end subroutine cal_sph_monitor_data
!
!  --------------------------------------------------------------------
!
      subroutine output_sph_monitor_data(time_d, sph_params, sph_rj,    &
     &          ipol, rj_fld, monitor, SR_sig)
!
      use t_solver_SR
      use output_sph_pwr_volume_file
      use write_picked_sph_spectr
      use write_sph_gauss_coefs
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      if(iflag_debug.gt.0)  write(*,*) 'write_total_energy_to_screen'
      call write_total_energy_to_screen(my_rank, time_d, monitor%pwr)
!
      call output_sph_mean_square_files                                 &
     &   (monitor%ene_labels, time_d, sph_params, sph_rj, monitor%pwr)
!
      if(monitor%heat_Nusselt%iflag_Nusselt .ne. 0) then
        call write_no_heat_source_Nu(sph_rj%idx_rj_degree_zero,         &
     &      time_d%i_time_step, time_d%time, monitor%heat_Nusselt)
      end if
!
      if(monitor%comp_Nusselt%iflag_Nusselt .ne. 0) then
        call write_no_heat_source_Nu(sph_rj%idx_rj_degree_zero,         &
     &      time_d%i_time_step, time_d%time, monitor%comp_Nusselt)
      end if
!
      call write_dipolarity(my_rank, time_d%i_time_step, time_d%time,   &
     &    sph_params%radius_CMB, ipol, monitor%pwr, monitor%dip)
      call write_typical_scales(time_d%i_time_step, time_d%time,        &
     &                          monitor%pwr, monitor%tsl)
!
      call write_each_picked_specr_file                                 &
     &   (time_d, sph_rj, rj_fld, monitor%pick_coef)
      call append_sph_gauss_coefs_file(time_d, sph_params, sph_rj,      &
     &    ipol, rj_fld, monitor%gauss_coef, SR_sig)
!
      end subroutine output_sph_monitor_data
!
!  --------------------------------------------------------------------
!
      subroutine output_sph_mean_square_files                           &
     &         (ene_labels, time_d, sph_params, sph_rj, pwr)
!
      use output_sph_volume_ave_file
      use output_sph_pwr_volume_file
      use output_sph_pwr_layer_file
!
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      call write_sph_vol_ave_file                                       &
     &   (ene_labels, time_d, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_file                                        &
     &   (my_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_spectr_file                                 &
     &   (my_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
      call write_sph_layer_ms_file                                      &
     &   (my_rank, ene_labels, time_d, sph_params, pwr)
      call write_sph_layer_spectr_file                                  &
     &   (my_rank, ene_labels, time_d, sph_params, pwr)
!
      end subroutine output_sph_mean_square_files
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine cal_write_no_heat_sourse_Nu                            &
     &         (is_scalar, is_source, is_grad_s, time_d, sph, sc_prop,  &
     &          sph_bc_S, sph_bc_U, bcs_S, fdm2_center, r_2nd,          &
     &          band_s00_poisson_fixS, rj_fld, Nusselt)
!
      use pickup_gauss_coefficients
      use cal_heat_source_Nu
!
      integer(kind = kint), intent(in) :: is_scalar, is_source
      integer(kind = kint), intent(in) :: is_grad_s
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S, sph_bc_U
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(phys_data), intent(in) :: rj_fld
      type(band_matrix_type), intent(in) :: band_s00_poisson_fixS
!
      type(nusselt_number_data), intent(inout) :: Nusselt
!
!
      if(Nusselt%iflag_Nusselt .eq. 0) return
      call sel_Nusselt_routine(is_scalar, is_source, is_grad_s,         &
     &    sph, r_2nd, sc_prop, sph_bc_S, sph_bc_U, bcs_S,               &
     &    fdm2_center, band_s00_poisson_fixS, rj_fld, Nusselt)
      call write_no_heat_source_Nu(sph%sph_rj%idx_rj_degree_zero,       &
     &    time_d%i_time_step, time_d%time, Nusselt)
!
      end subroutine cal_write_no_heat_sourse_Nu
!
!  --------------------------------------------------------------------
!
      subroutine cal_write_dipolarity(time_d, sph_params,               &
     &          ipol, rj_fld, pwr, dip)
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(dipolarity_data), intent(inout) :: dip
!
!
      call cal_CMB_dipolarity(my_rank, rj_fld, pwr, dip)
      call write_dipolarity(my_rank, time_d%i_time_step, time_d%time,   &
     &    sph_params%radius_CMB, ipol, pwr, dip)
!
      end subroutine cal_write_dipolarity
!
!  --------------------------------------------------------------------
!
      subroutine cal_write_typical_scale(time_d, rj_fld, pwr, tsl)
!
      type(time_data), intent(in) :: time_d
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(typical_scale_data), intent(inout) :: tsl
!
!
      call cal_typical_scales(rj_fld, pwr, tsl)
      call write_typical_scales(time_d%i_time_step, time_d%time,        &
     &                          pwr, tsl)
!
      end subroutine cal_write_typical_scale
!
!  --------------------------------------------------------------------
!
      end module cal_write_sph_monitor_data
