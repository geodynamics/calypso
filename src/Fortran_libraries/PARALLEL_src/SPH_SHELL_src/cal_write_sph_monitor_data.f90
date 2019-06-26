!>@file   cal_write_sph_monitor_data.f90
!!@brief  module cal_write_sph_monitor_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine cal_sph_monitor_data                                 &
!!     &         (sph_params, sph_rj, sph_bc_U, leg, ipol, rj_fld,      &
!!     &          pwr, WK_pwr, Nusselt)
!!      subroutine output_sph_monitor_data(time_d, sph_params, sph_rj,  &
!!     &          ipol, rj_fld, pwr, pick_coef, gauss_coef, Nusselt)
!!
!!      subroutine cal_write_sph_mean_square(time_d, sph_params, sph_rj,&
!!     &          leg, ipol, rj_fld, pwr, WK_pwr)
!!      subroutine cal_write_layerd_sph_rms(time_d, sph_params, sph_rj, &
!!     &          leg, ipol, rj_fld, pwr, WK_pwr)
!!      subroutine cal_write_no_heat_sourse_Nu(time_d, sph_rj,           &
!!     &          sph_bc_U, ipol, rj_fld, Nusselt)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!        type(picked_spectrum_data), intent(inout) :: pick_coef
!!        type(picked_spectrum_data), intent(inout) :: gauss_coef
!!        type(nusselt_number_data), intent(inout) :: Nusselt
!!@endverbatim
!
      module cal_write_sph_monitor_data
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_phys_data
      use t_boundary_data_sph_MHD
      use t_schmidt_poly_on_rtm
      use t_time_data
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_pickup_sph_spectr_data
      use t_no_heat_Nusselt
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine cal_sph_monitor_data                                   &
     &         (sph_params, sph_rj, sph_bc_U, leg, ipol, rj_fld,        &
     &          pwr, WK_pwr, Nusselt)
!
      use cal_rms_fields_by_sph
      use pickup_sph_spectr_data
      use pickup_gauss_coefficients
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
      type(nusselt_number_data), intent(inout) :: Nusselt
!
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph_params, sph_rj, ipol, rj_fld, leg%g_sph_rj, pwr, WK_pwr)
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_no_heat_source_Nu'
      call cal_no_heat_source_Nu(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0),                         &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj, ipol,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld, Nusselt)
!
      end subroutine cal_sph_monitor_data
!
!  --------------------------------------------------------------------
!
      subroutine output_sph_monitor_data(time_d, sph_params, sph_rj,    &
     &          ipol, rj_fld, pwr, pick_coef, gauss_coef, Nusselt)
!
      use output_sph_m_square_file
      use MPI_picked_sph_spectr_IO
      use MPI_sph_gauss_coefs_IO
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(picked_spectrum_data), intent(inout) :: pick_coef
      type(picked_spectrum_data), intent(inout) :: gauss_coef
      type(nusselt_number_data), intent(inout) :: Nusselt
!
!
      if(iflag_debug.gt.0)  write(*,*) 'write_total_energy_to_screen'
      call write_total_energy_to_screen(my_rank, time_d, pwr)
!
      call write_sph_vol_ave_file(time_d, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_file                                        &
     &   (my_rank, time_d, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_spectr_file                                 &
     &   (my_rank, time_d, sph_params, sph_rj, pwr)
      call write_sph_layer_ms_file (my_rank, time_d, sph_params, pwr)
      call write_sph_layer_spectr_file                                  &
     &   (my_rank, time_d, sph_params, pwr)
!
      call write_no_heat_source_Nu(sph_rj%idx_rj_degree_zero,           &
     &    time_d%i_time_step, time_d%time, Nusselt)
!
      call append_picked_spectrum_file                                  &
     &   (time_d, sph_rj, rj_fld, pick_coef)
      call append_sph_gauss_coefs_file                                  &
     &   (time_d, sph_params, sph_rj, ipol, rj_fld, gauss_coef)
!
      end subroutine output_sph_monitor_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine cal_write_sph_mean_square(time_d, sph_params, sph_rj,  &
     &          leg, ipol, rj_fld, pwr, WK_pwr)
!
      use cal_rms_fields_by_sph
      use output_sph_m_square_file
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      call cal_mean_squre_in_shell                                      &
     &   (sph_params, sph_rj, ipol, rj_fld, leg%g_sph_rj, pwr, WK_pwr)
!
      call write_sph_vol_ave_file(time_d, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_file                                        &
     &   (my_rank, time_d, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_spectr_file                                 &
     &   (my_rank, time_d, sph_params, sph_rj, pwr)
      call write_sph_layer_ms_file(my_rank, time_d, sph_params, pwr)
      call write_sph_layer_spectr_file                                  &
     &   (my_rank, time_d, sph_params, pwr)
!
      end subroutine cal_write_sph_mean_square
!
!  --------------------------------------------------------------------
!
      subroutine cal_write_layerd_sph_rms(time_d, sph_params, sph_rj,   &
     &          leg, ipol, rj_fld, pwr, WK_pwr)
!
      use cal_rms_fields_by_sph
      use output_sph_m_square_file
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      call cal_mean_squre_in_shell                                      &
     &   (sph_params, sph_rj, ipol, rj_fld, leg%g_sph_rj, pwr, WK_pwr)
      call write_sph_layer_ms_file(my_rank, time_d, sph_params, pwr)
!
      end subroutine cal_write_layerd_sph_rms
!
!  --------------------------------------------------------------------
!
      subroutine cal_write_no_heat_sourse_Nu(time_d, sph_rj,            &
     &          sph_bc_U, ipol, rj_fld, Nusselt)
!
      use pickup_gauss_coefficients
!
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(nusselt_number_data), intent(inout) :: Nusselt
!
!
      call cal_no_heat_source_Nu(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0),                         &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj, ipol,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,               &
     &    Nusselt)
      call write_no_heat_source_Nu(sph_rj%idx_rj_degree_zero,           &
     &    time_d%i_time_step, time_d%time, Nusselt)
!
      end subroutine cal_write_no_heat_sourse_Nu
!
!  --------------------------------------------------------------------
!
      end module cal_write_sph_monitor_data
