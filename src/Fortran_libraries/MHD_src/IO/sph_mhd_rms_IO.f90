!>@file   sph_mhd_rms_IO.f90
!!@brief  module sph_mhd_rms_IO
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine open_sph_vol_rms_file_mhd                            &
!!     &         (sph_params, sph_rj, ipol, rj_fld, pwr, WK_pwr)
!!      subroutine output_rms_sph_mhd_control                           &
!!     &         (time_d, sph_params, sph_rj, sph_bc_U, leg,            &
!!     &          ipol, rj_fld, pwr, WK_pwr)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!@endverbatim
!
      module sph_mhd_rms_IO
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_schmidt_poly_on_rtm
      use t_phys_address
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_pickup_sph_spectr_data
      use t_no_heat_Nusselt
      use t_IO_step_parameter
!
      use pickup_sph_spectr_data
      use pickup_gauss_coefficients
      use output_sph_m_square_file
!
      implicit none
!
!
!>        Structure for pickup list
      type(pickup_mode_list), save :: pick_list1
!>        Structure for pickup list
      type(picked_spectrum_data), save :: pick1
!>        File prefix for picked spectrum file
      character(len = kchara) :: pickup_sph_head =  'picked_ene_spec'
!
!
!>      Structure for pickup list for gauss coefficients
      type(pickup_mode_list), save :: gauss_list1
!>      Structure for gauss coeffciients
!!      Radius to evaluate Gauss coefficients (Default: 6400km/2200km)
!!      gauss1%radius_gl(1) = 2.82
      type(picked_spectrum_data), save :: gauss1
!>      File prefix for Gauss coefficients file
      character(len = kchara) :: gauss_coefs_file_head
!
!>      Structure for Nusselt number data
      type(nusselt_number_data), save :: Nu_type1
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file_mhd                              &
     &         (sph_params, sph_rj, ipol, rj_fld, pwr, WK_pwr)
!
      use m_error_IDs
      use cal_rms_fields_by_sph
      use output_sph_m_square_file
      use gauss_coefs_monitor_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: iflag
!
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_rms_4_sph_spectr'
      call init_rms_4_sph_spectr                                        &
     &   (sph_params, sph_rj, rj_fld, pwr, WK_pwr)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'check_sph_vol_ms_file'
      iflag = check_sph_vol_ms_file(my_rank, sph_params, sph_rj, pwr)
      call MPI_Bcast(iflag, ione, CALYPSO_INTEGER, izero,               &
     &               CALYPSO_COMM, ierr_MPI)
      if(iflag .gt. 0) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Field information might be updated.')
      end if
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_sph_spec_4_monitor'
      call init_sph_spec_4_monitor                                      &
     &   (sph_params%l_truncation, sph_rj, rj_fld, pick_list1, pick1)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_gauss_coefs_4_monitor'
      call init_gauss_coefs_4_monitor                                   &
     &   (sph_params%l_truncation, sph_rj, ipol, gauss_list1, gauss1)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'check_gauss_coefs_file'
      iflag = check_gauss_coefs_file                                    &
     &      (my_rank, gauss_coefs_file_head, gauss1)
      call MPI_Bcast(iflag, ione, CALYPSO_INTEGER, izero,               &
     &               CALYPSO_COMM, ierr_MPI)
      if(iflag .gt. 0) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Field information might be updated.')
      end if
!
      end subroutine open_sph_vol_rms_file_mhd
!
!  --------------------------------------------------------------------
!
      subroutine output_rms_sph_mhd_control                             &
     &         (time_d, sph_params, sph_rj, sph_bc_U, leg,              &
     &          ipol, rj_fld, pwr, WK_pwr)
!
      use t_time_data
      use t_boundary_params_sph_MHD
      use m_machine_parameter
!
      use cal_rms_fields_by_sph
      use volume_average_4_sph
      use picked_sph_spectr_data_IO
      use gauss_coefs_monitor_IO
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph_params%l_truncation, sph_rj, ipol, rj_fld, leg%g_sph_rj,  &
     &    pwr, WK_pwr)
      if(iflag_debug.gt.0)  write(*,*) 'cal_gauss_coefficients'
      call cal_gauss_coefficients                                       &
     &   (sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_rj%nidx_rj, sph_rj%radius_1d_rj_r, ipol,                  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld, gauss1)
      if(iflag_debug.gt.0)  write(*,*) 'pickup_sph_spec_4_monitor'
      call pickup_sph_spec_4_monitor                                    &
     &   (sph_rj, rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,    &
     &    rj_fld%istack_component, rj_fld%d_fld, pick1)
      if(iflag_debug.gt.0)  write(*,*) 'cal_no_heat_source_Nu'
      call cal_no_heat_source_Nu(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0),                         &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj, ipol,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld, Nu_type1)
!
      if(iflag_debug.gt.0)  write(*,*) 'write_total_energy_to_screen'
      call write_total_energy_to_screen(my_rank, time_d, pwr)
!
      call write_sph_vol_ave_file(time_d, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_file                                        &
     &   (my_rank, time_d, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_spectr_file                                 &
     &   (my_rank, time_d, sph_params, sph_rj, pwr)
      call write_sph_layer_ms_file                                      &
     &   (my_rank, time_d, sph_params, pwr)
      call write_sph_layer_spectr_file                                  &
     &   (my_rank, time_d, sph_params, pwr)
!
      call write_gauss_coefs_4_monitor                                  &
     &   (my_rank, time_d%i_time_step, time_d%time,                     &
     &    gauss_coefs_file_head, gauss1)
      call write_sph_spec_monitor(pickup_sph_head, my_rank,             &
     &    time_d%i_time_step, time_d%time, pick1)
!
      call write_no_heat_source_Nu(sph_rj%idx_rj_degree_zero,           &
     &    time_d%i_time_step, time_d%time, Nu_type1)
!
      end subroutine output_rms_sph_mhd_control
!
!  --------------------------------------------------------------------
!
      end module sph_mhd_rms_IO
