!>@file   t_sph_mhd_monitor_data_IO.f90
!!@brief  module t_sph_mhd_monitor_data_IO
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine open_sph_vol_rms_file_mhd(sph, ipol, rj_fld,         &
!!     &                                     monitor, SR_sig)
!!      subroutine output_rms_sph_mhd_control                           &
!!     &         (time_d, SPH_MHD, sph_MHD_bc, leg, monitor, SR_sig)
!!      subroutine init_rms_4_sph_spectr_4_mhd(sph, rj_fld, monitor)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(time_data), intent(in) :: time_d
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!
      module t_sph_mhd_monitor_data_IO
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_SPH_mesh_field_data
      use t_schmidt_poly_on_rtm
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_pickup_sph_spectr_data
      use t_no_heat_Nusselt
      use t_IO_step_parameter
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_energy_label_parameters
!
      use pickup_sph_spectr_data
      use output_sph_m_square_file
!
      implicit none
!
!
      type sph_mhd_monitor_data
!>        Structure for pickup list
        type(pickup_mode_list) :: pick_list
!>          Structure for pickup list
        type(picked_spectrum_data) :: pick_coef
!
!>        Structure for pickup list for gauss coefficients
        type(pickup_mode_list) :: gauss_list
!>        Structure for gauss coeffciients
!!        Radius to evaluate Gauss coefficients (Default: 6400km/2200km)
!!        gauss_coef%radius_gl(1) = 2.82
        type(picked_spectrum_data) :: gauss_coef
!
!>        Structure for Nusselt number data
        type(nusselt_number_data) :: Nusselt
!
!
!>        Structure of mean square data
        type(sph_mean_squares) :: pwr
!>        Work area of mean square data
        type(sph_mean_square_work) :: WK_pwr
!
!>        Structure of label for energies
        type(energy_label_param) :: ene_labels
!
        integer(kind = kint) :: ltr_crust
      end type sph_mhd_monitor_data
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file_mhd(sph, ipol, rj_fld,           &
     &                                     monitor, SR_sig)
!
      use m_error_IDs
      use pickup_gauss_coefficients
      use cal_rms_fields_by_sph
      use output_sph_m_square_file
      use write_sph_gauss_coefs
      use calypso_mpi_int
      use t_solver_SR
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: iflag
!
!
      call init_rms_4_sph_spectr_4_mhd(sph, rj_fld, monitor)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'check_sph_vol_ms_file'
      iflag = check_sph_vol_ms_file(my_rank, monitor%ene_labels,        &
     &                              sph%sph_params, sph%sph_rj,         &
     &                              monitor%pwr)
      call calypso_mpi_bcast_one_int(iflag, 0)
      if(iflag .gt. 0) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Field information might be updated.')
      end if
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_sph_spec_4_monitor'
      call init_sph_spec_4_monitor(sph%sph_params, sph%sph_rj,          &
     &    rj_fld, monitor%pick_list, monitor%pick_coef)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_gauss_coefs_4_monitor'
      call init_gauss_coefs_4_monitor(sph%sph_params, sph%sph_rj,       &
     &    ipol, monitor%gauss_list, monitor%gauss_coef, SR_sig)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'check_gauss_coefs_num'
      iflag = check_gauss_coefs_num(monitor%gauss_coef)
      call calypso_mpi_bcast_one_int(iflag, 0)
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
     &         (time_d, SPH_MHD, sph_MHD_bc, leg, monitor, SR_sig)
!
      use t_solver_SR
      use t_time_data
      use t_boundary_data_sph_MHD
      use m_machine_parameter
!
      use cal_write_sph_monitor_data
!
      type(time_data), intent(in) :: time_d
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      call cal_sph_monitor_data                                         &
     &   (SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj,                   &
     &    sph_MHD_bc%sph_bc_U, leg, SPH_MHD%ipol, SPH_MHD%fld,          &
     &    monitor%pwr, monitor%WK_pwr, monitor%Nusselt)
!
      call output_sph_monitor_data                                      &
     &   (monitor%ene_labels, time_d, SPH_MHD%sph%sph_params,           &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld,                &
     &    monitor%pwr, monitor%pick_coef, monitor%gauss_coef,           &
     &    monitor%Nusselt, SR_sig)
!
      end subroutine output_rms_sph_mhd_control
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine init_rms_4_sph_spectr_4_mhd(sph, rj_fld, monitor)
!
      use t_energy_label_parameters
      use cal_rms_fields_by_sph
!
      type(sph_grids), intent(in) :: sph
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
      if(iflag_debug .gt. 0) write(*,*) 'init_rms_4_sph_spectr'
      call init_energy_labels_base(monitor%ene_labels)
      call init_rms_4_sph_spectr                                        &
     &   (sph%sph_params, sph%sph_rj, rj_fld,                           &
     &    monitor%pwr, monitor%WK_pwr)
!
      end subroutine init_rms_4_sph_spectr_4_mhd
!
!  --------------------------------------------------------------------
!
      end module t_sph_mhd_monitor_data_IO
