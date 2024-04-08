!>@file   t_sph_mhd_monitor_data_IO.f90
!!@brief  module t_sph_mhd_monitor_data_IO
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine open_sph_vol_rms_file_mhd(sph, sph_bc_U, ipol,        &
!!     &                                     rj_fld, monitor, SR_sig)
!!      subroutine init_rms_4_sph_spectr_4_mhd(sph, rj_fld, monitor)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(time_data), intent(in) :: time_d
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(send_recv_status), intent(inout) :: SR_sig
!!
!!      subroutine init_sph_spectr_data_and_file(sph, rj_fld, monitor)
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
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
      use t_CMB_dipolarity
      use t_sph_typical_scales
      use t_IO_step_parameter
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_energy_label_parameters
      use t_boundary_params_sph_MHD
      use t_field_on_circle
      use t_field_4_dynamobench
!
      use pickup_sph_spectr_data
!
      implicit none
!
!>      STructure for monitor data for spherical dynamos
      type sph_mhd_monitor_data
!>         Structure for pickup list
        type(pickup_mode_list) :: pick_list
!>          Structure for pickup list
        type(picked_spectrum_data) :: pick_coef
!
!>        Structure for pickup list for gauss coefficients
        type(pickup_mode_list) :: gauss_list
!>        Structure for gauss coeffciients
!!        Radius to evaluate Gauss coefficients (Default: 6400km/2200km)
!!        gauss_coef%radius_gl(1,1) = 2.82
        type(picked_spectrum_data) :: gauss_coef
!
!>        Structure for Nusselt number data
        type(nusselt_number_data) :: heat_Nusselt
!>        Structure for Nusselt number data
        type(nusselt_number_data) :: comp_Nusselt
!
!>        Structure for dipolarity data
        type(dipolarity_data) :: dip
!>        Structure for typical scale data
        type(typical_scale_data) :: tsl
!
!
!>        Structure of mean square data
        type(sph_mean_squares) :: pwr
!>        Work area of mean square data
        type(sph_mean_square_work) :: WK_pwr
!
!>        Structure of label for energies
        type(energy_label_param) :: ene_labels
!>        Truncation level for crustal filtering
        integer(kind = kint) :: ltr_crust
!
!>        Data on equator of mid-depth
        type(circle_fld_maker) :: circ_mid_eq
!>        Data for dynamo benchmark
        type(dynamobench_monitor) :: bench
!
!>        Data on circles
        type(mul_fields_on_circle) :: mul_circle
!
!>        Structure of mean square data
        type(sph_mean_squares) :: lor_spectr
!>        Work area of mean square data
        type(sph_mean_square_work) :: WK_lor_spectr
      end type sph_mhd_monitor_data
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file_mhd(sph, sph_bc_U, ipol,         &
     &                                     rj_fld, monitor, SR_sig)
!
      use calypso_mpi_int
      use m_error_IDs
      use cal_rms_fields_by_sph
      use cal_CMB_dipolarity
      use write_picked_sph_spectr
      use t_solver_SR
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: ierr_lc, ierr_gl
!
!
      call init_energy_labels_base(monitor%ene_labels)
      call init_sph_spectr_data_and_file(sph, rj_fld, monitor)
!
      call init_dipolarity_4_sph_spectr(sph%sph_params, monitor%pwr,    &
     &                                  monitor%dip)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_sph_spec_4_monitor'
      call init_sph_spec_4_monitor(sph%sph_params, sph%sph_rj,          &
     &    rj_fld, monitor%pick_list, monitor%pick_coef)
      ierr_lc =  error_picked_spectr_files(sph%sph_params,              &
     &                                     monitor%pick_coef)
!
      call calypso_mpi_allreduce_one_int(ierr_lc, ierr_gl, MPI_SUM)
      if(ierr_gl .gt. 0) then
        write(e_message,*) ierr_gl,                                     &
     &      ' pickup mode files have wrong header. Check field defs.'
        call calypso_mpi_barrier
        call calypso_MPI_abort(ierr_file, e_message)
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'init_gauss_coefs_data_and_file'
      call init_gauss_coefs_data_and_file(sph, ipol,                    &
     &    monitor%gauss_list, monitor%gauss_coef, SR_sig)
!
      call init_l_scale_data_and_file(sph, sph_bc_U,                    &
     &                                rj_fld, monitor)
!
      end subroutine open_sph_vol_rms_file_mhd
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine init_sph_spectr_data_and_file(sph, rj_fld, monitor)
!
      use m_error_IDs
      use t_energy_label_parameters
      use cal_rms_fields_by_sph
      use init_rms_4_sph_spectr
      use calypso_mpi_logical
      use output_sph_pwr_volume_file
!
      type(sph_grids), intent(in) :: sph
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      logical :: flag
!
!
      if(iflag_debug .gt. 0) write(*,*) 's_init_rms_4_sph_spectr'
      call s_init_rms_4_sph_spectr(sph%sph_params, sph%sph_rj, rj_fld,  &
     &    monitor%dip%iflag_dipolarity, monitor%pwr, monitor%WK_pwr)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'error_sph_vol_ms_file'
      flag = error_sph_vol_ms_file(my_rank, monitor%ene_labels,         &
     &                             sph%sph_params, sph%sph_rj,          &
     &                             monitor%pwr%v_spectr(1))
      call calypso_mpi_bcast_one_logical                                &
     &  (flag, monitor%pwr%v_spectr(1)%irank_m)
      if(flag) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Field information might be updated.')
      end if
!
      end subroutine init_sph_spectr_data_and_file
!
!  --------------------------------------------------------------------
!
      subroutine init_gauss_coefs_data_and_file                         &
     &         (sph, ipol, gauss_list, gauss_coef, SR_sig)
!
      use m_error_IDs
      use write_sph_gauss_coefs
      use pickup_gauss_coefficients
      use calypso_mpi_logical
      use t_solver_SR
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
!
      type(pickup_mode_list), intent(inout) :: gauss_list
      type(picked_spectrum_data), intent(inout) :: gauss_coef
      type(send_recv_status), intent(inout) :: SR_sig
!
      logical :: flag
!
!
      if(iflag_debug.gt. 0) write(*,*) 'init_gauss_coefs_4_monitor'
      call init_gauss_coefs_4_monitor(sph%sph_params, sph%sph_rj,       &
     &    ipol, gauss_list, gauss_coef, SR_sig)
!
      if(iflag_debug .gt. 0) write(*,*) 'error_gauss_coefs_header'
      flag = error_gauss_coefs_header(sph%sph_params, sph%sph_rj,       &
     &                                gauss_coef)
      call calypso_mpi_bcast_one_logical(flag, 0)
      if(flag) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Gauss coefficient information might be updated.')
      end if
!
      end subroutine init_gauss_coefs_data_and_file
!
! -----------------------------------------------------------------------
!
      subroutine init_l_scale_data_and_file                             &
     &         (sph, sph_bc_U, rj_fld, monitor)
!
      use m_error_IDs
      use cal_typical_scale
      use calypso_mpi_logical
      use write_typical_scale
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      logical :: flag
!
!
      if(iflag_debug.gt. 0) write(*,*) 'init_typical_scales'
      call init_typical_scales(rj_fld, monitor%pwr, monitor%tsl)
!
      if(iflag_debug .gt. 0) write(*,*) 'error_typical_scale_header'
      flag = error_typical_scale_header(sph%sph_params, sph%sph_rj,     &
     &                              sph_bc_U, monitor%pwr, monitor%tsl)
      call calypso_mpi_bcast_one_logical                                &
    &    (flag, monitor%pwr%v_spectr(1)%irank_m)
      if(flag) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Length scale information might be updated.')
      end if
!
      end subroutine init_l_scale_data_and_file
!
! -----------------------------------------------------------------------
!
      end module t_sph_mhd_monitor_data_IO
