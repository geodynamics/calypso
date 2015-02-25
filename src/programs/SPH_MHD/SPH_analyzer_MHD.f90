!
!     module SPH_analyzer_MHD
!
!      subroutine SPH_initialize_MHD
!      subroutine SPH_analyze_MHD(i_step, iflag_finish)
!
!      Written by H. Matsui
!
      module SPH_analyzer_MHD
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_MHD
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
      use m_rms_4_sph_spectr
      use m_sph_boundary_input_data
!
      use set_control_sph_mhd
      use parallel_load_data_4_sph
      use set_initial_sph_dynamo
      use set_reference_sph_mhd
      use set_bc_sph_mhd
      use material_property
      use sph_transforms_4_MHD
      use set_radius_func
      use const_radial_mat_4_sph
      use cal_rms_fields_by_sph
      use sph_mhd_rms_IO
      use cal_sol_sph_MHD_crank
      use cal_nonlinear
!
      use m_work_time
!
!
!   Load spherical harmonics data
!
      call start_eleps_time(4)
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh
!
      if (iflag_boundary_file .eq. id_read_boundary_file) then
        if (iflag_debug.eq.1) write(*,*) 'read_boundary_spectr_file'
        call read_boundary_spectr_file
      end if
      call end_eleps_time(4)
!
!   Allocate spectr field data
!
      call allocate_phys_rj_data
      call set_sph_sprctr_data_address
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_rms_4_sph_spectr'
      call init_rms_4_sph_spectr
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'set_radius_rot_reft_dat_4_sph'
      call set_radius_rot_reft_dat_4_sph(depth_high_t, depth_low_t,     &
     &    high_temp, low_temp, angular)
!
      if (iflag_debug.gt.0) write(*,*) 'const_2nd_fdm_matrices'
      call const_2nd_fdm_matrices
!
      if (iflag_debug.gt.0) write(*,*) 'const_2nd_fdm_coefs'
      call const_2nd_fdm_coefs
!
! ---------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' set_material_property'
      call set_material_property
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*) 's_set_bc_sph_mhd'
      call s_set_bc_sph_mhd
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_data_control'
      call sph_initial_data_control
!
      if(iflag_debug.gt.0) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' const_radial_mat_sph_mhd'
      call const_radial_mat_sph_mhd
!*
!* obtain linear terms for starting
!*
       if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
       call set_sph_field_to_start
!
!* obtain nonlinear terms for starting
!*
       if(iflag_debug .gt. 0) write(*,*) 'first nonlinear'
       call nonlinear
!
!* -----  Open Volume integration data files -----------------
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      call start_eleps_time(4)
      call open_sph_vol_rms_file_mhd
      call end_eleps_time(4)
!
      end subroutine SPH_initialize_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_MHD(i_step, iflag_finish)
!
      use m_work_time
      use m_t_step_parameter
!
      use cal_momentum_eq_explicit
      use cal_sol_sph_MHD_crank
      use cal_nonlinear
      use set_reference_sph_mhd
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
!
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(inout) :: iflag_finish
!
      real(kind = kreal) :: total_max
!
!*  ----------  add time evolution -----------------
!*
!
      call start_eleps_time(5)
      call start_eleps_time(6)
      if(i_step .eq. 1) then
        if(iflag_debug.gt.0) write(*,*) 'cal_expricit_sph_euler'
        call cal_expricit_sph_euler(i_step)
      else
        if(iflag_debug.gt.0) write(*,*) 'cal_expricit_sph_adams'
        call cal_expricit_sph_adams
      end if
!*
!*  ----------  time evolution by inplicit method ----------
!*
      call start_eleps_time(7)
      call s_cal_sol_sph_MHD_crank
      call end_eleps_time(7)
      call end_eleps_time(6)
!*
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear
      call end_eleps_time(8)
      call end_eleps_time(5)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd
      call end_eleps_time(9)
!
!*  -----------  output restart data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(10)
      if(iflag_debug.gt.0) write(*,*) 'output_sph_restart_control'
      call output_sph_restart_control
!
      total_time = MPI_WTIME() - total_start
      call MPI_allREDUCE (total_time, total_max, ione, CALYPSO_REAL,    &
     &    MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if      (istep_rst_end .eq. -1                                    &
     &   .and. total_max.gt.elapsed_time) then
        call output_sph_rst_by_elaps
        iflag_finish = 1
      end if
      call end_eleps_time(10)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control
      call end_eleps_time(11)
!
      if(iflag_debug.gt.0) write(*,*) 'sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph
!
      if(i_step .ge. i_step_number .and. i_step_number.gt.0) then
        iflag_finish = 1
      end if
      call end_eleps_time(4)
!
      end subroutine SPH_analyze_MHD
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_MHD
!
!      end subroutine SPH_finalize_MHD
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_MHD
