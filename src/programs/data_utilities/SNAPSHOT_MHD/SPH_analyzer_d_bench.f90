!>@file   SPH_analyzer_d_bench.f90
!!        module SPH_analyzer_d_bench
!!
!!@author H. Matsui
!!@date   Programmed in 2012
!!@n      modified in 2013
!
!>@brief spherical harmonics part of 
!!        Initialzation and evolution loop for dynamo benchmark check
!!
!!@verbatim
!!      subroutine SPH_init_sph_dbench
!!      subroutine SPH_analyze_dbench(i_step)
!!      subroutine SPH_finalize_dbench
!!@endverbatim
!
      module SPH_analyzer_d_bench
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
      subroutine SPH_init_sph_dbench
!
      use m_constants
      use m_array_for_send_recv
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_rms_4_sph_spectr
      use m_node_id_spherical_IO
!
      use set_control_sph_mhd
      use load_data_for_sph_IO
      use set_reference_sph_mhd
      use set_bc_sph_mhd
      use material_property
      use sph_transforms_4_MHD
      use set_radius_func
      use cal_sph_bc_fdm_matrix
      use const_radial_mat_4_sph
      use cal_rms_fields_by_sph
      use const_coriolis_sph
      use cvt_nod_data_to_sph_data
      use r_interpolate_sph_data
      use sph_mhd_rst_IO_control
      use m_field_at_mid_equator
!
!
!   Load spherical harmonics data
!
      if (iflag_debug.eq.1) write(*,*) 'input_sph_trans_grids'
      call input_sph_trans_grids(my_rank)
!
!   Allocate spectr field data
!
      call allocate_phys_rj_data
      call allocate_phys_rtp_data
      call allocate_rot_rj_data
      call set_sph_sprctr_data_address
      call set_sph_nod_data_address
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, nnod_rtp)
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
      if (iflag_debug.gt.0) write(*,*) 's_cal_sph_bc_fdm_matrices'
      call s_cal_sph_bc_fdm_matrices
!
      if (iflag_debug.gt.0) write(*,*) 'const_2nd_fdm_coefs'
      call const_2nd_fdm_coefs
!
!* -----  set integrals for coriolis term -----------------
!*
      if(iflag_4_coriolis .gt. id_turn_OFF) then
        if ( iflag_debug.gt.0 ) write(*,*) 'init_sum_coriolis_sph'
        call init_sum_coriolis_sph
      end if
!
! --------- set reference temperature
!
      call allocate_reft_rj_data
      call s_set_ref_temp_sph_mhd
!      call check_reference_temp(my_rank)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD
!
! ---------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'set_material_property'
      call set_material_property
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_set_bc_sph_mhd'
      call s_set_bc_sph_mhd
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_const_radial_mat_4_sph'
      call s_const_radial_mat_4_sph
!
!     --------------------- 
!  set original spectr mesh data for extension of B
!
      call init_radial_sph_interpolation
!
!* -----  find mid-equator point -----------------
!*
      call set_mid_equator_point_global
!
      end subroutine SPH_init_sph_dbench
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_dbench(i_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_node_id_spherical_IO
      use m_field_4_dynamobench
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use set_reference_sph_mhd
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use const_data_4_dynamobench
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap(i_step)
!
      call sync_temp_by_per_temp_sph
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(12)
      call nonlinear
      call end_eleps_time(12)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(4)
      call start_eleps_time(7)
!
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd
      call end_eleps_time(7)
!
!*  -----------  lead mid-equator field --------------
!*
      call start_eleps_time(10)
      if(iflag_debug.gt.0)  write(*,*) 'const_data_4_dynamobench'
      call s_const_data_4_dynamobench
      call output_field_4_dynamobench(i_step, time)
      call end_eleps_time(10)
      call end_eleps_time(4)
!
      end subroutine SPH_analyze_dbench
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_dbench
!
!      end subroutine SPH_finalize_dbench
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_d_bench
