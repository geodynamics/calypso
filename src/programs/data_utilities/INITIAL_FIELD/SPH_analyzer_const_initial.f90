!
!     module SPH_analyzer_const_initial
!
!      subroutine SPH_initialize_MHD
!
!      Written by H. Matsui
!
      module SPH_analyzer_const_initial
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
      subroutine SPH_const_initial
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
!
      use m_geometry_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
      use m_rms_4_sph_spectr
!
      use set_control_sph_mhd
      use load_data_for_sph_IO
      use const_sph_initial_spectr
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
      use sph_mhd_rms_IO
      use cal_sol_sph_MHD_crank
      use cal_nonlinear
!
      use m_work_time
!
!
!   Load spherical harmonics data
!
      if (iflag_debug.eq.1) write(*,*) 'input_sph_trans_grids'
      call start_eleps_time(4)
      call input_sph_trans_grids(my_rank)
      call end_eleps_time(4)
!
!   Allocate spectr field data
!
      call allocate_phys_rj_data
      call allocate_phys_rtp_data
      call allocate_rot_rj_data
      call set_sph_sprctr_data_address
      call set_sph_nod_data_address
!
!      if(iflag_debug .gt. 0) call check_add_trans_sph_MHD
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
!      if (iflag_debug.gt.0) write(*,*) 'const_2nd_fdm_matrices'
!      call const_2nd_fdm_matrices
!
!      if (iflag_debug.gt.0) write(*,*) 's_cal_sph_bc_fdm_matrices'
!      call s_cal_sph_bc_fdm_matrices
!
!      if (iflag_debug.gt.0) write(*,*) 'const_2nd_fdm_coefs'
!      call const_2nd_fdm_coefs
!
!* -----  set integrals for coriolis term -----------------
!*
!      if(iflag_4_coriolis .gt. id_turn_OFF) then
!        if ( iflag_debug.gt.0 ) write(*,*) 'init_sum_coriolis_sph'
!        call init_sum_coriolis_sph
!      end if
!
! --------- set reference temperature
!
!      call allocate_reft_rj_data
!      call s_set_ref_temp_sph_mhd
!      call check_reference_temp(my_rank)
!
! ---------------------------------
!
!      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
!      call init_sph_transform_MHD
!
! ---------------------------------
!
!      if(iflag_debug.gt.0) write(*,*)' set_material_property'
!      call set_material_property
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_spectrum'
      call sph_initial_spectrum
!
      end subroutine SPH_const_initial
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_const_initial
