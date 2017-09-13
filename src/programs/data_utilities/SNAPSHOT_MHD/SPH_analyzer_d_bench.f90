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
!!      subroutine SPH_init_sph_dbench(MHD_files, bc_IO, iphys, cdat)
!!        type(phys_address), intent(in) :: iphys
!!      subroutine SPH_analyze_dbench(i_step, MHD_files, cdat, bench)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(phys_data), intent(inout) :: cdat
!!        type(dynamobench_monitor), intent(inout) :: bench
!!      subroutine SPH_finalize_dbench
!!@endverbatim
!
      module SPH_analyzer_d_bench
!
      use m_precision
      use m_MHD_step_parameter
      use m_boundary_data_sph_MHD
      use m_radial_matrices_sph
      use t_phys_address
      use t_MHD_file_parameter
      use t_field_on_circle
      use t_field_4_dynamobench
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_dbench(MHD_files, bc_IO, iphys, cdat)
!
      use m_constants
      use m_array_for_send_recv
      use calypso_mpi
      use m_machine_parameter
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_rms_4_sph_spectr
      use m_physical_property
      use m_sph_trans_arrays_MHD
      use m_bc_data_list
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use set_sph_phys_address
      use const_fdm_coefs
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use cal_nonlinear
      use init_sphrical_transform_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use cal_rms_fields_by_sph
      use r_interpolate_sph_data
      use sph_mhd_rst_IO_control
      use field_at_mid_equator
      use check_dependency_for_MHD
      use input_control_sph_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(boundary_spectra), intent(in) :: bc_IO
      type(phys_address), intent(in) :: iphys
!
      type(circle_fld_maker), intent(inout) :: cdat
!
!   Allocate spectr field data
!
      call set_sph_MHD_sprctr_data                                      &
     &   (sph1%sph_rj, MHD_prop1, ipol, idpdr, itor, rj_fld1)
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, sph1%sph_rtp%nnod_rtp)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_rms_4_sph_spectr'
      call init_rms_4_sph_spectr(sph1%sph_params,                       &
     &    sph1%sph_rj, rj_fld1, pwr1, WK_pwr)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo                                     &
     &   (bc_IO, sph_grps1, MHD_BC1, ipol, sph1,                        &
     &    omega_sph1, ref_temp1, ref_comp1, MHD_prop1, sph_MHD_bc1,     &
     &    r_2nd, rj_fld1)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD(MHD_prop1, sph_MHD_bc1,               &
     &    ipol, idpdr, itor, iphys, sph1, comms_sph1, omega_sph1,       &
     &    trans_p1, trns_WK1, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'const_radial_mat_sph_snap'
      call const_radial_mat_sph_snap(MHD_prop1, sph_MHD_bc1,            &
     &    sph1%sph_rj, r_2nd, trans_p1%leg, sph_MHD_mat1)
!
!     --------------------- 
!  set original spectr mesh data for extension of B
!
      call init_radial_sph_interpolation                                &
     &   (MHD_files%org_rj_file_IO, sph1%sph_params, sph1%sph_rj)
!
!* -----  find mid-equator point -----------------
!*
      call set_mid_equator_point_global                                 &
     &   (sph1%sph_params, sph1%sph_rtp, sph1%sph_rj, cdat)
!
      end subroutine SPH_init_sph_dbench
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_dbench(i_step, MHD_files, cdat, bench)
!
      use m_work_time
      use m_physical_property
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_sph_trans_arrays_MHD
      use m_rms_4_sph_spectr
!
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use const_data_4_dynamobench
      use input_control_sph_MHD
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(circle_fld_maker), intent(inout) :: cdat
      type(dynamobench_monitor), intent(inout) :: bench
!
      integer(kind = kint) :: iflag
!
!
      call read_alloc_sph_rst_4_snap(i_step,                            &
     &    MHD_files%org_rj_file_IO, MHD_files%fst_file_IO, sph1%sph_rj, &
     &    ipol, rj_fld1, MHD_step1%rst_step, MHD_step1%init_d)
      call copy_time_data(MHD_step1%init_d, MHD_step1%time_d)
!
      call sync_temp_by_per_temp_sph(ref_temp1, ref_comp1, MHD_prop1,   &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(sph1%sph_rj, r_2nd,                   &
     &    MHD_prop1, sph_MHD_bc1, trans_p1%leg, ipol, itor, rj_fld1)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_elapsed_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(ref_temp1, ref_comp1, MHD_prop1,  &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      iflag = lead_field_data_flag(i_step, MHD_step1)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
        call s_lead_fields_4_sph_mhd                                    &
     &     (sph1, comms_sph1, r_2nd, MHD_prop1, sph_MHD_bc1, trans_p1,  &
     &      ipol, sph_MHD_mat1, trns_WK1, rj_fld1)
      end if
      call end_elapsed_time(9)
!
!*  -----------  lead mid-equator field --------------
!*
      call start_elapsed_time(4)
      call start_elapsed_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'const_data_4_dynamobench'
      call s_const_data_4_dynamobench                                   &
     &   (MHD_step1%time_d%time, sph1%sph_params, sph1%sph_rj,          &
     &    sph_MHD_bc1, trans_p1%leg, ipol, itor, rj_fld1,               &
     &    cdat, pwr1, bench, WK_pwr)
      call output_field_4_dynamobench(i_step, MHD_step1%time_d%time,    &
     &   sph_MHD_bc1%sph_bc_U, sph_MHD_bc1%sph_bc_B, ipol, bench)
      call end_elapsed_time(11)
      call end_elapsed_time(4)
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
