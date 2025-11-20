!>@file   main_sph_initial_fld.f90
!!@brief  program sph_meke_initial
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to generate initial field
!!@n       Define initial field at const_sph_initial_spectr.f90
!
     program sph_meke_initial
!
      use m_precision
      use calypso_mpi
!
      use t_spherical_MHD
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!
!
      call calypso_MPI_init
      call initialize_const_sph_initial(MHD_ctl_name)
      call calypso_MPI_finalize
!
      stop
!
! ----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine initialize_const_sph_initial(control_file_name)
!
      use t_ctl_data_MHD
      use input_control_sph_MHD
!
      character(len=kchara), intent(in) :: control_file_name
!
!>      Control struture for MHD simulation
      type(spherical_MHD), save :: SMHDs
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: MHD_ctl1
!
!
!   Load parameter file
!
      if(iflag_debug.eq.1) write(*,*) 'input_control_4_SPH_MHD_nosnap'
      call input_control_4_SPH_MHD_nosnap(control_file_name,            &
     &    SMHDs%MHD_files, MHD_ctl1, SMHDs%MHD_step, SMHDs%SPH_model,   &
     &    SMHDs%SPH_WK, SMHDs%SPH_MHD)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_const_initial_field'
      call SPH_const_initial_field(SMHDs%MHD_files, SMHDs%MHD_step,     &
     &    SMHDs%SPH_model, SMHDs%SPH_MHD, SMHDs%SPH_WK)
!
      end subroutine initialize_const_sph_initial
!
! ----------------------------------------------------------------------
!
      subroutine SPH_const_initial_field(MHD_files, MHD_step,           &
     &                                   SPH_model, SPH_MHD, SPH_WK)
!
      use init_radial_infos_sph_mhd
      use init_sph_radius_variations
      use radial_reference_field_IO
      use check_dependency_for_MHD
      use input_control_sph_MHD
      use schmidt_poly_on_rtm_grid
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
!   Allocate spectr field data
!
      call set_sph_MHD_sprctr_data(SPH_model%MHD_prop, SPH_MHD)
!
      call alloc_schmidt_normalize(SPH_MHD%sph%sph_rlm%nidx_rlm(2),     &
     &    SPH_MHD%sph%sph_rj%nidx_rj(2), SPH_WK%trans_p%leg)
      call copy_sph_normalization_2_rlm(SPH_MHD%sph%sph_rlm,            &
     &    SPH_WK%trans_p%leg%g_sph_rlm)
      call copy_sph_normalization_2_rj(SPH_MHD%sph%sph_rj,              &
     &    SPH_WK%trans_p%leg%g_sph_rj)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(SPH_MHD%ipol, SPH_MHD%sph,          &
     &    SPH_WK%r_2nd, SPH_WK%r_n2e_3rd, SPH_WK%r_e2n_1st,             &
     &    SPH_model%omega_sph, SPH_model%MHD_prop)
!
      call init_radial_reference_data(SPH_MHD%sph%sph_rj, SPH_MHD%ipol, &
     &    SPH_model%MHD_prop, SPH_model%refs)
      call init_radius_variations_sph_mhd(SPH_MHD%sph, SPH_WK%r_2nd,    &
     &    SPH_model%MHD_prop, SPH_model%refs)
!
      if (iflag_debug.gt.0) write(*,*) 'init_bc_infos_sph_mhd_evo'
      call init_bc_infos_sph_mhd_evo(SPH_model%bc_IO, SPH_MHD%groups,   &
     &    SPH_model%MHD_BC, SPH_MHD%ipol, SPH_MHD%sph, SPH_WK%r_2nd,    &
     &    SPH_model%MHD_prop, SPH_model%refs%ref_field,                 &
     &    SPH_model%sph_MHD_bc)
!
      call init_reference_fields                                        &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_WK%r_2nd, SPH_model%refs,      &
     &    SPH_MHD%fld, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)

! ---------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_spectrum'
      call sph_initial_spectrum(MHD_files%fst_file_IO,                  &
     &    SPH_model%sph_MHD_bc, SPH_MHD, MHD_step)
!
      end subroutine SPH_const_initial_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sph_initial_spectrum(fst_file_IO, sph_MHD_bc,          &
     &                                SPH_MHD, MHD_step)
!
      use t_phys_address
      use t_field_data_IO
      use m_initial_field_control
      use m_t_step_parameter
      use sph_mhd_rst_IO_control
      use set_sph_restart_IO
!
      type(field_IO_params), intent(in) :: fst_file_IO
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(MHD_step_param), intent(inout) :: MHD_step
!
!>      Structure of restart IO data
      type(field_IO), save :: sph_fst_IO
!
!
!  Set heat source if  heat source is exist
      call set_initial_heat_source_sph(sph_MHD_bc%sph_bc_T,             &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld)
!  Set light element source if light element is exist
      call set_initial_light_source_sph(sph_MHD_bc%sph_bc_C,            &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld)
!
!  Copy initial field to restart IO data
      call set_sph_restart_num_to_IO(SPH_MHD%fld, sph_fst_IO)
!
      call output_sph_restart_control                                   &
     &   (MHD_step%time_d%i_time_step, fst_file_IO,                     &
     &    MHD_step%time_d, SPH_MHD%fld, MHD_step%rst_step, sph_fst_IO)
!
      end subroutine sph_initial_spectrum
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_heat_source_sph(sph_bc_T, sph_rj,          &
     &                                       ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      real (kind = kreal) :: rr
      integer(kind = kint) :: ii, k, jj
!
!
      if(ipol%base%i_heat_source .le. izero) return
!$omp parallel do
      do ii = 1, sph_rj%nnod_rj
        rj_fld%d_fld(ii,ipol%base%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_address(sph_rj, 0, 0)
!
      if (jj .gt. 0) then
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
          ii = local_sph_node_address(sph_rj, k, jj)
          rr = sph_rj%radius_1d_rj_r(k)
!   Substitute initial heat source
          rj_fld%d_fld(ii,ipol%base%i_heat_source) = two / rr
        end do
      end if
!
      end subroutine set_initial_heat_source_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_light_source_sph                           &
     &         (sph_bc_C, sph_rj, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!      real (kind = kreal) :: rr
      integer(kind = kint) :: ii, k, jj
!
!
      if(ipol%base%i_light_source .le. izero) return
!$omp parallel do
      do ii = 1, sph_rj%nnod_rj
        rj_fld%d_fld(ii,ipol%base%i_light_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_address(sph_rj, 0, 0)
!
      if (jj .gt. 0) then
        do k = sph_bc_C%kr_in, sph_bc_C%kr_out
          ii = local_sph_node_address(sph_rj, k, jj)
!          rr = sph_rj%radius_1d_rj_r(k)
!   Substitute initial heat source
          rj_fld%d_fld(ii,ipol%base%i_light_source) = one
        end do
      end if
!
      end subroutine set_initial_light_source_sph
!
!-----------------------------------------------------------------------
!
      end program sph_meke_initial
