!!@brief  module SPH_analyzer_add_initial
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Jan.., 2014
!
!>@brief  Main spectrum method loop to generate initial field
!!@n      Initial field definision is in  const_sph_initial_spectr.f90
!!
!!@verbatim
!!      subroutine initialize_add_sph_initial(control_file_name, sadi)
!!        character(len=kchara), intent(in) :: control_file_name
!!        type(SPH_add_initial), intent(inout) :: sadi
!!@endverbatim
!
      module SPH_analyzer_add_initial
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use t_spherical_MHD
      use t_FEM_mesh_field_data
      use t_field_data_IO
!
      implicit none
!
!>      Control struture for MHD simulation
      type(spherical_MHD), save, private :: SMHDs
!>        Structure of restart IO data
      type(field_IO), save, private :: rst_IO1
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_data), save, private :: FEM_DATs
!
      private :: SPH_add_initial_field
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_add_sph_initial(control_file_name)
!
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_psf
      use set_control_sph_mhd
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
!
      character(len=kchara), intent(in) :: control_file_name
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control) :: MHD_ctl1
!>      Additional structures for spherical MHD dynamo with viz module
      type(add_psf_sph_mhd_ctl) :: add_SMHD_ctl1
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_debug .eq. 1) write(*,*) 'input_control_4_SPH_make_init'
      call input_control_4_SPH_make_init(control_file_name,             &
     &    SMHDs%MHD_files, MHD_ctl1, add_SMHD_ctl1,                     &
     &    SMHDs%MHD_step, SMHDs%SPH_model, SMHDs%SPH_WK,                &
     &    SMHDs%SPH_MHD, FEM_DATs)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!    precondition elaps start
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_add_initial_field'
      call SPH_add_initial_field(SMHDs%MHD_files, SMHDs%MHD_step,       &
    &     SMHDs%SPH_model, SMHDs%SPH_MHD, SMHDs%SPH_WK, rst_IO1)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_add_sph_initial
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_add_initial_field(MHD_files, MHD_step, SPH_model,  &
     &                                 SPH_MHD, SPH_WK, rst_IO)
!
      use set_control_sph_mhd
      use check_dependency_for_MHD
      use const_sph_initial_spectr
      use set_reference_sph_mhd
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use sph_transforms_4_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use set_initial_sph_dynamo
      use sph_mhd_rst_IO_control
      use input_control_sph_MHD
      use sph_radial_grad_4_magne
      use schmidt_poly_on_rtm_grid
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(field_IO), intent(inout) :: rst_IO
!
!   Allocate spectr field data
!
      call init_sph_MHD_field_data                                      &
     &   (SPH_MHD%sph, SPH_MHD%fld, SPH_MHD%ipol)
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(SPH_model%bc_IO, SPH_MHD%groups,    &
     &   SPH_model%MHD_BC, SPH_MHD%ipol, SPH_MHD%sph, SPH_WK%r_2nd,     &
     &   SPH_model%omega_sph, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
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
      if(iflag_debug.gt.0) write(*,*)' read_alloc_sph_restart_data'
      call read_alloc_sph_restart_data(MHD_files%fst_file_IO,           &
     &    MHD_step%init_d, SPH_MHD%fld, MHD_step%rst_step)
!
! ---------------------------------
!
      call init_reference_fields                                        &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_WK%r_2nd, SPH_model%refs,      &
     &    SPH_MHD%fld, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
! ---------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_spectrum'
      call sph_initial_spectrum(MHD_files%fst_file_IO,                  &
     &    SPH_model%sph_MHD_bc, SPH_MHD, MHD_step,                      &
     &    MHD_step%rst_step, rst_IO)
!
      call extend_by_potential_with_j                                   &
     &   (SPH_MHD%sph%sph_rj, SPH_model%sph_MHD_bc%sph_bc_B,            &
     &    SPH_MHD%ipol%base%i_magne, SPH_MHD%ipol%base%i_current,       &
     &    SPH_MHD%fld)
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      end subroutine SPH_add_initial_field
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_add_initial
