!!@brief  module SPH_analyzer_add_initial
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Jan.., 2014
!
!>@brief  Main spectrum method loop to generate initial field
!!@n      Initial field definision is in  const_sph_initial_spectr.f90
!!
!!@verbatim
!!      subroutine initialize_add_sph_initial
!!      subroutine SPH_add_initial_field
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
      use m_SPH_MHD_model_data
      use m_MHD_step_parameter
      use t_SPH_mesh_field_data
      use t_ctl_data_MHD
      use t_MHD_file_parameter
      use t_SPH_mesh_field_data
      use t_field_data_IO
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!>      Control struture for MHD simulation
      type(DNS_mhd_simulation_control), save :: DNS_MHD_ctl1
      private :: MHD_ctl_name, DNS_MHD_ctl1
!
!>      Structure of spectr grid and data
      type(SPH_mesh_field_data), save, private :: SPH_MHD1
!>      Structure of restart IO data
      type(field_IO), save, private :: rst_IO1
!
      private :: SPH_add_initial_field
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_add_sph_initial
!
      use t_ctl_data_sph_MHD_psf
      use set_control_sph_mhd
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_noviz'
      call read_control_4_sph_MHD_noviz(MHD_ctl_name, DNS_MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_4_SPH_make_init'
      call input_control_4_SPH_make_init                                &
     &   (MHD_files1, DNS_MHD_ctl1, MHD_step1, SPH_model1,              &
     &    SPH_WK1, SPH_MHD1, FEM_d1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!    precondition elaps start
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_add_initial_field'
      call SPH_add_initial_field(SPH_model1, SPH_MHD1, SPH_WK1)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_add_sph_initial
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_add_initial_field(SPH_model, SPH_MHD, SPH_WK)
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
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
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
      call read_alloc_sph_restart_data(MHD_files1%fst_file_IO,          &
     &    MHD_step1%init_d, SPH_MHD%fld, MHD_step1%rst_step)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd'
      call init_r_infos_sph_mhd(SPH_model%bc_IO, SPH_MHD%groups,        &
     &    SPH_model%MHD_BC, SPH_MHD%sph, SPH_model%MHD_prop,            &
     &    SPH_model%omega_sph, SPH_model%sph_MHD_bc)
!
      call init_reference_scalars                                       &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_WK%r_2nd,                      &
     &    SPH_model%ref_temp, SPH_model%ref_comp, SPH_MHD%fld,          &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
! ---------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_spectrum'
      call sph_initial_spectrum(MHD_files1%fst_file_IO,                 &
     &    SPH_model%sph_MHD_bc, SPH_MHD, MHD_step1%rst_step, rst_IO1)
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
