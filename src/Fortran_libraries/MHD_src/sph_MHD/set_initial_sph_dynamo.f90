!>@file   set_initial_sph_dynamo.f90
!!@brief  module set_initial_sph_dynamo
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine read_sph_initial_data_control(MHD_files, SPH_model,  &
!!     &          sph, ipol, MHD_step, rj_fld, sph_fst_IO)
!!      subroutine sph_initial_MHD_data_control                         &
!!     &         (MHD_files, SPH_model, SPH_MHD, MHD_step, sph_fst_IO)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: ipol
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!@endverbatim
!
!
      module set_initial_sph_dynamo
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_SPH_MHD_model_data
      use t_MHD_file_parameter
      use t_IO_step_parameter
      use t_time_data
      use t_spheric_parameter
      use t_boundary_params_sph_MHD
      use t_radial_reference_field
      use t_field_data_IO
      use t_phys_address
      use t_phys_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_sph_initial_data_control(MHD_files, SPH_model,    &
     &          sph, ipol, MHD_step, rj_fld, sph_fst_IO)
!
      use m_machine_parameter
      use m_initial_field_control
!
      use t_MHD_step_parameter
!
      use set_sph_restart_IO
      use sph_mhd_rst_IO_control
      use set_sph_restart_IO
      use sph_radial_grad_4_magne
      use calypso_mpi
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(in) :: SPH_model 
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: sph_fst_IO
!
!
      if (MHD_step%iflag_restart_mode .ne. i_rst_by_file) return
        if(iflag_debug .gt. 0) write(*,*) 'read_alloc_sph_restart_data'
        call read_alloc_sph_restart_data                                &
     &     (MHD_files%fst_file_IO, MHD_step%init_d, MHD_step%time_d,    &
     &      rj_fld, MHD_step%rst_step)
!
        call extend_by_potential_with_j                                 &
     &     (sph%sph_rj, SPH_model%sph_MHD_bc%sph_bc_B,                  &
     &      ipol%base%i_magne, ipol%base%i_current, rj_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'copy_time_step_data'
        call set_sph_restart_num_to_IO(rj_fld, sph_fst_IO)
        call calypso_mpi_barrier
!
      end subroutine read_sph_initial_data_control
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_initial_MHD_data_control(MHD_files, SPH_model,     &
     &          sph, ipol, MHD_step, rj_fld, sph_fst_IO)
!
      use m_machine_parameter
      use m_initial_field_control
!
      use t_MHD_step_parameter
!
      use set_sph_restart_IO
      use sph_mhd_rst_IO_control
      use initialize_sph_dynamo
      use set_sph_restart_IO
      use sph_radial_grad_4_magne
      use calypso_mpi
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(in) :: SPH_model 
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(MHD_step_param), intent(in) :: MHD_step
!
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: sph_fst_IO
!
!
      if(MHD_step%iflag_restart_mode .eq. i_rst_by_file) return
!
!   for dynamo benchmark
      if     (MHD_step%iflag_restart_mode .eq. i_rst_dbench0            &
     &   .or. MHD_step%iflag_restart_mode .eq. i_rst_dbench1            &
     &   .or. MHD_step%iflag_restart_mode .eq. i_rst_dbench2            &
     &   .or. MHD_step%iflag_restart_mode .eq. i_rst_dbench_qcv) then
        call sph_initial_data_4_benchmarks(MHD_step%iflag_restart_mode, &
     &                                     sph, ipol, rj_fld)
!
!   set small seed magnetic field
      else if(MHD_step%iflag_restart_mode .eq. i_rst_no_file) then
        call sph_initial_data_w_seed_B(sph, SPH_model%sph_MHD_bc,       &
     &                                 SPH_model%refs, ipol, rj_fld)
      end if
!
      call extend_by_potential_with_j                                   &
     &   (sph%sph_rj, SPH_model%sph_MHD_bc%sph_bc_B,                    &
     &    ipol%base%i_magne, ipol%base%i_current, rj_fld)
!
      if(iflag_debug .gt. 0) write(*,*) 'copy_time_step_data'
      call set_sph_restart_num_to_IO(rj_fld, sph_fst_IO)
!
      if (MHD_step%iflag_restart_mode.ne.i_rst_by_file                  &
     &     .and. MHD_step%init_d%i_time_step.eq.0) then
        if(iflag_debug .gt. 0) write(*,*) 'output_sph_restart_control'
        call output_sph_restart_control(MHD_step%init_d%i_time_step,    &
     &      MHD_files%fst_file_IO, MHD_step%time_d, rj_fld,             &
     &      MHD_step%rst_step, sph_fst_IO)
      end if
      call calypso_mpi_barrier
!
      end subroutine sph_initial_MHD_data_control
!
!-----------------------------------------------------------------------
!
      end module set_initial_sph_dynamo
