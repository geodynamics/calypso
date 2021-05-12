!>@file   analyzer_fujitsu_test.f90
!!@brief  module analyzer_fujitsu_test
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_fujitu_test
!!@endverbatim
!
      module analyzer_fujitsu_test
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_SPH_MHD_model_data
      use m_MHD_step_parameter
      use t_SPH_mesh_field_data
      use t_ctl_data_MHD
      use t_viz_sections
      use t_SPH_MHD_zonal_mean_viz
      use t_sph_trans_arrays_MHD
      use t_comm_table
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_MHD
      use init_sph_MHD_elapsed_label
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
!>      Structure of sectioning and isosurfaceing modules
      type(surfacing_modules), save, private :: viz_psfs1
!>      Structure of edge communication table
      type(communication_table), save, private :: edge_comm_M
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_fujitu_test
!
      use t_ctl_data_sph_MHD_psf
      use input_control_sph_MHD
      use FEM_to_PSF_bridge
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      MHD_step1%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_w_psf'
      call read_control_4_sph_MHD_w_psf(MHD_ctl_name, DNS_MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_MHD_psf'
      call input_control_SPH_MHD_psf                                    &
     &   (MHD_files1, DNS_MHD_ctl1, MHD_step1, SPH_model1,              &
     &    SPH_WK1, SPH_MHD1, FEM_d1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(MHD_files1, MHD_step1,                &
     &    FEM_d1%geofem, FEM_d1%field, FEM_d1%iphys,                    &
     &    MHD_IO1, FEM_d1%v_sol)
      call init_FEM_to_PSF_bridge                                       &
     &   (MHD_step1%viz_step, FEM_d1%geofem, edge_comm_M)
      return
!
      end subroutine initialize_sph_fujitu_test
!
! ----------------------------------------------------------------------
!
      end module analyzer_fujitsu_test
