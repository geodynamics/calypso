!>@file   analyzer_sph_MHD_w_4vizs.f90
!!@brief  module analyzer_sph_MHD_w_4vizs
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Nov., 2021
!
!>@brief  Main loop for MHD dynamo simulation with PVR
!!
!!@verbatim
!!      subroutine initialize_sph_mhd_w_4vizs
!!      subroutine evolution_sph_mhd_w_4vizs
!!@endverbatim
!
      module analyzer_sph_MHD_w_4vizs
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
      use t_ctl_data_MHD_w_4vizs
      use t_VIZ_mesh_field
      use t_four_visualizers
      use t_SPH_MHD_zonal_mean_viz
      use t_sph_trans_arrays_MHD
      use t_comm_table
      use t_mesh_SR
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
      type(DNS_mhd_sim_w_viz4_control), save :: DNS_MHD_ctl4
      private :: MHD_ctl_name, DNS_MHD_ctl4
!
!>      Structure of spectr grid and data
      type(SPH_mesh_field_data), save, private :: SPH_MHD1
!>      Structure of data for four visualizations
      type(VIZ_mesh_field), save, private :: VIZ_DAT4
!>      Structure of four visualization modules
      type(four_visualize_modules), save, private :: vizs4
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd_w_4vizs
!
      use t_ctl_data_sph_MHD_psf
      use input_control_sph_MHD
      use FEM_to_VIZ_bridge
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_4vizs'
      call read_control_4_sph_MHD_4vizs(MHD_ctl_name, DNS_MHD_ctl4)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_MHD_psf'
      call input_control_sph_MHD_4vizs                                  &
     &   (MHD_files1, DNS_MHD_ctl4, MHD_step1, SPH_model1,              &
     &    SPH_WK1, SPH_MHD1, FEM_d1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(MHD_files1, MHD_step1,                &
     &    FEM_d1%geofem, FEM_d1%field, FEM_d1%iphys, MHD_IO1, m_SR1)
      call init_FEM_to_VIZ_bridge                                       &
     &   (MHD_step1%viz_step, FEM_d1%geofem, VIZ_DAT4, m_SR1)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD(MHD_files1, SPH_model1, FEM_d1%iphys,     &
     &    MHD_step1, MHD_IO1%rst_IO, SPH_MHD1, SPH_WK1,                 &
     &    m_SR1%SR_sig, m_SR1%SR_r)
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0)  write(*,*) 'init_four_visualize'
      call init_four_visualize(MHD_step1%viz_step,                      &
     &    FEM_d1%geofem, FEM_d1%field, VIZ_DAT4,                        &
     &    DNS_MHD_ctl4%viz4_ctl, vizs4, m_SR1)
!
      call init_zonal_mean_sections(MHD_step1%viz_step, FEM_d1%geofem,  &
     &    VIZ_DAT4%edge_comm, FEM_d1%field, DNS_MHD_ctl4%zm_ctls,       &
     &    zmeans1, m_SR1%SR_sig, m_SR1%SR_il)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd_w_4vizs
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd_w_4vizs
!
      use output_viz_file_control
!
      integer(kind = kint) :: iflag_finish
!
!     ---------------------
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(MHD_step1%init_d, MHD_step1%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(MHD_step1%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,           &
     &                          MHD_step1)) then
          call alloc_sph_trans_area_snap                                &
     &       (SPH_MHD1%sph%sph_rtp, SPH_WK1%trns_WK)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(MHD_step1%time_d%i_time_step,              &
     &      MHD_files1, iflag_finish, SPH_model1, MHD_step1,            &
     &      MHD_IO1%rst_IO, SPH_MHD1, SPH_WK1,                          &
     &      m_SR1%SR_sig, m_SR1%SR_r)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,           &
     &                          MHD_step1)) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD(SPH_MHD1%sph, SPH_WK1%trns_WK,     &
     &        FEM_d1%geofem, FEM_d1%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHD_files1,                            &
     &      FEM_d1%geofem, FEM_d1%field, MHD_step1, MHD_IO1, m_SR1)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(MHD_step1%time_d%i_time_step,          &
     &                           MHD_step1%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface', my_rank
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(MHD_step1%time_d%i_time_step,         &
     &                            MHD_step1%viz_step)
          call visualize_four(MHD_step1%viz_step, MHD_step1%time_d,     &
     &        FEM_d1%geofem, FEM_d1%field, VIZ_DAT4, vizs4, m_SR1)
!*
!*  ----------- Zonal means --------------
!*
          if(MHD_step1%viz_step%istep_psf .ge. 0) then
            call SPH_MHD_zmean_sections(MHD_step1%viz_step,             &
     &          MHD_step1%time_d, SPH_MHD1%sph, FEM_d1%geofem,          &
     &          SPH_WK1%trns_WK, FEM_d1%field, zmeans1, m_SR1)
          end if
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!*
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,           &
     &                          MHD_step1)) then
          call dealloc_sph_trans_area_snap(SPH_WK1%trns_WK)
        end if
!
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, MHD_IO1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(SPH_MHD1%sph)
      if (iflag_debug.eq.1) write(*,*) 'output_elapsed_times '
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd_w_4vizs
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD_w_4vizs
