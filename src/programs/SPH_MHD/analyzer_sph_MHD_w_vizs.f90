!>@file   analyzer_sph_MHD_w_vizs.f90
!!@brief  module analyzer_sph_MHD_w_vizs
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mhd_w_vizs(control_file_name)
!!      subroutine evolution_sph_mhd_w_vizs
!!        character(len=kchara), intent(in) :: control_file_name
!!@endverbatim
!
      module analyzer_sph_MHD_w_vizs
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use t_spherical_MHD
      use t_sph_MHD_w_vizs
!
      implicit none
!
!
!>      Structure of the all data of program
      type(spherical_MHD), save, private :: MHDMs
!>      Structure for visualization in spherical MHD
      type(sph_MHD_w_vizs), save, private :: MVIZs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd_w_vizs(control_file_name)
!
      use t_time_data
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_vizs
      use t_SPH_mesh_field_data
      use t_SPH_MHD_zonal_mean_viz
      use t_three_visualizers
      use input_control_sph_MHD
      use set_control_sph_mhd
      use set_control_4_SPH_to_FEM
      use SPH_analyzer_MHD
      use FEM_analyzer_sph_MHD
      use FEM_to_VIZ_bridge
      use parallel_FEM_mesh_init
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD_vizs
!
      character(len=kchara), intent(in) :: control_file_name
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: MHD_ctl1
!>      Additional structures for spherical MHD dynamo with viz module
      type(add_vizs_sph_mhd_ctl), save :: add_VMHD_ctl1
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      MHDMs%MHD_step%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 's_input_control_SPH_MHD_vizs'
      call s_input_control_SPH_MHD_vizs(control_file_name,              &
     &    MHDMs%MHD_files, MHD_ctl1, add_VMHD_ctl1,                     &
     &    MHDMs%MHD_step, MHDMs%SPH_model, MHDMs%SPH_WK,                &
     &    MHDMs%SPH_MHD, MVIZs%FEM_DAT)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(MHDMs%MHD_files, MHDMs%MHD_step,      &
     &    MVIZs%FEM_DAT, MHDMs%MHD_IO, MHDMs%m_SR)
      call init_FEM_to_VIZ_bridge(MHDMs%MHD_step%viz_step,              &
     &    MVIZs%FEM_DAT%geofem, MVIZs%VIZ_DAT, MHDMs%m_SR)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD                                           &
     &   (MHDMs%MHD_files, MHDMs%SPH_model, MVIZs%FEM_DAT,              &
     &    MHDMs%MHD_step, MHDMs%MHD_IO%rst_IO, MHDMs%SPH_MHD,           &
     &    MHDMs%SPH_WK, MHDMs%m_SR)
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0) write(*,*) 'init_three_visualize'
      call init_three_visualize(MHDMs%MHD_step%viz_step,                &
     &    MVIZs%FEM_DAT%geofem, MVIZs%FEM_DAT%field, MVIZs%VIZ_DAT,     &
     &    add_VMHD_ctl1%viz3_ctls, MVIZs%VIZ3s, MHDMs%m_SR)
!
      call init_zonal_mean_vizs                                         &
     &   (MHDMs%MHD_step%viz_step, MVIZs%FEM_DAT%geofem,                &
     &    MVIZs%VIZ_DAT%edge_comm, MVIZs%FEM_DAT%field,                 &
     &    add_VMHD_ctl1%zm_ctls, MVIZs%zmeans, MHDMs%m_SR)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd_w_vizs
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd_w_vizs
!
      use t_time_data
      use t_SPH_MHD_zonal_mean_viz
      use t_three_visualizers
      use SPH_analyzer_MHD
      use FEM_analyzer_sph_MHD
      use output_viz_file_control
      use init_sph_MHD_elapsed_label
!
      integer(kind = kint) :: iflag_finish
!
!     ---------------------
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(MHDMs%MHD_step%init_d,                   &
     &                         MHDMs%MHD_step%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(MHDMs%MHD_step%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(MHDMs%MHD_step%time_d%i_time_step,      &
     &                          MHDMs%MHD_step)) then
          call alloc_sph_trans_area_snap(MHDMs%SPH_MHD%sph,             &
     &                                   MHDMs%SPH_WK%trns_WK)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(MHDMs%MHD_files, iflag_finish,             &
     &      MHDMs%SPH_model, MHDMs%MHD_step, MHDMs%MHD_IO%rst_IO,       &
     &      MHDMs%SPH_MHD, MHDMs%SPH_WK, MHDMs%m_SR)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(MHDMs%MHD_step%time_d%i_time_step,      &
     &                          MHDMs%MHD_step)) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD                                    &
     &       (MHDMs%SPH_MHD%sph, MHDMs%SPH_WK%trns_WK,                  &
     &        MVIZs%FEM_DAT%geofem, MVIZs%FEM_DAT%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHDMs%MHD_files, MVIZs%FEM_DAT,        &
     &      MHDMs%MHD_step, MHDMs%MHD_IO, MHDMs%m_SR)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(MHDMs%MHD_step%time_d%i_time_step,     &
     &                           MHDMs%MHD_step%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface', my_rank
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(MHDMs%MHD_step%time_d%i_time_step,    &
     &                            MHDMs%MHD_step%viz_step)
          call visualize_three                                          &
     &       (MHDMs%MHD_step%viz_step, MHDMs%MHD_step%time_d,           &
     &        MVIZs%FEM_DAT%geofem, MVIZs%FEM_DAT%field,                &
     &        MVIZs%VIZ_DAT, MVIZs%VIZ3s, MHDMs%m_SR)
!*
!*  ----------- Zonal means --------------
!*
          if(MHDMs%MHD_step%viz_step%istep_psf .ge. 0                   &
     &        .or. MHDMs%MHD_step%viz_step%istep_map .ge. 0) then
            call SPH_MHD_zmean_vizs                                     &
     &         (MHDMs%MHD_step%viz_step, MHDMs%MHD_step%time_d,         &
     &          MHDMs%SPH_MHD%sph, MVIZs%FEM_DAT%geofem,                &
     &          MHDMs%SPH_WK%trns_WK, MVIZs%FEM_DAT%field,              &
     &          MVIZs%zmeans, MHDMs%m_SR)
          end if
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!*
        if(lead_field_data_flag(MHDMs%MHD_step%time_d%i_time_step,      &
     &                          MHDMs%MHD_step)) then
          call dealloc_sph_trans_area_snap(MHDMs%SPH_WK%trns_WK)
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
      call FEM_finalize(MHDMs%MHD_files, MHDMs%MHD_step,                &
     &                  MHDMs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(MHDMs%SPH_MHD%sph)
      if (iflag_debug.eq.1) write(*,*) 'output_elapsed_times '
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd_w_vizs
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD_w_vizs
