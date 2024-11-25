!>@file   analyzer_sph_snap_w_psf.f90
!!@brief  module analyzer_sph_snap_w_psf
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine initialize_sph_snap_w_psf(control_file_name)
!!      subroutine evolution_sph_snap_w_psf
!!        character(len=kchara), intent(in) :: control_file_name
!!@endverbatim
!
      module analyzer_sph_snap_w_psf
!
      use m_precision
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_machine_parameter
      use t_spherical_MHD
      use t_sph_MHD_w_psf
      use t_elapsed_labels_4_SECTIONS
!
      implicit none
!
      real (kind=kreal), private  ::  total_start
!
!>      Structure of the all data of program
      type(spherical_MHD), save, private :: SNAPs
!>      Structure for visualization in spherical MHD
      type(sph_MHD_w_psf), save, private :: MPSFs
!
!>          Elapsed time labels
      logical, parameter :: flag_detailed1 = .TRUE.
      type(elapsed_labels_4_SECTIONS), save :: elps_SECT1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_snap_w_psf(control_file_name)
!
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_psf
      use t_viz_sections
      use t_SPH_MHD_zmean_sections
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
      use SPH_analyzer_snap_w_vizs
      use FEM_analyzer_sph_MHD
      use FEM_to_PSF_bridge
!
      character(len=kchara), intent(in) :: control_file_name
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: DNS_MHD_ctl1
!>      Additional structures for spherical MHD dynamo with viz module
      type(add_psf_sph_mhd_ctl), save :: add_SMHD_ctl1
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_4_SECT(flag_detailed1, elps_SECT1, elps1)
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_MHD_psf'
      call input_control_SPH_MHD_psf(control_file_name,                 &
     &    SNAPs%MHD_files, DNS_MHD_ctl1, add_SMHD_ctl1, SNAPs%MHD_step, &
     &    SNAPs%SPH_model, SNAPs%SPH_WK, SNAPs%SPH_MHD, MPSFs%FEM_DAT)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!     --------------------- 
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(SNAPs%MHD_files, SNAPs%MHD_step,      &
     &    MPSFs%FEM_DAT, SNAPs%MHD_IO, SNAPs%m_SR)
      call init_FEM_to_PSF_bridge                                       &
     &   (elps_SECT1, SNAPs%MHD_step%viz_step, MPSFs%FEM_DAT%geofem,    &
     &    MPSFs%edge_comm, SNAPs%m_SR)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap_vizs'
      call SPH_init_sph_snap_vizs                                       &
     &   (SNAPs%MHD_files, MPSFs%FEM_DAT, SNAPs%SPH_model,              &
     &    SNAPs%MHD_step, SNAPs%SPH_MHD, SNAPs%SPH_WK, SNAPs%m_SR)
!
!        Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize_surface'
      call init_visualize_surface(elps_SECT1, SNAPs%MHD_step%viz_step,  &
     &    MPSFs%FEM_DAT%geofem, MPSFs%edge_comm, MPSFs%FEM_DAT%field,   &
     &    add_SMHD_ctl1%surfacing_ctls, MPSFs%PSFs, SNAPs%m_SR)
      call init_zonal_mean_sections                                     &
     &   (elps_SECT1, SNAPs%MHD_step%viz_step, MPSFs%FEM_DAT%geofem,    &
     &    MPSFs%edge_comm, MPSFs%FEM_DAT%field, add_SMHD_ctl1%zm_sects, &
     &    MPSFs%zsectios, SNAPs%m_SR)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
      call reset_elapse_after_init_SECT(elps_SECT1, elps1)
!
      end subroutine initialize_sph_snap_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_snap_w_psf
!
      use t_viz_sections
      use t_SPH_MHD_zmean_sections
      use SPH_analyzer_snap_w_vizs
      use FEM_analyzer_sph_MHD
      use output_viz_file_control
      use set_time_step_params
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call set_from_initial_step(SNAPs%MHD_step%init_d,                 &
     &                           SNAPs%MHD_step%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(SNAPs%MHD_step%time_d)
        if(output_IO_flag(SNAPs%MHD_step%time_d%i_time_step,            &
     &                    SNAPs%MHD_step%rst_step) .eqv. .FALSE.) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(SNAPs%MHD_step%time_d%i_time_step,      &
     &                          SNAPs%MHD_step)) then
          call alloc_sph_trans_area_snap(SNAPs%SPH_MHD%sph,             &
     &                                   SNAPs%SPH_WK%trns_WK)
!
          if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap_vizs'
          call SPH_analyze_snap_vizs                                    &
     &       (SNAPs%MHD_files, SNAPs%SPH_model, SNAPs%MHD_step,         &
     &        SNAPs%SPH_MHD, SNAPs%SPH_WK, SNAPs%m_SR)
!*
!*  -----------  output field data --------------
!*
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD                                    &
     &       (SNAPs%SPH_MHD%sph, SNAPs%SPH_WK%trns_WK,                  &
     &        MPSFs%FEM_DAT%geofem, MPSFs%FEM_DAT%field)
!
          if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
          call FEM_analyze_sph_MHD(SNAPs%MHD_files,                     &
     &        MPSFs%FEM_DAT, SNAPs%MHD_step, SNAPs%MHD_IO, SNAPs%m_SR)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
        end if
!
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(SNAPs%MHD_step%time_d%i_time_step,     &
     &                           SNAPs%MHD_step%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface'
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(SNAPs%MHD_step%time_d%i_time_step,    &
     &                          SNAPs%MHD_step%viz_step)
          call visualize_surface(elps_SECT1,                            &
     &        SNAPs%MHD_step%viz_step, SNAPs%MHD_step%time_d,           &
     &        MPSFs%FEM_DAT%geofem, MPSFs%edge_comm,                    &
     &        MPSFs%FEM_DAT%field, MPSFs%PSFs, SNAPs%m_SR)
!*
!*  ----------- Zonal means --------------
!*
          if(SNAPs%MHD_step%viz_step%istep_psf .ge. 0) then
            call SPH_MHD_zmean_sections(elps_SECT1,                     &
     &          SNAPs%MHD_step%viz_step, SNAPs%MHD_step%time_d,         &
     &          SNAPs%SPH_MHD%sph, MPSFs%FEM_DAT%geofem,                &
     &          SNAPs%SPH_WK%trns_WK, MPSFs%FEM_DAT%field,              &
     &          MPSFs%zsectios, SNAPs%m_SR)
          end if
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!
        if(lead_field_data_flag(SNAPs%MHD_step%time_d%i_time_step,      &
     &                          SNAPs%MHD_step)) then
           call dealloc_sph_trans_area_snap(SNAPs%SPH_WK%trns_WK)
        end if
!
!*  -----------  exit loop --------------
!*
        if(SNAPs%MHD_step%time_d%i_time_step                            &
     &        .ge. SNAPs%MHD_step%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(SNAPs%MHD_files, SNAPs%MHD_step, SNAPs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_snap_w_psf
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_snap_w_psf
