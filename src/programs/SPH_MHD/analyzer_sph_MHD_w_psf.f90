!>@file   analyzer_sph_MHD_w_psf.f90
!!@brief  module analyzer_sph_MHD_w_psf
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mhd_w_psf(control_file_name, MHDMs)
!!      subroutine evolution_sph_mhd_w_psf(MHDMs)
!!        character(len=kchara), intent(in) :: control_file_name
!!        type(sph_MHD_w_psf), intent(inout) :: MHDMs
!!@endverbatim
!
      module analyzer_sph_MHD_w_psf
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use t_spherical_MHD
      use t_sph_MHD_w_psf
      use t_elapsed_labels_4_SECTIONS
!
      implicit none
!
!>      Structure of the all data of program
      type(spherical_MHD), save, private :: MHDMs
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
      subroutine initialize_sph_mhd_w_psf(control_file_name)
!
      use t_time_data
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_psf
      use t_SPH_mesh_field_data
      use t_SPH_MHD_zmean_sections
      use t_viz_sections
      use input_control_sph_MHD
      use set_control_sph_mhd
      use set_control_4_SPH_to_FEM
      use SPH_analyzer_MHD
      use FEM_analyzer_sph_MHD
      use FEM_to_PSF_bridge
      use parallel_FEM_mesh_init
      use init_sph_MHD_elapsed_label
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
      MHDMs%MHD_step%finish_d%started_time = MPI_WTIME()
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
     &    MHDMs%MHD_files, DNS_MHD_ctl1, add_SMHD_ctl1, MHDMs%MHD_step, &
     &    MHDMs%SPH_model, MHDMs%SPH_WK, MHDMs%SPH_MHD, MPSFs%FEM_DAT)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(MHDMs%MHD_files, MHDMs%MHD_step,      &
     &    MPSFs%FEM_DAT, MHDMs%MHD_IO, MHDMs%m_SR)
      call init_FEM_to_PSF_bridge(elps_SECT1, MHDMs%MHD_step%viz_step,  &
     &    MPSFs%FEM_DAT%geofem, MPSFs%edge_comm, MHDMs%m_SR)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD                                           &
     &   (MHDMs%MHD_files, MHDMs%SPH_model, MPSFs%FEM_DAT,              &
     &    MHDMs%MHD_step, MHDMs%MHD_IO%rst_IO, MHDMs%SPH_MHD,           &
     &    MHDMs%SPH_WK, MHDMs%m_SR)
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize_surface'
      call init_visualize_surface(elps_SECT1, MHDMs%MHD_step%viz_step,  &
     &    MPSFs%FEM_DAT%geofem, MPSFs%edge_comm, MPSFs%FEM_DAT%field,   &
     &    add_SMHD_ctl1%surfacing_ctls, MPSFs%PSFs, MHDMs%m_SR)
!
      call init_zonal_mean_sections                                     &
     &   (elps_SECT1, MHDMs%MHD_step%viz_step,                          &
     &    MPSFs%FEM_DAT%geofem, MPSFs%edge_comm, MPSFs%FEM_DAT%field,   &
     &    add_SMHD_ctl1%zm_sects, MPSFs%zsectios, MHDMs%m_SR)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd_w_psf
!
      use t_time_data
      use t_SPH_MHD_zmean_sections
      use t_viz_sections
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
!*
!*  -------  time evelution loop start -----------
!*
      iflag_finish = 0
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
     &     MHDMs%SPH_model, MHDMs%MHD_step, MHDMs%MHD_IO%rst_IO,        &
     &     MHDMs%SPH_MHD, MHDMs%SPH_WK, MHDMs%m_SR)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(MHDMs%MHD_step%time_d%i_time_step,      &
     &                          MHDMs%MHD_step)) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD                                    &
     &       (MHDMs%SPH_MHD%sph, MHDMs%SPH_WK%trns_WK,                  &
     &        MPSFs%FEM_DAT%geofem, MPSFs%FEM_DAT%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHDMs%MHD_files, MPSFs%FEM_DAT,        &
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
          call visualize_surface(elps_SECT1,                            &
     &        MHDMs%MHD_step%viz_step, MHDMs%MHD_step%time_d,           &
     &        MPSFs%FEM_DAT%geofem, MPSFs%edge_comm,                    &
     &        MPSFs%FEM_DAT%field, MPSFs%PSFs, MHDMs%m_SR)
!*
!*  ----------- Zonal means --------------
!*
          if(MHDMs%MHD_step%viz_step%istep_psf .ge. 0) then
            call SPH_MHD_zmean_sections(elps_SECT1,                     &
     &          MHDMs%MHD_step%viz_step, MHDMs%MHD_step%time_d,         &
     &          MHDMs%SPH_MHD%sph, MPSFs%FEM_DAT%geofem,                &
     &          MHDMs%SPH_WK%trns_WK, MPSFs%FEM_DAT%field,              &
     &          MPSFs%zsectios, MHDMs%m_SR)
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
      call FEM_finalize(MHDMs%MHD_files, MHDMs%MHD_step, MHDMs%MHD_IO)
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
      end subroutine evolution_sph_mhd_w_psf
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD_w_psf
