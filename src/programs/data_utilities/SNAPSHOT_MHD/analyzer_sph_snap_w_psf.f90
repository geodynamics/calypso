!>@file   analyzer_sph_snap_w_psf.f90
!!@brief  module analyzer_sph_snap_w_psf
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine initialize_sph_snap_w_psf
!!      subroutine evolution_sph_snap_w_psf
!!@endverbatim
!
      module analyzer_sph_snap_w_psf
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_MHD_step_parameter
      use m_SPH_MHD_model_data
      use m_work_time
      use m_elapsed_labels_SEND_RECV
      use t_step_parameter
      use t_viz_sections
      use t_SPH_MHD_zonal_mean_viz
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap_w_psf
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: snap_ctl_name = 'control_snapshot'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_snap_w_psf
!
      use t_ctl_data_sph_MHD_psf
      use m_ctl_data_sph_MHD
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
      use output_viz_file_control
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_w_psf'
      call read_control_4_sph_MHD_w_psf(snap_ctl_name, DNS_MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_MHD_psf'
      call input_control_SPH_MHD_psf                                    &
     &   (MHD_files1, DNS_MHD_ctl1, MHD_step1, SPH_model1,              &
     &    SPH_WK1%trns_WK, SPH_WK1%monitor, SPH_MHD1, FEM_d1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!     --------------------- 
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD                                       &
     &   (MHD_files1, MHD_step1, FEM_d1%geofem, FEM_d1%ele_mesh,        &
     &    FEM_d1%iphys, FEM_d1%field, MHD_IO1)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap_psf'
      call SPH_init_sph_snap_psf                                        &
     &   (MHD_files1, FEM_d1%iphys, SPH_model1, SPH_MHD1, SPH_WK1)
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize_surface'
      call init_visualize_surface                                       &
     &   (FEM_d1%geofem, FEM_d1%ele_mesh, FEM_d1%field,                 &
     &    DNS_MHD_ctl1%psf_ctls, DNS_MHD_ctl1%iso_ctls, viz_psfs1)
!
      call init_zonal_mean_sections                                     &
     &   (FEM_d1%geofem, FEM_d1%ele_mesh, FEM_d1%field,                 &
     &    DNS_MHD_ctl1%zm_ctls, zmeans1)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_snap_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_snap_w_psf
!
      use output_viz_file_control
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: iflag
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call s_initialize_time_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
!
        iflag = output_IO_flag(MHD_step1%time_d%i_time_step,            &
     &                         MHD_step1%rst_step)
        if(iflag .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap_psf'
        call SPH_analyze_snap_psf(MHD_step1%time_d%i_time_step,         &
     &      MHD_files1, SPH_model1, MHD_step1, SPH_MHD1, SPH_WK1)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        iflag = lead_field_data_flag(MHD_step1%time_d%i_time_step,      &
     &                               MHD_step1)
        if(iflag .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD(SPH_MHD1%sph, SPH_WK1%trns_WK,     &
     &        FEM_d1%geofem%mesh, FEM_d1%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHD_files1,                            &
     &      FEM_d1%geofem, FEM_d1%field, MHD_step1, visval, MHD_IO1)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface'
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call visualize_surface(MHD_step1%viz_step, MHD_step1%time_d,  &
     &        FEM_d1%geofem, FEM_d1%ele_mesh, FEM_d1%field, viz_psfs1)
!*
!*  ----------- Zonal means --------------
!*
          call SPH_MHD_zmean_sections                                   &
     &       (MHD_step1%viz_step, MHD_step1%time_d, SPH_MHD1%sph,       &
     &        FEM_d1%geofem, FEM_d1%ele_mesh, SPH_WK1%trns_WK,          &
     &        FEM_d1%field, zmeans1)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!
!*  -----------  exit loop --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, MHD_IO1)
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
