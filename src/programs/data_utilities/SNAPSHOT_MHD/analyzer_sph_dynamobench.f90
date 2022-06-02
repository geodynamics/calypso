!>@file   analyzer_sph_dynamobench.f90
!!        module analyzer_sph_dynamobench
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!! @n      modified in 2013
!
!> @brief Initialzation and evolution loop for dynamo benchmark check
!!
!!@verbatim
!!      subroutine initialize_sph_dynamobench
!!      subroutine evolution_sph_dynamobench
!!@endverbatim
!
      module analyzer_sph_dynamobench
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_ctl_data_MHD
      use t_field_on_circle
      use t_field_4_dynamobench
      use t_step_parameter
      use t_mesh_SR
!
      use SPH_analyzer_d_bench
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!>      Control struture for MHD simulation
      type(DNS_mhd_simulation_control), save :: DNS_MHD_ctl1
      private :: snap_ctl_name, DNS_MHD_ctl1
!
!>      Structure of spectr grid and data
      type(SPH_mesh_field_data), save, private :: SPH_MHD1
!
!>      Structure of field on mid-depth and equator
      type(circle_fld_maker), save, private :: cdat1
!>      Structure of benchmark result data
      type(dynamobench_monitor), save, private :: bench1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_dynamobench
!
      use t_ctl_data_sph_MHD_psf
      use init_sph_MHD_elapsed_label
      use input_control_dynamobench
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
      call read_control_4_sph_MHD_noviz(snap_ctl_name, DNS_MHD_ctl1)

      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_dynamobench'
      call input_control_SPH_dynamobench                                &
     &   (MHD_files1, SPH_model1%bc_IO, DNS_MHD_ctl1, SPH_MHD1,         &
     &    FEM_d1%field, MHD_step1, SPH_model1%MHD_prop,                 &
     &    SPH_model1%MHD_BC, SPH_WK1, cdat1, bench1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!        Initialize spherical transform dynamo
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_dbench'
      call SPH_init_sph_dbench                                          &
     &   (MHD_files1, FEM_d1%iphys, SPH_model1, MHD_step1,              &
     &    SPH_MHD1, SPH_WK1, m_SR1%SR_sig, m_SR1%SR_r, cdat1)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_dynamobench
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_dynamobench
!
      use output_viz_file_control
      use t_sph_trans_arrays_MHD
      use set_time_step_params
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call set_from_initial_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
        if(output_IO_flag(MHD_step1%time_d%i_time_step,                 &
     &                    MHD_step1%rst_step) .eqv. .FALSE.) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,           &
     &                          MHD_step1)) then
          call alloc_sph_trans_area_snap                                &
     &       (SPH_MHD1%sph%sph_rtp, SPH_WK1%trns_WK)
!
          if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_dbench'
          call SPH_analyze_dbench(MHD_step1%time_d%i_time_step,         &
     &        MHD_files1, SPH_model1, SPH_MHD1, SPH_WK1,                &
     &        m_SR1%SR_sig, m_SR1%SR_r, cdat1, bench1)
!
           call dealloc_sph_trans_area_snap(SPH_WK1%trns_WK)
        end if
!*
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
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_dbench'
!      call SPH_finalize_dbench
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_dynamobench
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_dynamobench
