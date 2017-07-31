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
      use m_physical_property
      use t_step_parameter
!
      use SPH_analyzer_d_bench
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
      subroutine initialize_sph_dynamobench
!
      use t_ctl_data_sph_MHD_psf
      use m_ctl_data_sph_MHD
      use m_spheric_parameter
      use m_node_phys_data
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_trans_arrays_MHD
      use m_bc_data_list
      use m_flexible_time_step
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
!
!   Load parameter file
!
      call start_elapsed_time(1)
      call start_elapsed_time(4)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_noviz'
      call read_control_4_sph_MHD_noviz(snap_ctl_name, DNS_MHD_ctl1)

      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_dynamobench'
      call input_control_SPH_dynamobench(MHD_files1, bc_sph_IO1,        &
     &    DNS_MHD_ctl1, sph1, comms_sph1, sph_grps1, rj_fld1, nod_fld1, &
     &    pwr1, flex_p1, MHD_step1, MHD_prop1, MHD_BC1, trns_WK1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      call end_elapsed_time(4)
!
!    precondition elaps start
!
      call start_elapsed_time(2)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_dbench'
      call SPH_init_sph_dbench(MHD_files1, bc_sph_IO1, iphys)
      call calypso_MPI_barrier
!
      call end_elapsed_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_dynamobench
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_dynamobench
!
      integer(kind = kint) :: iflag
!
!*  -----------  set initial step data --------------
!*
      call start_elapsed_time(3)
      call s_initialize_time_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
!
        iflag = output_IO_flag(MHD_step1%time_d%i_time_step,            &
     &                         MHD_step1%rst_step)
        if(MHD_step1%time_d%i_time_step .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_dbench'
        call SPH_analyze_dbench                                         &
     &     (MHD_step1%time_d%i_time_step, MHD_files1)
!*
!*  -----------  exit loop --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      call end_elapsed_time(3)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_dbench'
!      call SPH_finalize_dbench
!
      call copy_COMM_TIME_to_elaps(num_elapsed)
      call end_elapsed_time(1)
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
