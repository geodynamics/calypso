!>@file   analyzer_sph_MHD_w_psf.f90
!!@brief  module analyzer_sph_MHD_w_psf
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mhd_w_psf
!!      subroutine evolution_sph_mhd_w_psf
!!@endverbatim
!
      module analyzer_sph_MHD_w_psf
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_MHD
      use sections_for_1st
      use init_sph_MHD_elapsed_label
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd_w_psf
!
      use set_control_sph_mhd
      use set_control_SPH_to_FEM
      use m_ctl_data_sph_MHD_psf
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
!
!   Load parameter file
!
      call start_eleps_time(1)
      call start_eleps_time(4)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_w_psf'
      call read_control_4_sph_MHD_w_psf
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_sph_mhd'
      call set_control_4_sph_mhd
      call set_control_4_SPH_to_FEM
!
!    IO elapsed end
!    precondition elaps start
!
      call end_eleps_time(4)
      call start_eleps_time(2)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize'
      call FEM_initialize
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize_surface'
      call init_visualize_surface
!
      call calypso_MPI_barrier
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd_w_psf
!
      integer(kind = kint) :: visval, iflag_finish
      integer(kind = kint) :: istep_psf, istep_iso
      integer(kind = kint) :: istep_pvr, istep_fline
!
!     ---------------------
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      time =       time_init
      i_step_MHD = i_step_init
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        time = time + dt
        i_step_MHD = i_step_MHD + 1
        istep_max_dt = i_step_MHD
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(i_step_MHD, iflag_finish)
!*
!*  -----------  output field data --------------
!*
        call start_eleps_time(4)
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
        call SPH_to_FEM_bridge_MHD
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze'
        call FEM_analyze(i_step_MHD, istep_psf, istep_iso,              &
     &      istep_pvr, istep_fline, visval)
!
        call end_eleps_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface', my_rank
          call start_eleps_time(12)
          call visualize_surface(istep_psf, istep_iso)
          call end_eleps_time(12)
        end if
!
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data
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
