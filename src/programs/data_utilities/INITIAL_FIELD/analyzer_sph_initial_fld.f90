!>@file   analyzer_sph_initial_fld.f90
!!@brief  module analyzer_sph_initial_fld
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate initial field
!!@n       Define initial field at const_sph_initial_spectr.f90
!!
!!@verbatim
!!      subroutine initialize_const_sph_initial
!!@endverbatim
!
!
      module analyzer_sph_initial_fld
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_work_time
      use m_control_parameter
      use m_control_params_sph_MHD
      use m_t_int_parameter
      use m_t_step_parameter
!
      use const_coriolis_sph
!
      use SPH_analyzer_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_const_sph_initial
!
      use set_control_sph_mhd
      use m_ctl_data_noviz_MHD
      use init_sph_MHD_elapsed_label
      use SPH_analyzer_const_initial
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_MHD_noviz'
      call read_control_4_MHD_noviz
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_sph_mhd'
      call set_control_4_sph_mhd
!
!    IO elapsed end 
!    precondition elaps start
!
      call end_eleps_time(4)
      call start_eleps_time(2)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_const_initial
!
      call end_eleps_time(2)
!
      end subroutine initialize_const_sph_initial
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_initial_fld
