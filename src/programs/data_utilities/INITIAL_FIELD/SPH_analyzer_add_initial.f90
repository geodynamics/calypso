!!@brief  module SPH_analyzer_add_initial
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Jan.., 2014
!
!>@brief  Main spectrum method loop to generate initial field
!!@n      Initial field definision is in  const_sph_initial_spectr.f90
!!
!!@verbatim
!!      subroutine initialize_add_sph_initial
!!@endverbatim
!
      module SPH_analyzer_add_initial
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_work_time
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
!
      implicit none
!
      private :: SPH_add_initial_field
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_add_sph_initial
!
      use set_control_sph_mhd
      use m_ctl_data_sph_MHD_noviz
      use init_sph_MHD_elapsed_label
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_noviz'
      call read_control_4_sph_MHD_noviz
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
      call SPH_add_initial_field
!
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_add_sph_initial
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_add_initial_field
!
      use m_geometry_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_physical_property
!
      use set_control_sph_mhd
      use parallel_load_data_4_sph
      use const_sph_initial_spectr
      use set_reference_sph_mhd
      use set_bc_sph_mhd
      use material_property
      use sph_transforms_4_MHD
      use set_radius_func
      use const_radial_mat_4_sph
      use set_initial_sph_dynamo
      use sph_mhd_rst_IO_control
!
!
!
!   Load spherical harmonics data
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call start_eleps_time(4)
      call load_para_sph_mesh
      call end_eleps_time(4)
!
!   Allocate spectr field data
!
      call allocate_phys_rj_data
      call set_sph_sprctr_data_address
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'set_radius_rot_reft_dat_4_sph'
      call set_radius_rot_reft_dat_4_sph(depth_high_t, depth_low_t,     &
     &    high_temp, low_temp, angular)
!
      if(iflag_debug.gt.0) write(*,*) 's_set_bc_sph_mhd'
      call s_set_bc_sph_mhd
!
! ---------------------------------
!
     if(iflag_debug.gt.0) write(*,*)' read_alloc_sph_restart_data'
     call read_alloc_sph_restart_data
     istep_max_dt = i_step_init
!
     if(iflag_debug.gt.0) write(*,*)' sph_initial_spectrum'
     call sph_initial_spectrum
!
      end subroutine SPH_add_initial_field
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_add_initial
