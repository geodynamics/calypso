!>@file   set_control_sph_mhd.f90
!!@brief  module set_control_sph_mhd
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_control_4_sph_mhd
!!@endverbatim
!
      module set_control_sph_mhd
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_4_sph_mhd
!
      use m_control_params_2nd_files
!
      use set_control_platform_data
      use set_ctl_parallel_platform
      use set_control_4_model
      use set_control_sph_data_MHD
      use set_control_4_force
      use set_control_4_normalize
      use set_control_4_time_steps
!
      use set_control_4_velo
      use set_control_4_press
      use set_control_4_temp
      use set_control_4_magne
      use set_control_4_composition
      use set_control_4_pickup_sph
      use output_parallel_ucd_file
      use check_read_bc_file
!
      use check_dependency_for_MHD
!
!   set parameters for data files
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call check_control_num_domains
      call set_control_smp_def(my_rank)
      call set_control_mesh_def
      call set_control_sph_mesh
      call set_control_restart_file_def
      call set_control_parallel_field_def
      call set_control_org_fld_file_def
!
      call s_set_control_4_model
!
!   set forces
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_force'
      call s_set_control_4_force
!
!   set parameters for general information
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data_MHD'
      call s_set_control_sph_data_MHD
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_normalize'
      call s_set_control_4_normalize
!
!   set boundary conditions for temperature
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_temp'
      call s_set_control_4_temp
!
!   set boundary conditions for velocity
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_velo'
      call s_set_control_4_velo
!
!  set boundary conditions for pressure
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_press'
      call s_set_control_4_press
!
!   set boundary conditions for composition variation
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_composition'
      call s_set_control_4_composition
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_magne'
      call s_set_control_4_magne
!
!   set flag to read boundary condition file
!
      if (iflag_debug.gt.0) write(*,*) 'check_read_boundary_files'
      call check_read_boundary_files
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_time_steps'
      call s_set_control_4_time_steps
      call s_set_control_4_crank
!
!  check dependencies
!
      call check_SPH_MHD_dependencies
!
!   set_pickup modes
!
      call set_ctl_params_pick_sph
      call set_ctl_params_pick_gauss
      call set_ctl_params_no_heat_Nu
!
      end subroutine set_control_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      end module set_control_sph_mhd
