!>@file   input_control_sph_MHD.f90
!!@brief  module input_control_sph_MHD
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_SPH_mesh
!!      subroutine input_control_4_SPH_MHD_nosnap
!!
!!      subroutine input_control_4_SPH_make_init
!!      subroutine input_control_SPH_dynamobench
!!@endverbatim
!
!
      module input_control_sph_MHD
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
      subroutine input_control_SPH_mesh
!
      use m_control_parameter
      use m_spheric_parameter
      use m_sph_boundary_input_data
      use set_control_sph_mhd
      use set_control_SPH_to_FEM
      use parallel_load_data_4_sph
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_sph_mhd'
      call set_control_4_sph_mhd
      call set_control_4_SPH_to_FEM
!
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh
!
      if (iflag_boundary_file .eq. id_read_boundary_file) then
        if (iflag_debug.eq.1) write(*,*) 'read_boundary_spectr_file'
        call read_boundary_spectr_file
      end if
!
      end subroutine input_control_SPH_mesh
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_MHD_nosnap
!
      use m_control_parameter
      use m_sph_boundary_input_data
      use set_control_sph_mhd
      use parallel_load_data_4_sph
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_sph_mhd'
      call set_control_4_sph_mhd
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh
!
      if (iflag_boundary_file .eq. id_read_boundary_file) then
        if (iflag_debug.eq.1) write(*,*) 'read_boundary_spectr_file'
        call read_boundary_spectr_file
      end if
!
      end subroutine input_control_4_SPH_MHD_nosnap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_make_init
!
      use m_control_parameter
      use m_sph_boundary_input_data
      use set_control_sph_mhd
      use parallel_load_data_4_sph
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_sph_mhd'
      call set_control_4_sph_mhd
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh
!
      end subroutine input_control_4_SPH_make_init
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_dynamobench
!
      use m_control_parameter
      use set_control_sph_mhd
      use set_control_SPH_to_FEM
      use set_control_sph_data_MHD
      use parallel_load_data_4_sph
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_sph_mhd'
      call set_control_4_sph_mhd
      call set_control_4_SPH_to_FEM
      call set_ctl_params_dynamobench
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh
!
      end subroutine input_control_SPH_dynamobench
!
! ----------------------------------------------------------------------
!
      end module input_control_sph_MHD
