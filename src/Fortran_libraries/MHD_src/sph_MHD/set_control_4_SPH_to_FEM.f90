!>@file   set_control_4_SPH_to_FEM.f90
!!@brief  module set_control_4_SPH_to_FEM
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine sph_boundary_IO_control(MHD_prop, MHD_BC, bc_IO)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!@endverbatim
!
!
      module set_control_4_SPH_to_FEM
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_spheric_parameter
      use t_phys_data
      use t_sph_boundary_input_data
      use t_bc_data_list
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sph_boundary_IO_control(MHD_prop, MHD_BC, bc_IO)
!
      use check_read_bc_file
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(boundary_spectra), intent(inout) :: bc_IO
!
      integer(kind = kint) :: iflag
!
!
      iflag = check_read_boundary_files(MHD_prop, MHD_BC)
      if (iflag .eq. id_no_boundary_file) return
!
      if (iflag_debug.eq.1) write(*,*) 'read_boundary_spectr_file'
      call read_boundary_spectr_file(bc_IO)
!
      end subroutine sph_boundary_IO_control
!
! ----------------------------------------------------------------------
!
      end module set_control_4_SPH_to_FEM
