!>@file   set_control_4_SPH_to_FEM.f90
!!@brief  module set_control_4_SPH_to_FEM
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine s_set_control_4_SPH_to_FEM                           &
!!     &         (spctl, sph_params, rj_fld, nod_fld)
!!        type(sphere_data_control), intent(in) :: spctl
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(phys_data), intent(inout) :: nod_fld
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
      subroutine s_set_control_4_SPH_to_FEM                             &
     &         (spctl, sph_params, rj_fld, nod_fld)
!
      use t_ctl_data_MHD
      use t_ctl_data_4_sphere_model
!
      use ordering_field_by_viz
      use node_monitor_IO
      use set_controls_4_sph_shell
!
      type(sphere_data_control), intent(in) :: spctl
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
!
!
      call set_FEM_mesh_mode_4_SPH(spctl, sph_params%iflag_shell_mode)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'copy_rj_spec_name_to_nod_fld'
      call copy_field_name_type(rj_fld, nod_fld)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     call check_nodal_field_name_type(6, nod_fld)
!
      call count_field_4_monitor                                        &
     &   (rj_fld%num_phys, rj_fld%num_component,                        &
     &    rj_fld%iflag_monitor, num_field_monitor, ntot_comp_monitor)
!
      end subroutine s_set_control_4_SPH_to_FEM
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
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
