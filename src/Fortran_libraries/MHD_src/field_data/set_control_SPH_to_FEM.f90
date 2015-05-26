!>@file   set_control_SPH_to_FEM.f90
!!@brief  module set_control_SPH_to_FEM
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2006
!
!>@brief Set control data to transfer
!!       from speherical frid to FEM grid
!!
!!@verbatim
!!     subroutine set_control_4_SPH_to_FEM
!!@endverbatim
!
      module set_control_SPH_to_FEM
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_to_FEM
!
      use m_machine_parameter
      use m_node_phys_data
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_ctl_data_4_sphere_model
      use copy_rj_spec_name_to_node
      use ordering_field_by_viz
      use node_monitor_IO
      use set_controls_4_sph_shell
!
!
      call set_FEM_mesh_mode_4_SPH(iflag_shell_mode)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'copy_rj_spec_name_to_nod_fld'
      call copy_rj_spec_name_to_nod_fld
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     call check_nodal_field_name
!
      call count_field_4_monitor(num_phys_rj, num_phys_comp_rj,         &
     &    iflag_monitor_rj, num_field_monitor, ntot_comp_monitor)
!
      end subroutine set_control_4_SPH_to_FEM
!
! -----------------------------------------------------------------------
!
      end module set_control_SPH_to_FEM
