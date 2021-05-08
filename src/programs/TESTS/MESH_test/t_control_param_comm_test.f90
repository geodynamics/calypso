!
!      module t_control_param_comm_test
!
!     Written by H. Matsui on July, 2006
!
!!     subroutine s_input_control_comm_test(comm_tctl, T_files)
!!       type(comm_test_control), intent(inout) :: comm_tctl
!!       type(comm_test_files_param), intent(inout) ::  T_files
!
      module t_control_param_comm_test
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
!
      type comm_test_files_param
!>        Integer flag to output surface data
        integer(kind = kint) :: iflag_output_SURF = 0
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
      end type comm_test_files_param
!
      private :: set_ctl_params_4_comm_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_comm_test(comm_tctl, T_files)
!
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_comm_test
!
      use set_surface_data_4_IO
      use set_edge_data_4_IO
!
      type(comm_test_control), intent(inout) :: comm_tctl
      type(comm_test_files_param), intent(inout) ::  T_files
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_comm_test'
      call read_control_4_comm_test(comm_tctl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_comm_test'
      call set_ctl_params_4_comm_test                                   &
     &  (comm_tctl%plt, comm_tctl%Fmesh_ctl, T_files)
!
      end subroutine s_input_control_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_4_comm_test(plt, Fmesh_ctl, T_files)
!
      use calypso_mpi
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use set_control_platform_item
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: plt
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
      type(comm_test_files_param), intent(inout) :: T_files
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_mesh_def(plt, T_files%mesh_file_IO)
      call set_FEM_surface_output_flag                                  &
     &   (Fmesh_ctl, T_files%iflag_output_SURF)
!
      end subroutine set_ctl_params_4_comm_test
!
!   --------------------------------------------------------------------
!
      end module t_control_param_comm_test
