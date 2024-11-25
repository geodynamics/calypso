!>@file   input_control_four_vizs.f90
!!@brief  module input_control_four_vizs
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!
!>@brief Control data for visualization without repartitioning
!!
!!@verbatim
!!      subroutine s_input_control_four_vizs                            &
!!     &         (ctl_file_name, viz4_ctl, FEM_viz, t_viz_param)
!!        character(len = kchara), intent(in) :: ctl_file_name
!!        type(control_data_four_vizs), intent(inout) :: viz4_ctl
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!
!!      subroutine bcast_four_vizs_control_data(viz4_ctl)
!!        type(control_data_four_vizs), intent(inout) :: viz4_ctl
!!      subroutine set_ctl_params_four_vizs                             &
!!     &         (pvr_vizs_c, FEM_viz, t_viz_param, ierr)
!!        type(control_data_four_vizs), intent(in) :: pvr_vizs_c
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!@endverbatim
!
      module input_control_four_vizs
!
      use m_precision
      use m_machine_parameter
      use t_control_data_four_vizs
      use t_FEM_mesh_field_4_viz
      use t_VIZ_only_step_parameter
!
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_four_vizs_control_data, set_ctl_params_four_vizs
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_input_control_four_vizs                              &
     &         (ctl_file_name, viz4_ctl, FEM_viz, t_viz_param)
!
      use t_read_control_elements
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(control_data_four_vizs), intent(inout) :: viz4_ctl
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
!
      integer(kind = kint) :: ierr
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        call read_control_file_four_vizs(ctl_file_name,                 &
     &                                   viz4_ctl, c_buf1)
      end if
      call bcast_four_vizs_control_data(viz4_ctl)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(viz4_ctl%i_viz_only_file,                &
     &                             'control file is broken')
      end if
!
!       set control data
      call set_ctl_params_four_vizs(viz4_ctl, FEM_viz,                  &
     &                              t_viz_param, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
      end subroutine s_input_control_four_vizs
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_four_vizs_control_data(viz4_ctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_ctl_data_viz4
      use bcast_control_arrays
!
      type(control_data_four_vizs), intent(inout) :: viz4_ctl
!
!
      call bcast_ctl_array_c3(viz4_ctl%viz_field_ctl)
      call bcast_ctl_data_4_platform(viz4_ctl%viz_plt)
      call bcast_ctl_data_4_time_step(viz4_ctl%t_viz_ctl)
!
      call bcast_viz4_controls(viz4_ctl%viz4_ctl)
!
      call calypso_mpi_bcast_one_int(viz4_ctl%i_viz_only_file, 0)
!
      end subroutine bcast_four_vizs_control_data
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_four_vizs                               &
     &         (pvr_vizs_c, FEM_viz, t_viz_param, ierr)
!
      use t_control_data_four_vizs
      use t_VIZ_only_step_parameter
!
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_item
      use set_control_platform_data
      use parallel_ucd_IO_select
!
      type(control_data_four_vizs), intent(in) :: pvr_vizs_c
!
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, pvr_vizs_c%viz_plt)
      call set_control_smp_def(my_rank, pvr_vizs_c%viz_plt)
      call set_control_parallel_mesh(pvr_vizs_c%viz_plt,                &
     &                               FEM_viz%mesh_file_IO)
      call set_merged_ucd_file_define(pvr_vizs_c%viz_plt,               &
     &                                FEM_viz%ucd_file_IO)
!
      call init_viz_field_list_control(pvr_vizs_c%viz_field_ctl,        &
     &                                 FEM_viz%viz_fld_list)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (pvr_vizs_c%t_viz_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
!
      end subroutine set_ctl_params_four_vizs
!
! ----------------------------------------------------------------------
!
      end module input_control_four_vizs
