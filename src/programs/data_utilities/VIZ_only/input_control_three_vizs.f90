!>@file   input_control_three_vizs.f90
!!@brief  module input_control_three_vizs
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!
!>@brief Control data for visualization without repartitioning
!!
!!@verbatim
!!      subroutine s_input_control_three_vizs                           &
!!     &         (file_name, viz3_c, FEM_viz, t_viz_param)
!!        character(len = kchara), intent(in) :: file_name
!!        type(control_data_three_vizs), intent(inout) :: viz3_c
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!
!!      subroutine bcast_three_vizs_control_data(viz3_c)
!!        type(control_data_three_vizs), intent(inout) :: viz3_c
!!      subroutine set_ctl_params_three_vizs                            &
!!     &         (pvr_vizs_c, FEM_viz, t_viz_param, ierr)
!!        type(control_data_three_vizs), intent(in) :: pvr_vizs_c
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!@endverbatim
!
      module input_control_three_vizs
!
      use m_precision
      use m_machine_parameter
      use t_control_data_three_vizs
      use t_FEM_mesh_field_4_viz
      use t_VIZ_only_step_parameter
!
      use calypso_mpi
!
      implicit  none
!
!
      integer(kind = kint), parameter :: viz_ctl_file_code = 11
      character(len = kchara), parameter :: fname_viz_ctl = "ctl_viz"
!
      private :: bcast_three_vizs_control_data
      private :: set_ctl_params_three_vizs
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_input_control_three_vizs                             &
     &         (file_name, viz3_c, FEM_viz, t_viz_param)
!
      use t_read_control_elements
!
      character(len = kchara), intent(in) :: file_name
      type(control_data_three_vizs), intent(inout) :: viz3_c
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
!
      integer(kind = kint) :: ierr
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
!
!       load control file
      if(my_rank .eq. 0) then
        call read_control_file_three_vizs(file_name, viz3_c, c_buf1)
      end if
      call bcast_three_vizs_control_data(viz3_c)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(c_buf1%iend, 'control file is broken')
      end if
!
!       set control data
      call set_ctl_params_three_vizs(viz3_c, FEM_viz,                   &
     &                               t_viz_param, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
      end subroutine s_input_control_three_vizs
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_three_vizs_control_data(viz3_c)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_ctl_data_viz3
      use bcast_control_arrays
!
      type(control_data_three_vizs), intent(inout) :: viz3_c
!
!
      call bcast_ctl_array_c3(viz3_c%viz_field_ctl)
      call bcast_ctl_data_4_platform(viz3_c%viz_plt)
      call bcast_ctl_data_4_time_step(viz3_c%t_viz_ctl)
!
      call bcast_viz3_controls(viz3_c%viz3_ctl)
!
      call calypso_mpi_bcast_one_int(viz3_c%i_viz_only_file, 0)
!
      end subroutine bcast_three_vizs_control_data
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_three_vizs                              &
     &         (pvr_vizs_c, FEM_viz, t_viz_param, ierr)
!
      use t_VIZ_only_step_parameter
!
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_item
      use set_control_platform_data
      use parallel_ucd_IO_select
!
      type(control_data_three_vizs), intent(in) :: pvr_vizs_c
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
      end subroutine set_ctl_params_three_vizs
!
! ----------------------------------------------------------------------
!
      end module input_control_three_vizs
