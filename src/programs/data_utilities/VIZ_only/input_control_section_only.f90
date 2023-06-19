!>@file   input_control_section_only.f90
!!@brief  module input_control_section_only
!!
!!@author H. Matsui
!!@date Programmed in March. 2023
!
!>@brief  Control data of node monitoring
!!
!!@verbatim
!!      subroutine s_input_control_section_only                         &
!!     &         (ctl_file_name, sec_viz_ctl, FEM_viz, t_viz_param)
!!        character(len = kchara), intent(in) :: ctl_file_name
!!        type(control_data_section_only), intent(inout) :: sec_viz_ctl
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin visualizer
!!    begin data_files_def
!!      ...
!!    end data_files_def
!!
!!    begin time_step_ctl
!!      ...
!!    end time_step_ctl
!!
!!    begin visual_control
!!      ...
!!    end  visual_control
!!  end  visualizer
!!
!!    -------------------------------------------------------------------
!!@endverbatim
!
      module input_control_section_only
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_section_only
      use t_FEM_mesh_field_4_viz
      use t_VIZ_only_step_parameter
!
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_section_control_data
      private :: set_control_params_4_sections
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_input_control_section_only                           &
     &         (ctl_file_name, sec_viz_ctl, FEM_viz, t_viz_param)
!
      use t_read_control_elements
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
!
      integer(kind = kint) :: ierr
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        call read_control_file_section_only(ctl_file_name,              &
     &                                      sec_viz_ctl, c_buf1)
      end if
      call bcast_section_control_data(sec_viz_ctl)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(sec_viz_ctl%i_viz_only_file,             &
     &                             'control file is broken')
      end if
!
!       set control data
      call set_control_params_4_sections(sec_viz_ctl,                   &
     &                                   FEM_viz, t_viz_param, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
      end subroutine s_input_control_section_only
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_section_control_data(sec_viz_ctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_ctl_data_surfacings
      use bcast_control_arrays
!
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
!
!
      call bcast_ctl_array_c3(sec_viz_ctl%viz_field_ctl)
      call bcast_ctl_data_4_platform(sec_viz_ctl%sect_plt)
      call bcast_ctl_data_4_time_step(sec_viz_ctl%t_sect_ctl)
      call bcast_surfacing_controls(sec_viz_ctl%surfacing_ctls)
!
      call calypso_mpi_bcast_one_int(sec_viz_ctl%i_viz_only_file, 0)
!
      end subroutine bcast_section_control_data
!
!   --------------------------------------------------------------------
!
      subroutine set_control_params_4_sections                          &
     &         (sec_viz_ctl, FEM_viz, t_viz_param, ierr)
!
      use t_viz_sections
      use t_control_data_section_only
      use t_VIZ_only_step_parameter
!
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_item
      use set_control_platform_data
      use parallel_ucd_IO_select
!
      type(control_data_section_only), intent(in) :: sec_viz_ctl
!
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, sec_viz_ctl%sect_plt)
      call set_control_smp_def(my_rank, sec_viz_ctl%sect_plt)
      call set_control_parallel_mesh(sec_viz_ctl%sect_plt,              &
     &                               FEM_viz%mesh_file_IO)
      call set_merged_ucd_file_define(sec_viz_ctl%sect_plt,             &
     &                                FEM_viz%ucd_file_IO)
!
      call init_viz_field_list_control(sec_viz_ctl%viz_field_ctl,       &
     &                                 FEM_viz%viz_fld_list)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (sec_viz_ctl%t_sect_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
!
      end subroutine set_control_params_4_sections
!
! ----------------------------------------------------------------------
!
      end module input_control_section_only
