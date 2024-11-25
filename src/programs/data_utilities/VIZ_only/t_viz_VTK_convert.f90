!t_viz_VTK_convert.f90
!      module t_viz_VTK_convert
!
!      Written by H. Matsui on Apr., 2012
!
!>@file   t_viz_VTK_convert.f90
!!@brief  module t_viz_VTK_convert
!!
!!@auther   Hiroaki Matsui
!!@date  Programmed by H.Matsui in Apr., 2012
!
!>@brief Top routine for VTK convert
!!
!!@verbatim
!!      subroutine init_visualize_convert_vtk                           &
!!     &         (geofem, nod_fld, ucd_step, output_vtk_fmt_ctl,        &
!!     &          ucd_file_IO, vtk_file_IO, vtk_out, SR_sig, SR_i)
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(field_IO_params), intent(in) :: ucd_file_IO
!!        type(read_character_item), intent(in) :: output_vtk_fmt_ctl
!!        type(field_IO_params), intent(inout) :: vtk_file_IO
!!        type(ucd_data), intent(inout) :: vtk_out
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!      subroutine visualize_convert_vtk(i_step, istep_ucd, elps_SECT,  &
!!     &                                 time_d, vtk_file_IO, vtk_out)
!!        type(time_data), intent(in) :: time_d
!!        type(elapsed_labels_4_SECTIONS), intent(in) :: elps_SECT
!!        type(field_IO_params), intent(in) :: vtk_file_IO
!!        type(ucd_data), intent(in) :: vtk_out
!!@endverbatim
!
      module t_viz_VTK_convert
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_VIZ_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_solver_SR
      use t_solver_SR_int
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_convert_vtk                             &
     &         (geofem, nod_fld, ucd_step, output_vtk_fmt_ctl,          &
     &          ucd_file_IO, vtk_file_IO, vtk_out, SR_sig, SR_i)
!
      use m_field_file_format
      use t_control_array_character
      use output_parallel_ucd_file
      use mpi_abort_by_missing_zlib
!
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(IO_step_param), intent(in) :: ucd_step
      type(field_IO_params), intent(in) :: ucd_file_IO
      type(read_character_item), intent(in) :: output_vtk_fmt_ctl
!
      type(field_IO_params), intent(inout) :: vtk_file_IO
      type(ucd_data), intent(inout) :: vtk_out
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call copy_file_params_type(ucd_file_IO, vtk_file_IO)
!
      vtk_file_IO%iflag_format                                          &
     &   = choose_para_fld_file_format(output_vtk_fmt_ctl%charavalue,   &
     &                                 output_vtk_fmt_ctl%iflag)
      call mpi_abort_by_no_zlib_in_fld(vtk_file_IO%file_prefix,         &
     &                                 vtk_file_IO%iflag_format)
!
      if(vtk_file_IO%iflag_format .eq. ucd_file_IO%iflag_format) then
        call calypso_mpi_abort                                          &
     &     (201, 'Set different file format from original')
      end if
      if(      vtk_file_IO%iflag_format .ne. iflag_vtk                  &
     &   .and. vtk_file_IO%iflag_format .ne. iflag_sgl_vtk              &
     &   .and. vtk_file_IO%iflag_format .ne. iflag_sgl_hdf5             &
     &   .and. vtk_file_IO%iflag_format .ne. iflag_vtk_gz               &
     &   .and. vtk_file_IO%iflag_format .ne. iflag_sgl_vtk_gz) then
        call calypso_mpi_abort                                          &
     &     (201, 'Set VTK or HDF file for output')
      end if
!
!
      if(ucd_step%increment .eq. 0) return
      call link_output_grd_file                                         &
     &   (geofem%mesh%node, geofem%mesh%ele, geofem%mesh%nod_comm,      &
     &    nod_fld, vtk_file_IO, vtk_out, SR_sig, SR_i)
!
      end subroutine init_visualize_convert_vtk
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_convert_vtk(i_step, istep_ucd, elps_SECT,    &
     &                                 time_d, vtk_file_IO, vtk_out)
!
      use t_elapsed_labels_4_SECTIONS
      use parallel_ucd_IO_select
!
      integer(kind = kint), intent(in) :: i_step, istep_ucd
      type(elapsed_labels_4_SECTIONS), intent(in) :: elps_SECT
      type(time_data), intent(in) :: time_d
      type(field_IO_params), intent(in) :: vtk_file_IO
      type(ucd_data), intent(in) :: vtk_out
!
!
      if(elps_SECT%flag_elapsed_S)                                      &
     &           call start_elapsed_time(elps_SECT%ist_elapsed_S+5)
      call sel_write_parallel_ucd_file                                  &
     &   (istep_ucd, vtk_file_IO, time_d, vtk_out)
      if(elps_SECT%flag_elapsed_S)                                      &
     &           call end_elapsed_time(elps_SECT%ist_elapsed_S+5)
!
      end subroutine visualize_convert_vtk
!
!  ---------------------------------------------------------------------
!
      end module t_viz_VTK_convert
