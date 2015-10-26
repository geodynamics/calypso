!output_parallel_ucd_file.f90
!      module output_parallel_ucd_file
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!      subroutine set_control_parallel_field_def
!
!!      subroutine output_grd_file(node, ele, nod_comm)
!!      subroutine output_udt_one_snapshot                              &
!!     &         (istep_ucd, node, ele, nod_comm)
!!      subroutine finalize_ucd_file_output
!
      module output_parallel_ucd_file
!
      use m_precision
      use calypso_mpi
      use m_field_file_format
      use m_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_control_parallel_field_def
!
      use parallel_ucd_IO_select
!
!
      call set_merged_ucd_file_define(fem_ucd)
!
      end subroutine set_control_parallel_field_def
!
! -----------------------------------------------------------------------
!
      subroutine output_grd_file(node, ele, nod_comm)
!
      use t_geometry_data
      use t_comm_table
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
!
!
      call link_fem_num_field_2_ucd_out
      call link_local_mesh_4_ucd_out
      call link_fem_field_data_2_ucd_out
!
      if (fem_ucd%ifmt_file/icent .eq. iflag_single/icent) then
        call init_merged_ucd                                            &
     &     (node, ele, nod_comm, fem_ucd, merged_ucd)
      end if
!
      call sel_write_parallel_ucd_mesh(fem_ucd, merged_ucd)
!
      if(   mod(fem_ucd%ifmt_file,icent)/iten .eq. iflag_udt/iten       &
     & .or. mod(fem_ucd%ifmt_file,icent)/iten .eq. iflag_vtd/iten) then
        call deallocate_ucd_ele(fem_ucd)
      end if
!
      if(mod(fem_ucd%ifmt_file,icent)/iten .eq. iflag_vtd/iten) then
        call deallocate_ucd_node(fem_ucd)
      end if
!
      end subroutine output_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine output_udt_one_snapshot                                &
     &         (istep_ucd, node, ele, nod_comm)
!
      use t_geometry_data
      use t_comm_table
      use merged_udt_vtk_file_IO
      use copy_time_steps_4_restart
      use parallel_ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep_ucd
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
!
!
      call link_fem_num_field_2_ucd_out
      call link_local_mesh_4_ucd_out
      call link_fem_field_data_2_ucd_out
!
      if (fem_ucd%ifmt_file/icent .eq. iflag_single/icent) then
        call init_merged_ucd                                            &
     &     (node, ele, nod_comm, fem_ucd, merged_ucd)
      end if
!
      call copy_time_steps_to_restart
      call sel_write_parallel_ucd_file(istep_ucd, fem_ucd, merged_ucd)
!
      call deallocate_ucd_node(fem_ucd)
!
      call deallocate_ucd_ele(fem_ucd)
      call disconnect_ucd_data(fem_ucd)
!
      if (fem_ucd%ifmt_file/icent .eq. iflag_single/icent) then
        call finalize_merged_ucd(fem_ucd, merged_ucd)
      end if
!
      end subroutine output_udt_one_snapshot
!
!-----------------------------------------------------------------------
!
      subroutine finalize_ucd_file_output
!
      use merged_udt_vtk_file_IO
!
!
      if (fem_ucd%ifmt_file/icent .eq. iflag_single/icent) then
        call finalize_merged_ucd(fem_ucd, merged_ucd)
      end if
!
      end subroutine finalize_ucd_file_output
!
!-----------------------------------------------------------------------
!
      end module output_parallel_ucd_file
