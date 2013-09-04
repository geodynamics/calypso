!output_parallel_ucd_file.f90
!      module output_parallel_ucd_file
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!      subroutine output_grd_file
!      subroutine output_udt_one_snapshot(istep_udt)
!      subroutine finalize_ucd_file_output
!
      module output_parallel_ucd_file
!
      use m_precision
      use m_parallel_var_dof
      use m_field_file_format
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine output_grd_file
!
      use m_ucd_data
      use set_ucd_data
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
!
!
      call link_num_field_2_output
      call link_local_mesh_4_ucd
      call link_field_data_2_output
!
      if (itype_ucd_data_file/100 .eq. iflag_single/100) then
        call init_merged_ucd
      end if
!
      call sel_write_parallel_ucd_mesh
!
      if(   mod(itype_ucd_data_file,100)/10 .eq. iflag_udt/10           &
     & .or. mod(itype_ucd_data_file,100)/10 .eq. iflag_vtd/10) then
        call deallocate_ucd_ele
      end if
!
      if(mod(itype_ucd_data_file,100)/10 .eq. iflag_vtd/10) then
        call deallocate_ucd_node
      end if
!
      end subroutine output_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine output_udt_one_snapshot(istep_udt)
!
      use m_ucd_data
      use set_ucd_data
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep_udt
!
!
      call link_num_field_2_output
      call link_local_mesh_4_ucd
      call link_field_data_2_output
!
      if (itype_ucd_data_file/100 .eq. iflag_single/100) then
        call init_merged_ucd
      end if
!
      call sel_write_parallel_ucd_file(istep_udt)
!
      call deallocate_ucd_node
!
      call deallocate_ucd_ele
      call disconnect_ucd_data
!
      if (itype_ucd_data_file/100 .eq. iflag_single/100) then
        call finalize_merged_ucd
      end if
!
      end subroutine output_udt_one_snapshot
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine finalize_ucd_file_output
!
      use merged_udt_vtk_file_IO
!
!
      if (itype_ucd_data_file/100 .eq. iflag_single/100) then
        call finalize_merged_ucd
      end if
!
      end subroutine finalize_ucd_file_output
!
!-----------------------------------------------------------------------
!
      end module output_parallel_ucd_file
