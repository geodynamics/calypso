!
!      module output_ucd_file_control
!
!      Programmed by H.Matsui on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!     subroutine s_output_ucd_file_control
!     subroutine output_grd_file_4_snapshot
!     subroutine output_grd_file_w_org_connect(my_rank)
!
      module output_ucd_file_control
!
      use m_precision
!
      implicit none
!
      private :: link_local_org_mesh_4_ucd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_output_ucd_file_control
!
      use calypso_mpi
      use m_control_parameter
      use m_t_step_parameter
      use m_ucd_data
      use parallel_ucd_IO_select
      use copy_time_steps_4_restart
!      use range_data_IO
!
      integer(kind = kint) :: istep_ucd
!
!
      if(i_step_output_ucd .eq. 0) return
      if(mod(istep_max_dt,i_step_output_ucd) .ne. 0) return
!
      istep_ucd = istep_max_dt / i_step_output_ucd
!
      call copy_time_steps_to_restart
      call sel_write_parallel_ucd_file(istep_ucd, fem_ucd, merged_ucd)
!      call output_range_data(istep_ucd, time)
!
      end subroutine s_output_ucd_file_control
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine output_grd_file_4_snapshot
!
      use m_t_step_parameter
      use output_parallel_ucd_file
!
!
      if(i_step_output_ucd .eq. 0) return
      call output_grd_file
!
      end subroutine output_grd_file_4_snapshot
!
!-----------------------------------------------------------------------
!
      subroutine output_grd_file_w_org_connect
!
      use m_ucd_data
      use m_geometry_parameter
      use m_field_file_format
      use m_t_step_parameter
      use set_ucd_data
!
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
!
!
      if(i_step_output_ucd .eq. 0) return
!
      call link_fem_num_field_2_ucd_out
      call link_local_org_mesh_4_ucd
      call link_fem_field_data_2_ucd_out
!
      if (fem_ucd%ifmt_file/100 .eq. iflag_single/100) then
        call init_merged_ucd(fem_ucd, merged_ucd)
      end if
!
      call sel_write_parallel_ucd_mesh(fem_ucd, merged_ucd)
!
      if(   mod(fem_ucd%ifmt_file,100)/10 .eq. iflag_udt/10             &
     & .or. mod(fem_ucd%ifmt_file,100)/10 .eq. iflag_vtd/10) then
        call deallocate_ucd_ele(fem_ucd)
      end if
!
      if(mod(fem_ucd%ifmt_file,100)/10 .eq. iflag_vtd/10) then
        call deallocate_ucd_node(fem_ucd)
      end if
!
      end subroutine output_grd_file_w_org_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_global_org_mesh_4_ucd
!
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_ucd_data
      use set_and_cal_udt_data
!
!
      call link_fem_node_data_2_ucd_out
      call const_udt_global_connect(internal_node, numele, nnod_4_ele,  &
     &    globalelmid_org, ie_org, fem_ucd)
!
      end subroutine link_global_org_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_local_org_mesh_4_ucd
!
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_ucd_data
      use set_and_cal_udt_data
!
!
      call const_udt_local_nodes(numnod, xx, fem_ucd)
      call const_udt_local_connect(internal_node, numele, nnod_4_ele,   &
     &    ie_org, fem_ucd)
!
      end subroutine link_local_org_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      end module output_ucd_file_control
