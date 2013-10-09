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
      private :: link_global_org_mesh_4_ucd, link_local_org_mesh_4_ucd
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
!
      if(i_step_output_ucd .eq. 0) return
      if(mod(istep_max_dt,i_step_output_ucd) .ne. 0) return
!
      ucd_step = istep_max_dt / i_step_output_ucd
!
      call copy_time_steps_to_restart
      call sel_write_parallel_ucd_file(ucd_step)
!      call output_range_data
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
      call link_num_field_2_output
      call link_local_org_mesh_4_ucd
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
      xx_ucd =>      xx
      inod_gl_ucd => globalnodid
!
      call count_udt_elements(internal_node, numele, nnod_4_ele,        &
     &    ie_org)
      call allocate_ucd_ele
!
      call set_udt_global_connect(internal_node, numele, nnod_4_ele,    &
     &    globalelmid_org, ie_org)
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
      call allocate_ucd_node
      call set_udt_local_nodes(numnod, xx)
!
      call count_udt_elements(internal_node, numele, nnod_4_ele,        &
     &    ie_org)
      call allocate_ucd_ele
!
      call set_udt_local_connect(internal_node, numele, nnod_4_ele,     &
     &    ie_org)
!
      end subroutine link_local_org_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      end module output_ucd_file_control
