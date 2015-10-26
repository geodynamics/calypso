!>@file   m_ucd_data.f90
!!@brief  module m_ucd_data
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO
!!
!!@verbatim
!!      subroutine set_control_ucd_file_def
!!      subroutine link_nnod_stacks_2_ucd_out(nprocs)
!!@endverbatim
!
      module m_ucd_data
!
      use m_precision
      use m_field_file_format
      use m_file_format_switch
!
      use t_ucd_data
!
      implicit none
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: fem_ucd
!
!>        Instance for numbers of FEM mesh for merged IO
      type(merged_ucd_data), save :: merged_ucd
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_ucd_file_def
!
      use ucd_IO_select
!
      call set_ucd_file_define(fem_ucd)
!
      end subroutine set_control_ucd_file_def
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_local_mesh_4_ucd_out
!
      use m_geometry_data
      use set_and_cal_udt_data
!
!
      call const_udt_local_nodes(node1%numnod, node1%xx, fem_ucd)
      call const_udt_local_connect(node1%internal_node,                 &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie, fem_ucd)
!
      end subroutine link_local_mesh_4_ucd_out
!
!-----------------------------------------------------------------------
!
      subroutine link_global_mesh_4_ucd_out
!
      use m_geometry_data
      use set_ucd_data
      use set_and_cal_udt_data
!
!
      call link_node_data_2_output                                      &
     &   (node1%numnod, node1%inod_global, node1%xx, fem_ucd)
      call const_udt_global_connect(node1%internal_node,                &
     &    ele1%numele, ele1%nnod_4_ele, ele1%iele_global, ele1%ie,      &
     &    fem_ucd)
!
      end subroutine link_global_mesh_4_ucd_out
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine link_fem_num_field_2_ucd_out
!
      use m_geometry_data
      use m_node_phys_data
      use set_ucd_data
!
!
      call link_num_field_2_output                                      &
     &   (node1%numnod, nod_fld1%num_phys_viz, fem_ucd)
!
      end subroutine link_fem_num_field_2_ucd_out
!
!-----------------------------------------------------------------------
!
      subroutine link_fem_node_data_2_ucd_out
!
      use m_geometry_data
      use set_ucd_data
!
!
      call link_node_data_2_output                                      &
     &   (node1%numnod, node1%inod_global, node1%xx, fem_ucd)
!
      end subroutine link_fem_node_data_2_ucd_out
!
!-----------------------------------------------------------------------
!
      subroutine link_fem_field_data_2_ucd_out
!
      use m_geometry_data
      use m_node_phys_data
      use set_ucd_data
!
!
      call link_field_data_2_output(node1%numnod, nod_fld1%num_phys,    &
     &    nod_fld1%ntot_phys, nod_fld1%num_phys_viz,                    &
     &    nod_fld1%ntot_phys_viz, nod_fld1%num_component,               &
     &    nod_fld1%phys_name, nod_fld1%d_fld, fem_ucd)
!
      end subroutine link_fem_field_data_2_ucd_out
!
!-----------------------------------------------------------------------
!
      subroutine link_nnod_stacks_2_ucd_out(nprocs)
!
      use m_geometry_data
      use set_ucd_data
!
      integer(kind = kint),  intent(in) :: nprocs
!
!
      call link_numnod_stacks_2_output(nprocs, node1%istack_numnod,     &
     &    node1%istack_internod, ele1%istack_numele, merged_ucd)
!
      end subroutine link_nnod_stacks_2_ucd_out
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_output_ucd_file_once(my_rank, istep_ucd,          &
     &          ifile_format, ucd_prefix)
!
      use m_geometry_data
      use m_node_phys_data
!
      use set_ucd_data
      use ucd_IO_select
!
      character(len=kchara), intent(in) :: ucd_prefix
      integer(kind = kint),  intent(in) :: ifile_format
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
      type(ucd_data) :: local_ucd
!
!
      call link_field_data_2_output(node1%numnod, nod_fld1%num_phys,    &
     &    nod_fld1%ntot_phys, nod_fld1%num_phys_viz,                    &
     &    nod_fld1%ntot_phys_viz, nod_fld1%num_component,               &
     &    nod_fld1%phys_name, nod_fld1%d_fld, local_ucd)
!
      call set_ucd_file_format(ifile_format, fem_ucd)
      call set_ucd_file_prefix(ucd_prefix, fem_ucd)
      call sel_write_udt_file(my_rank, istep_ucd, local_ucd)
      call disconnect_ucd_data(local_ucd)
!
      end subroutine link_output_ucd_file_once
!
! -----------------------------------------------------------------------
!
      end module m_ucd_data
