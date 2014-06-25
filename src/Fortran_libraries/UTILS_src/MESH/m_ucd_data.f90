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
!!      subroutine set_ucd_file_prefix(file_prefix)
!!      subroutine set_ucd_file_format(ifile_format)
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
!
      subroutine set_ucd_file_prefix(file_prefix)
!
      character(len=kchara), intent(in) :: file_prefix
!
      fem_ucd%file_prefix = file_prefix
!
      end subroutine set_ucd_file_prefix
!
! -----------------------------------------------------------------------
!
      subroutine set_ucd_file_format(ifile_format)
!
      integer(kind = kint), intent(in) :: ifile_format
!
      fem_ucd%ifmt_file = ifile_format
!
      end subroutine set_ucd_file_format
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_local_mesh_4_ucd_out
!
      use m_geometry_parameter
      use m_geometry_data
      use set_and_cal_udt_data
!
!
      call const_udt_local_nodes(numnod, xx, fem_ucd)
      call const_udt_local_connect(internal_node, numele, nnod_4_ele,   &
     &    ie, fem_ucd)
!
      end subroutine link_local_mesh_4_ucd_out
!
!-----------------------------------------------------------------------
!
      subroutine link_global_mesh_4_ucd_out
!
      use m_geometry_parameter
      use m_geometry_data
      use set_ucd_data
      use set_and_cal_udt_data
!
!
      call link_node_data_2_output(numnod, globalnodid, xx, fem_ucd)
      call const_udt_global_connect(internal_node, numele, nnod_4_ele,  &
     &    globalelmid, ie, fem_ucd)
!
      end subroutine link_global_mesh_4_ucd_out
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine link_fem_num_field_2_ucd_out
!
      use m_geometry_parameter
      use m_node_phys_data
      use set_ucd_data
!
!
      call link_num_field_2_output(numnod, num_nod_phys_vis, fem_ucd)
!
      end subroutine link_fem_num_field_2_ucd_out
!
!-----------------------------------------------------------------------
!
      subroutine link_fem_node_data_2_ucd_out
!
      use m_geometry_parameter
      use m_geometry_data
      use set_ucd_data
!
!
      call link_node_data_2_output(numnod, globalnodid, xx, fem_ucd)
!
      end subroutine link_fem_node_data_2_ucd_out
!
!-----------------------------------------------------------------------
!
      subroutine link_fem_field_data_2_ucd_out
!
      use m_geometry_parameter
      use m_node_phys_data
      use set_ucd_data
!
!
      call link_field_data_2_output(numnod, num_nod_phys,               &
     &    num_tot_nod_phys, num_nod_phys_vis, num_tot_nod_phys_vis,     &
     &    num_nod_component, phys_nod_name, d_nod, fem_ucd)
!
      end subroutine link_fem_field_data_2_ucd_out
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_output_ucd_file_once(my_rank, istep_ucd,          &
     &          ifile_format, ucd_prefix)
!
      use m_geometry_parameter
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
      call link_field_data_2_output(numnod, num_nod_phys,               &
     &    num_tot_nod_phys, num_nod_phys_vis, num_tot_nod_phys_vis,     &
     &    num_nod_component, phys_nod_name, d_nod, local_ucd)
!
      call set_ucd_file_format(ifile_format)
      call set_ucd_file_prefix(ucd_prefix)
      call sel_write_udt_file(my_rank, istep_ucd, local_ucd)
      call disconnect_ucd_data(local_ucd)
!
      end subroutine link_output_ucd_file_once
!
! -----------------------------------------------------------------------
!
      end module m_ucd_data
