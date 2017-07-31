!>@file   mesh_data_IO_b.f90
!!@brief  module mesh_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for Binary mesh data IO
!!
!!@verbatim
!!      subroutine write_geometry_data_b(my_rank_IO, mesh_IO)
!!      subroutine write_mesh_groups_b(mesh_group_IO)
!!      subroutine read_geometry_data_b(my_rank_IO, mesh_IO, ierr)
!!      subroutine read_mesh_groups_b(mesh_group_IO)
!!      subroutine read_num_node_ele_b(my_rank_IO, mesh_IO, ierr)
!!      subroutine read_num_node_b(my_rank_IO, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine write_filter_geometry_b(my_rank_IO, comm_IO, nod_IO)
!!      subroutine read_filter_geometry_b                               &
!!     &         (my_rank_IO, comm_IO, nod_IO, ierr)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!@endverbatim
!
      module mesh_data_IO_b
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
!
      implicit  none
!
      private :: read_geometry_info_b, read_number_of_node_b
      private :: write_geometry_info_b, read_number_of_element_b
      private :: write_element_info_b, read_element_info_b
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_geometry_data_b(my_rank_IO, mesh_IO)
!
      use domain_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call write_domain_info_b(my_rank_IO, mesh_IO%nod_comm)
!
      call write_geometry_info_b(mesh_IO%node)
      call write_element_info_b(mesh_IO%ele)
!
      call write_import_data_b(mesh_IO%nod_comm)
      call write_export_data_b(mesh_IO%nod_comm)
!
      end subroutine write_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine write_mesh_groups_b(mesh_group_IO)
!
      use groups_IO_b
!
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   write node group
      call write_grp_data_b(mesh_group_IO%nod_grp)
!  write element group
      call write_grp_data_b(mesh_group_IO%ele_grp)
!  write surface group
      call write_surf_grp_data_b(mesh_group_IO%surf_grp)
!
      end subroutine write_mesh_groups_b
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_b(my_rank_IO, comm_IO, nod_IO)
!
      use domain_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(node_data), intent(inout) :: nod_IO
!
!
      call write_domain_info_b(my_rank_IO, comm_IO)
!
      call write_geometry_info_b(nod_IO)
!
      call write_import_data_b(comm_IO)
      call write_export_data_b(comm_IO)
!
      end subroutine write_filter_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_geometry_data_b(my_rank_IO, mesh_IO, ierr)
!
      use domain_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_domain_info_b(my_rank_IO, mesh_IO%nod_comm, ierr)
      call read_number_of_node_b(mesh_IO%node)
      call read_geometry_info_b(mesh_IO%node)
!
!  ----  read element data -------
!
      call read_number_of_element_b(mesh_IO%ele)
      call read_element_info_b(mesh_IO%ele)
!
! ----  import & export 
!
      call read_import_data_b(mesh_IO%nod_comm)
      call read_export_data_b(mesh_IO%nod_comm)
!
      end subroutine read_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine read_mesh_groups_b(mesh_group_IO)
!
      use groups_IO_b
!
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call read_group_data_b(mesh_group_IO%nod_grp)
!   read element group
      call read_group_data_b(mesh_group_IO%ele_grp)
!   read surface group
      call read_surf_grp_data_b(mesh_group_IO%surf_grp)
!
      end subroutine read_mesh_groups_b
!
!------------------------------------------------------------------
!
      subroutine read_num_node_ele_b(my_rank_IO, mesh_IO, ierr)
!
      use domain_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_domain_info_b(my_rank_IO, mesh_IO%nod_comm, ierr)
      call read_number_of_node_b(mesh_IO%node)
      call read_geometry_info_b(mesh_IO%node)
!
!  ----  read element data -------
!
      call read_number_of_element_b(mesh_IO%ele)
!
      end subroutine read_num_node_ele_b
!
!------------------------------------------------------------------
!
      subroutine read_num_node_b(my_rank_IO, mesh_IO, ierr)
!
      use domain_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_domain_info_b(my_rank_IO, mesh_IO%nod_comm, ierr)
      call read_number_of_node_b(mesh_IO%node)
!
      end subroutine read_num_node_b
!
!------------------------------------------------------------------
!
      subroutine read_filter_geometry_b                                 &
     &         (my_rank_IO, comm_IO, nod_IO, ierr)
!
      use domain_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(communication_table), intent(inout) :: comm_IO
      type(node_data), intent(inout) :: nod_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_domain_info_b(my_rank_IO, comm_IO, ierr)
      call read_number_of_node_b(nod_IO)
      call read_geometry_info_b(nod_IO)
!
! ----  import & export 
!
      call read_import_data_b(comm_IO)
      call read_export_data_b(comm_IO)
!
      end subroutine read_filter_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geometry_info_b(nod_IO)
!
      use binary_IO
!
      type(node_data), intent(inout) :: nod_IO
!
!
      call write_one_integer_b(nod_IO%numnod)
      call write_one_integer_b(nod_IO%internal_node)
!
      call write_mul_int8_b(nod_IO%numnod, nod_IO%inod_global)
      call write_2d_vector_b(nod_IO%numnod, ithree, nod_IO%xx)
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine write_element_info_b(ele_IO)
!
      use binary_IO
!
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i
      integer (kind = kint), allocatable :: ie_tmp(:)
!
!
      call write_one_integer_b(ele_IO%numele)
!
      call write_mul_integer_b(ele_IO%numele, ele_IO%elmtyp)
      call write_mul_int8_b(ele_IO%numele, ele_IO%iele_global)
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        ie_tmp(1:ele_IO%nodelm(i)) = ele_IO%ie(i,1:ele_IO%nodelm(i))
        call write_mul_integer_b(ele_IO%nodelm(i), ie_tmp)
      end do
      deallocate(ie_tmp)
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine write_element_info_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_node_b(nod_IO)
!
      use binary_IO
!
      type(node_data), intent(inout) :: nod_IO
!
!
      call read_one_integer_b(nod_IO%numnod)
      call read_one_integer_b(nod_IO%internal_node)
!
      end subroutine read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine read_geometry_info_b(nod_IO)
!
      use binary_IO
!
      type(node_data), intent(inout) :: nod_IO
!
!
      call alloc_node_geometry_base(nod_IO)
!
      call read_mul_int8_b(nod_IO%numnod, nod_IO%inod_global)
      call read_2d_vector_b(nod_IO%numnod, ithree, nod_IO%xx)
!
      end subroutine read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine read_number_of_element_b(ele_IO)
!
      use binary_IO
!
      type(element_data), intent(inout) :: ele_IO
!
!
      call read_one_integer_b(ele_IO%numele)
!
      end subroutine read_number_of_element_b
!
!------------------------------------------------------------------
!
      subroutine read_element_info_b(ele_IO)
!
      use binary_IO
      use set_nnod_4_ele_by_type
!
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i
      integer (kind = kint), allocatable :: ie_tmp(:)
!
!
      call alloc_element_types(ele_IO)
      call read_mul_integer_b(ele_IO%numele, ele_IO%elmtyp)
!
      ele_IO%nnod_4_ele = 0
      do i = 1, ele_IO%numele
        call s_set_nnod_4_ele_by_type                                   &
     &     (ele_IO%elmtyp(i), ele_IO%nodelm(i))
        ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
      end do
!
      call alloc_ele_connectivity(ele_IO)
!
      call read_mul_int8_b(ele_IO%numele, ele_IO%iele_global)
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        call read_mul_integer_b(ele_IO%nodelm(i), ie_tmp)
        ele_IO%ie(i,1:ele_IO%nodelm(i)) = ie_tmp(1:ele_IO%nodelm(i))
      end do
      deallocate(ie_tmp)
!
      end subroutine read_element_info_b
!
!------------------------------------------------------------------
!
      end module mesh_data_IO_b
