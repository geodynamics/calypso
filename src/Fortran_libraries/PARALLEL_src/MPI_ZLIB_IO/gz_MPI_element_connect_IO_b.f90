!>@file  gz_MPI_element_connect_IO_b.f90
!!      module gz_MPI_element_connect_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element connectivity
!!
!!@verbatim
!!      subroutine gz_mpi_write_element_info_b(IO_param, ele_IO)
!!      subroutine gz_mpi_write_surf_4_ele_b(IO_param, sfed_IO)
!!      subroutine gz_mpi_write_edge_4_ele_b(IO_param, sfed_IO)
!!
!!      subroutine gz_mpi_read_num_element_b(IO_param, ele_IO)
!!      subroutine gz_mpi_read_ele_info_b(IO_param, ele_IO)
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine gz_mpi_read_surf_4_elem_b(IO_param, sfed_IO)
!!      subroutine gz_mpi_read_edge_4_element_b(IO_param, sfed_IO)
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module gz_MPI_element_connect_IO_b
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_surf_edge_IO
      use t_read_mesh_data
      use t_calypso_mpi_IO_param
!
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_datum_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_element_info_b(IO_param, ele_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: num
!
!
      call gz_mpi_write_one_integer_b(IO_param, ele_IO%numele)
!
      call gz_mpi_write_int_vector_b                                    &
     &   (IO_param, ele_IO%numele, ele_IO%elmtyp)
      call gz_mpi_write_int8_vector_b                                   &
     &   (IO_param, ele_IO%numele, ele_IO%iele_global)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call gz_mpi_write_int_vector_b(IO_param, num, ele_IO%ie)
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine gz_mpi_write_element_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_surf_4_ele_b(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer (kind = kint) :: num
!
!
      call gz_mpi_write_one_integer_b(IO_param, sfed_IO%nsurf_in_ele)
      call gz_mpi_write_one_integer_b(IO_param, sfed_IO%nsf_4_ele)
!
      num = sfed_IO%nsf_4_ele * sfed_IO%nsurf_in_ele
      call gz_mpi_write_int_vector_b                                    &
     &   (IO_param, num, sfed_IO%isf_for_ele)
!
      call dealloc_surface_connect_IO(sfed_IO)
!
      end subroutine gz_mpi_write_surf_4_ele_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_edge_4_ele_b(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer (kind = kint) :: num
!
!
      call gz_mpi_write_one_integer_b(IO_param, sfed_IO%nedge_in_ele)
      call gz_mpi_write_one_integer_b(IO_param, sfed_IO%ned_4_ele)
!
      num = sfed_IO%ned_4_ele * sfed_IO%nedge_in_ele
      call gz_mpi_write_int_vector_b                                    &
     &   (IO_param, num, sfed_IO%iedge_for_ele)
!
      call dealloc_edge_connect_IO(sfed_IO)
!
      end subroutine gz_mpi_write_edge_4_ele_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_num_element_b(IO_param, ele_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
!
      call gz_mpi_read_one_integer_b(IO_param, ele_IO%numele)
!
      end subroutine gz_mpi_read_num_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_ele_info_b(IO_param, ele_IO)
!
      use gz_MPI_binary_data_IO
      use set_nnod_4_ele_by_type
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: num, i
!
!
      call alloc_element_types(ele_IO)
      call gz_mpi_read_int_vector_b                                     &
     &   (IO_param, ele_IO%numele, ele_IO%elmtyp)
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
      call gz_mpi_read_int8_vector_b                                    &
     &   (IO_param, ele_IO%numele, ele_IO%iele_global)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call gz_mpi_read_int_vector_b(IO_param, num, ele_IO%ie)
!
      end subroutine gz_mpi_read_ele_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_surf_4_elem_b(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: nsf_4_ele, nsurf_in_ele, num
!
      call gz_mpi_read_one_integer_b(IO_param, nsurf_in_ele)
      call gz_mpi_read_one_integer_b(IO_param, nsf_4_ele)
      call alloc_surface_connect_IO                                     &
     &   (nsf_4_ele, nsurf_in_ele, sfed_IO)
!
      num = sfed_IO%nsf_4_ele * sfed_IO%nsurf_in_ele
      call gz_mpi_read_int_vector_b                                     &
     &   (IO_param, num, sfed_IO%isf_for_ele)
!
      end subroutine gz_mpi_read_surf_4_elem_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_edge_4_element_b(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: ned_4_ele, nedge_in_ele, num
!
      call gz_mpi_read_one_integer_b(IO_param, nedge_in_ele)
      call gz_mpi_read_one_integer_b(IO_param, ned_4_ele)
      call alloc_edge_connect_IO(ned_4_ele, nedge_in_ele, sfed_IO)
!
      num = sfed_IO%ned_4_ele * sfed_IO%nedge_in_ele
      call gz_mpi_read_int_vector_b                                     &
     &   (IO_param, num, sfed_IO%iedge_for_ele)
!
      end subroutine gz_mpi_read_edge_4_element_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_element_connect_IO_b
