!>@file  MPI_element_connect_IO_b.f90
!!      module MPI_element_connect_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element connectivity
!!
!!@verbatim
!!      subroutine mpi_write_element_info_b(IO_param, ele_IO)
!!      subroutine mpi_write_surface_4_element_b(IO_param, sfed_IO)
!!      subroutine mpi_write_edge_4_element_b(IO_param, sfed_IO)
!!
!!      subroutine mpi_read_element_info_b(IO_param, ele_IO)
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine mpi_read_surface_4_element_b(IO_param, sfed_IO)
!!      subroutine mpi_read_edge_4_element_b(IO_param, sfed_IO)
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module MPI_element_connect_IO_b
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_surf_edge_IO
      use t_read_mesh_data
      use t_calypso_mpi_IO_param
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_write_element_info_b(IO_param, ele_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: num
!
!
      call mpi_write_one_integer_b(IO_param, ele_IO%numele)
!
      call set_istack_4_parallell_data(ele_IO%numele, IO_param)
      call mpi_write_int_vector_b                                       &
     &   (IO_param, ele_IO%numele, ele_IO%elmtyp)
      call mpi_write_int8_vector_b                                      &
     &   (IO_param, ele_IO%numele, ele_IO%iele_global)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call mul_istack_4_parallell_vect(ele_IO%nnod_4_ele, IO_param)
      call mpi_write_int_vector_b(IO_param, num, ele_IO%ie)
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine mpi_write_element_info_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_surface_4_element_b(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: num
!
!
      call mpi_write_one_integer_b(IO_param, sfed_IO%nsurf_in_ele)
      call mpi_write_one_integer_b(IO_param, sfed_IO%nsf_4_ele)
!
      num = sfed_IO%nsf_4_ele * sfed_IO%nsurf_in_ele
      call mul_istack_4_parallell_vect(sfed_IO%nsurf_in_ele, IO_param)
      call mpi_write_int_vector_b(IO_param, num, sfed_IO%isf_for_ele)
!
      call dealloc_surface_connect_IO(sfed_IO)
!
      end subroutine mpi_write_surface_4_element_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_edge_4_element_b(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: num
!
!
      call mpi_write_one_integer_b(IO_param, sfed_IO%nedge_in_ele)
      call mpi_write_one_integer_b(IO_param, sfed_IO%ned_4_ele)
!
      num = sfed_IO%ned_4_ele * sfed_IO%nedge_in_ele
      call mul_istack_4_parallell_vect(sfed_IO%nedge_in_ele, IO_param)
      call mpi_write_int_vector_b(IO_param, num, sfed_IO%iedge_for_ele)
!
      call dealloc_edge_connect_IO(sfed_IO)
!
      end subroutine mpi_write_edge_4_element_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_number_of_element_b(IO_param, ele_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
!
      call mpi_read_one_integer_b(IO_param, ele_IO%numele)
!
      end subroutine mpi_read_number_of_element_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_element_info_b(IO_param, ele_IO)
!
      use set_nnod_4_ele_by_type
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: num, i
!
!
      call alloc_element_types(ele_IO)
!
      call mpi_read_int_vector_b                                        &
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
      call mpi_read_int8_vector_b                                       &
     &   (IO_param, ele_IO%numele, ele_IO%iele_global)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call mpi_read_int_vector_b(IO_param, num, ele_IO%ie)
!
      end subroutine mpi_read_element_info_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_surface_4_element_b(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: nsf_4_ele, nsurf_in_ele
      integer(kind = kint) :: num
!
      call mpi_read_one_integer_b(IO_param, nsurf_in_ele)
      call mpi_read_one_integer_b(IO_param, nsf_4_ele)
      call alloc_surface_connect_IO                                     &
     &   (nsf_4_ele, nsurf_in_ele, sfed_IO)
!
      num = sfed_IO%nsf_4_ele * sfed_IO%nsurf_in_ele
      call mpi_read_int_vector_b(IO_param, num, sfed_IO%isf_for_ele)
!
      end subroutine mpi_read_surface_4_element_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_edge_4_element_b(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: ned_4_ele, nedge_in_ele
      integer(kind = kint) :: num
!
!
      call mpi_read_one_integer_b(IO_param, nedge_in_ele)
      call mpi_read_one_integer_b(IO_param, ned_4_ele)
      call alloc_edge_connect_IO(ned_4_ele, nedge_in_ele, sfed_IO)
!
      num = sfed_IO%ned_4_ele * sfed_IO%nedge_in_ele
      call mpi_read_int_vector_b(IO_param, num, sfed_IO%iedge_for_ele)
!
      end subroutine mpi_read_edge_4_element_b
!
!------------------------------------------------------------------
!
      end module MPI_element_connect_IO_b
