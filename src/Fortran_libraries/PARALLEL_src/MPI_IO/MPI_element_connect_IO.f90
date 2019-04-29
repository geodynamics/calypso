!>@file  MPI_element_connect_IO.f90
!!      module MPI_element_connect_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element connectivity
!!
!!@verbatim
!!      subroutine mpi_write_element_info(IO_param, ele_IO)
!!      subroutine mpi_write_surface_4_element(IO_param, sfed_IO)
!!      subroutine mpi_write_edge_4_element(IO_param, sfed_IO)
!!        type(element_data), intent(in) :: ele_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine mpi_read_num_element(IO_param, ele_IO)
!!      subroutine mpi_read_element_info(IO_param, ele_IO)
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine mpi_read_surface_4_element(IO_param, sfed_IO)
!!      subroutine mpi_read_edge_4_element(IO_param, sfed_IO)
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module MPI_element_connect_IO
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_surf_edge_IO
      use t_read_mesh_data
      use t_calypso_mpi_IO_param
!
      use MPI_ascii_data_IO
      use MPI_integer_list_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_write_element_info(IO_param, ele_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(in) :: ele_IO
!
!
      call mpi_write_num_of_data(IO_param, ele_IO%numele)
      call mpi_write_element_type                                       &
     &   (IO_param, iten, ele_IO%numele, ele_IO%elmtyp)
!
      call mpi_write_num_of_data(IO_param, ele_IO%numele)
      call mpi_write_ele_connect                                        &
     &   (IO_param, ele_IO%numele, ele_IO%nnod_4_ele,                   &
     &    ele_IO%iele_global, ele_IO%ie)
!
      end subroutine mpi_write_element_info
!
!------------------------------------------------------------------
!
      subroutine mpi_write_surface_4_element(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call mpi_write_num_of_data(IO_param, sfed_IO%nsurf_in_ele)
      call mpi_write_num_of_data(IO_param, sfed_IO%nsf_4_ele)
!
      call mpi_write_int_list(IO_param, sfed_IO%nsf_4_ele,              &
     &    sfed_IO%nsurf_in_ele, sfed_IO%isf_for_ele)
!
      end subroutine mpi_write_surface_4_element
!
!------------------------------------------------------------------
!
      subroutine mpi_write_edge_4_element(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call mpi_write_num_of_data(IO_param, sfed_IO%nedge_in_ele)
      call mpi_write_num_of_data(IO_param, sfed_IO%ned_4_ele)
!
      call mpi_write_int_list(IO_param, sfed_IO%ned_4_ele,              &
     &    sfed_IO%nedge_in_ele, sfed_IO%iedge_for_ele)
!
      end subroutine mpi_write_edge_4_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_num_element(IO_param, ele_IO)
!
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
      call mpi_read_num_of_data(IO_param, ele_IO%numele)
!
      end subroutine mpi_read_num_element
!
!------------------------------------------------------------------
!
      subroutine mpi_read_element_info(IO_param, ele_IO)
!
      use set_nnod_4_ele_by_type
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i, num_tmp
!
!
      call alloc_element_types(ele_IO)
!
      call mpi_read_element_type                                        &
     &   (IO_param, iten, ele_IO%numele, ele_IO%elmtyp)
      call calypso_mpi_barrier
!
      ele_IO%nnod_4_ele = 0
      do i = 1, ele_IO%numele
        call s_set_nnod_4_ele_by_type                                   &
     &     (ele_IO%elmtyp(i), ele_IO%nodelm(i))
        ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
      end do
!
      call mpi_read_num_of_data(IO_param, num_tmp)
      call alloc_ele_connectivity(ele_IO)
!
      call mpi_read_ele_connect                                         &
     &   (IO_param, ele_IO%numele, ele_IO%nnod_4_ele,                   &
     &    ele_IO%iele_global, ele_IO%ie)
      call calypso_mpi_barrier
!
      end subroutine mpi_read_element_info
!
!------------------------------------------------------------------
!
      subroutine mpi_read_surface_4_element(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: nsf_4_ele, nsurf_in_ele
!
      call mpi_read_num_of_data(IO_param, nsurf_in_ele)
      call mpi_read_num_of_data(IO_param, nsf_4_ele)
      call alloc_surface_connect_IO                                     &
     &   (nsf_4_ele, nsurf_in_ele, sfed_IO)
!
      call mpi_read_int_list(IO_param, sfed_IO%nsf_4_ele,               &
     &    sfed_IO%nsurf_in_ele, sfed_IO%isf_for_ele)
!
      end subroutine mpi_read_surface_4_element
!
!------------------------------------------------------------------
!
      subroutine mpi_read_edge_4_element(IO_param, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: ned_4_ele, nedge_in_ele
!
      call mpi_read_num_of_data(IO_param, nedge_in_ele)
      call mpi_read_num_of_data(IO_param, ned_4_ele)
      call alloc_edge_connect_IO(ned_4_ele, nedge_in_ele, sfed_IO)
!
      call mpi_read_int_list(IO_param, sfed_IO%ned_4_ele,               &
     &    sfed_IO%nedge_in_ele, sfed_IO%iedge_for_ele)
!
      end subroutine mpi_read_edge_4_element
!
!------------------------------------------------------------------
!
      end module MPI_element_connect_IO
