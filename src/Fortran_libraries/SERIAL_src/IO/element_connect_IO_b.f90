!>@file  element_connect_IO_b.f90
!!      module element_connect_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element connectivity
!!
!!@verbatim
!!      subroutine write_element_info_b(ele_IO)
!!      subroutine write_surface_4_element_b(sfed_IO)
!!      subroutine write_edge_4_element_b(sfed_IO)
!!
!!      subroutine read_number_of_element_b(ele_IO)
!!      subroutine read_element_info_b(ele_IO)
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine read_surface_4_element_b(sfed_IO)
!!      subroutine read_edge_4_element_b(sfed_IO)
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!!
      module element_connect_IO_b
!
      use m_precision
      use t_geometry_data
      use t_surf_edge_IO
      use t_read_mesh_data
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
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
!
      subroutine write_surface_4_element_b(sfed_IO)
!
      use binary_IO
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: num
!
!
      num = sfed_IO%nsf_4_ele * sfed_IO%nsurf_in_ele
      call write_one_integer_b(sfed_IO%nsf_4_ele)
      call write_one_integer_b(sfed_IO%nsurf_in_ele)
      call write_mul_integer_b(num, sfed_IO%isf_for_ele)
!
      call dealloc_surface_connect_IO(sfed_IO)
!
      end subroutine write_surface_4_element_b
!
!------------------------------------------------------------------
!
      subroutine write_edge_4_element_b(sfed_IO)
!
      use binary_IO
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: num
!
!
      num = sfed_IO%ned_4_ele * sfed_IO%nedge_in_ele
      call write_one_integer_b(sfed_IO%ned_4_ele)
      call write_one_integer_b(sfed_IO%nedge_in_ele)
      call write_mul_integer_b(num, sfed_IO%iedge_for_ele)
!
      call dealloc_edge_connect_IO(sfed_IO)
!
      end subroutine write_edge_4_element_b
!
!------------------------------------------------------------------
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
      subroutine read_surface_4_element_b(sfed_IO)
!
      use binary_IO
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: num, nsf_4_ele, nsurf_in_ele
!
      call read_one_integer_b(nsf_4_ele)
      call read_one_integer_b(nsurf_in_ele)
      call alloc_surface_connect_IO                                     &
     &   (nsf_4_ele, nsurf_in_ele, sfed_IO)
!
      num = sfed_IO%nsf_4_ele * sfed_IO%nsurf_in_ele
      call read_mul_integer_b(num, sfed_IO%isf_for_ele)
!
      end subroutine read_surface_4_element_b
!
!------------------------------------------------------------------
!
      subroutine read_edge_4_element_b(sfed_IO)
!
      use binary_IO
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: num, ned_4_ele, nedge_in_ele
!
      call read_one_integer_b(ned_4_ele)
      call read_one_integer_b(nedge_in_ele)
      call alloc_edge_connect_IO(ned_4_ele, nedge_in_ele, sfed_IO)
!
      num = sfed_IO%ned_4_ele * sfed_IO%nedge_in_ele
      call read_mul_integer_b(num, sfed_IO%iedge_for_ele)
!
      end subroutine read_edge_4_element_b
!
!------------------------------------------------------------------
!
      end module element_connect_IO_b
