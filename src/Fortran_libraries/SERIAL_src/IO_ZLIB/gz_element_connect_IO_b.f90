!> @file  gz_element_connect_IO_b.f90
!!      module gz_element_connect_IO_b
!!
!! @author  H. Matsui
!! @date Written in Oct., 2006
!
!> @brief Element data IO using zlib
!!
!!@verbatim
!!      subroutine gz_write_element_info_b(ele_IO)
!!      subroutine gz_write_surface_4_element_b(sfed_IO)
!!      subroutine gz_write_edge_4_element_b(sfed_IO)
!!
!!      subroutine gz_read_number_of_element_b(ele_IO)
!!      subroutine gz_read_element_info_b(ele_IO)
!!        type(element_data), intent(inout) :: ele_IO
!!      subroutine gz_read_surface_4_element_b(sfed_IO)
!!      subroutine gz_read_edge_4_element_b(sfed_IO)
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module gz_element_connect_IO_b
!
      use m_precision
!
      use t_geometry_data
      use t_surf_edge_IO
      use skip_gz_comment
!
      implicit none
!
      integer (kind = kint), allocatable, private :: ie_tmp(:)
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_element_info_b(ele_IO)
!
      use gz_binary_IO
!
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i
!
!
      call gz_write_one_integer_b(ele_IO%numele)
!
      call gz_write_mul_integer_b(ele_IO%numele, ele_IO%elmtyp)
      call gz_write_mul_int8_b(ele_IO%numele, ele_IO%iele_global)
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        ie_tmp(1:ele_IO%nodelm(i)) = ele_IO%ie(i,1:ele_IO%nodelm(i))
        call gz_write_mul_integer_b(ele_IO%nodelm(i), ie_tmp)
      end do
      deallocate(ie_tmp)
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine gz_write_element_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_surface_4_element_b(sfed_IO)
!
      use gz_binary_IO
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i
!
      call gz_write_one_integer_b(sfed_IO%nsf_4_ele)
      call gz_write_one_integer_b(sfed_IO%nsurf_in_ele)
!
      allocate(ie_tmp(sfed_IO%nsurf_in_ele))
      do i = 1, sfed_IO%nsf_4_ele
        ie_tmp(1:sfed_IO%nsurf_in_ele)                                  &
     &          = sfed_IO%isf_for_ele(i,1:sfed_IO%nsurf_in_ele)
        call gz_write_mul_integer_b(sfed_IO%nsurf_in_ele, ie_tmp)
      end do
      deallocate(ie_tmp)
!
      call dealloc_surface_connect_IO(sfed_IO)
!
      end subroutine gz_write_surface_4_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_edge_4_element_b(sfed_IO)
!
      use gz_binary_IO
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i
!
      call gz_write_one_integer_b(sfed_IO%ned_4_ele)
      call gz_write_one_integer_b(sfed_IO%nedge_in_ele)
!
      allocate(ie_tmp(sfed_IO%nedge_in_ele))
      do i = 1, sfed_IO%ned_4_ele
        ie_tmp(1:sfed_IO%nedge_in_ele)                                  &
     &          = sfed_IO%iedge_for_ele(i,1:sfed_IO%nedge_in_ele)
        call gz_write_mul_integer_b(sfed_IO%nedge_in_ele, ie_tmp)
      end do
      deallocate(ie_tmp)
!
      call dealloc_edge_connect_IO(sfed_IO)
!
      end subroutine gz_write_edge_4_element_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_number_of_element_b(ele_IO)
!
      use gz_binary_IO
!
      type(element_data), intent(inout) :: ele_IO
!
!
      call gz_read_one_integer_b(ele_IO%numele)
!
      end subroutine gz_read_number_of_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_element_info_b(ele_IO)
!
      use gz_binary_IO
      use set_nnod_4_ele_by_type
!
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i
!
!
      call alloc_element_types(ele_IO)
      call gz_read_mul_integer_b(ele_IO%numele, ele_IO%elmtyp)
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
      call gz_read_mul_int8_b(ele_IO%numele, ele_IO%iele_global)
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        call gz_read_mul_integer_b(ele_IO%nodelm(i), ie_tmp)
        ele_IO%ie(i,1:ele_IO%nodelm(i)) = ie_tmp(1:ele_IO%nodelm(i))
      end do
      deallocate(ie_tmp)
!
      end subroutine gz_read_element_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_surface_4_element_b(sfed_IO)
!
      use gz_binary_IO
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i, nsf_4_ele, nsurf_in_ele
!
!
      call gz_read_one_integer_b(nsf_4_ele)
      call gz_read_one_integer_b(nsurf_in_ele)
      call alloc_surface_connect_IO(nsf_4_ele, nsurf_in_ele, sfed_IO)
!
      allocate(ie_tmp(sfed_IO%nsurf_in_ele))
      do i = 1, sfed_IO%nsf_4_ele
        call gz_read_mul_integer_b(sfed_IO%nsurf_in_ele, ie_tmp)
        sfed_IO%isf_for_ele(i,1:sfed_IO%nsurf_in_ele)                   &
     &        = ie_tmp(1:sfed_IO%nsurf_in_ele)
      end do
      deallocate(ie_tmp)
!
      end subroutine gz_read_surface_4_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_edge_4_element_b(sfed_IO)
!
      use gz_binary_IO
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i, ned_4_ele, nedge_in_ele
!
!
      call gz_read_one_integer_b(ned_4_ele)
      call gz_read_one_integer_b(nedge_in_ele)
      call alloc_edge_connect_IO(ned_4_ele, nedge_in_ele, sfed_IO)
!
      allocate(ie_tmp(sfed_IO%nedge_in_ele))
      do i = 1, sfed_IO%ned_4_ele
        call gz_read_mul_integer_b(sfed_IO%nedge_in_ele, ie_tmp)
        sfed_IO%iedge_for_ele(i,1:sfed_IO%nedge_in_ele)                 &
     &        = ie_tmp(1:sfed_IO%nedge_in_ele)
      end do
      deallocate(ie_tmp)
!
      end subroutine gz_read_edge_4_element_b
!
!------------------------------------------------------------------
!
      end module gz_element_connect_IO_b
