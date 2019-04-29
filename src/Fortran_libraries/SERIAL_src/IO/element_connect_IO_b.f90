!>@file  element_connect_IO_b.f90
!!      module element_connect_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element connectivity
!!
!!@verbatim
!!      subroutine write_element_info_b(ele_IO, bflag)
!!      subroutine write_surface_4_element_b(sfed_IO, bflag)
!!      subroutine write_edge_4_element_b(sfed_IO, bflag)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(element_data), intent(in) :: ele_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine read_number_of_element_b(bflag, ele_IO)
!!      subroutine read_element_info_b(bflag, ele_IO)
!!      subroutine read_surface_4_element_b(bflag, sfed_IO)
!!      subroutine read_edge_4_element_b(bflag, sfed_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(element_data), intent(inout) :: ele_IO
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
      subroutine write_element_info_b(ele_IO, bflag)
!
      use binary_IO
!
      type(element_data), intent(in) :: ele_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: num64
      integer (kind = kint) :: i
      integer (kind = kint), allocatable :: ie_tmp(:)
!
!
      call write_one_integer_b(ele_IO%numele, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = ele_IO%numele
      call write_mul_integer_b(num64, ele_IO%elmtyp, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_mul_int8_b(num64, ele_IO%iele_global, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        num64 = ele_IO%nodelm(i)
        ie_tmp(1:ele_IO%nodelm(i)) = ele_IO%ie(i,1:ele_IO%nodelm(i))
        call write_mul_integer_b(num64, ie_tmp, bflag)
        if(bflag%ierr_IO .ne. 0) return
      end do
      deallocate(ie_tmp)
!
      end subroutine write_element_info_b
!
!------------------------------------------------------------------
!
      subroutine write_surface_4_element_b(sfed_IO, bflag)
!
      use binary_IO
!
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = sfed_IO%nsf_4_ele * sfed_IO%nsurf_in_ele
      call write_one_integer_b(sfed_IO%nsf_4_ele, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_one_integer_b(sfed_IO%nsurf_in_ele, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_mul_integer_b(num64, sfed_IO%isf_for_ele, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_surface_4_element_b
!
!------------------------------------------------------------------
!
      subroutine write_edge_4_element_b(sfed_IO, bflag)
!
      use binary_IO
!
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = sfed_IO%ned_4_ele * sfed_IO%nedge_in_ele
      call write_one_integer_b(sfed_IO%ned_4_ele, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_one_integer_b(sfed_IO%nedge_in_ele, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_mul_integer_b(num64, sfed_IO%iedge_for_ele, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_edge_4_element_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_element_b(bflag, ele_IO)
!
      use binary_IO
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(element_data), intent(inout) :: ele_IO
!
!
      call read_one_integer_b(bflag, ele_IO%numele)
!
      end subroutine read_number_of_element_b
!
!------------------------------------------------------------------
!
      subroutine read_element_info_b(bflag, ele_IO)
!
      use binary_IO
      use set_nnod_4_ele_by_type
      use transfer_to_long_integers
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(element_data), intent(inout) :: ele_IO
!
      integer(kind = kint) :: i
      integer(kind = kint), allocatable :: ie_tmp(:)
!
!
      call alloc_element_types(ele_IO)
      call read_mul_integer_b                                           &
     &   (bflag, cast_long(ele_IO%numele), ele_IO%elmtyp)
      if(bflag%ierr_IO .ne. 0) return
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
      call read_mul_int8_b                                              &
     &   (bflag, cast_long(ele_IO%numele), ele_IO%iele_global)
      if(bflag%ierr_IO .ne. 0) return
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        call read_mul_integer_b                                         &
     &     (bflag, cast_long(ele_IO%nodelm(i)), ie_tmp)
        if(bflag%ierr_IO .ne. 0) return
!
        ele_IO%ie(i,1:ele_IO%nodelm(i)) = ie_tmp(1:ele_IO%nodelm(i))
      end do
      deallocate(ie_tmp)
!
      end subroutine read_element_info_b
!
!------------------------------------------------------------------
!
      subroutine read_surface_4_element_b(bflag, sfed_IO)
!
      use binary_IO
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint) :: nsf_4_ele, nsurf_in_ele
!
      call read_one_integer_b(bflag, nsf_4_ele)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_one_integer_b(bflag, nsurf_in_ele)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_surface_connect_IO                                     &
     &   (nsf_4_ele, nsurf_in_ele, sfed_IO)
!
      num64 = sfed_IO%nsf_4_ele * sfed_IO%nsurf_in_ele
      call read_mul_integer_b(bflag, num64, sfed_IO%isf_for_ele)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_surface_4_element_b
!
!------------------------------------------------------------------
!
      subroutine read_edge_4_element_b(bflag, sfed_IO)
!
      use binary_IO
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint) :: ned_4_ele, nedge_in_ele
!
!
      call read_one_integer_b(bflag, ned_4_ele)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_one_integer_b(bflag, nedge_in_ele)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_edge_connect_IO(ned_4_ele, nedge_in_ele, sfed_IO)
!
      num64 = sfed_IO%ned_4_ele * sfed_IO%nedge_in_ele
      call read_mul_integer_b(bflag, num64, sfed_IO%iedge_for_ele)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_edge_4_element_b
!
!------------------------------------------------------------------
!
      end module element_connect_IO_b
