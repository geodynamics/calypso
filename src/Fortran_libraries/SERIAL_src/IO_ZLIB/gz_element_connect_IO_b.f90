!> @file  gz_element_connect_IO_b.f90
!!      module gz_element_connect_IO_b
!!
!! @author  H. Matsui
!! @date Written in Oct., 2006
!
!> @brief Element data IO using zlib
!!
!!@verbatim
!!      subroutine gz_write_element_info_b(FPz_f, ele_IO, zbuf)
!!      subroutine gz_write_surface_4_element_b(FPz_f, sfed_IO, zbuf)
!!      subroutine gz_write_edge_4_element_b(FPz_f, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(element_data), intent(in) :: ele_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_number_of_element_b(FPz_f, zbuf, ele_IO)
!!      subroutine gz_read_element_info_b(FPz_f, zbuf, ele_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(element_data), intent(inout) :: ele_IO
!!      subroutine gz_read_surface_4_element_b(FPz_f, zbuf, sfed_IO)
!!      subroutine gz_read_edge_4_element_b(FPz_f, zbuf, sfed_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module gz_element_connect_IO_b
!
      use m_precision
!
      use t_geometry_data
      use t_surf_edge_IO
      use t_buffer_4_gzip
      use binary_IO
      use transfer_to_long_integers
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
      subroutine gz_write_element_info_b(FPz_f, ele_IO, zbuf)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(element_data), intent(in) :: ele_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer (kind = kint) :: i
!
!
      call gz_write_one_integer_b(FPz_f, ele_IO%numele, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, cast_long(ele_IO%numele), ele_IO%elmtyp, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_mul_int8_b                                          &
     &   (FPz_f, cast_long(ele_IO%numele), ele_IO%iele_global, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        ie_tmp(1:ele_IO%nodelm(i)) = ele_IO%ie(i,1:ele_IO%nodelm(i))
        call gz_write_mul_integer_b                                     &
     &     (FPz_f, cast_long(ele_IO%nodelm(i)), ie_tmp, zbuf)
        if(zbuf%ierr_zlib .ne. 0) return
      end do
      deallocate(ie_tmp)
!
      end subroutine gz_write_element_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_surface_4_element_b(FPz_f, sfed_IO, zbuf)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
      call gz_write_one_integer_b(FPz_f, sfed_IO%nsf_4_ele, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_one_integer_b(FPz_f, sfed_IO%nsurf_in_ele, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      allocate(ie_tmp(sfed_IO%nsurf_in_ele))
      do i = 1, sfed_IO%nsf_4_ele
        ie_tmp(1:sfed_IO%nsurf_in_ele)                                  &
     &          = sfed_IO%isf_for_ele(i,1:sfed_IO%nsurf_in_ele)
        call gz_write_mul_integer_b                                     &
     &     (FPz_f, cast_long(sfed_IO%nsurf_in_ele), ie_tmp, zbuf)
        if(zbuf%ierr_zlib .ne. 0) return
      end do
      deallocate(ie_tmp)
!
      end subroutine gz_write_surface_4_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_edge_4_element_b(FPz_f, sfed_IO, zbuf)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
!
      call gz_write_one_integer_b(FPz_f, sfed_IO%ned_4_ele, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_one_integer_b(FPz_f, sfed_IO%nedge_in_ele, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      allocate(ie_tmp(sfed_IO%nedge_in_ele))
      do i = 1, sfed_IO%ned_4_ele
        ie_tmp(1:sfed_IO%nedge_in_ele)                                  &
     &          = sfed_IO%iedge_for_ele(i,1:sfed_IO%nedge_in_ele)
        call gz_write_mul_integer_b                                     &
     &     (FPz_f, cast_long(sfed_IO%nedge_in_ele), ie_tmp, zbuf)
        if(zbuf%ierr_zlib .ne. 0) return
      end do
      deallocate(ie_tmp)
!
      end subroutine gz_write_edge_4_element_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_number_of_element_b(FPz_f, zbuf, ele_IO)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(element_data), intent(inout) :: ele_IO
!
!
      call gz_read_one_integer_b(FPz_f, zbuf, ele_IO%numele)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_number_of_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_element_info_b(FPz_f, zbuf, ele_IO)
!
      use gz_binary_IO
      use set_nnod_4_ele_by_type
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i
!
!
      call alloc_element_types(ele_IO)
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, cast_long(ele_IO%numele), ele_IO%elmtyp)
      if(zbuf%ierr_zlib .ne. 0) return
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
      call gz_read_mul_int8_b                                           &
     &   (FPz_f, zbuf, cast_long(ele_IO%numele), ele_IO%iele_global)
      if(zbuf%ierr_zlib .ne. 0) return
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        call gz_read_mul_integer_b                                      &
     &     (FPz_f, zbuf, cast_long(ele_IO%nodelm(i)), ie_tmp)
        if(zbuf%ierr_zlib .ne. 0) return
!
        ele_IO%ie(i,1:ele_IO%nodelm(i)) = ie_tmp(1:ele_IO%nodelm(i))
      end do
      deallocate(ie_tmp)
!
      end subroutine gz_read_element_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_surface_4_element_b(FPz_f, zbuf, sfed_IO)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i, nsf_4_ele, nsurf_in_ele
!
!
      call gz_read_one_integer_b(FPz_f, zbuf, nsf_4_ele)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_one_integer_b(FPz_f, zbuf, nsurf_in_ele)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call alloc_surface_connect_IO(nsf_4_ele, nsurf_in_ele, sfed_IO)
!
      allocate(ie_tmp(sfed_IO%nsurf_in_ele))
      do i = 1, sfed_IO%nsf_4_ele
        call gz_read_mul_integer_b                                      &
     &     (FPz_f, zbuf, cast_long(sfed_IO%nsurf_in_ele), ie_tmp)
        if(zbuf%ierr_zlib .ne. 0) return
!
        sfed_IO%isf_for_ele(i,1:sfed_IO%nsurf_in_ele)                   &
     &        = ie_tmp(1:sfed_IO%nsurf_in_ele)
      end do
      deallocate(ie_tmp)
!
      end subroutine gz_read_surface_4_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_edge_4_element_b(FPz_f, zbuf, sfed_IO)
!
      use gz_binary_IO
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i, ned_4_ele, nedge_in_ele
!
!
      call gz_read_one_integer_b(FPz_f, zbuf, ned_4_ele)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_one_integer_b(FPz_f, zbuf, nedge_in_ele)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call alloc_edge_connect_IO(ned_4_ele, nedge_in_ele, sfed_IO)
!
      allocate(ie_tmp(sfed_IO%nedge_in_ele))
      do i = 1, sfed_IO%ned_4_ele
        call gz_read_mul_integer_b                                      &
     &     (FPz_f, zbuf, cast_long(sfed_IO%nedge_in_ele), ie_tmp)
        if(zbuf%ierr_zlib .ne. 0) return
!
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
