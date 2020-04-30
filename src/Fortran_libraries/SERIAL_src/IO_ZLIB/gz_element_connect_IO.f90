!> @file  gz_element_connect_IO.f90
!!      module gz_element_connect_IO
!!
!! @author  H. Matsui
!! @date Written in Oct., 2006
!
!> @brief Element data IO using zlib
!!
!!@verbatim
!!      subroutine gz_write_element_info(ele_IO, zbuf)
!!        type(element_data), intent(in) :: ele_IO
!!      subroutine gz_write_surface_4_element(sfed_IO, zbuf)
!!      subroutine gz_write_edge_4_element(sfed_IO, zbuf)
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_number_of_element(ele_IO, zbuf)
!!      subroutine gz_read_element_info(ele_IO, zbuf)
!!        type(element_data), intent(inout) :: ele_IO
!!      subroutine gz_read_surface_4_element(sfed_IO, zbuf)
!!      subroutine gz_read_edge_4_element(sfed_IO, zbuf)
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_element_connect_IO
!
      use m_precision
!
      use t_geometry_data
      use t_surf_edge_IO
      use t_buffer_4_gzip
!
      implicit none
!
      character(len=kchara), private :: fmt_txt
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_element_info(ele_IO, zbuf)
!
      use gzip_file_access
      use gz_data_IO
!
      type(element_data), intent(in) :: ele_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer (kind = kint) :: i
!
!
      write(zbuf%fixbuf(1),'(i16,2a1)')                                 &
     &                                ele_IO%numele, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      call write_gz_multi_int_10i8(ele_IO%numele, ele_IO%elmtyp, zbuf)
!
      do i=1, ele_IO%numele
        write(fmt_txt,'(a5,i3,a8)')                                     &
     &         '(i16,', ele_IO%nodelm(i), 'i16,2a1)'
        write(zbuf%fixbuf(1),fmt_txt) ele_IO%iele_global(i),            &
     &         ele_IO%ie(i,1:ele_IO%nodelm(i)), char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end do
!
      end subroutine gz_write_element_info
!
!------------------------------------------------------------------
!
      subroutine gz_write_surface_4_element(sfed_IO, zbuf)
!
      use gzip_file_access
!
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
      write(zbuf%fixbuf(1),'(2i16,2a1)')                                &
     &     sfed_IO%nsf_4_ele, sfed_IO%nsurf_in_ele, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      do i = 1, sfed_IO%nsf_4_ele
        write(zbuf%fixbuf(1),'(6i16,2a1)')                              &
     &         sfed_IO%isf_for_ele(i,1:sfed_IO%nsurf_in_ele), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end do
!
      end subroutine gz_write_surface_4_element
!
!------------------------------------------------------------------
!
      subroutine gz_write_edge_4_element(sfed_IO, zbuf)
!
      use gzip_file_access
!
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
      write(zbuf%fixbuf(1),'(2i16,2a1)')                                &
     &      sfed_IO%ned_4_ele, sfed_IO%nedge_in_ele, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      do i = 1, sfed_IO%ned_4_ele
        write(zbuf%fixbuf(1),'(12i16,2a1)')                             &
     &        sfed_IO%iedge_for_ele(i,1:sfed_IO%nedge_in_ele),          &
     &        char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end do
!
      end subroutine gz_write_edge_4_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_number_of_element(ele_IO, zbuf)
!
      use skip_gz_comment
!
      type(element_data), intent(inout) :: ele_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call skip_gz_comment_int(ele_IO%numele, zbuf)
!       write(*,*) ele_IO%numele
!
      end subroutine gz_read_number_of_element
!
!------------------------------------------------------------------
!
      subroutine gz_read_element_info(ele_IO, zbuf)
!
      use set_nnod_4_ele_by_type
      use gzip_file_access
      use gz_data_IO
!
      type(element_data), intent(inout) :: ele_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer (kind = kint) :: i
!
!
       call alloc_element_types(ele_IO)
       call read_gz_multi_int(ele_IO%numele, ele_IO%elmtyp, zbuf)
!
       ele_IO%nnod_4_ele = 0
       do i = 1, ele_IO%numele
         call s_set_nnod_4_ele_by_type                                  &
     &      (ele_IO%elmtyp(i), ele_IO%nodelm(i))
         ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
       end do
!
       call alloc_ele_connectivity(ele_IO)
!
       do i=1, ele_IO%numele
        call get_one_line_text_from_gz(zbuf)
        read(zbuf%fixbuf(1),*) ele_IO%iele_global(i),                   &
     &                 ele_IO%ie(i,1:ele_IO%nodelm(i))
       end do
!
      end subroutine gz_read_element_info
!
!------------------------------------------------------------------
!
      subroutine gz_read_surface_4_element(sfed_IO, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, nsf_4_ele, nsurf_in_ele
!
!
      call skip_gz_comment_int(nsf_4_ele, zbuf)
      read(zbuf%fixbuf(1),*) nsf_4_ele, nsurf_in_ele
      call alloc_surface_connect_IO(nsf_4_ele, nsurf_in_ele, sfed_IO)
!
      do i = 1, sfed_IO%nsf_4_ele
        call get_one_line_text_from_gz(zbuf)
        read(zbuf%fixbuf(1),*)                                          &
     &                  sfed_IO%isf_for_ele(i,1:sfed_IO%nsurf_in_ele)
      end do
!
      end subroutine gz_read_surface_4_element
!
!------------------------------------------------------------------
!
      subroutine gz_read_edge_4_element(sfed_IO, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, ned_4_ele, nedge_in_ele
!
!
      call skip_gz_comment_int(ned_4_ele, zbuf)
      read(zbuf%fixbuf(1),*) ned_4_ele, nedge_in_ele
      call alloc_edge_connect_IO(ned_4_ele, nedge_in_ele, sfed_IO)
!
      do i = 1, sfed_IO%ned_4_ele
        call get_one_line_text_from_gz(zbuf)
        read(zbuf%fixbuf(1),*)                                          &
     &         sfed_IO%iedge_for_ele(i,1:sfed_IO%nedge_in_ele)
      end do
!
      end subroutine gz_read_edge_4_element
!
!------------------------------------------------------------------
!
      end module gz_element_connect_IO
