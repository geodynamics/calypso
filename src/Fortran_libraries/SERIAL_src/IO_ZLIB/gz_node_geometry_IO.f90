!> @file  gz_node_geometry_IO.f90
!!      module gz_node_geometry_IO
!!
!! @author  H. Matsui
!! @date Written in Aug., 2006
!
!> @brief Node data IO using zlib
!!
!!@verbatim
!!      subroutine gz_write_geometry_info(FPz_f, nod_IO, zbuf)
!!      subroutine gz_write_scalar_in_element                           &
!!     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!!      subroutine gz_write_vector_in_element                           &
!!     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!        type(buffer_4_gzip) , intent(inout) :: zbuf
!!
!!      subroutine gz_read_number_of_node(FPz_f, nod_IO, zbuf)
!!      subroutine gz_read_geometry_info(FPz_f, nod_IO, zbuf)
!!      subroutine gz_read_scalar_in_element                            &
!!     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!!      subroutine gz_read_vector_in_element                            &
!!     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!        type(buffer_4_gzip) , intent(inout) :: zbuf
!!@endverbatim
!
      module gz_node_geometry_IO
!
      use m_precision
!
      use t_geometry_data
      use t_surf_edge_IO
      use t_buffer_4_gzip
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_geometry_info(FPz_f, nod_IO, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(in) :: nod_IO
      type(buffer_4_gzip) , intent(inout) :: zbuf
!
      integer (kind = kint) :: i
!
!
      write(zbuf%fixbuf(1),'(2i16,2a1)')                                &
     &       nod_IO%numnod, nod_IO%internal_node, char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      do i=1, nod_IO%numnod
        write(zbuf%fixbuf(1),'(i16,1p3E25.15e3,2a1)')                   &
     &       nod_IO%inod_global(i), nod_IO%xx(i,1:3), char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end do
!
      end subroutine gz_write_geometry_info
!
!------------------------------------------------------------------
!
      subroutine gz_write_scalar_in_element                             &
     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip) , intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
      write(zbuf%fixbuf(1),'(2i16,2a1)')                                &
     &       nod_IO%numnod, nod_IO%internal_node, char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      do i = 1, nod_IO%numnod
        write(zbuf%fixbuf(1),'(1p3e23.15,2a1)') sfed_IO%ele_scalar(i),  &
     &                                          char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end do
!
      end subroutine gz_write_scalar_in_element
!
!------------------------------------------------------------------
!
      subroutine gz_write_vector_in_element                             &
     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(buffer_4_gzip) , intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
      write(zbuf%fixbuf(1),'(2i16,2a1)')                                &
     &      nod_IO%numnod, nod_IO%internal_node, char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      do i = 1, nod_IO%numnod
        write(zbuf%fixbuf(1),'(1p3e23.15,2a1)')                         &
     &       sfed_IO%ele_vector(i,1:3), char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end do
!
      end subroutine gz_write_vector_in_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_number_of_node(FPz_f, nod_IO, zbuf)
!
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(inout) :: nod_IO
      type(buffer_4_gzip) , intent(inout) :: zbuf
!
!
      call skip_gz_comment_int(FPz_f, nod_IO%numnod, zbuf)
      read(zbuf%fixbuf(1),*) nod_IO%numnod, nod_IO%internal_node
!
      end subroutine gz_read_number_of_node
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_info(FPz_f, nod_IO, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(inout) :: nod_IO
      type(buffer_4_gzip) , intent(inout) :: zbuf
!
      integer (kind = kint) :: i, k
!
!
      call gz_read_number_of_node(FPz_f, nod_IO, zbuf)
      call alloc_node_geometry_base(nod_IO)
!
      do i=1, nod_IO%numnod
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*)                                          &
     &              nod_IO%inod_global(i), (nod_IO%xx(i,k),k=1,3)
      end do
!
      end subroutine gz_read_geometry_info
!
!------------------------------------------------------------------
!
      subroutine gz_read_scalar_in_element                              &
     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      type(buffer_4_gzip) , intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
!
      call skip_gz_comment_int(FPz_f, nod_IO%numnod, zbuf)
      read(zbuf%fixbuf(1),*) nod_IO%numnod, nod_IO%internal_node
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      do i = 1, nod_IO%numnod
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*) sfed_IO%ele_scalar(i)
      end do
!
      end subroutine gz_read_scalar_in_element
!
!------------------------------------------------------------------
!
      subroutine gz_read_vector_in_element                              &
     &         (FPz_f, nod_IO, sfed_IO, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      type(buffer_4_gzip) , intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
!
      call skip_gz_comment_int(FPz_f, nod_IO%numnod, zbuf)
      read(zbuf%fixbuf(1),*) nod_IO%numnod, nod_IO%internal_node
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      do i = 1, nod_IO%numnod
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*) sfed_IO%ele_vector(i,1:3)
      end do
!
      end subroutine gz_read_vector_in_element
!
!------------------------------------------------------------------
!
      end module gz_node_geometry_IO
