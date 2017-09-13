!> @file  gz_node_geometry_IO.f90
!!      module gz_node_geometry_IO
!!
!! @author  H. Matsui
!! @date Written in Aug., 2006
!
!> @brief Node data IO using zlib
!!
!!@verbatim
!!      subroutine gz_write_geometry_info(nod_IO)
!!      subroutine gz_write_scalar_in_element(nod_IO, sfed_IO)
!!      subroutine gz_write_vector_in_element(nod_IO, sfed_IO)
!!
!!      subroutine gz_read_number_of_node(nod_IO)
!!      subroutine gz_read_geometry_info(nod_IO)
!!      subroutine gz_read_scalar_in_element(nod_IO, sfed_IO)
!!      subroutine gz_read_vector_in_element(nod_IO, sfed_IO)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module gz_node_geometry_IO
!
      use m_precision
!
      use t_geometry_data
      use t_surf_edge_IO
      use skip_gz_comment
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_geometry_info(nod_IO)
!
      type(node_data), intent(inout) :: nod_IO
!
      integer (kind = kint) :: i
!
!
      write(textbuf,'(2i16,a1)') nod_IO%numnod, nod_IO%internal_node,   &
     &      char(0)
      call gz_write_textbuf_w_lf
!
      do i=1, nod_IO%numnod
        write(textbuf,'(i16,1p3E25.15e3,a1)')  nod_IO%inod_global(i),   &
     &        nod_IO%xx(i,1:3), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine gz_write_geometry_info
!
!------------------------------------------------------------------
!
      subroutine gz_write_scalar_in_element(nod_IO, sfed_IO)
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i
!
      write(textbuf,'(2i16,a1)')                                        &
     &       nod_IO%numnod, nod_IO%internal_node, char(0)
      call gz_write_textbuf_w_lf
!
      do i = 1, nod_IO%numnod
        write(textbuf,'(1p3e23.15,a1)') sfed_IO%ele_scalar(i), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      call dealloc_ele_scalar_IO(sfed_IO)
!
      end subroutine gz_write_scalar_in_element
!
!------------------------------------------------------------------
!
      subroutine gz_write_vector_in_element(nod_IO, sfed_IO)
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i
!
      write(textbuf,'(2i16)')                                           &
     &      nod_IO%numnod, nod_IO%internal_node, char(0)
      call gz_write_textbuf_w_lf
!
      do i = 1, nod_IO%numnod
        write(textbuf,'(1p3e23.15,a1)')                                 &
     &       sfed_IO%ele_vector(i,1:3), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      call dealloc_ele_vector_IO(sfed_IO)
!
      end subroutine gz_write_vector_in_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_number_of_node(nod_IO)
!
      type(node_data), intent(inout) :: nod_IO
!
!
      call skip_gz_comment_int(nod_IO%numnod)
      read(textbuf,*) nod_IO%numnod, nod_IO%internal_node
!
      end subroutine gz_read_number_of_node
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_info(nod_IO)
!
      type(node_data), intent(inout) :: nod_IO
!
      integer (kind = kint) :: i, k
!
!
      call alloc_node_geometry_base(nod_IO)
!
      do i=1, nod_IO%numnod
        call get_one_line_from_gz_f
        read(textbuf,*)  nod_IO%inod_global(i), (nod_IO%xx(i,k),k=1,3)
      end do
!
      end subroutine gz_read_geometry_info
!
!------------------------------------------------------------------
!
      subroutine gz_read_scalar_in_element(nod_IO, sfed_IO)
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i
!
!
      call skip_gz_comment_int(nod_IO%numnod)
      read(textbuf,*) nod_IO%numnod, nod_IO%internal_node
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      do i = 1, nod_IO%numnod
        call get_one_line_from_gz_f
        read(textbuf,*) sfed_IO%ele_scalar(i)
      end do
!
      end subroutine gz_read_scalar_in_element
!
!------------------------------------------------------------------
!
      subroutine gz_read_vector_in_element(nod_IO, sfed_IO)
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i
!
!
      call skip_gz_comment_int(nod_IO%numnod)
      read(textbuf,*) nod_IO%numnod, nod_IO%internal_node
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      do i = 1, nod_IO%numnod
        call get_one_line_from_gz_f
        read(textbuf,*) sfed_IO%ele_vector(i,1:3)
      end do
!
      end subroutine gz_read_vector_in_element
!
!------------------------------------------------------------------
!
      end module gz_node_geometry_IO
