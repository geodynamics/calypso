!> @file  gz_node_geometry_IO.f90
!!      module gz_node_geometry_IO
!!
!! @author  H. Matsui
!! @date Written in Aug., 2006
!
!> @brief Node data IO using zlib
!!
!!@verbatim
!!      subroutine write_geometry_info_gz(nod_IO)
!!
!!      subroutine read_number_of_node_gz(nod_IO)
!!      subroutine read_geometry_info_gz(nod_IO)
!!        type(node_data), intent(inout) :: nod_IO
!!@endverbatim
!
      module gz_node_geometry_IO
!
      use m_precision
!
      use t_geometry_data
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
      subroutine write_geometry_info_gz(nod_IO)
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
      end subroutine write_geometry_info_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_node_gz(nod_IO)
!
      type(node_data), intent(inout) :: nod_IO
!
!
      call skip_gz_comment_int(nod_IO%numnod)
      read(textbuf,*) nod_IO%numnod, nod_IO%internal_node
!
      end subroutine read_number_of_node_gz
!
!------------------------------------------------------------------
!
      subroutine read_geometry_info_gz(nod_IO)
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
      end subroutine read_geometry_info_gz
!
!------------------------------------------------------------------
!
      end module gz_node_geometry_IO
