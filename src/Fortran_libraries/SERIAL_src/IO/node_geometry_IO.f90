!>@file   node_geometry_IO.f90
!!@brief  module node_geometry_IO
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on  Aug., 2006
!
!>@brief  routines for ASCII data IO for mesh geometry
!!
!!@verbatim
!!      function write_each_node_buffer(inod, nod_IO)
!!      subroutine read_each_node_buffer(textbuf, inod, nod_IO)
!!
!!      subroutine write_geometry_info(id_file, nod_IO)
!!      subroutine write_scalar_in_element(id_file, nod_IO, sfed_IO)
!!      subroutine write_vector_in_element(id_file, nod_IO, sfed_IO)
!!
!!      subroutine read_number_of_node(id_file, nod_IO)
!!      subroutine read_geometry_info(id_file, nod_IO)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module node_geometry_IO
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
!
      implicit none
!
      integer(kind = kint), parameter                                   &
     &             :: len_each_node_buffer = 16 + 3*25 + 1
!
      character(len=255) :: character_4_read
      private :: character_4_read
!
      private :: write_each_node_buffer
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      function write_each_node_buffer(inod, nod_IO)
!
      integer (kind = kint), intent(in) :: inod
      type(node_data), intent(in) :: nod_IO
!
      character(len=len_each_node_buffer) :: write_each_node_buffer
!
!
      write(write_each_node_buffer,'(i16,1p3E25.15e3,a1)')              &
     &         nod_IO%inod_global(inod), nod_IO%xx(inod,1:3), char(10)
!
      end function write_each_node_buffer
!
! -------------------------------------------------------------------
!
      subroutine read_each_node_buffer(textbuf, inod, nod_IO)
!
      integer (kind = kint), intent(in) :: inod
      character(len=len_each_node_buffer), intent(in) :: textbuf
      type(node_data), intent(inout) :: nod_IO
!
!
      read(textbuf,*) nod_IO%inod_global(inod), nod_IO%xx(inod,1:3)
!
      end subroutine read_each_node_buffer
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_geometry_info(id_file, nod_IO)
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
!
      integer (kind = kint) :: i
!
!
      write(id_file,'(2i16)') nod_IO%numnod, nod_IO%internal_node
!
      do i=1, nod_IO%numnod
        write(id_file,'(i16,1p3E25.15e3)')  nod_IO%inod_global(i),      &
     &        nod_IO%xx(i,1:3)
      end do
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine write_geometry_info
!
!------------------------------------------------------------------
!
      subroutine write_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i
!
      write(id_file,'(2i16)') nod_IO%numnod, nod_IO%internal_node
      do i = 1, nod_IO%numnod
        write(id_file,'(i16, 1p3e23.15)') i, sfed_IO%ele_scalar(i)
      end do
!
      call dealloc_ele_scalar_IO(sfed_IO)
!
      end subroutine write_scalar_in_element
!
!------------------------------------------------------------------
!
      subroutine write_vector_in_element(id_file, nod_IO, sfed_IO)
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i
!
      write(id_file,'(2i16)') nod_IO%numnod, nod_IO%internal_node
      do i = 1, nod_IO%numnod
        write(id_file,'(i16,1p3e23.15)') i, sfed_IO%ele_vector(i,1:3)
      end do
!
      call dealloc_ele_vector_IO(sfed_IO)
!
      end subroutine write_vector_in_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_node(id_file, nod_IO)
!
      use skip_comment_f
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) nod_IO%numnod, nod_IO%internal_node
!      write(*,*) nod_IO%numnod, nod_IO%internal_node
!
      end subroutine read_number_of_node
!
!------------------------------------------------------------------
!
      subroutine read_geometry_info(id_file, nod_IO)
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
!
      integer (kind = kint) :: i, k
!
!
      call alloc_node_geometry_base(nod_IO)
!
      do i=1, nod_IO%numnod
        read(id_file,*)  nod_IO%inod_global(i), (nod_IO%xx(i,k),k=1,3)
      end do
!
      end subroutine read_geometry_info
!
!------------------------------------------------------------------
!
      end module node_geometry_IO
