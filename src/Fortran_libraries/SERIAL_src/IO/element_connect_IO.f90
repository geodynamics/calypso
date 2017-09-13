!>@file  element_connect_IO.f90
!!      module element_connect_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element connectivity
!!
!!@verbatim
!!      subroutine write_element_info(id_file, ele_IO)
!!      subroutine write_surface_4_element(id_file, sfed_IO)
!!      subroutine write_edge_4_element(id_file, sfed_IO)
!!
!!      subroutine read_number_of_element(id_file, ele_IO)
!!      subroutine read_element_info(id_file, ele_IO)
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine read_surface_4_element(id_file, sfed_IO)
!!      subroutine read_edge_4_element(id_file, sfed_IO)
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!!
!!@param  id_file  File ID
!
      module element_connect_IO
!
      use m_precision
      use t_geometry_data
      use t_surf_edge_IO
      use t_read_mesh_data
!
      implicit none
!
!
      character(len=255) :: character_4_read
      private :: character_4_read
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine write_element_info(id_file, ele_IO)
!
      integer (kind = kint), intent(in) :: id_file
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i
!
!
      write(id_file,'(i16)') ele_IO%numele
      write(id_file,'(10i16)') ele_IO%elmtyp(1:ele_IO%numele)
!
      do i=1, ele_IO%numele
        write(id_file,'(28i16)') ele_IO%iele_global(i),                 &
     &    ele_IO%ie(i,1:ele_IO%nodelm(i))
      end do
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine write_element_info
!
!------------------------------------------------------------------
!
      subroutine write_surface_4_element(id_file, sfed_IO)
!
      integer (kind = kint), intent(in) :: id_file
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i
!
      write(id_file,'(2i16)') sfed_IO%nsf_4_ele, sfed_IO%nsurf_in_ele
!
      do i = 1, sfed_IO%nsf_4_ele
        write(id_file,'(6i16)')                                         &
     &        sfed_IO%isf_for_ele(i,1:sfed_IO%nsurf_in_ele)
      end do
!
      call dealloc_surface_connect_IO(sfed_IO)
!
      end subroutine write_surface_4_element
!
!------------------------------------------------------------------
!
      subroutine write_edge_4_element(id_file, sfed_IO)
!
      integer (kind = kint), intent(in) :: id_file
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i
!
      write(id_file,'(2i16)') sfed_IO%ned_4_ele, sfed_IO%nedge_in_ele
!
      do i = 1, sfed_IO%ned_4_ele
        write(id_file,'(12i16)')                                        &
     &         sfed_IO%iedge_for_ele(i,1:sfed_IO%nedge_in_ele)
      end do
!
      call dealloc_edge_connect_IO(sfed_IO)
!
      end subroutine write_edge_4_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_element(id_file, ele_IO)
!
      use skip_comment_f
!
      integer (kind = kint), intent(in) :: id_file
      type(element_data), intent(inout) :: ele_IO
!
!
      call skip_comment(character_4_read,id_file)
!
      read(character_4_read,*) ele_IO%numele
!
      end subroutine read_number_of_element
!
!------------------------------------------------------------------
!
      subroutine read_element_info(id_file, ele_IO)
!
      use set_nnod_4_ele_by_type
!
      integer (kind = kint), intent(in) :: id_file
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i
!
!
       call alloc_element_types(ele_IO)
!
       read(id_file,*) (ele_IO%elmtyp(i),i=1,ele_IO%numele)
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
         read(id_file,*) ele_IO%iele_global(i),                         &
     &                  ele_IO%ie(i,1:ele_IO%nodelm(i))
       end do
!
       end subroutine read_element_info
!
!------------------------------------------------------------------
!
      subroutine read_surface_4_element(id_file, sfed_IO)
!
      integer (kind = kint), intent(in) :: id_file
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i, nsf_4_ele, nsurf_in_ele
!
      read(id_file,*) nsf_4_ele, nsurf_in_ele
      call alloc_surface_connect_IO                                     &
     &   (nsf_4_ele, nsurf_in_ele, sfed_IO)
!
      do i = 1, sfed_IO%nsf_4_ele
        read(id_file,*) sfed_IO%isf_for_ele(i,1:sfed_IO%nsurf_in_ele)
      end do
!
      end subroutine read_surface_4_element
!
!------------------------------------------------------------------
!
      subroutine read_edge_4_element(id_file, sfed_IO)
!
      integer (kind = kint), intent(in) :: id_file
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: i, ned_4_ele, nedge_in_ele
!
      read(id_file,*) ned_4_ele, nedge_in_ele
      call alloc_edge_connect_IO(ned_4_ele, nedge_in_ele, sfed_IO)
!
      do i = 1, sfed_IO%ned_4_ele
        read(id_file,*) sfed_IO%iedge_for_ele(i,1:sfed_IO%nedge_in_ele)
      end do
!
      end subroutine read_edge_4_element
!
!------------------------------------------------------------------
!
      end module element_connect_IO
