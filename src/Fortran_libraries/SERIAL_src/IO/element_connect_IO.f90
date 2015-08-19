!>@file  element_connect_IO.f90
!!      module element_connect_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element connectivity
!!
!!@verbatim
!!      subroutine write_element_info(id_file)
!!      subroutine write_surface_4_element(id_file)
!!      subroutine write_edge_4_element(id_file)
!!      subroutine write_element_info_b(id_file)
!!
!!      subroutine read_number_of_element(id_file)
!!      subroutine read_element_info(id_file)
!!      subroutine read_number_of_element_b(id_file)
!!      subroutine read_element_info_b(id_file)
!!@endverbatim
!!
!!@param  id_file  File ID
!
      module element_connect_IO
!
      use m_precision
!
      use m_read_mesh_data
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
      subroutine write_element_info(id_file)
!
      integer (kind = kint), intent(in) :: id_file
      integer (kind = kint) :: i
!
!
      write(id_file,'(i16)') numele_dummy
      write(id_file,'(10i16)') i_ele_dummy(1:numele_dummy)
!
      do i=1, numele_dummy
        write(id_file,'(28i16)') globalelmid_dummy(i),                  &
     &    ie_dummy(i,1:nodelm_dummy(i))
      end do
!
      call deallocate_ele_info_dummy
!
      end subroutine write_element_info
!
!------------------------------------------------------------------
!
      subroutine write_surface_4_element(id_file)
!
      integer (kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      write(id_file,'(2i16)') nsf_4_ele_IO, nsurf_in_ele_IO
!
      do i = 1, nsf_4_ele_IO
        write(id_file,'(10i16)') i, isf_4_ele_IO(i,1:nsurf_in_ele_IO)
      end do
!
      call deallocate_surface_connect_IO
!
      end subroutine write_surface_4_element
!
!------------------------------------------------------------------
!
      subroutine write_edge_4_element(id_file)
!
      integer (kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      write(id_file,'(2i16)') ned_4_ele_IO, nedge_in_ele_IO
!
      do i = 1, ned_4_ele_IO
        write(id_file,'(15i16)') i, iedge_4_ele_IO(i,1:nedge_in_ele_IO)
      end do
!
      call deallocate_edge_connect_IO
!
      end subroutine write_edge_4_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_element_info_b(id_file)
!
      integer (kind = kint), intent(in) :: id_file
      integer (kind = kint) :: i
!
!
      write(id_file) numele_dummy
!
      write(id_file) i_ele_dummy(1:numele_dummy)
!
      write(id_file) globalelmid_dummy(1:numele_dummy)
      write(id_file) (ie_dummy(i,1:nodelm_dummy(i)),i=1,numele_dummy)
!
      call deallocate_ele_info_dummy
!
      end subroutine write_element_info_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_element(id_file)
!
      use skip_comment_f
!
      integer (kind = kint), intent(in) :: id_file
!
      call skip_comment(character_4_read,id_file)
!
      read(character_4_read,*) numele_dummy
!       write(*,*) numele_dummy
!
      end subroutine read_number_of_element
!
!------------------------------------------------------------------
!
       subroutine read_element_info(id_file)
!
       use set_nnod_4_ele_by_type
!
       integer (kind = kint), intent(in) :: id_file
       integer (kind = kint) :: i
!
!
       call allocate_ele_info_dummy
!
       read(id_file,*) (i_ele_dummy(i),i=1,numele_dummy)
!
       nnod_4_ele_dummy = 0
       do i = 1, numele_dummy
         call s_set_nnod_4_ele_by_type(i_ele_dummy(i), nodelm_dummy(i))
         nnod_4_ele_dummy = max(nnod_4_ele_dummy,nodelm_dummy(i))
       end do
!
       call allocate_connect_dummy
!
       do i=1, numele_dummy
!
        read(id_file,*) globalelmid_dummy(i),                           &
     &                  ie_dummy(i,1:nodelm_dummy(i))
       end do
!
       end subroutine read_element_info
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine read_number_of_element_b(id_file)
!
      integer (kind = kint), intent(in) :: id_file
!
       read(id_file) numele_dummy
!       write(*,*) numele_dummy
!
      end subroutine read_number_of_element_b
!
!------------------------------------------------------------------
!
       subroutine read_element_info_b(id_file)
!
       use set_nnod_4_ele_by_type
!
       integer (kind = kint), intent(in) :: id_file
       integer (kind = kint) :: i
!
!
       call allocate_ele_info_dummy
!
       read(id_file) (i_ele_dummy(i),i=1,numele_dummy)
!
       nnod_4_ele_dummy = 0
       do i = 1, numele_dummy
         call s_set_nnod_4_ele_by_type(i_ele_dummy(i), nodelm_dummy(i))
         nnod_4_ele_dummy = max(nnod_4_ele_dummy,nodelm_dummy(i))
       end do
!
       call allocate_connect_dummy
!
       read(id_file) globalelmid_dummy(1:numele_dummy)
       read(id_file) (ie_dummy(i,1:nodelm_dummy(i)),i=1,numele_dummy)
!
       end subroutine read_element_info_b
!
!------------------------------------------------------------------
!
      end module element_connect_IO
