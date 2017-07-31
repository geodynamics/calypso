!> @file  gz_element_connect_IO.f90
!!      module gz_element_connect_IO
!!
!! @author  H. Matsui
!! @date Written in Oct., 2006
!
!> @brief Element data IO using zlib
!!
!!@verbatim
!!      subroutine write_element_info_gz(ele_IO)
!!
!!      subroutine read_number_of_element_gz(ele_IO)
!!      subroutine read_element_info_gz(ele_IO)
!!        type(element_data), intent(inout) :: ele_IO
!!@endverbatim
!
      module gz_element_connect_IO
!
      use m_precision
!
      use t_geometry_data
      use skip_gz_comment
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
      subroutine write_element_info_gz(ele_IO)
!
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i
!
!
      write(textbuf,'(i16,a1)') ele_IO%numele, char(0)
      call gz_write_textbuf_w_lf
      call write_gz_multi_int_10i8(ele_IO%numele, ele_IO%elmtyp)
!
      do i=1, ele_IO%numele
        write(fmt_txt,'(a5,i3,a7)')                                     &
     &         '(i16,', ele_IO%nodelm(i), 'i16,a1)'
        write(textbuf,fmt_txt) ele_IO%iele_global(i),                   &
     &         ele_IO%ie(i,1:ele_IO%nodelm(i)), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine write_element_info_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_element_gz(ele_IO)
!
      type(element_data), intent(inout) :: ele_IO
!
!
      call skip_gz_comment_int(ele_IO%numele)
!       write(*,*) ele_IO%numele
!
      end subroutine read_number_of_element_gz
!
!------------------------------------------------------------------
!
      subroutine read_element_info_gz(ele_IO)
!
      use set_nnod_4_ele_by_type
!
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: i
!
!
       call alloc_element_types(ele_IO)
       call read_gz_multi_int(ele_IO%numele, ele_IO%elmtyp)
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
        call get_one_line_from_gz_f
        read(textbuf,*) ele_IO%iele_global(i),                          &
     &                 ele_IO%ie(i,1:ele_IO%nodelm(i))
       end do
!
      end subroutine read_element_info_gz
!
!------------------------------------------------------------------
!
      end module gz_element_connect_IO
