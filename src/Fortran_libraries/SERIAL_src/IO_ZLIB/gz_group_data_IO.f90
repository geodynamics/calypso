!
!      module gz_group_data_IO
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine read_group_data_gz(FPz_f, grp_IO, zbuf)
!!      subroutine read_surface_group_data_gz(FPz_f, surf_grp_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine write_group_data_gz(FPz_f, grp_IO, zbuf)
!!      subroutine write_surf_group_data_gz(FPz_f, surf_grp_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(group_data), intent(in) :: grp_IO
!!        type(surface_group_data), intent(in) :: surf_grp_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!
      module gz_group_data_IO
!
      use m_precision
!
      use m_constants
      use t_group_data
      use t_buffer_4_gzip
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_group_data_gz(FPz_f, grp_IO, zbuf)
!
      use gz_data_IO
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(group_data), intent(inout) :: grp_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, ist, num
!
!
      call skip_gz_comment_int(FPz_f, grp_IO%num_grp, zbuf)
      call alloc_group_num(grp_IO)
!
      if (grp_IO%num_grp .gt. 0) then
        call read_gz_integer_stack(FPz_f, grp_IO%num_grp,               &
     &      grp_IO%istack_grp, grp_IO%num_item, zbuf)
!
        call alloc_group_item(grp_IO)
        do i = 1, grp_IO%num_grp
          ist = grp_IO%istack_grp(i-1)+1
          num = grp_IO%istack_grp(i) - grp_IO%istack_grp(i-1)
          call skip_gz_comment_chara(FPz_f, grp_IO%grp_name(i), zbuf)
!
          if(num .gt. 0) then
            call read_gz_multi_int(FPz_f, num,                          &
     &                             grp_IO%item_grp(ist), zbuf)
          end if
        end do
      else
        call alloc_group_item(grp_IO)
      end if
!
      end subroutine read_group_data_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_surface_group_data_gz(FPz_f, surf_grp_IO, zbuf)
!
      use gz_data_IO
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(surface_group_data), intent(inout) :: surf_grp_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, ist, num
!
!
      call skip_gz_comment_int(FPz_f, surf_grp_IO%num_grp, zbuf)
      call alloc_sf_group_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call read_gz_integer_stack(FPz_f, surf_grp_IO%num_grp,          &
     &      surf_grp_IO%istack_grp, surf_grp_IO%num_item, zbuf)
        call alloc_sf_group_item(surf_grp_IO)
!
        do i = 1, surf_grp_IO%num_grp
          ist = surf_grp_IO%istack_grp(i-1)+1
          num = surf_grp_IO%istack_grp(i) - surf_grp_IO%istack_grp(i-1)
!
          call skip_gz_comment_chara                                    &
     &       (FPz_f, surf_grp_IO%grp_name(i), zbuf)
!
          if(num .gt. 0) then
            call read_gz_surf_group(FPz_f, ione, surf_grp_IO%num_item,  &
     &          surf_grp_IO%istack_grp(i-1), surf_grp_IO%item_sf_grp,   &
     &          zbuf)
            call read_gz_surf_group(FPz_f, itwo, surf_grp_IO%num_item,  &
     &          surf_grp_IO%istack_grp(i-1), surf_grp_IO%item_sf_grp,   &
     &          zbuf)
          end if
        end do
      else
        call alloc_sf_group_item(surf_grp_IO)
      end if
!
      end subroutine read_surface_group_data_gz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_group_data_gz(FPz_f, grp_IO, zbuf)
!
      use gzip_file_access
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(group_data), intent(in) :: grp_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, ist, num
!
!
      write(zbuf%fixbuf(1),'(i16,2a1)') grp_IO%num_grp,                 &
     &                                 char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      if (grp_IO%num_grp .gt. 0) then
        call write_gz_multi_int_8i16                                    &
     &     (FPz_f, grp_IO%num_grp, grp_IO%istack_grp(1), zbuf)
!
        do i = 1, grp_IO%num_grp
          ist = grp_IO%istack_grp(i-1)+1
          num = grp_IO%istack_grp(i) - grp_IO%istack_grp(i-1)
!
          write(zbuf%fixbuf(1),'(a,2a1)')                               &
     &                      trim(grp_IO%grp_name(i)), char(10), char(0)
          call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
          if(num .le. 0) then
            write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
            call gz_write_textbuf_no_lf(FPz_f, zbuf)
          else
            call write_gz_multi_int_8i16                                &
     &         (FPz_f, num, grp_IO%item_grp(ist), zbuf)
          end if
!
        end do
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end if
!
      end subroutine write_group_data_gz
!
! -----------------------------------------------------------------------
!
      subroutine write_surf_group_data_gz(FPz_f, surf_grp_IO, zbuf)
!
      use gzip_file_access
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(surface_group_data), intent(in) :: surf_grp_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, ist, num
!
!
      write(zbuf%fixbuf(1),'(i16,2a1)') surf_grp_IO%num_grp,            &
     &                                 char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call write_gz_multi_int_8i16(FPz_f, surf_grp_IO%num_grp,        &
     &      surf_grp_IO%istack_grp(1), zbuf)
!
        do i = 1, surf_grp_IO%num_grp
          ist = surf_grp_IO%istack_grp(i-1)+1
          num = surf_grp_IO%istack_grp(i) - surf_grp_IO%istack_grp(i-1)
!
          write(zbuf%fixbuf(1),'(a,2a1)')                               &
     &                 trim(surf_grp_IO%grp_name(i)), char(10), char(0)
          call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
          if(num .le. 0) then
            write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
            call gz_write_textbuf_no_lf(FPz_f, zbuf)
            write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
            call gz_write_textbuf_no_lf(FPz_f, zbuf)
          else
            call write_gz_surf_group(FPz_f, ione, surf_grp_IO%num_item, &
     &          surf_grp_IO%istack_grp(i-1), surf_grp_IO%item_sf_grp,   &
     &          zbuf)
            call write_gz_surf_group(FPz_f, itwo, surf_grp_IO%num_item, &
     &          surf_grp_IO%istack_grp(i-1), surf_grp_IO%item_sf_grp,   &
     &          zbuf)
          end if
!
        end do
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end if
!
      end subroutine write_surf_group_data_gz
!
! -----------------------------------------------------------------------
!
      end module gz_group_data_IO
