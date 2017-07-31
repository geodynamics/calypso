!
!      module gz_group_data_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_group_item_gz(ngrp, ntot, istack, name, item)
!      subroutine read_surface_group_item_gz(ngrp, ntot,                &
!     &          istack, name, item_sf)
!
!      subroutine write_group_data_gz(ngrp, ntot, istack, name, item)
!      subroutine write_surf_group_data_gz(ngrp, ntot,                  &
!     &          istack, name, item_sf)
!
      module gz_group_data_IO
!
      use m_precision
!
      use m_constants
      use skip_gz_comment
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_group_item_gz(ngrp, ntot, istack, name, item)
!
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
!
      integer(kind = kint), intent(inout) :: item(ntot)
      character(len = kchara), intent(inout) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, num
!
!
      if (ngrp .le. 0) return
!
      do i = 1, ngrp
        ist = istack(i-1)+1
        num = istack(i) - istack(i-1)
!
        call skip_gz_comment_chara( name(i) )
!
        if(num .gt. 0) call read_gz_multi_int(num, item(ist))
      end do
!
      end subroutine read_group_item_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_surface_group_item_gz(ngrp, ntot,                 &
     &          istack, name, item_sf)
!
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
!
      integer(kind = kint), intent(inout) :: item_sf(2,ntot)
      character(len = kchara), intent(inout) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, num
!
!
      if (ngrp .le. 0) return
!
      do i = 1, ngrp
        ist = istack(i-1)+1
        num = istack(i) - istack(i-1)
!
        call skip_gz_comment_chara( name(i) )
!
        if(num .gt. 0) then
          call read_gz_surf_group(ione, ntot, istack(i-1), item_sf)
          call read_gz_surf_group(itwo, ntot, istack(i-1), item_sf)
        end if
      end do
!
      end subroutine read_surface_group_item_gz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_group_data_gz(ngrp, ntot, istack, name, item)
!
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
      integer(kind = kint), intent(in) :: item(ntot)
      character(len = kchara), intent(in) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, num
!
!
      write(textbuf,'(i16,a1)') ngrp, char(0)
      call gz_write_textbuf_w_lf
!
      if (ngrp .gt. 0) then
        call write_gz_multi_int_8i10(ngrp, istack(1))
!
        do i = 1, ngrp
          ist = istack(i-1)+1
          num = istack(i) - istack(i-1)
!
          write(textbuf,'(a,a1)') trim(name(i)), char(0)
          call gz_write_textbuf_w_lf
!
          if(num .le. 0) then
            write(textbuf,'(a1)') char(0)
            call gz_write_textbuf_w_lf
          else
            call write_gz_multi_int_8i10(num, item(ist))
          end if
!
        end do
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
      end subroutine write_group_data_gz
!
! -----------------------------------------------------------------------
!
      subroutine write_surf_group_data_gz(ngrp, ntot,                   &
     &          istack, name, item_sf)
!
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
      integer(kind = kint), intent(in) :: item_sf(2,ntot)
      character(len = kchara), intent(in) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, num
!
!
      write(textbuf,'(i16,a1)') ngrp, char(0)
      call gz_write_textbuf_w_lf
!
      if (ngrp .gt. 0) then
        call write_gz_multi_int_8i10(ngrp, istack(1))
!
        do i = 1, ngrp
          ist = istack(i-1)+1
          num = istack(i) - istack(i-1)
!
          write(textbuf,'(a,a1)') trim(name(i)), char(0)
          call gz_write_textbuf_w_lf
!
          if(num .le. 0) then
            write(textbuf,'(a1)') char(0)
            call gz_write_textbuf_w_lf
            write(textbuf,'(a1)') char(0)
            call gz_write_textbuf_w_lf
          else
            call write_gz_surf_group(ione, ntot, istack(i-1), item_sf)
            call write_gz_surf_group(itwo, ntot, istack(i-1), item_sf)
          end if
!
        end do
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
      end subroutine write_surf_group_data_gz
!
! -----------------------------------------------------------------------
!
      end module gz_group_data_IO
