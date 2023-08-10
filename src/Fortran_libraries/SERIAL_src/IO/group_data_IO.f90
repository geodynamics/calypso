!>@file   group_data_IO.f90
!!@brief  module group_data_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Routines for ASCII group data IO
!!
!!@verbatim
!!      subroutine read_group_stack(id_file, ngrp, ntot, istack, iend)
!!      subroutine read_group_item(id_file, ngrp, ntot, istack, name,   &
!!     &                           item, iend)
!!      subroutine read_surface_group_item(id_file, ngrp, ntot,         &
!!     &                                   istack, name, item_sf, iend)
!!
!!      subroutine write_group_data(id_file, ngrp, ntot, istack, name,  &
!!     &          item)
!!      subroutine write_surf_group_data(id_file, ngrp, ntot,           &
!!@verbatim
!
      module group_data_IO
!
      use m_precision
      use m_constants
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_group_stack(id_file, ngrp, ntot, istack, iend)
!
      use field_data_IO
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ngrp
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:ngrp)
      integer(kind = kint), intent(inout) :: iend
!
!
      call read_arrays_for_stacks(id_file, ngrp, izero,                 &
     &                            ntot, istack, iend)
!
      end subroutine read_group_stack
!
! -----------------------------------------------------------------------
!
      subroutine read_group_item(id_file, ngrp, ntot, istack, name,     &
     &                           item, iend)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
!
      integer(kind = kint), intent(inout) :: item(ntot)
      character(len = kchara), intent(inout) :: name(ngrp)
      integer(kind = kint), intent(inout) :: iend
!
      integer(kind = kint) :: i, ist, ied
!
!
      if (ngrp .le. 0) return
!
      do i = 1, ngrp
        ist = istack(i-1)+1
        ied = istack(i)
        call skip_comment(id_file, character_4_read, iend)
        if(iend .gt. 0) return
        read(character_4_read,*) name(i)
        read(id_file,*) item(ist:ied)
      end do
!
      end subroutine read_group_item
!
! -----------------------------------------------------------------------
!
      subroutine read_surface_group_item(id_file, ngrp, ntot,           &
     &                                   istack, name, item_sf, iend)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
!
      integer(kind = kint), intent(inout) :: item_sf(2,ntot)
      character(len = kchara), intent(inout) :: name(ngrp)
      integer(kind = kint), intent(inout) :: iend
!
      integer(kind = kint) :: j, ist, ied
!
!
      if (ngrp .le. 0) return
!
      do j = 1, ngrp
        ist = istack(j-1)+1
        ied = istack(j)
        call skip_comment(id_file, character_4_read, iend)
        if(iend .gt. 0) return
        read(character_4_read,*) name(j)
        read(id_file,*) item_sf(1,ist:ied)
        read(id_file,*) item_sf(2,ist:ied)
      end do
!
      end subroutine read_surface_group_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_group_data(id_file, ngrp, ntot, istack, name,    &
     &          item)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
      integer(kind = kint), intent(in) :: item(ntot)
      character(len = kchara), intent(in) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, ied
!
!
      write(id_file,'(i16)') ngrp
      if (ngrp .gt. 0) then
        write(id_file,'(8i16)') istack(1:ngrp)
        do i = 1, ngrp
          ist = istack(i-1)+1
          ied = istack(i)
          write(id_file,'(a)') trim(name(i))
          write(id_file,'(8i16)') item(ist:ied)
        end do
      else
        write(id_file,*) ''
      end if
!
      end subroutine write_group_data
!
! -----------------------------------------------------------------------
!
      subroutine write_surf_group_data(id_file, ngrp, ntot,             &
     &          istack, name, item_sf)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
      integer(kind = kint), intent(in) :: item_sf(2,ntot)
      character(len = kchara), intent(in) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, ied
!
!
      write(id_file,'(i16)') ngrp
      if (ngrp .gt. 0) then
        write(id_file,'(8i16)') istack(1:ngrp)
        do i = 1, ngrp
          ist = istack(i-1)+1
          ied = istack(i)
          write(id_file,'(a)') trim(name(i))
          write(id_file,'(8i16)') item_sf(1,ist:ied)
          write(id_file,'(8i16)') item_sf(2,ist:ied)
        end do
      else
        write(id_file,*) ''
        write(id_file,*) ''
      end if
!
      end subroutine write_surf_group_data
!
! -----------------------------------------------------------------------
!
      end module group_data_IO
