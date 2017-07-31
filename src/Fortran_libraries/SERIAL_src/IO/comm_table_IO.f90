!>@file   comm_table_IO.f90
!!@brief  module comm_table_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Routines for communcation table IO
!!
!!@verbatim
!!      subroutine read_send_recv_item(id_file, ntot_sr, inod_sr)
!!      subroutine read_send_recv_work(id_file, ntot_sr, nwork,         &
!!     &          inod_sr, idx_work)
!!      subroutine write_send_recv_data(id_file, num_sr, ntot_sr,       &
!!     &          istack_sr, inod_sr)
!!      subroutine write_send_recv_work(id_file, num_sr, ntot_sr, nwork,&
!!     &          istack_sr, inod_sr, idx_work)
!!@endverbatim
!
      module comm_table_IO
!
      use m_precision
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
      subroutine read_send_recv_item(id_file, ntot_sr, inod_sr)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ntot_sr
      integer(kind = kint), intent(inout) :: inod_sr(ntot_sr)
!
      integer(kind = kint) :: i
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) inod_sr(1)
      do i = 2, ntot_sr
        read(id_file,*) inod_sr(i)
      end do
!
      end subroutine read_send_recv_item
!
! -----------------------------------------------------------------------
!
      subroutine read_send_recv_work(id_file, ntot_sr, nwork,           &
     &          inod_sr, idx_work)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ntot_sr, nwork
      integer(kind = kint), intent(inout) :: inod_sr(ntot_sr)
      integer(kind = kint), intent(inout) :: idx_work(ntot_sr,nwork)
!
      integer(kind = kint) :: i
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) inod_sr(1), idx_work(1,1:nwork)
      do i = 2, ntot_sr
        read(id_file,*) inod_sr(i), idx_work(i,1:nwork)
      end do
!
      end subroutine read_send_recv_work
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_send_recv_data(id_file, num_sr, ntot_sr,         &
     &          istack_sr, inod_sr)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: num_sr, ntot_sr
      integer(kind = kint), intent(in) :: istack_sr(0:num_sr)
      integer(kind = kint), intent(in) :: inod_sr(ntot_sr)
!
      integer(kind = kint) :: i
!
      if (num_sr .gt. 0) then
        write(id_file,'(8i16)') istack_sr(1:num_sr)
        do i = 1, ntot_sr
          write(id_file,'(i16)') inod_sr(i)
        end do
      else
        write(id_file,'(a)')
      end if
!
      end subroutine write_send_recv_data
!
! -----------------------------------------------------------------------
!
      subroutine write_send_recv_work(id_file, num_sr, ntot_sr, nwork,  &
     &          istack_sr, inod_sr, idx_work)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: num_sr, ntot_sr, nwork
      integer(kind = kint), intent(in) :: istack_sr(0:num_sr)
      integer(kind = kint), intent(in) :: inod_sr(ntot_sr)
      integer(kind = kint), intent(in) :: idx_work(ntot_sr,nwork)
!
      integer(kind = kint) :: i
!
!
      if (num_sr .gt. 0) then
        write(id_file,'(8i16)') istack_sr(1:num_sr)
        do i = 1, ntot_sr
          write(id_file,'(20i16)') inod_sr(i), idx_work(i,1:nwork)
        end do
      else
        write(id_file,'(a)')
      end if
!
      end subroutine write_send_recv_work
!
! -----------------------------------------------------------------------
!
      end module comm_table_IO
