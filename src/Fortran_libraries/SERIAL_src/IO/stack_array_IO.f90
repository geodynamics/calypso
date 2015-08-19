!>@file  stack_array_IO.f90
!!       module stack_array_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief routines for stack list IO
!!
!!@verbatim
!!      subroutine read_arrays_for_stacks(file_id, num, istack_begin,   &
!!     &          ntot, istack)
!!      subroutine read_arrays_for_stacks_b(file_id, num, istack_begin, &
!!    &           ntot,istack)
!!
!!      subroutine write_arrays_for_stacks(file_id, num, istack)
!!      subroutine write_arrays_for_stacks_b(file_id, num, istack)
!!@endverbatim
!
      module stack_array_IO
!
      use m_precision
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_arrays_for_stacks(file_id, num, istack_begin,     &
      &         ntot, istack)
!
      use skip_comment_f
!
      integer (kind = kint), intent(in) :: file_id
      integer (kind = kint), intent(in) :: num, istack_begin
      integer (kind = kint), intent(inout) :: ntot
      integer (kind = kint), intent(inout) :: istack(0:num)
!
      integer (kind = kint) :: i, ii
!
!
      istack(0:num) = istack_begin-1
!
      if(num .gt. 0) then
        character_4_read = ''
        call skip_comment(character_4_read,file_id)
        read(character_4_read,*,end=41) istack
   41   continue
!
        ii = num+1
        do i = num, 1, -1 
          if ( istack(i-1) .eq. (istack_begin-1) ) ii = i
        end do
!
        do i = ii-1, 1, -1
         istack(i) = istack(i-1)
        end do
        istack(0) = istack_begin
!
        if ( ii .le. num ) then
          read(file_id,*) (istack(i),i=ii,num)
        end if
      end if
!
      ntot = istack(num)
!
      end subroutine read_arrays_for_stacks
!
!------------------------------------------------------------------
!
      subroutine read_arrays_for_stacks_b(file_id, num, istack_begin,   &
     &          ntot, istack)
!
      integer (kind = kint), intent(in) :: file_id
      integer (kind = kint), intent(in) :: num, istack_begin
      integer (kind = kint), intent(inout) :: ntot
      integer (kind = kint), intent(inout) :: istack(0:num)
!
!
      istack(0) = istack_begin
      if(num .gt. 0) read(file_id) istack(1:num)
      ntot = istack(num)
!
      end subroutine read_arrays_for_stacks_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_arrays_for_stacks(file_id, num, istack)
!
      integer (kind = kint), intent(in) :: file_id
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(in) :: istack(0:num)
!
!
      if(num .gt. 0) write(file_id,'(8i16)') istack(1:num)
!
      end subroutine write_arrays_for_stacks
!
!------------------------------------------------------------------
!
      subroutine write_arrays_for_stacks_b(file_id, num, istack)
!
      integer (kind = kint), intent(in) :: file_id
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(in) :: istack(0:num)
!
!
      if(num .gt. 0) write(file_id) istack(1:num)
!
      end subroutine write_arrays_for_stacks_b
!
!------------------------------------------------------------------
!
      end module stack_array_IO
