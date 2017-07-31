!>@file   cal_minmax_and_stacks.f90
!!@brief  module cal_minmax_and_stacks
!!
!!@author H. Matsui
!!@date Programmed. 2005
!
!>@brief  Set end points of listed data
!!
!!@verbatim
!!      subroutine cal_divide_and_rest(i_quot, i_rest, i_org, i_divide)
!!      subroutine set_number_of_segments(i_divide, i_quot, i_rest,     &
!!     &          num_l)
!!      subroutine set_stack_of_segments(i_divide, i_quot, i_rest,      &
!!     &          istart, istack)
!!
!!      subroutine s_cal_numbers_from_stack(num, num_grp, istack_grp)
!!
!!      subroutine s_cal_total_and_stacks(num, num_grp, istack_begin,   &
!!     &          istack_grp, ntot_grp)
!!      subroutine s_cal_minmax_and_stacks(num, num_grp, istack_begin,  &
!!     &          istack_grp, ntot_grp, max_grp, min_grp)
!!      subroutine s_cal_dbl_minmax_and_stacks(num_out, num_in, num_grp,&
!!     &          istack_begin, istack_grp, ntot_grp, max_grp, min_grp)
!!
!!      subroutine count_number_4_smp( np_smp, istart, iend,            &
!!     &        i_smp_stack, max_4_smp)
!!      subroutine set_group_size_4_smp(np_smp, num_group,              &
!!     &          istack_group, istack_group_smp, max_4_smp)
!!@endverbatim
!!
!!@n @param  i_quot     quotient
!!@n @param  i_rest     rest of division
!!@n @param  i_org      Input integer
!!@n @param  i_divide   Number to devide
!!
!
      module cal_minmax_and_stacks
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_divide_and_rest(i_quot, i_rest, i_org, i_divide)
!
      integer(kind= kint), intent(in) :: i_org, i_divide
      integer(kind= kint), intent(inout) :: i_rest, i_quot
!
      i_rest = mod(i_org,i_divide)
      i_quot = (i_org-i_rest) / i_divide
!
      end subroutine cal_divide_and_rest
!
!   --------------------------------------------------------------------
!
      subroutine set_number_of_segments(i_divide, i_quot, i_rest,       &
     &          num_l)
!
      integer(kind= kint), intent(in) :: i_divide, i_rest, i_quot
      integer(kind= kint), intent(inout) :: num_l(i_divide)
!
      integer(kind = kint) :: inum
!
!
      do inum = 1, i_rest
        num_l(inum) =    i_quot + 1
      end do
      do inum = i_rest+1, i_divide
        num_l(inum) = i_quot
      end do
!
      end subroutine set_number_of_segments
!
!   --------------------------------------------------------------------
!
      subroutine set_stack_of_segments(i_divide, i_quot, i_rest,        &
     &          istart, istack)
!
      integer(kind= kint), intent(in) :: i_divide, i_rest
      integer(kind= kint), intent(in) :: i_quot, istart
      integer(kind= kint), intent(inout) :: istack(0:i_divide)
!
      integer(kind = kint) :: inum
!
!
      istack(0) = istart - 1
      do inum = 1, i_rest
        istack(inum) = istack(inum-1) + i_quot + 1
      end do
      do inum = i_rest+1, i_divide
        istack(inum) = istack(inum-1) + i_quot
      end do
!
      end subroutine set_stack_of_segments
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine s_cal_numbers_from_stack(num, num_grp, istack_grp)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: istack_grp(0:num)
!
      integer(kind = kint), intent(inout) :: num_grp(num)
!
      integer(kind = kint) :: inum
!
!
      do inum = 1, num
        num_grp(inum) = istack_grp(inum) - istack_grp(inum-1)
      end do
!
      end subroutine s_cal_numbers_from_stack
!
!   --------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_cal_total_and_stacks(num, num_grp, istack_begin,     &
     &          istack_grp, ntot_grp)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: istack_begin
      integer(kind = kint), intent(in) :: num_grp(num)
!
      integer(kind = kint), intent(inout) :: ntot_grp
      integer(kind = kint), intent(inout) :: istack_grp(0:num)
!
      integer(kind = kint) :: inum
!
!
      istack_grp(0) = istack_begin
      do inum = 1, num
        istack_grp(inum) = istack_grp(inum-1) + num_grp(inum)
      end do
      ntot_grp = istack_grp(num)
!
      end subroutine s_cal_total_and_stacks
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_minmax_and_stacks(num, num_grp, istack_begin,    &
     &          istack_grp, ntot_grp, max_grp, min_grp)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: istack_begin
      integer(kind = kint), intent(in) :: num_grp(num)
!
      integer(kind = kint), intent(inout) :: ntot_grp
      integer(kind = kint), intent(inout) :: max_grp, min_grp
      integer(kind = kint), intent(inout) :: istack_grp(0:num)
!
      integer(kind = kint) :: inum
!
!
      max_grp = 0
      istack_grp(0) = istack_begin
      do inum = 1, num
        istack_grp(inum) = istack_grp(inum-1) + num_grp(inum)
        max_grp = max(max_grp,num_grp(inum))
      end do
      ntot_grp = istack_grp(num)
!
      min_grp = max_grp
      do inum = 1, num
        min_grp = min(min_grp,num_grp(inum))
      end do
!
      end subroutine s_cal_minmax_and_stacks
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_dbl_minmax_and_stacks(num_out, num_in, num_grp,  &
     &          istack_begin, istack_grp, ntot_grp, max_grp, min_grp)
!
      integer(kind = kint), intent(in) :: num_in, num_out
      integer(kind = kint), intent(in) :: istack_begin
      integer(kind = kint), intent(in) :: num_grp(num_in*num_out)
!
      integer(kind = kint), intent(inout) :: ntot_grp
      integer(kind = kint), intent(inout) :: max_grp, min_grp
      integer(kind = kint), intent(inout)                               &
     &      :: istack_grp(0:num_in*num_out)
!
      integer(kind = kint) :: ntot_tmp, max_tmp, min_tmp
      integer(kind = kint) :: inum, ist, istart_tmp
!
!
      istack_grp(0) = istack_begin
      do inum = 1, num_out
        ist = num_in*(inum-1)
        istart_tmp =  istack_grp(ist)
!
        call s_cal_minmax_and_stacks(num_in, num_grp(ist+1),            &
     &      istart_tmp, istack_grp(ist), ntot_tmp, max_tmp, min_tmp)
!
        if(inum.eq.1) then
          max_grp = max_tmp
          min_grp = min_tmp
        else
          max_grp = max(max_grp, max_tmp)
          min_grp = min(min_grp, min_tmp)
        end if
!
      end do
      ntot_grp = istack_grp(num_in*num_out)
!
      end subroutine s_cal_dbl_minmax_and_stacks
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_number_4_smp( np_smp, istart, iend,              &
     &        i_smp_stack, max_4_smp)
!
      integer(kind = kint), intent(in)  :: np_smp
      integer(kind = kint), intent(in)  :: istart, iend
      integer(kind = kint), intent(inout) :: i_smp_stack(0:np_smp)
      integer(kind = kint), intent(inout) :: max_4_smp
!
      integer(kind = kint) :: num
      integer(kind = kint)  :: irest, i8
!
       num = iend - istart + 1
       call cal_divide_and_rest(i8, irest, num, np_smp)
       max_4_smp = i8 + 1
!
       call set_stack_of_segments(np_smp, i8, irest, istart,            &
     &     i_smp_stack)
!
       end subroutine count_number_4_smp
!
!  ---------------------------------------------------------------------
!
      subroutine set_group_size_4_smp(np_smp, num_group,                &
     &          istack_group, istack_group_smp, max_4_smp)
!
      integer(kind = kint), intent(in) :: np_smp, num_group
      integer(kind = kint), intent(in) :: istack_group(0:num_group)
      integer(kind = kint), intent(inout) :: max_4_smp
      integer(kind = kint), intent(inout) ::                            &
     &                     istack_group_smp(0:np_smp*num_group)
!
      integer(kind = kint) :: i_grp, id_smp, ist, ied, imax
!
      max_4_smp = 0
      do i_grp = 1, num_group
        id_smp = np_smp*(i_grp-1)
        ist = istack_group(i_grp-1) + 1
        ied = istack_group(i_grp  )
        call count_number_4_smp(np_smp, ist, ied,                       &
     &       istack_group_smp(id_smp), imax )
        max_4_smp = max(max_4_smp,imax)
      end do
!
      end subroutine set_group_size_4_smp
!
!-----------------------------------------------------------------------
!
      end module cal_minmax_and_stacks
