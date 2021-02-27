!>@file   t_failed_export_list.f90
!!@brief  module t_failed_export_list
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Routines to construca element communication table
!!
!!@verbatim
!!      subroutine set_failed_export(inum, item_export_e, dist_min,     &
!!     &                             fail_comm)
!!        type(failed_item), intent(inout) :: fail_comm
!!
!!      subroutine alloc_failed_export(num, fail_tbl)
!!      subroutine dealloc_failed_export(fail_tbl)
!!      subroutine append_failed_export(fail_comm_in, fail_tbl)
!!        type(failed_item), intent(in) :: fail_comm_in
!!        type(failed_table), intent(inout) :: fail_tbl
!!@endverbatim
!!
      module t_failed_export_list
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type failed_item
        integer(kind = kint) :: id_failed
        integer(kind = kint) :: item_fail
        real(kind = kreal) ::   dist_fail
      end type failed_item
!
      type failed_table
        integer(kind = kint) :: num_fail
        type(failed_item), allocatable :: fail_comm(:)
      end type failed_table
!
      private :: copy_failed_export
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_failed_export(inum, item_export_e, dist_min,       &
     &                             fail_comm)
!
      integer(kind = kint), intent(in) :: inum
      integer(kind = kint), intent(in) :: item_export_e
      real(kind = kreal), intent(in) ::    dist_min
!
      type(failed_item), intent(inout) :: fail_comm
!
!
      fail_comm%id_failed = inum
      fail_comm%item_fail = item_export_e
      fail_comm%dist_fail = dist_min
!
      end subroutine set_failed_export
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_failed_export(num, fail_tbl)
!
      integer(kind = kint), intent(in) :: num
      type(failed_table), intent(inout) :: fail_tbl
!
      fail_tbl%num_fail = num
      allocate(fail_tbl%fail_comm(fail_tbl%num_fail))
!
      end subroutine alloc_failed_export
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_failed_export(fail_tbl)
!
      type(failed_table), intent(inout) :: fail_tbl
!
      deallocate(fail_tbl%fail_comm)
!
      end subroutine dealloc_failed_export
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine append_failed_export(fail_comm_in, fail_tbl)
!
      type(failed_item), intent(in) :: fail_comm_in
      type(failed_table), intent(inout) :: fail_tbl
!
      integer(kind = kint) :: i
      type(failed_table) :: fail_tmp
!
!
      call alloc_failed_export(fail_tbl%num_fail, fail_tmp)
      do i = 1, fail_tbl%num_fail
        call copy_failed_export(fail_tbl%fail_comm(i),                  &
     &                          fail_tmp%fail_comm(i))
      end do
      call dealloc_failed_export(fail_tbl)
!
      call alloc_failed_export((fail_tbl%num_fail+1), fail_tbl)
      do i = 1, fail_tbl%num_fail-1
        call copy_failed_export(fail_tmp%fail_comm(i),                  &
     &                          fail_tbl%fail_comm(i))
      end do
      call dealloc_failed_export(fail_tmp)
!
      call copy_failed_export                                           &
     &   (fail_comm_in, fail_tbl%fail_comm(fail_tbl%num_fail))
!
      end subroutine append_failed_export
!
!-----------------------------------------------------------------------
!
      subroutine copy_failed_export(org_fail_comm, new_fail_comm)
!
      type(failed_item), intent(in) :: org_fail_comm
      type(failed_item), intent(inout) :: new_fail_comm
!
      new_fail_comm%id_failed = org_fail_comm%id_failed
      new_fail_comm%item_fail = org_fail_comm%item_fail
      new_fail_comm%dist_fail = org_fail_comm%dist_fail
!
      end subroutine copy_failed_export
!
!-----------------------------------------------------------------------
!
      end module t_failed_export_list
