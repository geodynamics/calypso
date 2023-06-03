!>@file   t_work_for_comm_check.f90
!!@brief  module t_work_for_comm_check
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Sep., 2007
!
!>@brief  Work area for communication check by position
!!
!!@verbatim
!!      subroutine alloc_geom_4_comm_test(num, wk_check)
!!      subroutine dealloc_ele_4_comm_test(wk_check)
!!        type(work_for_comm_check), intent(inout) :: wk_check
!!
!!      subroutine alloc_diff_ele_comm_test(wk_check)
!!      subroutine dealloc_diff_ele_comm_test(wk_check)
!!        type(work_for_comm_check), intent(inout) :: wk_check
!!
!!      subroutine alloc_comm_stack_ctest_IO(wk_check)
!!      subroutine alloc_ele_comm_test_IO(wk_check)
!!      subroutine dealloc_ele_comm_test_IO(wk_check)
!!        type(work_for_comm_check), intent(inout) :: wk_check
!!@endverbatim
!
      module t_work_for_comm_check
!
      use m_precision
      use m_constants
!
      implicit  none
!
      type work_for_comm_check
        integer(kind = kint) :: nnod
        integer(kind = kint_gl), allocatable :: i_gl_test(:)
        real(kind = kreal), allocatable ::   xx_test(:)
!
        integer(kind = kint) :: num_diff
        integer(kind = kint), allocatable :: i_diff(:)
        real(kind = kreal), allocatable :: x_diff(:)
!
        integer(kind = kint_gl), allocatable :: istack_diff_pe(:)
        integer(kind = kint), allocatable :: i_diff_IO(:)
        real(kind = kreal), allocatable :: x_diff_IO(:)
      end type work_for_comm_check
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_geom_4_comm_test(num, wk_check)
!
      integer(kind = kint), intent(in) :: num
      type(work_for_comm_check), intent(inout) :: wk_check
!
!
      wk_check%nnod = num
      allocate(wk_check%i_gl_test(wk_check%nnod))
      allocate(wk_check%xx_test(3*wk_check%nnod))
!
      if(wk_check%nnod .gt. 0) wk_check%i_gl_test = 0.0d0
      if(wk_check%nnod .gt. 0) wk_check%xx_test =   0.0d0
!
      end subroutine alloc_geom_4_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_ele_4_comm_test(wk_check)
!
      type(work_for_comm_check), intent(inout) :: wk_check
!
      deallocate(wk_check%xx_test, wk_check%i_gl_test)
!
      end subroutine dealloc_ele_4_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_diff_ele_comm_test(wk_check)
!
      type(work_for_comm_check), intent(inout) :: wk_check
!
      allocate( wk_check%i_diff(wk_check%num_diff) )
      allocate( wk_check%x_diff(6*wk_check%num_diff) )
!
      if(wk_check%num_diff .le. 0) return
      wk_check%i_diff =     0
      wk_check%x_diff =     0.0d0
!
      end subroutine alloc_diff_ele_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_diff_ele_comm_test(wk_check)
!
      type(work_for_comm_check), intent(inout) :: wk_check
!
      deallocate(wk_check%i_diff, wk_check%x_diff)
!
      end subroutine dealloc_diff_ele_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_comm_stack_ctest_IO(wk_check)
!
      use calypso_mpi
!
      type(work_for_comm_check), intent(inout) :: wk_check
!
      allocate( wk_check%istack_diff_pe(0:nprocs)  )
      wk_check%istack_diff_pe =  0
!
      end subroutine alloc_comm_stack_ctest_IO
!
! ----------------------------------------------------------------------
!
      subroutine alloc_ele_comm_test_IO(wk_check)
!
      use calypso_mpi
!
      type(work_for_comm_check), intent(inout) :: wk_check
!
      allocate(wk_check%i_diff_IO(wk_check%istack_diff_pe(nprocs)))
      allocate(wk_check%x_diff_IO(6*wk_check%istack_diff_pe(nprocs)))
!
      if(wk_check%istack_diff_pe(nprocs) .le. 0) return
      wk_check%i_diff_IO =     0
      wk_check%x_diff_IO =     0.0d0
!
      end subroutine alloc_ele_comm_test_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_ele_comm_test_IO(wk_check)
!
      type(work_for_comm_check), intent(inout) :: wk_check
!
      deallocate(wk_check%i_diff_IO, wk_check%x_diff_IO)
      deallocate(wk_check%istack_diff_pe)
!
      end subroutine dealloc_ele_comm_test_IO
!
! ----------------------------------------------------------------------
!
      end module t_work_for_comm_check
