!>@file  t_group_connects.f90
!!      module t_group_connects
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2008
!
!>@brief Structure of connectivity data for group items
!!
!!@verbatim
!!      subroutine alloc_num_other_grp(num_grp, e_grp)
!!      subroutine alloc_item_other_grp(e_grp)
!!
!!      subroutine dealloc_grp_connect(e_grp)
!!@endverbatim
!
      module t_group_connects
!
      use m_precision
!
      implicit  none
!
!>   Structure of connectivities for groups
      type group_connect_data
!>   total number of connectivity for group
        integer(kind=kint) :: ntot_e_grp
!>   number of item for each group
        integer(kind=kint), allocatable :: nitem_e_grp(:)
!>   end number of item for each group
        integer(kind=kint), allocatable :: istack_e_grp(:)
!
!>   local item ID for group
        integer(kind=kint), allocatable :: item_e_grp(:)
      end type group_connect_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_num_other_grp(num_grp, e_grp)
!
      integer(kind = kint), intent(in) :: num_grp
      type(group_connect_data), intent(inout) :: e_grp
!
!
      allocate(e_grp%nitem_e_grp(num_grp))
      allocate(e_grp%istack_e_grp(0:num_grp))
!
      if (num_grp .gt. 0) e_grp%nitem_e_grp = 0
      e_grp%istack_e_grp = 0
!
      end subroutine alloc_num_other_grp
!
!-----------------------------------------------------------------------
!
      subroutine alloc_item_other_grp(e_grp)
!
      type(group_connect_data), intent(inout) :: e_grp
!
      allocate(e_grp%item_e_grp(e_grp%ntot_e_grp))
      if (e_grp%ntot_e_grp .gt. 0) e_grp%item_e_grp = 0
!
      end subroutine alloc_item_other_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_grp_connect(e_grp)
!
      type(group_connect_data), intent(inout) :: e_grp
!←
!
      deallocate(e_grp%item_e_grp)
      deallocate(e_grp%nitem_e_grp )
      deallocate(e_grp%istack_e_grp)
!
      end subroutine dealloc_grp_connect
!
!-----------------------------------------------------------------------
!
      end module t_group_connects
