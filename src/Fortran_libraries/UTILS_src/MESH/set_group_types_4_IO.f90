!set_group_types_4_IO.f90
!     module set_group_types_4_IO
!
!      written by H. Matsui on Dec., 2008
!
!!      subroutine set_gruop_stracture(org_group, new_group)
!!        type(group_data), intent(in) :: org_group
!!        type(group_data), intent(inout) :: new_group
!!      subroutine set_surf_grp_stracture(org_sf_grp, new_sf_grp)
!!        type(surface_group_data), intent(in) :: org_sf_grp
!!        type(surface_group_data), intent(inout) :: new_sf_grp
!
      module set_group_types_4_IO
!
      use m_precision
!
      use t_group_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_gruop_stracture(org_group, new_group)
!
      type(group_data), intent(in) :: org_group
      type(group_data), intent(inout) :: new_group
!
      integer(kind = kint) :: igrp
!
!
      new_group%num_grp =  org_group%num_grp
      call allocate_grp_type_num(new_group)
!
      do igrp = 1, new_group%num_grp
        new_group%grp_name(igrp) =   org_group%grp_name(igrp)
        new_group%istack_grp(igrp) = org_group%istack_grp(igrp)
        new_group%nitem_grp(igrp) =  org_group%istack_grp(igrp)         &
     &                             - org_group%istack_grp(igrp-1)
      end do
!
      new_group%num_item = org_group%num_item
!
      if (new_group%num_item .gt. 0) then
        call allocate_grp_type_item(new_group)
        new_group%item_grp(1:new_group%num_item)                        &
     &        = org_group%item_grp(1:new_group%num_item)
      else
        call allocate_grp_type_item(new_group)
      end if
!
      end subroutine set_gruop_stracture
!
! -----------------------------------------------------------------------
!
      subroutine set_surf_grp_stracture(org_sf_grp, new_sf_grp)
!
      type(surface_group_data), intent(in) :: org_sf_grp
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: igrp
!
!
      new_sf_grp%num_grp = org_sf_grp%num_grp
      call allocate_sf_grp_type_num(new_sf_grp)
!
      do igrp = 1, new_sf_grp%num_grp
        new_sf_grp%grp_name(igrp) =   org_sf_grp%grp_name(igrp)
        new_sf_grp%istack_grp(igrp) = org_sf_grp%istack_grp(igrp)
        new_sf_grp%nitem_grp(igrp) =  org_sf_grp%istack_grp(igrp)       &
     &                            - org_sf_grp%istack_grp(igrp-1)
      end do
!
      new_sf_grp%num_item = org_sf_grp%num_item
      call allocate_sf_grp_type_item(new_sf_grp)
!
      if (new_sf_grp%num_item .gt. 0) then
        new_sf_grp%item_sf_grp(1,1:new_sf_grp%num_item)                 &
     &        = org_sf_grp%item_sf_grp(1,1:new_sf_grp%num_item)
        new_sf_grp%item_sf_grp(2,1:new_sf_grp%num_item)                 &
     &        = org_sf_grp%item_sf_grp(2,1:new_sf_grp%num_item)
      end if
!
      end subroutine set_surf_grp_stracture
!
!-----------------------------------------------------------------------
!
      end module set_group_types_4_IO
