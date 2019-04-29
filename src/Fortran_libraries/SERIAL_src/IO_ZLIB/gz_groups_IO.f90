!>@file   gz_groups_IO.f90
!!@brief  module gz_groups_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Routine for group data IO using zlib
!!
!!@verbatim
!!      subroutine read_group_data_gz(grp_IO)
!!      subroutine read_surf_grp_data_gz(surf_grp_IO)
!!        type(group_data), intent(inout) :: grp_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!
!!      subroutine write_grp_data_gz(grp_IO)
!!      subroutine write_surf_grp_data_gz(surf_grp_IO)
!!        type(group_data), intent(in) :: grp_IO
!!        type(surface_group_data), intent(in) :: surf_grp_IO
!!@endverbatim
!
      module gz_groups_IO
!
      use m_precision
!
      use t_group_data
!
      use gz_group_data_IO
      use skip_gz_comment
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_group_data_gz(grp_IO)
!
      type(group_data), intent(inout) :: grp_IO
!
!
      call skip_gz_comment_int(grp_IO%num_grp)
      call alloc_group_num(grp_IO)
!
      if (grp_IO%num_grp .gt. 0) then
        call read_gz_integer_stack(grp_IO%num_grp,                      &
     &      grp_IO%istack_grp, grp_IO%num_item)
!
        call alloc_group_item(grp_IO)
        call read_group_item_gz(grp_IO%num_grp,                         &
     &      grp_IO%num_item, grp_IO%istack_grp,                         &
     &      grp_IO%grp_name, grp_IO%item_grp)
!
      else
        call alloc_group_item(grp_IO)
      end if
!
      end subroutine read_group_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_surf_grp_data_gz(surf_grp_IO)
!
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
!
      call skip_gz_comment_int(surf_grp_IO%num_grp)
      call alloc_sf_group_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call read_gz_integer_stack(surf_grp_IO%num_grp,                 &
     &      surf_grp_IO%istack_grp, surf_grp_IO%num_item)
!
        call alloc_sf_group_item(surf_grp_IO)
        call read_surface_group_item_gz(surf_grp_IO%num_grp,            &
     &      surf_grp_IO%num_item, surf_grp_IO%istack_grp,               &
     &      surf_grp_IO%grp_name, surf_grp_IO%item_sf_grp)
!
      else
        call alloc_sf_group_item(surf_grp_IO)
      end if
!
      end subroutine read_surf_grp_data_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_grp_data_gz(grp_IO)
!
      type(group_data), intent(in) :: grp_IO
!
!
      call write_group_data_gz(grp_IO%num_grp, grp_IO%num_item,         &
     &    grp_IO%istack_grp, grp_IO%grp_name, grp_IO%item_grp)
!
      end subroutine write_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_surf_grp_data_gz(surf_grp_IO)
!
      type(surface_group_data), intent(in) :: surf_grp_IO
!
!
      call write_surf_group_data_gz(surf_grp_IO%num_grp,                &
     &    surf_grp_IO%num_item, surf_grp_IO%istack_grp,                 &
     &    surf_grp_IO%grp_name, surf_grp_IO%item_sf_grp)
!
      end subroutine write_surf_grp_data_gz
!
!------------------------------------------------------------------
!
      end module gz_groups_IO
