!>@file  gz_groups_IO_b.f90
!!       module gz_groups_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in July, 2007
!
!> @brief Binary output routines for group data
!!
!!@verbatim
!!      subroutine gz_read_group_data_b(group_IO)
!!      subroutine gz_read_surf_grp_data_b(surf_grp_IO)
!!
!!      subroutine gz_write_grp_data_b(group_IO)
!!      subroutine gz_write_surf_grp_data_b(surf_grp_IO)
!!        type(group_data), intent(inout) :: group_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!@endverbatim
!
      module gz_groups_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
!
      use gz_binary_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_group_data_b(group_IO)
!
      type(group_data), intent(inout) :: group_IO
!
!
      call gz_read_one_integer_b(group_IO%num_grp)
      call allocate_grp_type_num(group_IO)
!
      if (group_IO%num_grp .gt. 0) then
        call gz_read_integer_stack_b(group_IO%num_grp,                  &
     &      group_IO%istack_grp, group_IO%num_item)
        call gz_read_mul_character_b                                    &
     &     (group_IO%num_grp, group_IO%grp_name)
!
        call allocate_grp_type_item(group_IO)
!
        call gz_read_mul_integer_b                                      &
     &     (group_IO%num_item, group_IO%item_grp)
      else
        group_IO%num_item = 0
        call allocate_grp_type_item(group_IO)
      end if
!
      end subroutine gz_read_group_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_surf_grp_data_b(surf_grp_IO)
!
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint) :: nitem
!
!
      call gz_read_one_integer_b(surf_grp_IO%num_grp)
      call allocate_sf_grp_type_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call gz_read_integer_stack_b(surf_grp_IO%num_grp,               &
     &      surf_grp_IO%istack_grp, surf_grp_IO%num_item)
        call gz_read_mul_character_b                                    &
     &     (surf_grp_IO%num_grp, surf_grp_IO%grp_name)
!
        call allocate_sf_grp_type_item(surf_grp_IO)
!
        nitem = 2 * surf_grp_IO%num_item
        call gz_read_mul_integer_b(nitem, surf_grp_IO%item_sf_grp)
      else
        call allocate_sf_grp_type_item(surf_grp_IO)
      end if
!
      end subroutine gz_read_surf_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_grp_data_b(group_IO)
!
      type(group_data), intent(inout) :: group_IO
!
!
      call gz_write_one_integer_b(group_IO%num_grp)
      call gz_write_integer_stack_b                                     &
     &  (group_IO%num_grp, group_IO%istack_grp)
      call gz_write_mul_character_b                                     &
     &   (group_IO%num_grp, group_IO%grp_name)
      call gz_write_mul_integer_b                                       &
     &   (group_IO%num_item, group_IO%item_grp)
!
      call deallocate_grp_type(group_IO)
!
      end subroutine gz_write_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_surf_grp_data_b(surf_grp_IO)
!
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint) :: nitem
!
!
      call gz_write_one_integer_b(surf_grp_IO%num_grp)
      call gz_write_integer_stack_b                                     &
     &   (surf_grp_IO%num_grp, surf_grp_IO%istack_grp)
      call gz_write_mul_character_b                                     &
     &   (surf_grp_IO%num_grp, surf_grp_IO%grp_name)
!
      nitem = 2 * surf_grp_IO%num_item
      call gz_write_mul_integer_b(nitem, surf_grp_IO%item_sf_grp)
!
      call deallocate_sf_grp_type(surf_grp_IO)
!
      end subroutine gz_write_surf_grp_data_b
!
!------------------------------------------------------------------
!
      end module gz_groups_IO_b
