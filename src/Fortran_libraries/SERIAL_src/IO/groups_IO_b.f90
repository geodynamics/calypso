!>@file  groups_IO_b.f90
!!       module groups_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in July, 2007
!
!> @brief Binary output routines for group data
!!
!!@verbatim
!!      subroutine read_group_data_b(group_IO)
!!      subroutine read_surf_grp_data_b(surf_grp_IO)
!!
!!      subroutine write_grp_data_b(group_IO)
!!      subroutine write_surf_grp_data_b(surf_grp_IO)
!!        type(group_data), intent(inout) :: group_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!@endverbatim
!
      module groups_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
!
      use binary_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_group_data_b(group_IO)
!
      type(group_data), intent(inout) :: group_IO
!
!
      call read_one_integer_b(group_IO%num_grp)
      call allocate_grp_type_num(group_IO)
!
      if (group_IO%num_grp .gt. 0) then
        call read_integer_stack_b(group_IO%num_grp,                     &
     &      group_IO%istack_grp, group_IO%num_item)
        call read_mul_character_b                                       &
     &     (group_IO%num_grp, group_IO%grp_name)
!
        call allocate_grp_type_item(group_IO)
!
        call read_mul_integer_b                                         &
     &     (group_IO%num_item, group_IO%item_grp)
      else
        group_IO%num_item = 0
        call allocate_grp_type_item(group_IO)
      end if
!
      end subroutine read_group_data_b
!
!------------------------------------------------------------------
!
      subroutine read_surf_grp_data_b(surf_grp_IO)
!
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint) :: nitem
!
!
      call read_one_integer_b(surf_grp_IO%num_grp)
      call allocate_sf_grp_type_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call read_integer_stack_b(surf_grp_IO%num_grp,                  &
     &      surf_grp_IO%istack_grp, surf_grp_IO%num_item)
        call read_mul_character_b                                       &
     &     (surf_grp_IO%num_grp, surf_grp_IO%grp_name)
!
        call allocate_sf_grp_type_item(surf_grp_IO)
!
        nitem = 2 * surf_grp_IO%num_item
        call read_mul_integer_b(nitem, surf_grp_IO%item_sf_grp)
      else
        call allocate_sf_grp_type_item(surf_grp_IO)
      end if
!
      end subroutine read_surf_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_grp_data_b(group_IO)
!
      type(group_data), intent(inout) :: group_IO
!
!
      call write_one_integer_b(group_IO%num_grp)
      call write_integer_stack_b(group_IO%num_grp, group_IO%istack_grp)
      call write_mul_character_b                                        &
     &   (group_IO%num_grp, group_IO%grp_name)
      call write_mul_integer_b(group_IO%num_item, group_IO%item_grp)
!
      call deallocate_grp_type(group_IO)
!
      end subroutine write_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine write_surf_grp_data_b(surf_grp_IO)
!
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint) :: nitem
!
!
      call write_one_integer_b(surf_grp_IO%num_grp)
      call write_integer_stack_b                                        &
     &   (surf_grp_IO%num_grp, surf_grp_IO%istack_grp)
      call write_mul_character_b                                        &
     &   (surf_grp_IO%num_grp, surf_grp_IO%grp_name)
!
      nitem = 2 * surf_grp_IO%num_item
      call write_mul_integer_b(nitem, surf_grp_IO%item_sf_grp)
!
      call deallocate_sf_grp_type(surf_grp_IO)
!
      end subroutine write_surf_grp_data_b
!
!------------------------------------------------------------------
!
      end module groups_IO_b
