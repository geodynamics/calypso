!>@file  gz_groups_IO_b.f90
!!       module gz_groups_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in July, 2007
!
!> @brief Binary output routines for group data
!!
!!@verbatim
!!      subroutine gz_read_group_data_b(bflag, group_IO)
!!      subroutine gz_read_surf_grp_data_b(bflag, surf_grp_IO)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(group_data), intent(inout) :: group_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!
!!      subroutine gz_write_grp_data_b(group_IO, bflag)
!!      subroutine gz_write_surf_grp_data_b(surf_grp_IO, bflag)
!!        type(group_data), intent(in) :: group_IO
!!        type(surface_group_data), intent(in) :: surf_grp_IO
!!        type(binary_IO_flags), intent(inout) :: bflag
!!@endverbatim
!
      module gz_groups_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
!
      use binary_IO
      use gz_binary_IO
      use transfer_to_long_integers
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_group_data_b(bflag, group_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(group_data), intent(inout) :: group_IO
!
!
      call gz_read_one_integer_b(bflag, group_IO%num_grp)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_group_num(group_IO)
!
      if (group_IO%num_grp .gt. 0) then
        call gz_read_integer_stack_b                                    &
     &     (bflag, cast_long(group_IO%num_grp),                         &
     &      group_IO%istack_grp, group_IO%num_item)
        if(bflag%ierr_IO .ne. 0) return
!
        call gz_read_mul_character_b                                    &
     &     (bflag, cast_long(group_IO%num_grp), group_IO%grp_name)
        if(bflag%ierr_IO .ne. 0) return
!
        call alloc_group_item(group_IO)
!
        call gz_read_mul_integer_b                                      &
     &     (bflag, cast_long(group_IO%num_item), group_IO%item_grp)
        if(bflag%ierr_IO .ne. 0) return
      else
        group_IO%num_item = 0
        call alloc_group_item(group_IO)
      end if
!
      end subroutine gz_read_group_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_surf_grp_data_b(bflag, surf_grp_IO)
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint_gl) :: nitem
!
!
      call gz_read_one_integer_b(bflag, surf_grp_IO%num_grp)
      if(bflag%ierr_IO .ne. 0) return
!
      call alloc_sf_group_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call gz_read_integer_stack_b                                    &
     &     (bflag, cast_long(surf_grp_IO%num_grp),                      &
     &      surf_grp_IO%istack_grp, surf_grp_IO%num_item)
        if(bflag%ierr_IO .ne. 0) return
!
        call gz_read_mul_character_b(bflag,                             &
     &      cast_long(surf_grp_IO%num_grp), surf_grp_IO%grp_name)
        if(bflag%ierr_IO .ne. 0) return
!
        call alloc_sf_group_item(surf_grp_IO)
!
        nitem = 2 * surf_grp_IO%num_item
        call gz_read_mul_integer_b                                      &
     &     (bflag, nitem, surf_grp_IO%item_sf_grp)
        if(bflag%ierr_IO .ne. 0) return
      else
        call alloc_sf_group_item(surf_grp_IO)
      end if
!
      end subroutine gz_read_surf_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_grp_data_b(group_IO, bflag)
!
      type(group_data), intent(in) :: group_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call gz_write_one_integer_b(group_IO%num_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_integer_stack_b                                     &
     &   (cast_long(group_IO%num_grp), group_IO%istack_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_mul_character_b                                     &
     &   (cast_long(group_IO%num_grp), group_IO%grp_name, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      call gz_write_mul_integer_b                                       &
     &   (cast_long(group_IO%num_item), group_IO%item_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_surf_grp_data_b(surf_grp_IO, bflag)
!
      type(surface_group_data), intent(in) :: surf_grp_IO
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: nitem
!
!
      call gz_write_one_integer_b(surf_grp_IO%num_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_integer_stack_b(cast_long(surf_grp_IO%num_grp),     &
     &    surf_grp_IO%istack_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call gz_write_mul_character_b                                     &
     &   (cast_long(surf_grp_IO%num_grp), surf_grp_IO%grp_name, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      nitem = 2 * surf_grp_IO%num_item
      call gz_write_mul_integer_b                                       &
     &   (nitem, surf_grp_IO%item_sf_grp, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine gz_write_surf_grp_data_b
!
!------------------------------------------------------------------
!
      end module gz_groups_IO_b
