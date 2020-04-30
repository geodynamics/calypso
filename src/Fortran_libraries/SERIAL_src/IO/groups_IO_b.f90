!>@file  groups_IO_b.f90
!!       module groups_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in July, 2007
!
!> @brief Binary output routines for group data
!!
!!@verbatim
!!      subroutine read_group_data_b(bbuf, group_IO)
!!      subroutine read_surf_grp_data_b(bbuf, surf_grp_IO)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(group_data), intent(inout) :: group_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!
!!      subroutine write_grp_data_b(group_IO, bbuf)
!!      subroutine write_surf_grp_data_b(surf_grp_IO, bbuf)
!!        type(group_data), intent(in) :: group_IO
!!        type(surface_group_data), intent(in) :: surf_grp_IO
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module groups_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
      use t_binary_IO_buffer
!
      use binary_IO
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
      subroutine read_group_data_b(bbuf, group_IO)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(group_data), intent(inout) :: group_IO
!
!
      call read_one_integer_b(bbuf, group_IO%num_grp)
      if(bbuf%ierr_bin .gt. 0) return
!
      call alloc_group_num(group_IO)
!
      if (group_IO%num_grp .gt. 0) then
        call read_integer_stack_b(bbuf, cast_long(group_IO%num_grp),    &
     &      group_IO%istack_grp, group_IO%num_item)
        if(bbuf%ierr_bin .gt. 0) return
!
        call read_mul_character_b                                       &
     &     (bbuf, group_IO%num_grp, group_IO%grp_name)
        if(bbuf%ierr_bin .ne. 0) return
!
        call alloc_group_item(group_IO)
!
        call read_mul_integer_b                                         &
     &     (bbuf, cast_long(group_IO%num_item), group_IO%item_grp)
        if(bbuf%ierr_bin .gt. 0) return
      else
        group_IO%num_item = 0
        call alloc_group_item(group_IO)
      end if
!
      end subroutine read_group_data_b
!
!------------------------------------------------------------------
!
      subroutine read_surf_grp_data_b(bbuf, surf_grp_IO)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call read_one_integer_b(bbuf, surf_grp_IO%num_grp)
      if(bbuf%ierr_bin .gt. 0) return
!
      call alloc_sf_group_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call read_integer_stack_b                                       &
     &     (bbuf, cast_long(surf_grp_IO%num_grp),                       &
     &      surf_grp_IO%istack_grp, surf_grp_IO%num_item)
        if(bbuf%ierr_bin .gt. 0) return
!
        call read_mul_character_b                                       &
     &     (bbuf, surf_grp_IO%num_grp, surf_grp_IO%grp_name)
        if(bbuf%ierr_bin .ne. 0) return
!
        call alloc_sf_group_item(surf_grp_IO)
!
        num64 = 2 * surf_grp_IO%num_item
        call read_mul_integer_b(bbuf, num64, surf_grp_IO%item_sf_grp)
        if(bbuf%ierr_bin .gt. 0) return
      else
        call alloc_sf_group_item(surf_grp_IO)
      end if
!
      end subroutine read_surf_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_grp_data_b(group_IO, bbuf)
!
      type(group_data), intent(in) :: group_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_one_integer_b(group_IO%num_grp, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_integer_stack_b(cast_long(group_IO%num_grp),           &
     &    group_IO%istack_grp, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_mul_character_b                                        &
     &   (group_IO%num_grp, group_IO%grp_name, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_mul_integer_b(cast_long(group_IO%num_item),            &
     &    group_IO%item_grp, bbuf)
!
      end subroutine write_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine write_surf_grp_data_b(surf_grp_IO, bbuf)
!
      type(surface_group_data), intent(in) :: surf_grp_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: num64
!
!
      call write_one_integer_b(surf_grp_IO%num_grp, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_integer_stack_b(cast_long(surf_grp_IO%num_grp),        &
     &    surf_grp_IO%istack_grp, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_mul_character_b                                        &
     &   (surf_grp_IO%num_grp, surf_grp_IO%grp_name, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      num64 = 2 * surf_grp_IO%num_item
      call write_mul_integer_b(num64, surf_grp_IO%item_sf_grp, bbuf)
!
      end subroutine write_surf_grp_data_b
!
!------------------------------------------------------------------
!
      end module groups_IO_b
