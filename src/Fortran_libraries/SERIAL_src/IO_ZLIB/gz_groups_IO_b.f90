!>@file  gz_groups_IO_b.f90
!!       module gz_groups_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in July, 2007
!
!> @brief Binary output routines for group data
!!
!!@verbatim
!!      subroutine gz_read_group_data_b(FPz_f, zbuf, group_IO)
!!      subroutine gz_read_surf_grp_data_b(FPz_f, zbuf, surf_grp_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(group_data), intent(inout) :: group_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!
!!      subroutine gz_write_grp_data_b(FPz_f, group_IO, zbuf)
!!      subroutine gz_write_surf_grp_data_b(FPz_f, surf_grp_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(group_data), intent(in) :: group_IO
!!        type(surface_group_data), intent(in) :: surf_grp_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_groups_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
      use t_buffer_4_gzip
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
      subroutine gz_read_group_data_b(FPz_f, zbuf, group_IO)
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(group_data), intent(inout) :: group_IO
!
!
      call gz_read_one_integer_b(FPz_f, zbuf, group_IO%num_grp)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call alloc_group_num(group_IO)
!
      if (group_IO%num_grp .gt. 0) then
        call gz_read_integer_stack_b                                    &
     &     (FPz_f, zbuf, cast_long(group_IO%num_grp),                   &
     &      group_IO%istack_grp, group_IO%num_item)
        if(zbuf%ierr_zlib .ne. 0) return
!
        call gz_read_mul_character_b(FPz_f, zbuf,                       &
     &      cast_long(group_IO%num_grp), group_IO%grp_name)
        if(zbuf%ierr_zlib .ne. 0) return
!
        call alloc_group_item(group_IO)
!
        call gz_read_mul_integer_b(FPz_f, zbuf,                         &
     &      cast_long(group_IO%num_item), group_IO%item_grp)
        if(zbuf%ierr_zlib .ne. 0) return
      else
        group_IO%num_item = 0
        call alloc_group_item(group_IO)
      end if
!
      end subroutine gz_read_group_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_surf_grp_data_b(FPz_f, zbuf, surf_grp_IO)
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint_gl) :: nitem
!
!
      call gz_read_one_integer_b(FPz_f, zbuf, surf_grp_IO%num_grp)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call alloc_sf_group_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call gz_read_integer_stack_b                                    &
     &     (FPz_f, zbuf, cast_long(surf_grp_IO%num_grp),                &
     &      surf_grp_IO%istack_grp, surf_grp_IO%num_item)
        if(zbuf%ierr_zlib .ne. 0) return
!
        call gz_read_mul_character_b(FPz_f, zbuf,                       &
     &      cast_long(surf_grp_IO%num_grp), surf_grp_IO%grp_name)
        if(zbuf%ierr_zlib .ne. 0) return
!
        call alloc_sf_group_item(surf_grp_IO)
!
        nitem = 2 * surf_grp_IO%num_item
        call gz_read_mul_integer_b(FPz_f, zbuf,                         &
     &      nitem, surf_grp_IO%item_sf_grp)
        if(zbuf%ierr_zlib .ne. 0) return
      else
        call alloc_sf_group_item(surf_grp_IO)
      end if
!
      end subroutine gz_read_surf_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_grp_data_b(FPz_f, group_IO, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(group_data), intent(in) :: group_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_write_one_integer_b(FPz_f, group_IO%num_grp, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_integer_stack_b(FPz_f,                              &
     &    cast_long(group_IO%num_grp), group_IO%istack_grp, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_mul_character_b                                     &
     &   (FPz_f, cast_long(group_IO%num_grp), group_IO%grp_name, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, cast_long(group_IO%num_item), group_IO%item_grp, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_surf_grp_data_b(FPz_f, surf_grp_IO, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(surface_group_data), intent(in) :: surf_grp_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: nitem
!
!
      call gz_write_one_integer_b(FPz_f, surf_grp_IO%num_grp, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_integer_stack_b(FPz_f,                              &
     &    cast_long(surf_grp_IO%num_grp), surf_grp_IO%istack_grp, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_mul_character_b                                     &
     &   (FPz_f, cast_long(surf_grp_IO%num_grp),                        &
     &    surf_grp_IO%grp_name, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      nitem = 2 * surf_grp_IO%num_item
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, nitem, surf_grp_IO%item_sf_grp, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_surf_grp_data_b
!
!------------------------------------------------------------------
!
      end module gz_groups_IO_b
