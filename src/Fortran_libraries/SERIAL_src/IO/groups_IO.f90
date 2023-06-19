!>@file  groups_IO.f90
!!       module groups_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Base routines for spectrum group data IO
!!
!!@verbatim
!!      subroutine read_group_data(id_file, grp_IO, iend)
!!      subroutine read_surf_grp_data(id_file, surf_grp_IO, iend)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(group_data), intent(inout) :: grp_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!        integer(kind = kint), intent(inout) :: iend
!!
!!      subroutine write_grp_data(id_file, grp_IO)
!!      subroutine write_surf_grp_data(id_file, surf_grp_IO)
!!        type(group_data), intent(in) :: grp_IO
!!        type(surface_group_data), intent(in) :: surf_grp_IO
!!@endverbatim
!
      module groups_IO
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use field_data_IO
      use group_data_IO
      use skip_comment_f
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_group_data(id_file, grp_IO, iend)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: id_file
      type(group_data), intent(inout) :: grp_IO
      integer(kind = kint), intent(inout) :: iend
!
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) grp_IO%num_grp
!
      call alloc_group_num(grp_IO)
!
      if (grp_IO%num_grp .gt. 0) then
        call read_group_stack(id_file, grp_IO%num_grp,                  &
     &      grp_IO%num_item, grp_IO%istack_grp, iend)
        if(iend .gt. 0) return
!
        call alloc_group_item(grp_IO)
        call read_group_item(id_file, grp_IO%num_grp,                   &
     &      grp_IO%num_item, grp_IO%istack_grp,                         &
     &      grp_IO%grp_name, grp_IO%item_grp, iend)
      else
        grp_IO%num_item = 0
        call alloc_group_item(grp_IO)
      end if
!
      end subroutine read_group_data
!
!------------------------------------------------------------------
!
      subroutine read_surf_grp_data(id_file, surf_grp_IO, iend)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: id_file
      type(surface_group_data), intent(inout) :: surf_grp_IO
      integer(kind = kint), intent(inout) :: iend
!
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) surf_grp_IO%num_grp
!
      call alloc_sf_group_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call read_group_stack(id_file, surf_grp_IO%num_grp,             &
     &      surf_grp_IO%num_item, surf_grp_IO%istack_grp, iend)
        if(iend .gt. 0) return
!
        call alloc_sf_group_item(surf_grp_IO)
        call read_surface_group_item(id_file, surf_grp_IO%num_grp,      &
     &      surf_grp_IO%num_item, surf_grp_IO%istack_grp,               &
     &      surf_grp_IO%grp_name, surf_grp_IO%item_sf_grp, iend)
      else
        call alloc_sf_group_item(surf_grp_IO)
      end if
!
      end subroutine read_surf_grp_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_grp_data(id_file, grp_IO)
!
      use m_sph_modes_grid_labels
      use t_group_data
!
      integer(kind = kint), intent(in) :: id_file
      type(group_data), intent(in) :: grp_IO
!
!
      call write_group_data(id_file, grp_IO%num_grp,                    &
     &    grp_IO%num_item, grp_IO%istack_grp,                           &
     &    grp_IO%grp_name, grp_IO%item_grp)
!
      end subroutine write_grp_data
!
!------------------------------------------------------------------
!
      subroutine write_surf_grp_data(id_file, surf_grp_IO)
!
      use m_sph_modes_grid_labels
      use t_group_data
!
      integer(kind = kint), intent(in) :: id_file
      type(surface_group_data), intent(in) :: surf_grp_IO
!
!
      call write_surf_group_data(id_file, surf_grp_IO%num_grp,          &
     &    surf_grp_IO%num_item, surf_grp_IO%istack_grp,                 &
     &    surf_grp_IO%grp_name, surf_grp_IO%item_sf_grp)
!
      end subroutine write_surf_grp_data
!
!------------------------------------------------------------------
!
      end module groups_IO
