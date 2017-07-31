!>@file  groups_IO.f90
!!       module groups_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Base routines for spectrum group data IO
!!
!!@verbatim
!!      subroutine read_group_datamesh_file_id, rj_grp_IO)
!!      subroutine read_surf_grp_data(id_file, surf_grp_IO)
!!
!!      subroutine write_grp_data(id_file, rj_grp_IO)
!!      subroutine write_surf_grp_data(id_file, surf_grp_IO)
!!        type(group_data), intent(inout) :: rj_grp_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
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
      subroutine read_group_data(id_file, rj_grp_IO)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: id_file
      type(group_data), intent(inout) :: rj_grp_IO
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) rj_grp_IO%num_grp
!
      call allocate_grp_type_num(rj_grp_IO)
!
      if (rj_grp_IO%num_grp .gt. 0) then
        call read_group_stack(id_file, rj_grp_IO%num_grp,               &
     &      rj_grp_IO%num_item, rj_grp_IO%istack_grp)
!
        call allocate_grp_type_item(rj_grp_IO)
        call read_group_item(id_file, rj_grp_IO%num_grp,                &
     &      rj_grp_IO%num_item, rj_grp_IO%istack_grp,                   &
     &      rj_grp_IO%grp_name, rj_grp_IO%item_grp)
!
      else
        rj_grp_IO%num_item = 0
        call allocate_grp_type_item(rj_grp_IO)
      end if
!
      end subroutine read_group_data
!
!------------------------------------------------------------------
!
      subroutine read_surf_grp_data(id_file, surf_grp_IO)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: id_file
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) surf_grp_IO%num_grp
!
      call allocate_sf_grp_type_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call read_group_stack(id_file, surf_grp_IO%num_grp,             &
     &      surf_grp_IO%num_item, surf_grp_IO%istack_grp)
!
        call allocate_sf_grp_type_item(surf_grp_IO)
        call read_surface_group_item(id_file, surf_grp_IO%num_grp,      &
     &      surf_grp_IO%num_item, surf_grp_IO%istack_grp,               &
     &      surf_grp_IO%grp_name, surf_grp_IO%item_sf_grp)
!
      else
        call allocate_sf_grp_type_item(surf_grp_IO)
      end if
!
      end subroutine read_surf_grp_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_grp_data(id_file, rj_grp_IO)
!
      use m_sph_modes_grid_labels
      use t_group_data
!
      integer(kind = kint), intent(in) :: id_file
      type(group_data), intent(inout) :: rj_grp_IO
!
!
      call write_group_data(id_file, rj_grp_IO%num_grp,                 &
     &    rj_grp_IO%num_item, rj_grp_IO%istack_grp,                     &
     &    rj_grp_IO%grp_name, rj_grp_IO%item_grp)
!
      call deallocate_grp_type(rj_grp_IO)
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
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
!
      call write_surf_group_data(id_file, surf_grp_IO%num_grp,          &
     &    surf_grp_IO%num_item, surf_grp_IO%istack_grp,                 &
     &    surf_grp_IO%grp_name, surf_grp_IO%item_sf_grp)
!
      call deallocate_sf_grp_type(surf_grp_IO)
!
      end subroutine write_surf_grp_data
!
!------------------------------------------------------------------
!
      end module groups_IO
