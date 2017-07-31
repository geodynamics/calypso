!gz_sph_rj_groups_IO.f90
!      module gz_sph_rj_groups_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_modes_rj_groups_gz
!      subroutine read_geom_rtp_groups_gz
!      subroutine write_modes_rj_groups_gz
!      subroutine write_geom_rtp_groups_gz
!
      module gz_sph_rj_groups_IO
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
      subroutine read_group_data_gz(rj_grp_IO)
!
      type(group_data), intent(inout) :: rj_grp_IO
!
!
      call skip_gz_comment_int(rj_grp_IO%num_grp)
      call allocate_grp_type_num(rj_grp_IO)
!
      if (rj_grp_IO%num_grp .gt. 0) then
        call read_gz_integer_stack(rj_grp_IO%num_grp,                   &
     &      rj_grp_IO%istack_grp, rj_grp_IO%num_item)
!
        call allocate_grp_type_item(rj_grp_IO)
        call read_group_item_gz(rj_grp_IO%num_grp,                      &
     &      rj_grp_IO%num_item, rj_grp_IO%istack_grp,                   &
     &      rj_grp_IO%grp_name, rj_grp_IO%item_grp)
!
      else
        call allocate_grp_type_item(rj_grp_IO)
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
      call allocate_sf_grp_type_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call read_gz_integer_stack(surf_grp_IO%num_grp,                 &
     &      surf_grp_IO%istack_grp, surf_grp_IO%num_item)
!
        call allocate_sf_grp_type_item(surf_grp_IO)
        call read_surface_group_item_gz(surf_grp_IO%num_grp,            &
     &      surf_grp_IO%num_item, surf_grp_IO%istack_grp,               &
     &      surf_grp_IO%grp_name, surf_grp_IO%item_sf_grp)
!
      else
        call allocate_sf_grp_type_item(surf_grp_IO)
      end if
!
      end subroutine read_surf_grp_data_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_grp_data_gz(rj_grp_IO)
!
      type(group_data), intent(inout) :: rj_grp_IO
!
!
      call write_group_data_gz(rj_grp_IO%num_grp,                       &
     &    rj_grp_IO%num_item, rj_grp_IO%istack_grp,                     &
     &    rj_grp_IO%grp_name, rj_grp_IO%item_grp)
!
      call deallocate_grp_type(rj_grp_IO)
!
      end subroutine write_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_surf_grp_data_gz(surf_grp_IO)
!
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
!
      call write_surf_group_data_gz(surf_grp_IO%num_grp,                &
     &    surf_grp_IO%num_item, surf_grp_IO%istack_grp,                 &
     &    surf_grp_IO%grp_name, surf_grp_IO%item_sf_grp)
!
      call deallocate_sf_grp_type(surf_grp_IO)
!
      end subroutine write_surf_grp_data_gz
!
!------------------------------------------------------------------
!
      end module gz_sph_rj_groups_IO
