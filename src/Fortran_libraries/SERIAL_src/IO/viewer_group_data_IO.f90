!
!      module viewer_group_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine write_domain_group_viewer(num_pe, domain_grps)
!!        type(viewer_surface_groups), intent(in) :: domain_grps
!!
!!      subroutine write_nod_group_viewer(num_pe, view_nod_grps)
!!        type(viewer_node_groups), intent(in) :: view_nod_grps
!!      subroutine write_ele_group_viewer(num_pe, view_ele_grps)
!!        type(viewer_surface_groups), intent(in) :: view_ele_grps
!!      subroutine write_surf_group_viewer(num_pe, view_sf_grps)
!!        type(viewer_surface_groups), intent(in) :: view_sf_grps
!
      module viewer_group_data_IO
!
      use m_precision
      use m_constants
!
      use t_viewer_mesh
      use t_viewer_group
      use m_viewer_mesh_labels
!
      implicit none
!
      private :: write_viewer_group_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_domain_group_viewer(num_pe, domain_grps)
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(in) :: domain_grps
!
      integer(kind = kint) :: ip, ist, ied
!
!
      write(surface_id,'(a)', advance='NO') hd_domain_nod_grp()
!
      write(surface_id,'(i16)') domain_grps%node_grp%num_item
      write(surface_id,'(8i16)')                                        &
     &   domain_grps%node_grp%istack_sf(1:num_pe)
      do ip = 1, num_pe
        ist = domain_grps%node_grp%istack_sf(ip-1) + 1
        ied = domain_grps%node_grp%istack_sf(ip)
        if(ied .ge. ist) write(surface_id,'(8i16)')                     &
     &                  domain_grps%node_grp%item_sf(ist:ied)
      end do
!
      write(surface_id,'(a)', advance='NO') hd_domain_surf_grp()
!
      write(surface_id,'(i16)') domain_grps%surf_grp%num_item
      write(surface_id,'(8i16)')                                        &
     &   domain_grps%surf_grp%istack_sf(1:num_pe)
!
      do ip = 1, num_pe
        ist = domain_grps%surf_grp%istack_sf(ip-1) + 1
        ied = domain_grps%surf_grp%istack_sf(ip)
        if(ied .ge. ist) write(surface_id,'(8i16)')                     &
     &                  domain_grps%surf_grp%item_sf(ist:ied)
      end do
!
      write(surface_id,'(a)', advance='NO') hd_domain_edge_grp()
!
!
      write(surface_id,'(i16)') domain_grps%edge_grp%num_item
      write(surface_id,'(8i16)')                                        &
     &     domain_grps%edge_grp%istack_sf(1:num_pe)
!
      do ip = 1, num_pe
        ist = domain_grps%edge_grp%istack_sf(ip-1) + 1
        ied = domain_grps%edge_grp%istack_sf(ip)
        if(ied .ge. ist) write(surface_id,'(8i16)')                     &
     &                  domain_grps%edge_grp%item_sf(ist:ied)
      end do
!
      end subroutine write_domain_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_nod_group_viewer(num_pe, view_nod_grps)
!
      use m_fem_mesh_labels
!
      integer, intent(in) :: num_pe
      type(viewer_node_groups), intent(in) :: view_nod_grps
!
!
      write(surface_id,'(a)', advance='NO') hd_fem_nodgrp()
      write(surface_id,'(i16)') view_nod_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_nod_grps%num_grp,                    &
     &    view_nod_grps%grp_name, view_nod_grps%node_grp)
!
      end subroutine write_nod_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_ele_group_viewer(num_pe, view_ele_grps)
!
      use m_fem_mesh_labels
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(in) :: view_ele_grps
!
!
      write(surface_id,'(a)', advance='NO') hd_fem_elegrp()
      write(surface_id,'(a)', advance='NO') hd_ele_surf_grp()
!
      write(surface_id,'(i16)') view_ele_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_ele_grps%num_grp,                    &
     &    view_ele_grps%grp_name, view_ele_grps%surf_grp)
!
!
      write(surface_id,'(a)', advance='NO') hd_ele_nod_grp()
      write(surface_id,'(i16)') view_ele_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_ele_grps%num_grp,                    &
     &    view_ele_grps%grp_name, view_ele_grps%node_grp)
!
      write(surface_id,'(a)', advance='NO') hd_ele_edge_grp()
      write(surface_id,'(i16)') view_ele_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_ele_grps%num_grp,                    &
     &    view_ele_grps%grp_name, view_ele_grps%edge_grp)
!
      end subroutine write_ele_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_group_viewer(num_pe, view_sf_grps)
!
      use m_fem_mesh_labels
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(in) :: view_sf_grps
!
!
      write(surface_id,'(a)', advance='NO') hd_fem_sfgrp()
      write(surface_id,'(a)', advance='NO') hd_surf_surf_grp()
      write(surface_id,'(i16)') view_sf_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_sf_grps%num_grp,                     &
     &    view_sf_grps%grp_name, view_sf_grps%surf_grp)
!
!
      write(surface_id,'(a)', advance='NO') hd_surf_nod_grp()
      write(surface_id,'(i16)') view_sf_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_sf_grps%num_grp,                     &
     &    view_sf_grps%grp_name, view_sf_grps%node_grp)
!
      write(surface_id,'(a)', advance='NO') hd_surf_edge_grp()
      write(surface_id,'(i16)') view_sf_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_sf_grps%num_grp,                     &
     &    view_sf_grps%grp_name, view_sf_grps%edge_grp)
!
      end subroutine write_surf_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_viewer_group_data(id_file, num_pe, ngrp,         &
     &          name, v_grp)
!
      integer(kind = kint), intent(in) :: id_file
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: ngrp
      character(len = kchara), intent(in) :: name(ngrp)
      type(viewer_group_data), intent(in) :: v_grp
!
      integer(kind = kint) :: i, ip, ist, ied
!
!
      do i = 1, ngrp
        ist = (i-1) * num_pe + 1
        ied = i*num_pe
        write(id_file,'(8i16)') v_grp%istack_sf(ist:ied)
      end do
!
      if (ngrp .gt. 0) then
        do i = 1, ngrp
          write(id_file,'(a)') trim(name(i))
          do ip = 1, num_pe
            ist = v_grp%istack_sf(num_pe*(i-1)+ip-1) + 1
            ied = v_grp%istack_sf(num_pe*(i-1)+ip)
            if(ied .ge. ist) write(id_file,'(8i16)')                    &
     &                            v_grp%item_sf(ist:ied)
          end do
        end do
      else
        write(id_file,*) ''
      end if
!
      end subroutine write_viewer_group_data
!
! -----------------------------------------------------------------------
!
      end module viewer_group_data_IO
