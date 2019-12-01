!viewer_mesh_data_IO.f90
!      module viewer_mesh_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine write_sgl_domain_data_viewer(view_mesh)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!      subroutine write_node_data_viewer(view_mesh)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!      subroutine write_surf_connect_viewer(view_mesh)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!      subroutine write_edge_connect_viewer(view_mesh)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!
      module viewer_mesh_data_IO
!
      use m_precision
      use m_constants
!
      use t_viewer_mesh
      use t_merged_viewer_mesh
      use m_viewer_mesh_labels
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_sgl_domain_data_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(in) :: view_mesh
!
!
      write(surface_id,'(a)',advance='NO') hd_ndomain_viewer()
!
      write(surface_id,'(i16)') ione
      write(surface_id,'(i16)') view_mesh%nnod_viewer
      write(surface_id,'(i16)') view_mesh%nsurf_viewer
      write(surface_id,'(i16)') view_mesh%nedge_viewer
!
      end subroutine write_sgl_domain_data_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_node_data_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      integer(kind = kint) :: i
!
      write(surface_id,'(a)',advance='NO') hd_node_viewer()
      write(surface_id,'(i16)') view_mesh%nnod_viewer
!
      do i = 1, view_mesh%nnod_viewer
        write(surface_id,'(i16,1p3E25.15e3)')                           &
     &       view_mesh%inod_gl_view(i), view_mesh%xx_view(i,1:3)
      end do
!
      end subroutine write_node_data_viewer
!
!------------------------------------------------------------------
!
      subroutine write_surf_connect_viewer                              &
     &         (num_pe, isurf_sf_stack, view_mesh)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: isurf_sf_stack(0:num_pe)
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      integer(kind = kint) :: i, ist, ied
!
!
      write(surface_id,'(a)',advance='NO') hd_surf_viewer()
!
      write(surface_id,'(i16)') view_mesh%nsurf_viewer
      do i = 1, num_pe
        ist = isurf_sf_stack(i-1) + 1
        ied = isurf_sf_stack(i)
        if(ied .ge. ist) write(surface_id,1003)                         &
     &                  view_mesh%surftyp_viewer(ist:ied)
      end do
!
      do i = 1, view_mesh%nsurf_viewer
       write(surface_id,'(10i16)')                                      &
     &        i, view_mesh%ie_sf_viewer(i,1:view_mesh%nnod_v_surf)
      end do
!
 1003 format(10i16)
!
      end subroutine write_surf_connect_viewer
!
!------------------------------------------------------------------
!
      subroutine write_edge_connect_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      integer(kind = kint) :: i
!
!
      write(surface_id,'(a)',advance='NO') hd_edge_viewer()
!
      write(surface_id,'(i16)') view_mesh%nedge_viewer
!
      do i = 1, view_mesh%nedge_viewer
       write(surface_id,'(10i16)')                                      &
     &     i, view_mesh%ie_edge_viewer(i,1:view_mesh%nnod_v_edge)
      end do
!
      write(surface_id,'(a)',advance='NO') hd_edge_on_sf_viewer()
      write(surface_id,'(i16)') view_mesh%nsurf_viewer
      do i = 1, view_mesh%nsurf_viewer
        write(surface_id,'(10i16)')                                     &
     &            i, view_mesh%iedge_sf_viewer(i,1:nedge_4_surf)
      end do
!
      end subroutine write_edge_connect_viewer
!
!------------------------------------------------------------------
!
      end module viewer_mesh_data_IO
