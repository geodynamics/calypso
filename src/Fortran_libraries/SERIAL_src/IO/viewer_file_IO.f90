!>@file   viewer_file_IO.f90
!!@brief  module viewer_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief ASCII viewer mesh file IO
!!
!!@verbatim
!!      subroutine output_single_surface_grid                           &
!!     &         (file_name, isurf_sf_stack, view_mesh, domain_grps,    &
!!     &          view_nod_grps, view_ele_grps, view_sf_grps)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_surface_groups), intent(in) :: domain_grps
!!        type(viewer_node_groups), intent(in) :: view_nod_grps
!!        type(viewer_surface_groups), intent(in) :: view_ele_grps
!!        type(viewer_surface_groups), intent(in) :: view_sf_grps
!!@endverbatim
!
      module viewer_file_IO
!
      use m_precision
!
      use m_file_format_switch
      use t_merged_viewer_mesh
      use t_file_IO_parameter
      use set_parallel_file_name
      use viewer_mesh_data_IO
      use viewer_group_data_IO
!
      implicit none
!
      private :: output_single_surface_grid
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_output_single_surface_grid                         &
     &         (id_rank, mesh_file, view_mesh, domain_grps,             &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
!
      type(field_IO_params), intent(in) :: mesh_file
      type(viewer_mesh_data), intent(in) :: view_mesh
      type(viewer_surface_groups), intent(in) :: domain_grps
      type(viewer_node_groups), intent(in) :: view_nod_grps
      type(viewer_surface_groups), intent(in) :: view_ele_grps
      type(viewer_surface_groups), intent(in) :: view_sf_grps
!
!
      integer(kind = kint) :: istack_nsurf(0:1)
      character(len = kchara) :: fname_tmp, file_name
!
!
      istack_nsurf(0) = 0
      istack_nsurf(1) = view_mesh%nsurf_viewer
!
      fname_tmp = add_process_id(id_rank, mesh_file%file_prefix)
      file_name = add_ksm_extension(fname_tmp)
!
      call output_single_surface_grid                                   &
     &   (file_name, istack_nsurf, view_mesh, domain_grps,              &
     &    view_nod_grps, view_ele_grps, view_sf_grps)
!
      end subroutine sel_output_single_surface_grid
!
!------------------------------------------------------------------
!
      subroutine output_single_surface_grid                             &
     &         (file_name, isurf_sf_stack, view_mesh, domain_grps,      &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: isurf_sf_stack(0:1)
!
      type(viewer_mesh_data), intent(in) :: view_mesh
      type(viewer_surface_groups), intent(in) :: domain_grps
      type(viewer_node_groups), intent(in) :: view_nod_grps
      type(viewer_surface_groups), intent(in) :: view_ele_grps
      type(viewer_surface_groups), intent(in) :: view_sf_grps
!
!
      write(*,*) 'subdomain surface mesh file name: ', trim(file_name)
      open (surface_id, file = file_name)
!
      call write_sgl_domain_data_viewer(view_mesh)
!
      call write_node_data_viewer(view_mesh)
      call write_surf_connect_viewer                                    &
     &   (1, isurf_sf_stack, view_mesh)
      call write_edge_connect_viewer(view_mesh)
!
      call write_domain_group_viewer(1, domain_grps)
!
      call write_nod_group_viewer(1, view_nod_grps)
      call write_ele_group_viewer(1, view_ele_grps)
      call write_surf_group_viewer(1, view_sf_grps)
!
      close(surface_id)
!
      end subroutine output_single_surface_grid
!
!------------------------------------------------------------------
!
      end module viewer_file_IO
