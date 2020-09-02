!>@file   gz_MPI_viewer_mesh_file_IO.f90
!!@brief  module gz_MPI_viewer_mesh_file_IO
!!
!!@author H.Matsui
!!@date      Programmed in Mar., 2018
!
!>@brief  Viewer mesh file IO for gzipped format
!!
!!@verbatim
!!      subroutine gz_mpi_write_viewer_mesh_file                        &
!!     &         (file_name, mgd_v_mesh, mgd_view_prm)
!!        type(merged_viewer_mesh), intent(in) :: mgd_v_mesh
!!        type(mpi_viewer_mesh_param), intent(in) :: mgd_view_prm
!!@endverbatim
!
      module gz_MPI_viewer_mesh_file_IO
!
      use m_precision
      use m_constants
!
      use t_merged_viewer_mesh
      use t_calypso_mpi_IO_param
!
      implicit none
!
      private :: gz_write_viewer_mesh_infos
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_mesh_file                          &
     &         (file_name, mgd_v_mesh, mgd_view_prm)
!
      use MPI_ascii_data_IO
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_name
      type(merged_viewer_mesh), intent(in) :: mgd_v_mesh
      type(mpi_viewer_mesh_param), intent(in) :: mgd_view_prm
!
      type(calypso_MPI_IO_params) :: IO_param
      character(len=kchara) :: gzip_name
!
!
      gzip_name = add_gzip_extension(file_name)
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped ascii mesh file: ', trim(gzip_name)
!
      call open_write_mpi_file(gzip_name, IO_param)
      call gz_write_viewer_mesh_infos                                   &
     &   (mgd_v_mesh, mgd_view_prm, IO_param)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_write_viewer_mesh_file
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_viewer_mesh_infos                             &
     &         (mgd_v_mesh, mgd_view_prm, IO_param)
!
      use m_viewer_mesh_labels
      use m_fem_mesh_labels
      use gz_MPI_ascii_data_IO
      use gz_MPI_viewer_mesh_IO
      use gz_MPI_integer_list_IO
      use m_phys_constants
      use transfer_to_long_integers
!
      type(merged_viewer_mesh), intent(in) :: mgd_v_mesh
      type(mpi_viewer_mesh_param), intent(in) :: mgd_view_prm
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint_gl) :: num_item64
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ndomain_viewer()), hd_ndomain_viewer())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(mgd_view_prm%num_pe_sf))
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_node_viewer()), hd_node_viewer())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(mgd_view_prm%istack_v_node(nprocs)))
!
      num_item64 = cast_long(mgd_v_mesh%view_mesh%nnod_viewer)
      call gz_mpi_write_viewer_position(IO_param, num_item64, n_vector, &
     &    mgd_v_mesh%view_mesh%inod_gl_view,                            &
     &    mgd_v_mesh%view_mesh%xx_view)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_surf_viewer()), hd_surf_viewer())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(mgd_view_prm%istack_v_surf(nprocs)))
!
      num_item64 = cast_long(mgd_v_mesh%view_mesh%nsurf_viewer)
      call gz_mpi_write_stack_over_domain(IO_param, num_item64)
      call gz_mpi_write_element_type                                    &
     &   (IO_param, iten, mgd_v_mesh%view_mesh%nsurf_viewer,            &
     &    mgd_v_mesh%view_mesh%surftyp_viewer)
!
      call gz_mpi_write_stack_over_domain(IO_param, num_item64)
      call gz_mpi_write_ele_connect                                     &
     &   (IO_param, mgd_v_mesh%view_mesh%nsurf_viewer,                  &
     &    mgd_v_mesh%view_mesh%nnod_v_surf,                             &
     &    mgd_v_mesh%view_mesh%isurf_gl_view,                           &
     &    mgd_v_mesh%view_mesh%ie_sf_viewer)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_viewer()), hd_edge_viewer())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(mgd_view_prm%istack_v_edge(nprocs)))
!
      num_item64 = cast_long(mgd_v_mesh%view_mesh%nedge_viewer)
      call gz_mpi_write_stack_over_domain(IO_param, num_item64)
      call gz_mpi_write_ele_connect                                     &
     &   (IO_param, mgd_v_mesh%view_mesh%nedge_viewer,                  &
     &    mgd_v_mesh%view_mesh%nnod_v_edge,                             &
     &    mgd_v_mesh%view_mesh%iedge_gl_view,                           &
     &    mgd_v_mesh%view_mesh%ie_edge_viewer)
!
!
      call gz_mpi_write_charahead(IO_param,                             &
     &    len(hd_edge_on_sf_viewer()), hd_edge_on_sf_viewer())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(mgd_view_prm%istack_v_surf(nprocs)))
!
      num_item64 = cast_long(mgd_v_mesh%view_mesh%nsurf_viewer)
      call gz_mpi_write_stack_over_domain(IO_param, num_item64)
      call gz_mpi_write_ele_connect(IO_param,                           &
     &    mgd_v_mesh%view_mesh%nsurf_viewer, nedge_4_surf,              &
     &    mgd_v_mesh%view_mesh%isurf_gl_view,                           &
     &    mgd_v_mesh%view_mesh%iedge_sf_viewer)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_domain_nod_grp()), hd_domain_nod_grp())
      call gz_mpi_write_domain_grp_data                                 &
     &   (IO_param, mgd_v_mesh%domain_grps%node_grp)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_domain_surf_grp()), hd_domain_surf_grp())
      call gz_mpi_write_domain_grp_data                                 &
     &   (IO_param, mgd_v_mesh%domain_grps%surf_grp)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_domain_edge_grp()), hd_domain_edge_grp())
      call gz_mpi_write_domain_grp_data                                 &
     &   (IO_param, mgd_v_mesh%domain_grps%edge_grp)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_nodgrp()), hd_fem_nodgrp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh%view_nod_grps%num_grp,                   &
     &    mgd_v_mesh%view_nod_grps%grp_name,                            &
     &    mgd_v_mesh%view_nod_grps%node_grp)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_elegrp()), hd_fem_elegrp())
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ele_surf_grp()), hd_ele_surf_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh%view_ele_grps%num_grp,                   &
     &    mgd_v_mesh%view_ele_grps%grp_name,                            &
     &    mgd_v_mesh%view_ele_grps%surf_grp)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ele_nod_grp()), hd_ele_nod_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh%view_ele_grps%num_grp,                   &
     &    mgd_v_mesh%view_ele_grps%grp_name,                            &
     &    mgd_v_mesh%view_ele_grps%node_grp)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ele_edge_grp()), hd_ele_edge_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh%view_ele_grps%num_grp,                   &
     &    mgd_v_mesh%view_ele_grps%grp_name,                            &
     &    mgd_v_mesh%view_ele_grps%edge_grp)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_sfgrp()), hd_fem_sfgrp())
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_surf_surf_grp()), hd_surf_surf_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh%view_sf_grps%num_grp,                    &
     &    mgd_v_mesh%view_sf_grps%grp_name,                             &
     &    mgd_v_mesh%view_sf_grps%surf_grp)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_surf_nod_grp()), hd_surf_nod_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh%view_sf_grps%num_grp,                    &
     &    mgd_v_mesh%view_sf_grps%grp_name,                             &
     &    mgd_v_mesh%view_sf_grps%node_grp)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_surf_edge_grp()), hd_surf_edge_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh%view_sf_grps%num_grp,                    &
     &    mgd_v_mesh%view_sf_grps%grp_name,                             &
     &    mgd_v_mesh%view_sf_grps%edge_grp)
!
      end subroutine gz_write_viewer_mesh_infos
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_viewer_mesh_file_IO
