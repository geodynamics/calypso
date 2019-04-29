!
!      module set_FEM_mesh_4_sph
!
!     Written by H. Matsui on March, 2013
!
!!      subroutine s_const_FEM_mesh_for_sph                             &
!!     &         (id_rank, nidx_rtp, r_global, gauss,                   &
!!     &          s3d_ranks, stk_lc1d, sph_gl1d, sph_params, sph_rtp,   &
!!     &          radial_rj_grp, mesh, group, ele_mesh, stbl)
!!        type(gauss_points), intent(in) :: gauss
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!        type(sph_1d_index_stack), intent(in)  :: stk_lc1d
!!        type(sph_1d_global_index), intent(in)  :: sph_gl1d
!!        type(comm_table_make_sph), intent(inout) :: stbl
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::  group
!!        type(element_geometry), intent(inout) :: ele_mesh
!
      module set_FEM_mesh_4_sph
!
      use m_precision
!
      implicit none
!
      private :: const_FEM_geometry_for_sph
      private :: const_FEM_groups_for_sph
      private :: const_nod_comm_table_for_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_FEM_mesh_for_sph                               &
     &         (id_rank, nidx_rtp, r_global, gauss, sph_params,         &
     &          sph_rtp, gen_sph, mesh, group, ele_mesh, stbl)
!
      use t_spheric_parameter
      use t_gauss_points
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use t_const_spherical_grid
      use t_sph_mesh_1d_connect
!
      use coordinate_converter
      use ordering_sph_mesh_to_rtp
      use set_nnod_4_ele_by_type
!
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(comm_table_make_sph), intent(inout) :: stbl
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer, intent(in) :: id_rank
      real(kind= kreal), intent(in)                                     &
     &            :: r_global(stbl%nidx_global_fem(1))
!
      type(gauss_points), intent(in) :: gauss
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::  group
      type(element_geometry), intent(inout) :: ele_mesh
!
      integer(kind = kint) :: ip_r, ip_t
!
!      write(*,*) 'gen_sph%radial_rj_grp_lc%grp_name', gen_sph%radial_rj_grp_lc%grp_name
!
      stbl%nidx_local_fem(1:3) = sph_rtp%nidx_rtp(1:3)
      stbl%nidx_local_fem(3) =   sph_params%m_folding                   &
     &                         * stbl%nidx_local_fem(3)
!
      ip_r = gen_sph%s3d_ranks%iglobal_rank_rtp(1,id_rank) + 1
      ip_t = gen_sph%s3d_ranks%iglobal_rank_rtp(2,id_rank) + 1
!
!  Construct element connectivity
      call const_FEM_geometry_for_sph(ip_r, ip_t, r_global,             &
     &    sph_params, gauss, stbl, mesh%node, mesh%ele)
!
!  Construct groups
      call const_FEM_groups_for_sph                                     &
     &   (ip_r, ip_t,  sph_params, gen_sph, stbl, group)
!
! Set communication table
      call const_nod_comm_table_for_sph(id_rank, ip_r, ip_t,            &
     &    gen_sph%s3d_ranks, stbl, mesh%nod_comm)
!
! Ordering to connect rtp data
      call s_ordering_sph_mesh_for_rtp                                  &
     &   (nidx_rtp, ip_r, ip_t, gen_sph%stk_lc1d, gen_sph%sph_gl1d,     &
     &    stbl, mesh%node, mesh%ele, group%nod_grp, mesh%nod_comm)
!
! Convert spherical coordinate to certesian
      call position_2_xyz(mesh%node%numnod,                             &
     &    mesh%node%rr, mesh%node%theta, mesh%node%phi,                 &
     &    mesh%node%xx(1:mesh%node%numnod,1),                           &
     &    mesh%node%xx(1:mesh%node%numnod,2),                           &
     &    mesh%node%xx(1:mesh%node%numnod,3))
!
      call set_3D_nnod_4_sfed_by_ele(mesh%ele%nnod_4_ele,               &
     &    ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
!
      end subroutine s_const_FEM_mesh_for_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_FEM_geometry_for_sph(ip_r, ip_t, r_global,       &
     &          sph_params, gauss, stbl, node, ele)
!
      use calypso_mpi
      use t_geometry_data
      use t_spheric_parameter
      use t_gauss_points
      use t_sph_mesh_1d_connect
!
      use set_sph_local_node
      use set_sph_local_element
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(gauss_points), intent(in) :: gauss
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t
      real(kind= kreal), intent(in)                                     &
     &               :: r_global(stbl%nidx_global_fem(1))
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
!
!  Construct node geometry
      call count_numnod_local_sph_mesh                                  &
     &   (sph_params%iflag_shell_mode, ip_r, ip_t, stbl, node)
!
      call alloc_node_geometry_w_sph(node)
      call set_local_nodes_sph_mesh(sph_params%iflag_shell_mode,        &
     &    ip_r, ip_t, gauss%n_point, r_global, gauss%colat, stbl, node)
!
!  Construct element connectivity
      call count_local_elements_sph_mesh                                &
     &   (ip_r, ip_t, sph_params, stbl, ele)
!
      call allocate_ele_connect_type(ele)
      call set_local_elements_sph_mesh                                  &
     &   (ip_r, ip_t, sph_params, stbl, ele)
!
      end subroutine const_FEM_geometry_for_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_FEM_groups_for_sph                               &
     &         (ip_r, ip_t, sph_params, gen_sph, stbl, group)
!
      use t_mesh_data
      use t_const_spherical_grid
      use t_sph_mesh_1d_connect
!
      use set_sph_node_group
      use set_sph_ele_group
      use set_sph_surf_group
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(sph_shell_parameters), intent(in) :: sph_params
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(comm_table_make_sph), intent(in) :: stbl
!
      type(mesh_groups), intent(inout) ::  group
!
!
!  Construct node group
      call count_sph_local_node_group                                   &
     &   (sph_params, gen_sph%radial_rj_grp_lc, group%nod_grp)
!
      call alloc_group_num(group%nod_grp)
      call count_sph_local_node_grp_item                                &
     &   (ip_r, ip_t, sph_params, gen_sph%radial_rj_grp_lc,             &
     &    stbl, group%nod_grp)
!
      call s_cal_total_and_stacks(group%nod_grp%num_grp,                &
     &    group%nod_grp%nitem_grp, izero, group%nod_grp%istack_grp,     &
     &    group%nod_grp%num_item)
!
      call alloc_group_item(group%nod_grp)
      call set_sph_local_node_grp_item                                  &
     &   (ip_r, ip_t, sph_params, gen_sph%radial_rj_grp_lc,             &
     &    stbl, group%nod_grp)
!
!  Construct element group
      call allocate_sph_ele_grp_flag(stbl)
      call count_sph_local_ele_group                                    &
     &   (group%ele_grp, gen_sph%radial_rj_grp_lc)
!
      call alloc_group_num(group%ele_grp)
      call count_sph_local_ele_grp_item                                 &
     &   (ip_r, ip_t, sph_params, stbl, gen_sph%radial_rj_grp_lc,       &
     &    group%ele_grp)
!
      call s_cal_total_and_stacks(group%ele_grp%num_grp,                &
     &    group%ele_grp%nitem_grp, izero, group%ele_grp%istack_grp,     &
     &    group%ele_grp%num_item)
!
      call alloc_group_item(group%ele_grp)
      call set_sph_local_ele_grp_item(ip_r, ip_t, sph_params, stbl,     &
     &    gen_sph%radial_rj_grp_lc, group%ele_grp)
!
      call deallocate_sph_ele_grp_flag
!
!  Construct surf group
      call count_sph_local_surf_group                                   &
     &   (gen_sph%radial_rj_grp_lc, group%surf_grp)
!
      call alloc_sf_group_num(group%surf_grp)
      call count_sph_local_surf_grp_item(ip_r, ip_t, sph_params,        &
     &    gen_sph%radial_rj_grp_lc, stbl, group%surf_grp)
!
      call s_cal_total_and_stacks(group%surf_grp%num_grp,               &
     &    group%surf_grp%nitem_grp, izero, group%surf_grp%istack_grp,   &
     &    group%surf_grp%num_item)
!
      call alloc_sf_group_item(group%surf_grp)
      call set_sph_local_surf_grp_item(ip_r, ip_t, sph_params,          &
     &    gen_sph%radial_rj_grp_lc, stbl, group%surf_grp)
!
      end subroutine const_FEM_groups_for_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_nod_comm_table_for_sph(id_rank, ip_r, ip_t,      &
     &          s3d_ranks, stbl, nod_comm)
!
      use t_comm_table
      use t_spheric_global_ranks
      use t_sph_mesh_1d_connect
      use const_comm_tbl_4_sph_mesh
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ip_r, ip_t
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(comm_table_make_sph), intent(inout) :: stbl
      type(communication_table), intent(inout) :: nod_comm
!
! Count subdomain to communicate
      call count_neib_4_sph_mesh(id_rank, ip_r, ip_t,                   &
     &    s3d_ranks, stbl, nod_comm)
      call count_neib_4_sph_center_mesh(id_rank, ip_r, ip_t,            &
     &    s3d_ranks, stbl, nod_comm)
!
      call alloc_comm_table_num(nod_comm)
!
! Set subdomain ID to communicate
      call set_neib_4_sph_mesh(id_rank, ip_r, ip_t,                     &
     &    s3d_ranks, stbl, nod_comm)
      call set_neib_4_sph_center_mesh(id_rank, ip_r, ip_t,              &
     &    s3d_ranks, stbl, nod_comm)
!
! Count number of nodes to communicate
      call count_import_4_sph_mesh                                      &
     &   (ip_r, ip_t, s3d_ranks, stbl, nod_comm)
      call count_export_4_sph_mesh                                      &
     &   (ip_r, ip_t, s3d_ranks, stbl, nod_comm)
!
!
      call alloc_import_item(nod_comm)
      call alloc_export_item(nod_comm)
      call alloc_1d_comm_tbl_4_sph                                      &
     &   (nod_comm%ntot_import, nod_comm%ntot_export, stbl)
!
! set node ID to communicate
      call set_import_rtp_sph_mesh                                      &
     &   (ip_r, ip_t, s3d_ranks, stbl, nod_comm)
      call set_export_rtp_sph_mesh                                      &
     &   (ip_r, ip_t, s3d_ranks, stbl, nod_comm)
!
      call dealloc_1d_comm_tbl_4_sph(stbl)
!
      end subroutine const_nod_comm_table_for_sph
!
! -----------------------------------------------------------------------
!
      end module set_FEM_mesh_4_sph
