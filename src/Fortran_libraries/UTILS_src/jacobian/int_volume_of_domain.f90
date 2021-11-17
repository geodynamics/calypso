!>@file  int_volume_of_domain.f90
!!       module int_volume_of_domain
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!!@date  programmed by H.Matsui and H.Okuda
!!@n                                    in July 2000 (ver 1.1)
!!@n        Modified by H. Matsui in Aug., 2006
!!@n        Modified by H. Matsui in June, 2007
!!@n        Modified by H. Matsui in Sep., 2016
!!@n
!> @brief Construct jacobians and volume integrations
!!
!!@verbatim
!!      subroutine jacobian_and_element_volume                          &
!!     &         (id_rank, nprocs, mesh, group, spfs, jacs)
!!      subroutine const_jacobian_and_volume                            &
!!     &         (id_rank, nprocs, mesh, group, spf_3d, jacs)
!!      subroutine const_jacobian_and_vol_layer(id_rank, nprocs,        &
!!     &          node, ele, surf_grp, infty_grp, spfs, jacs, layer_tbl)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(inout) :: ele
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(scalar_surf_BC_list), intent(inout) :: infty_grp
!!        type(shape_finctions_at_points), intent(inout) :: spfs
!!        type(volume_shape_function), intent(inout) :: spf_3d
!!        type(jacobians_type), intent(inout) :: jacs
!!        type(layering_tbl), intent(inout) :: layer_tbl
!!      subroutine s_int_volume_of_domain(ele, g_FEM, jac_3d)
!!@endverbatim
!
      module int_volume_of_domain
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_surface_boundary
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use t_jacobians
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine jacobian_and_element_volume                            &
     &         (id_rank, nprocs, mesh, group, spfs, jacs)
!
      use t_surface_group_normals
      use set_normal_vectors
      use const_jacobians_3d
!
      integer, intent(in) :: id_rank, nprocs
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacs
!
!
      call initialize_FEM_integration                                   &
     &   (jacs%g_FEM, spfs%spf_3d, spfs%spf_2d, spfs%spf_1d)
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_volume'
      call const_jacobian_and_volume                                    &
     &   (id_rank, nprocs, mesh, group, spfs%spf_3d, jacs)
      call dealloc_vol_shape_func(spfs%spf_3d)
!
!      call check_jacobians_trilinear                                   &
!     &   (id_rank, mesh%ele, jacs%jac_3d_l)
!
      end subroutine jacobian_and_element_volume
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_jacobian_and_volume                              &
     &         (id_rank, nprocs, mesh, group, spf_3d, jacs)
!
      use t_shape_functions
      use sum_volume_of_domain
      use const_jacobians_3d
      use const_bc_infty_surf_type
!
      integer, intent(in) :: id_rank, nprocs
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(volume_shape_function), intent(inout) :: spf_3d
      type(jacobians_type), intent(inout) :: jacs
!
!
      call empty_infty_surf_type(group%infty_grp)
!
      call alloc_vol_shape_func(mesh%ele%nnod_4_ele,                    &
     &                          jacs%g_FEM, spf_3d)
      call const_jacobians_element(id_rank, nprocs,                     &
     &    mesh%node, mesh%ele, group%surf_grp, group%infty_grp,         &
     &    spf_3d, jacs)
!
      call allocate_volume_4_smp
      call s_int_volume_of_domain(mesh%ele, jacs%g_FEM, jacs%jac_3d)
      call deallocate_volume_4_smp
!
      call dealloc_dxi_dx_element(mesh%ele, jacs)
!
      end subroutine const_jacobian_and_volume
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_and_vol_layer(id_rank, nprocs,          &
     &          node, ele, surf_grp, infty_grp, spfs, jacs, layer_tbl)
!
      use t_shape_functions
      use t_layering_ele_list
      use const_jacobians_3d
      use sum_volume_of_domain
      use cal_layered_volumes
      use const_bc_infty_surf_type
!
      integer, intent(in) :: id_rank, nprocs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(inout) :: ele
      type(surface_group_data), intent(inout) :: surf_grp
      type(scalar_surf_BC_list), intent(inout) :: infty_grp
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacs
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      call empty_infty_surf_type(infty_grp)
!
      call sel_max_int_point_by_etype(ele%nnod_4_ele, jacs%g_FEM)
      call initialize_FEM_integration                                   &
     &   (jacs%g_FEM, spfs%spf_3d, spfs%spf_2d, spfs%spf_1d)
!
      call alloc_vol_shape_func                                         &
     &   (ele%nnod_4_ele, jacs%g_FEM, spfs%spf_3d)
      call const_jacobians_element(id_rank, nprocs,                     &
     &    node, ele, surf_grp, infty_grp, spfs%spf_3d, jacs)
!
      call allocate_volume_4_smp
      call s_int_volume_of_domain(ele, jacs%g_FEM, jacs%jac_3d)
      call s_cal_layered_volumes(ele, layer_tbl)
      call deallocate_volume_4_smp
!
      call dealloc_dxi_dx_element(ele, jacs)
!
      end subroutine const_jacobian_and_vol_layer
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_int_volume_of_domain(ele, g_FEM, jac_3d)
!
      use calypso_mpi_real
      use fem_element_volume
      use sum_volume_of_domain
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(element_data), intent(inout) :: ele
!
!
!      write(*,*) 'fem_element_volume_pg', g_FEM%max_int_point
       call fem_element_volume_pg                                       &
     &    (ele, g_FEM, jac_3d, g_FEM%max_int_point)
!
!     ---  lead total volume
!
!      write(*,*) 'sum_4_volume'
      call sum_4_volume(ele%numele, ele%interior_ele,                   &
     &    ele%istack_ele_smp, ele%volume_ele, ele%volume_local)
!
!      write(*,*) 'MPI_allREDUCE'
      call calypso_mpi_allreduce_one_real                              &
     &   (ele%volume_local, ele%volume, MPI_SUM)
!
      if (ele%volume .eq. 0.0d0) then
        ele%a_vol = 1.0d30
      else
        ele%a_vol = 1.0d0 / ele%volume
      end if
!
      end subroutine s_int_volume_of_domain
!
!-----------------------------------------------------------------------
!
      end module int_volume_of_domain
