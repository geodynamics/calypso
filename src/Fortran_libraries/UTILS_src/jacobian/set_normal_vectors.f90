!>@file   set_normal_vectors.f90
!!@brief  module set_normal_vectors
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2006
!!
!>@brief Construct normal vector on surface data
!!
!!@verbatim
!!      subroutine surf_grp_jacobian_and_normal                         &
!!     &         (id_rank, nprocs, mesh, group, spfs, jacs)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!        type(shape_finctions_at_points), intent(inout) :: spfs
!!        type(jacobians_type), intent(inout) :: jacs
!!      subroutine surf_jacobian_sf_grp_normal(id_rank, nprocs,         &
!!     &          mesh, group, spfs, jacs)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!        type(shape_finctions_at_points), intent(inout) :: spfs
!!        type(jacobians_type), intent(inout) :: jacs
!!@endverbatim
!!
      module set_normal_vectors
!
      use m_precision
!
      use m_machine_parameter
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_normals
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_jacobian_2d
!
      implicit none
!
      private :: const_surf_group_normals, const_normal_vector
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine surf_grp_jacobian_and_normal                           &
     &         (id_rank, nprocs, mesh, group, spfs, jacs)
!
      use int_area_normal_4_surface
      use sum_normal_4_surf_group
      use set_connects_4_surf_group
!
      integer, intent(in) :: id_rank, nprocs
      type(mesh_geometry), intent(in) :: mesh
!
      type(mesh_groups), intent(inout) :: group
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacs
!
!
      call const_surf_group_normals                                     &
     &   (id_rank, nprocs, mesh%node, mesh%ele, mesh%surf,              &
     &    group%surf_grp, group%surf_grp_norm, spfs%spf_2d, jacs)
!      call dealloc_jacobians_surf_grp(mesh%surf, jacs)
!
      if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
      call s_sum_normal_4_surf_group(mesh%ele,                          &
     &    group%surf_grp, group%surf_grp_norm)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_surf_norm_node'
      call cal_surf_normal_at_nod(mesh%node, mesh%ele, mesh%surf,       &
     &    group%surf_grp, group%surf_grp_norm, group%surf_nod_grp)
!
      end subroutine surf_grp_jacobian_and_normal
!
!-----------------------------------------------------------------------
!
      subroutine surf_jacobian_sf_grp_normal(id_rank, nprocs,           &
     &          mesh, group, spfs, jacs)
!
      use sum_normal_4_surf_group
      use set_connects_4_surf_group
!
      integer, intent(in) :: id_rank, nprocs
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacs
!
!     --------------------- Surface jacobian for fieldline
!
      if (iflag_debug.eq.1) write(*,*)  'const_normal_vector'
      call const_normal_vector(id_rank, nprocs,                         &
     &    mesh%node, mesh%surf, spfs%spf_2d, jacs)
      call dealloc_jacobians_surface(mesh%surf, jacs)
!
      if (iflag_debug.eq.1)  write(*,*) 'pick_normal_of_surf_group'
      call pick_normal_of_surf_group(mesh%ele, mesh%surf, mesh%edge,    &
     &    group%surf_grp, group%surf_grp_norm)
!
      if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
      call s_sum_normal_4_surf_group(mesh%ele,                          &
     &    group%surf_grp, group%surf_grp_norm)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_surf_norm_node'
      call cal_surf_normal_at_nod(mesh%node, mesh%ele, mesh%surf,       &
     &    group%surf_grp, group%surf_grp_norm, group%surf_nod_grp)
!
      end subroutine surf_jacobian_sf_grp_normal
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_surf_group_normals                               &
     &         (id_rank, nprocs, node, ele, surf, sf_grp,               &
     &          surf_grp_norm, spf_2d, jacs)
!
      use int_area_normal_4_surface
      use sum_normal_4_surf_group
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_normals), intent(inout) :: surf_grp_norm
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_type), intent(inout) :: jacs
!
!
      if (iflag_debug.eq.1) write(*,*)  'const_jacobian_sf_grp'
      call alloc_surf_shape_func(surf%nnod_4_surf, jacs%g_FEM, spf_2d)
      call const_jacobians_surf_group(id_rank, nprocs,                  &
     &    node, ele, surf, sf_grp, spf_2d, jacs)
!
      call alloc_vectors_surf_group                                     &
     &   (sf_grp%num_grp, sf_grp%num_item, surf_grp_norm)
      call int_normal_surf_groups(sf_grp, jacs%g_FEM, jacs%jac_sf_grp,  &
     &    jacs%g_FEM%max_int_point, surf_grp_norm%area_sf_grp,          &
     &    surf_grp_norm%a_area_sf_grp, surf_grp_norm%vnorm_sf_grp)
      call dealloc_surf_shape_func(spf_2d)
!
      end subroutine const_surf_group_normals
!
!-----------------------------------------------------------------------
!
      subroutine const_normal_vector                                    &
     &         (id_rank, nprocs, node, surf, spf_2d, jacs)
!
      use int_area_normal_4_surface
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
!
      type(surface_data), intent(inout) :: surf
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_type), intent(inout) :: jacs
!
!
      call alloc_surf_shape_func(surf%nnod_4_surf, jacs%g_FEM, spf_2d)
      call const_jacobians_surface                                      &
     &   (id_rank, nprocs, node, surf, spf_2d, jacs)
!
      call alloc_normal_vector(surf)
      call int_normal_all_surf                                          &
     &   (surf, jacs%g_FEM, jacs%jac_2d, jacs%g_FEM%max_int_point,      &
     &    surf%area_surf, surf%a_area_surf, surf%vnorm_surf)
      call dealloc_surf_shape_func(spf_2d)
!
      end subroutine const_normal_vector
!
!-----------------------------------------------------------------------
!
      end module set_normal_vectors
