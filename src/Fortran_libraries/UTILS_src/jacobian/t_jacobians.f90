!>@file   set_interpolate_file_name.f90
!!@brief  module set_interpolate_file_name
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  Structure of Jacobian and difference of shape functions
!!
!!@verbatim
!!      subroutine const_jacobians_element(id_rank, nprocs,             &
!!     &          node, ele, surf_grp, infinity_list, jacs)
!!      subroutine const_jacobians_surf_group (id_rank, nprocs,         &
!!     &          node, ele, surf, surf_grp, spf_2d, jacs)
!!      subroutine const_jacobians_surface                              &
!!     &         (id_rank, nprocs, node, surf, spf_2d, jacs)
!!      subroutine const_jacobians_edge                                 &
!!     &         (id_rank, nprocs, node, edge, spf_1d, jacs)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in)  :: surf
!!        type(edge_data), intent(in)  :: edge
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(scalar_surf_BC_list), intent(in) :: infinity_list
!!        type(surface_shape_function), intent(inout) :: spf_2d
!!        type(jacobians_type), intent(inout) :: jacs
!!
!!      subroutine dealloc_dxi_dx_element(ele, jacs)
!!      subroutine dealloc_jacobians_element(ele, jacs)
!!      subroutine dealloc_jacobians_surf_grp(surf, jacs)
!!      subroutine dealloc_jacobians_surface(surf, jacs)
!!      subroutine dealloc_jacobians_edge(edge, jacs)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in)  :: surf
!!        type(edge_data), intent(in)  :: edge
!!        type(jacobians_type), intent(inout) :: jacs
!!@endverbatim
!
      module t_jacobians
!
      use m_precision
      use m_geometry_constants
!
      use t_fem_gauss_int_coefs
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_shape_functions
      use t_jacobian_3d
      use t_jacobian_2d
      use t_jacobian_1d
!
      implicit  none
!
!
!>     Stracture for Jacobians for FEM grid
      type jacobians_type
!>     Gauss points and weights
        type(FEM_gauss_int_coefs) :: g_FEM
!
!>     Stracture for Jacobians for element
        type(jacobians_3d) :: jac_3d
!>     Stracture for Jacobians for surface
        type(jacobians_2d) :: jac_2d
!>     Stracture for Jacobians for edge
        type(jacobians_1d) :: jac_1d
!>     Stracture for Jacobians for surafce group
        type(jacobians_2d) :: jac_sf_grp
!
!>     Stracture for Jacobians for linear element
        type(jacobians_3d), pointer ::  jac_3d_l
!>     Stracture for Jacobians for linear surface
        type(jacobians_2d), pointer ::  jac_2d_l
!>     Stracture for Jacobians for linear edge
        type(jacobians_1d), pointer  :: jac_1d_l
!>     Stracture for Jacobians for linear surafce group
        type(jacobians_2d), pointer :: jac_sf_grp_l
      end type jacobians_type
!
      private :: link_linear_jacobians_element
      private :: link_linear_jacobians_sf_grp
      private :: link_linear_jacobians_surface
      private :: link_linear_jacobians_edge
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_jacobians_element(id_rank, nprocs,               &
     &          node, ele, surf_grp, infinity_list, spf_3d, jacs)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(scalar_surf_BC_list), intent(in) :: infinity_list
      type(volume_shape_function), intent(inout) :: spf_3d
      type(jacobians_type), intent(inout) :: jacs
!
!
      call alloc_jacobians(ele%numele, ele%nnod_4_ele,                  &
     &    jacs%g_FEM%maxtot_int_3d, jacs%jac_3d)
      call alloc_inv_jacobian(ele%numele, jacs%jac_3d)
      call alloc_dxi_dx(ele%numele, jacs%jac_3d)
!
      if(id_rank .lt. nprocs) then
        call sel_jacobian_type                                          &
     &     (node, ele, jacs%g_FEM, spf_3d, jacs%jac_3d)
        call sel_jacobian_infinity(node, ele, surf_grp,                 &
     &      infinity_list, jacs%g_FEM, spf_3d, jacs%jac_3d)
      end if
      call dealloc_inv_jacobian(jacs%jac_3d)
!
      if(ele%nnod_4_ele .eq. num_t_linear) then
        call link_linear_jacobians_element(jacs%jac_3d, jacs)
      else
        allocate(jacs%jac_3d_l)
        call alloc_jacobians(ele%numele, num_t_linear,                  &
     &      jacs%g_FEM%maxtot_int_3d, jacs%jac_3d_l)
        call alloc_inv_jacobian(ele%numele, jacs%jac_3d)
        call alloc_dxi_dx(ele%numele, jacs%jac_3d_l)
!
        if(id_rank .lt. nprocs) then
          call cal_jacobian_trilinear                                   &
     &       (node, ele, jacs%g_FEM, spf_3d, jacs%jac_3d_l)
          call const_linear_jacobian_infinity(node, ele, surf_grp,      &
     &        infinity_list, jacs%g_FEM, spf_3d, jacs%jac_3d_l)
        end if
!
        call dealloc_inv_jacobian(jacs%jac_3d_l)
      end if
!
      end subroutine const_jacobians_element
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for surface group
!
      subroutine const_jacobians_surf_group (id_rank, nprocs,           &
     &          node, ele, surf, surf_grp, spf_2d, jacs)
!
      use const_jacobians_sf_grp
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in)  :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_type), intent(inout) :: jacs
!
!
      call alloc_2d_jac_type(surf_grp%num_item,                         &
     &    surf%nnod_4_surf, jacs%g_FEM%maxtot_int_2d, jacs%jac_sf_grp)
!
      if(id_rank .lt. nprocs) then
        call sel_jacobian_surface_grp (node, ele, surf, surf_grp,       &
     &      jacs%g_FEM, spf_2d, jacs%jac_sf_grp)
      end if
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        call link_linear_jacobians_sf_grp(jacs%jac_sf_grp, jacs)
      else
        allocate(jacs%jac_sf_grp_l)
        call alloc_2d_jac_type(surf_grp%num_item, num_linear_sf,        &
     &      jacs%g_FEM%maxtot_int_2d, jacs%jac_sf_grp_l)
!
        if(id_rank .lt. nprocs) then
          call const_jacobian_sf_grp_linear(node, ele, surf_grp,        &
     &        jacs%g_FEM, spf_2d, jacs%jac_sf_grp_l)
        end if
      end if
!
      end subroutine const_jacobians_surf_group
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for surface element
!
      subroutine const_jacobians_surface                                &
     &         (id_rank, nprocs, node, surf, spf_2d, jacs)
!
      use const_jacobians_2d
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_type), intent(inout) :: jacs
!
!
      call alloc_2d_jac_type(surf%numsurf,                              &
     &    surf%nnod_4_surf, jacs%g_FEM%maxtot_int_2d, jacs%jac_2d)
!
      if(id_rank .lt. nprocs) then
        call sel_jacobian_surface                                       &
     &     (node, surf, jacs%g_FEM, spf_2d, jacs%jac_2d)
      end if
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        call link_linear_jacobians_surface(jacs%jac_2d, jacs)
      else
        allocate(jacs%jac_2d_l)
        call alloc_2d_jac_type(surf%numsurf, num_linear_sf,             &
     &      jacs%g_FEM%maxtot_int_2d, jacs%jac_2d_l)
        if(id_rank .lt. nprocs) then
          call cal_jacobian_surface_linear                              &
     &       (node, surf, jacs%g_FEM, spf_2d, jacs%jac_2d_l)
        end if
      end if
!
      end subroutine const_jacobians_surface
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for edge element
!
      subroutine const_jacobians_edge                                   &
     &         (id_rank, nprocs, node, edge, spf_1d, jacs)
!
      use const_jacobians_1d
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(edge_shape_function), intent(inout) :: spf_1d
      type(jacobians_type), intent(inout) :: jacs
!
!
      call alloc_1d_jac_type(edge%numedge, edge%nnod_4_edge,            &
     &    jacs%g_FEM%maxtot_int_1d, jacs%jac_1d)
!
      if(id_rank .lt. nprocs) then
        call sel_jacobian_edge                                          &
     &     (node, edge, jacs%g_FEM, spf_1d, jacs%jac_1d)
      end if
!
      if(edge%nnod_4_edge .eq. num_linear_edge) then
        call link_linear_jacobians_edge(jacs%jac_1d, jacs)
      else
        allocate(jacs%jac_1d_l)
        call alloc_1d_jac_type(edge%numedge, num_linear_edge,           &
     &      jacs%g_FEM%maxtot_int_1d, jacs%jac_1d_l)
        if(id_rank .lt. nprocs) then
          call cal_jacobian_edge_linear                                 &
     &       (node, edge, jacs%g_FEM, spf_1d, jacs%jac_1d_l)
        end if
      end if
!
      end subroutine const_jacobians_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_dxi_dx_element(ele, jacs)
!
      type(element_data), intent(in) :: ele
      type(jacobians_type), intent(inout) :: jacs
!
!
      call dealloc_dxi_dx(jacs%jac_3d)
!
      if(ele%nnod_4_ele .ne. num_t_linear) then
        call dealloc_dxi_dx(jacs%jac_3d_l)
      end if
!
      end subroutine dealloc_dxi_dx_element
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_element(ele, jacs)
!
      type(element_data), intent(in) :: ele
      type(jacobians_type), intent(inout) :: jacs
!
!
      if(ele%nnod_4_ele .eq. num_t_linear) then
        nullify(jacs%jac_3d_l)
      else
        call dealloc_jacobians(jacs%jac_3d_l)
        deallocate(jacs%jac_3d_l)
      end if
!
      call dealloc_jacobians(jacs%jac_3d)
!
      end subroutine dealloc_jacobians_element
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_surf_grp(surf, jacs)
!
      use const_jacobians_2d
!
      type(surface_data), intent(in)  :: surf
      type(jacobians_type), intent(inout) :: jacs
!
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        nullify(jacs%jac_sf_grp_l)
      else
        call dealloc_2d_jac_type(jacs%jac_sf_grp_l)
        deallocate(jacs%jac_sf_grp_l)
      end if
!
      call dealloc_2d_jac_type(jacs%jac_sf_grp)
!
      end subroutine dealloc_jacobians_surf_grp
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_surface(surf, jacs)
!
      use const_jacobians_2d
!
      type(surface_data), intent(in)  :: surf
      type(jacobians_type), intent(inout) :: jacs
!
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        nullify(jacs%jac_2d_l)
      else
        call dealloc_2d_jac_type(jacs%jac_2d_l)
        deallocate(jacs%jac_2d_l)
      end if
!
      call dealloc_2d_jac_type(jacs%jac_2d)
!
      end subroutine dealloc_jacobians_surface
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_edge(edge, jacs)
!
      type(edge_data), intent(in)  :: edge
      type(jacobians_type), intent(inout) :: jacs
!
!
      if(edge%nnod_4_edge .eq. num_linear_edge) then
        nullify(jacs%jac_1d_l)
      else
        call dealloc_1d_jac_type(jacs%jac_1d_l)
        deallocate(jacs%jac_1d_l)
      end if
!
      call dealloc_1d_jac_type(jacs%jac_1d)
!
      end subroutine dealloc_jacobians_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_linear_jacobians_element(jac_3d, jacs)
!
      type(jacobians_3d), intent(in), target :: jac_3d
      type(jacobians_type), intent(inout) :: jacs
!
      jacs%jac_3d_l => jac_3d
!
      end subroutine link_linear_jacobians_element
!
! ----------------------------------------------------------------------
!
      subroutine link_linear_jacobians_sf_grp(jac_sf_grp, jacs)
!
      type(jacobians_2d), intent(in), target :: jac_sf_grp
      type(jacobians_type), intent(inout) :: jacs
!
      jacs%jac_sf_grp_l => jac_sf_grp
!
      end subroutine link_linear_jacobians_sf_grp
!
! ----------------------------------------------------------------------
!
      subroutine link_linear_jacobians_surface(jac_2d, jacs)
!
      type(jacobians_2d), intent(in), target :: jac_2d
      type(jacobians_type), intent(inout) :: jacs
!
      jacs%jac_2d_l => jac_2d
!
      end subroutine link_linear_jacobians_surface
!
! ----------------------------------------------------------------------
!
      subroutine link_linear_jacobians_edge(jac_1d, jacs)
!
      type(jacobians_1d), intent(in), target :: jac_1d
      type(jacobians_type), intent(inout) :: jacs
!
      jacs%jac_1d_l => jac_1d
!
      end subroutine link_linear_jacobians_edge
!
! ----------------------------------------------------------------------
!
      end module t_jacobians
