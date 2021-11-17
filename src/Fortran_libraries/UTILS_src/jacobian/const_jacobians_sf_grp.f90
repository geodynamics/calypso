!>@file  const_jacobians_sf_grp.f90
!!       module const_jacobians_sf_grp
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Construct Jacobians on surfaces
!!
!!@verbatim
!!      subroutine sel_jacobian_surface_grp(node, ele, surf,            &
!!     &          surf_grp, g_FEM, spf_2d, jac_sf_grp)
!!      subroutine const_jacobian_sf_grp_linear(node, ele, surf_grp,    &
!!     &          g_FEM, spf_2d_8, jac_sf_grp)
!!      subroutine const_jacobian_sf_grp_l_quad(node, ele, surf_grp,    &
!!     &          g_FEM, spf_2d_20, jac_sf_grp)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(jacobians_2d), intent(inout) :: jac_sf_grp
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(surface_shape_function), intent(inout) :: spf_2d
!!        type(jacobians_2d), intent(inout) :: jac_sf_grp
!!@endverbatim
!
      module const_jacobians_sf_grp
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_fem_gauss_int_coefs
      use t_shape_functions
      use t_jacobian_2d
!
      implicit  none
!
      private :: const_jacobian_sf_grp_quad, const_jacobian_sf_grp_lag
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_jacobian_surface_grp(node, ele, surf,              &
     &          surf_grp, g_FEM, spf_2d, jac_sf_grp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_data), intent(in)  :: surf
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      if (surf_grp%num_grp .gt. 0) then
        if      (surf%nnod_4_surf .eq. num_linear_sf) then
          call const_jacobian_sf_grp_linear(node, ele,                  &
     &        surf_grp, g_FEM, spf_2d, jac_sf_grp)
        else if (surf%nnod_4_surf .eq. num_quad_sf)   then
          call const_jacobian_sf_grp_quad(node, ele,                    &
     &        surf_grp, g_FEM, spf_2d, jac_sf_grp)
        else if (surf%nnod_4_surf .eq. num_lag_sf)   then
          call const_jacobian_sf_grp_lag(node, ele,                     &
     &        surf_grp, g_FEM, spf_2d, jac_sf_grp)
        end if
      end if
!
      end subroutine sel_jacobian_surface_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_sf_grp_linear(node, ele, surf_grp,      &
     &          g_FEM, spf_2d_8, jac_sf_grp)
!
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(surface_shape_function), intent(inout) :: spf_2d_8
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_linear(jac_sf_grp%ntot_int,          &
     &    jac_sf_grp%an_sf, spf_2d_8%dnxi_sf, spf_2d_8%dnei_sf,         &
     &    spf_2d_8%xi, spf_2d_8%ei)
!
!   jacobian for tri-linear elaments
      call cal_jacobian_sf_grp_4                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    g_FEM%max_int_point, g_FEM%int_start2,                        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, spf_2d_8%dnxi_sf, spf_2d_8%dnei_sf)
!
      end subroutine const_jacobian_sf_grp_linear
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_sf_grp_quad(node, ele, surf_grp,        &
     &          g_FEM, spf_2d_20, jac_sf_grp)
!
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(surface_shape_function), intent(inout) :: spf_2d_20
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_quad(jac_sf_grp%ntot_int,            &
     &    jac_sf_grp%an_sf, spf_2d_20%dnxi_sf, spf_2d_20%dnei_sf,       &
     &    spf_2d_20%xi, spf_2d_20%ei)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_sf_grp_8                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    g_FEM%max_int_point, g_FEM%int_start2,                        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, spf_2d_20%dnxi_sf, spf_2d_20%dnei_sf)
!
      end subroutine const_jacobian_sf_grp_quad
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_sf_grp_lag(node, ele, surf_grp,         &
     &          g_FEM, spf_2d_27, jac_sf_grp)
!
      use m_geometry_constants
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(surface_shape_function), intent(inout) :: spf_2d_27
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_lag(jac_sf_grp%ntot_int,             &
     &    jac_sf_grp%an_sf, spf_2d_27%dnxi_sf, spf_2d_27%dnei_sf,       &
     &    spf_2d_27%xi, spf_2d_27%ei)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_sf_grp_9                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    g_FEM%max_int_point, g_FEM%int_start2,                        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, spf_2d_27%dnxi_sf, spf_2d_27%dnei_sf)
!
!
      end subroutine const_jacobian_sf_grp_lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_sf_grp_l_quad(node, ele, surf_grp,      &
     &          g_FEM, spf_2d_20, jac_sf_grp)
!
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(surface_shape_function), intent(inout) :: spf_2d_20
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_quad(jac_sf_grp%ntot_int,            &
     &    jac_sf_grp%an_sf, spf_2d_20%dnxi_sf, spf_2d_20%dnei_sf,       &
     &    spf_2d_20%xi, spf_2d_20%ei)
!
!
!   jacobian for quadrature  elaments
      call cal_jacobian_sf_grp_4_8                                      &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    g_FEM%max_int_point, g_FEM%int_start2,                        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, spf_2d_20%dnxi_sf, spf_2d_20%dnei_sf)
!
      end subroutine const_jacobian_sf_grp_l_quad
!
!-----------------------------------------------------------------------
!
      end module const_jacobians_sf_grp
