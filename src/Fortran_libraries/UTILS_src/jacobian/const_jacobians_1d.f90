!>@file  const_jacobians_1d.f90
!!       module const_jacobians_1d
!!
!!@author H. Matsui
!!@date   Programmed in Dec., 2008
!
!> @brief  Construct Jacobians on edge
!!
!!@verbatim
!!      subroutine sel_jacobian_edge(node, edge, g_FEM, spf_1d, jac_1d)
!!      subroutine cal_jacobian_edge_linear                             &
!!     &         (node, edge, g_FEM, spf_1d_8, jac_1d)
!!      subroutine cal_jacobian_edge_quad_on_l                          &
!!     &         (node, edge, g_FEM, spf_1d_20, jac_1d)
!!        type(node_data), intent(in) :: node
!!        type(edge_data), intent(in)  :: edge
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(edge_shape_function), intent(inout) :: spf_1d
!!        type(jacobians_1d), intent(inout) :: jac_1d
!!@endverbatim
!
      module const_jacobians_1d
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_edge_data
      use t_fem_gauss_int_coefs
      use t_shape_functions
      use t_jacobian_1d
!
      implicit none
!
      private :: cal_jacobian_edge_quad
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_jacobian_edge(node, edge, g_FEM, spf_1d, jac_1d)
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(edge_shape_function), intent(inout) :: spf_1d
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      if      (edge%nnod_4_edge .eq. num_linear_edge) then
        call cal_jacobian_edge_linear                                   &
     &     (node, edge, g_FEM, spf_1d, jac_1d)
      else if (edge%nnod_4_edge .eq. num_quad_edge) then
        call cal_jacobian_edge_quad                                     &
     &     (node, edge, g_FEM, spf_1d, jac_1d)
      end if
!
      end subroutine sel_jacobian_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_linear                               &
     &         (node, edge, g_FEM, spf_1d_8, jac_1d)
!
      use cal_1edge_jacobians
      use cal_shape_function_1d
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(edge_shape_function), intent(inout) :: spf_1d_8
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call s_cal_shape_function_1d_linear                               &
     &   (jac_1d%ntot_int, jac_1d%an_edge,                              &
     &    spf_1d_8%dnxi_ed, spf_1d_8%xi)
!
!   jacobian for tri-linear elaments
      call cal_jacobian_1d_2                                            &
     &   (node%numnod, edge%numedge, edge%nnod_4_edge, edge%ie_edge,    &
     &    node%xx, np_smp, edge%istack_edge_smp,                        &
     &    g_FEM%max_int_point, g_FEM%int_start1,                        &
     &    jac_1d%ntot_int, jac_1d%xj_edge, jac_1d%axj_edge,             &
     &    jac_1d%xeg_edge, spf_1d_8%dnxi_ed)
!
      end subroutine cal_jacobian_edge_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_quad                                 &
     &         (node, edge, g_FEM, spf_1d_20, jac_1d)
!
      use cal_1edge_jacobians
      use cal_shape_function_1d
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(edge_shape_function), intent(inout) :: spf_1d_20
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call s_cal_shape_function_1d_quad                                 &
     &   (jac_1d%ntot_int, jac_1d%an_edge,                              &
     &    spf_1d_20%dnxi_ed, spf_1d_20%xi)
!
!   jacobian for quadrature elaments
!
      call cal_jacobian_1d_3                                            &
     &   (node%numnod, edge%numedge, edge%nnod_4_edge, edge%ie_edge,    &
     &    node%xx, np_smp, edge%istack_edge_smp,                        &
     &    g_FEM%max_int_point, g_FEM%int_start1,                        &
     &    jac_1d%ntot_int, jac_1d%xj_edge, jac_1d%axj_edge,             &
     &    jac_1d%xeg_edge, spf_1d_20%dnxi_ed)
!
      end subroutine cal_jacobian_edge_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_quad_on_l                            &
     &         (node, edge, g_FEM, spf_1d_20, jac_1d)
!
      use cal_1edge_jacobians
      use cal_shape_function_1d
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(edge_shape_function), intent(inout) :: spf_1d_20
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call s_cal_shape_function_1d_quad(jac_1d%ntot_int,                &
     &    jac_1d%an_edge, spf_1d_20%dnxi_ed, spf_1d_20%xi)
!
!   jacobian for quadrature elaments
      call cal_jacobian_1d_2_3                                          &
     &   (node%numnod, edge%numedge, edge%nnod_4_edge,                  &
     &    edge%ie_edge, node%xx, np_smp, edge%istack_edge_smp,          &
     &    g_FEM%max_int_point, g_FEM%int_start1,                        &
     &    jac_1d%ntot_int, jac_1d%xj_edge, jac_1d%axj_edge,             &
     &    jac_1d%xeg_edge, spf_1d_20%dnxi_ed)
!
      end subroutine cal_jacobian_edge_quad_on_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_shape_func_from_array(ntot_int_1d, nnod_4_edge,   &
     &          an_org, an_tgt)
!
      integer(kind = kint), intent(in) :: ntot_int_1d, nnod_4_edge
      real(kind=kreal), intent(in) :: an_org(nnod_4_edge,ntot_int_1d)
      real(kind=kreal), intent(inout)                                   &
     &                 :: an_tgt(nnod_4_edge,ntot_int_1d)
      integer(kind = kint) :: ix, k1
!
!
      do ix = 1, ntot_int_1d
        do k1 = 1, nnod_4_edge
          an_tgt(k1,ix) = an_org(k1,ix)
        end do
      end do
!
      end subroutine copy_shape_func_from_array
!
!-----------------------------------------------------------------------
!
      end module const_jacobians_1d
