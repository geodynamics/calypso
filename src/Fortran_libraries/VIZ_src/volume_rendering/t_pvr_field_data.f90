!>@file  t_pvr_field_data.f90
!!       module t_pvr_field_data
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief  Field data for volume rendering
!!
!!@verbatim
!!      subroutine alloc_nod_data_4_pvr(nnod, nele, field_pvr)
!!      subroutine dealloc_nod_data_4_pvr(field_pvr)
!!        integer(kind = kint), intent(in) :: nnod, nele
!!        type(rendering_parameter), intent(inout) :: field_pvr
!!      subroutine cal_field_4_each_pvr(node, ele, g_FEM, jac_3d,       &
!!     &          nod_fld, fld_params, field_pvr)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(pvr_field_parameter), intent(in) :: fld_params
!!        type(phys_data), intent(in) :: nod_fld
!!        type(pvr_field_data), intent(inout) :: field_pvr
!!@endverbatim
!
      module t_pvr_field_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>      Structure for field data for PVR
      type pvr_field_data
!>        Data for rendering
        real(kind = kreal), allocatable :: d_pvr(:)
!>        Gradient for rendering
        real(kind = kreal), allocatable :: grad_ele(:,:)
      end type pvr_field_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_data_4_pvr(nnod, nele, field_pvr)
!
      integer(kind = kint), intent(in) :: nnod, nele
      type(pvr_field_data), intent(inout) :: field_pvr
!
!
      allocate(field_pvr%d_pvr(nnod))
      allocate(field_pvr%grad_ele(nele,3))
!
      if(nnod .gt. 0) field_pvr%d_pvr =    0.0d0
      if(nele .gt. 0) field_pvr%grad_ele = 0.0d0
!
      end subroutine alloc_nod_data_4_pvr
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_data_4_pvr(field_pvr)
!
      type(pvr_field_data), intent(inout) :: field_pvr
!
!
      deallocate(field_pvr%d_pvr, field_pvr%grad_ele)
!
      end subroutine dealloc_nod_data_4_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_field_4_each_pvr(node, ele, g_FEM, jac_3d,         &
     &          nod_fld, fld_params, field_pvr)
!
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_geometries_in_pvr_screen
      use t_control_params_4_pvr
      use cal_gradient_on_element
      use convert_components_4_viz
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(pvr_field_parameter), intent(in) :: fld_params
      type(phys_data), intent(in) :: nod_fld
!
      type(pvr_field_data), intent(inout) :: field_pvr
!
!
      integer(kind = kint) :: i_field, ist_fld, num_comp
!
!
      i_field = fld_params%id_field
      ist_fld = nod_fld%istack_component(i_field-1)
      num_comp = nod_fld%istack_component(i_field) - ist_fld
      call convert_comps_4_viz                                          &
     &   (node%numnod, node%istack_nod_smp, node%xx, node%rr,           &
     &    node%a_r, node%ss, node%a_s, ione, num_comp,                  &
     &    fld_params%id_component, nod_fld%d_fld(1,ist_fld+1),          &
     &    field_pvr%d_pvr)
!
      call fem_gradient_on_element(ele%istack_ele_smp, node%numnod,     &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, ione, jac_3d%dnx, jac_3d%xjac,  &
     &    field_pvr%grad_ele, field_pvr%d_pvr)
!
      end subroutine cal_field_4_each_pvr
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_field_data
