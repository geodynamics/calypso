!>@file  set_fields_after_tracing.f90
!!       module set_fields_after_tracing
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief extend field line in each domain
!!
!!@verbatim
!!      subroutine fline_vector_at_one_element(iele, node, ele, v_trace,&
!!     &                                      v4_ele)
!!        integer(kind = kint), intent(in) :: iele
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        real(kind = kreal), intent(in) :: v_trace(node%numnod,3)
!!        real(kind = kreal), intent(inout) :: v4_ele(4,ele%nnod_4_ele)
!!      subroutine fline_colors_at_one_element(iele, ele, nod_fld,      &
!!     &                                       viz_fields, c_ele)
!!        integer(kind = kint), intent(in) :: iele
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!!
!!      subroutine fields_on_surf_from_one_ele                          &
!!     &         (isf_tgt, xi_surf, ele, surf, viz_fields,              &
!!     &          v4_ele, c_ele, x4_tgt, v4_tgt, c_tgt)
!!        integer(kind = kint), intent(in) :: isf_tgt
!!        real(kind = kreal), intent(in) :: xi_surf(2)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
!!        real(kind = kreal), intent(in)                                &
!!     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!!        real(kind = kreal), intent(in) :: x4_tgt(4)
!!        real(kind = kreal), intent(inout) :: v4_tgt(4)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: c_tgt(viz_fields%ntot_color_comp)
!!@endverbatim
!
      module set_fields_after_tracing
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
!
      use t_geometry_data
      use t_surface_data
      use t_phys_data
      use t_ctl_params_viz_fields
!
      implicit  none
!
      private :: field_on_surf_of_one_ele
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine fline_vector_at_one_element(iele, node, ele, v_trace,  &
     &                                      v4_ele)
!
      integer(kind = kint), intent(in) :: iele
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      real(kind = kreal), intent(in) :: v_trace(node%numnod,3)
!
      real(kind = kreal), intent(inout) :: v4_ele(4,ele%nnod_4_ele)
!
      integer(kind = kint) :: k1, inod
!
      do k1 = 1, ele%nnod_4_ele
        inod = ele%ie(iele,k1)
        v4_ele(1:3,k1) = v_trace(inod,1:3)
        v4_ele(4,k1) = one
      end do
!
      end subroutine fline_vector_at_one_element
!
!  ---------------------------------------------------------------------
!
      subroutine fline_colors_at_one_element(iele, ele, nod_fld,        &
     &                                       viz_fields, c_ele)
!
      use tracer_field_interpolate
!
      integer(kind = kint), intent(in) :: iele
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(inout)                                 &
     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!
      integer(kind = kint) :: k1, inod
!
      do k1 = 1, ele%nnod_4_ele
        inod = ele%ie(iele,k1)
        call cal_xyz_fields_at_node(inod, nod_fld, viz_fields,          &
     &                              c_ele(1,k1))
      end do
!
      end subroutine fline_colors_at_one_element
!
!  ---------------------------------------------------------------------
!
      subroutine fields_on_surf_from_one_ele                            &
     &         (isf_tgt, xi_surf, ele, surf, viz_fields,                &
     &          v4_ele, c_ele, x4_tgt, v4_tgt, c_tgt)
!
      use coordinate_converter
      use convert_components_4_viz
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: isf_tgt
      real(kind = kreal), intent(in) :: xi_surf(2)
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in)                                    &
     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!
      real(kind = kreal), intent(in) :: x4_tgt(4)
      real(kind = kreal), intent(inout) :: v4_tgt(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_tgt(viz_fields%ntot_color_comp)
!
      real(kind = kreal) :: c_xyz(viz_fields%ntot_org_comp)
      real(kind = kreal) :: v_work(4*surf%nnod_4_surf)
      real(kind = kreal)                                                &
     &         :: c_work(viz_fields%ntot_org_comp*surf%nnod_4_surf)
      real(kind = kreal) :: r(1), theta(1), phi(1)
      real(kind = kreal) :: a_r(1), rs(1), a_rs(1)
      integer(kind = kint) :: istack_single(0:1) = (/0, 1/)
      integer(kind = kint) :: inum, ist, jst
!
!
      call field_on_surf_of_one_ele(isf_tgt, ele, surf, xi_surf,        &
     &    ifour, v4_ele(1,1), v4_tgt(1), v_work(1))
      call field_on_surf_of_one_ele(isf_tgt, ele, surf, xi_surf,        &
     &    viz_fields%ntot_org_comp, c_ele(1,1), c_xyz(1), c_work(1))
!
      call position_2_sph(ione, x4_tgt(1), r, theta, phi,               &
     &                    a_r, rs, a_rs)
      do inum = 1, viz_fields%num_color_fields
        ist = viz_fields%istack_org_ncomp(inum-1)
        jst = viz_fields%istack_color_field(inum-1)
        call convert_comps_4_viz                                        &
     &     (ione, istack_single, x4_tgt(1), r, a_r, rs, a_rs,           &
     &      viz_fields%ncomp_color_field(inum),                         &
     &      viz_fields%ncomp_org_color_field(inum),                     &
     &      viz_fields%icomp_color_field(inum),                         &
     &      c_xyz(ist+1), c_tgt(jst+1))
      end do
!
      end subroutine fields_on_surf_from_one_ele
!
!  ---------------------------------------------------------------------
!
      subroutine field_on_surf_of_one_ele(isf_in_ele, ele, surf, xi,    &
     &                                    ncomp, v_ele, v_tgt, v_work)
!
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: isf_in_ele
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      real(kind = kreal), intent(in) :: xi(2)
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: v_ele(ncomp,ele%nnod_4_ele)
!
      real(kind = kreal), intent(inout) :: v_tgt(ncomp)
      real(kind = kreal), intent(inout)                                 &
     &                   :: v_work(ncomp,surf%nnod_4_surf)
!
      integer(kind = kint) :: k1, inod_lc
!
!
      do k1 = 1, 4
        inod_lc = surf%node_on_sf(k1,isf_in_ele)
        v_work(1:ncomp,k1) = v_ele(1:ncomp,inod_lc)
      end do
!
      call cal_surf_field_value_2d(ncomp, xi, v_work, v_tgt)
!
      end subroutine field_on_surf_of_one_ele
!
!  ---------------------------------------------------------------------
!
      end module set_fields_after_tracing

