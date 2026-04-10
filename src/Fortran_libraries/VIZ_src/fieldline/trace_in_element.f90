!>@file  trace_in_element.f90
!!       module trace_in_element
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief extend field line in each domain
!!
!!@verbatim
!!      subroutine find_backside_by_flux(surf, iflag_dir,               &
!!     &                                 v4_start, isurf_org)
!!        integer(kind = kint), intent(in) :: iflag_dir
!!        type(surface_data), intent(in) :: surf
!!        real(kind = kreal), intent(in) ::   v4_start(4)
!!        integer(kind = kint), intent(inout) :: isurf_org(2)
!!      subroutine check_exit_in_double_number(surf, para_surf,         &
!!     &                                       isurf_org, isurf_org_dbl)
!!        type(surface_data), intent(in) :: surf
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        integer(kind = kint), intent(in) :: isurf_org(2)
!!        integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
!!
!!      subroutine trace_to_element_wall(isf_org, iflag_dir, ele, surf, &
!!     &          x4_ele, v4_ele, x4_start, v4_start,                   &
!!     &          isf_tgt_8, xi_surf_8, x4_tgt_8, v4_tgt_8)
!!        integer(kind = kint), intent(in) :: isf_org
!!        integer(kind = kint), intent(in) :: iflag_dir
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        real(kind = kreal), intent(in) :: x4_ele(4,ele%nnod_4_ele)
!!        real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
!!        real(kind = kreal), intent(in) :: x4_start(4)
!!        real(kind = kreal), intent(in) :: v4_start(4)
!!        integer(kind = kint), intent(inout) :: isf_tgt_8
!!        real(kind = kreal), intent(inout) :: xi_surf_8(2)
!!        real(kind = kreal), intent(inout) :: x4_tgt_8(4)
!!        real(kind = kreal), intent(inout) :: v4_tgt_8(4)
!!
!!      subroutine fline_vector_at_one_element(iele, node, ele, v_trace,&
!!     &                                      v4_ele)
!!        integer(kind = kint), intent(in) :: iele
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        real(kind = kreal), intent(in) :: v_trace(node%numnod,3)
!!        real(kind = kreal), intent(inout) :: v4_ele(4,ele%nnod_4_ele)
!!@endverbatim
!
      module trace_in_element
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
      private :: velocity_on_surf_on_one_ele
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine find_backside_by_flux(surf, iflag_dir,                 &
     &                                 v4_start, isurf_org)
!
      integer(kind = kint), intent(in) :: iflag_dir
      type(surface_data), intent(in) :: surf
      real(kind = kreal), intent(in) ::   v4_start(4)
!
      integer(kind = kint), intent(inout) :: isurf_org(2)
!
      integer(kind = kint) :: isurf_sign, isurf_end
      real(kind = kreal) :: flux
!
!
      isurf_sign = surf%isf_4_ele(isurf_org(1),isurf_org(2))
      isurf_end = abs(isurf_sign)
      flux = (v4_start(1) * surf%vnorm_surf(isurf_end,1)                &
     &      + v4_start(2) * surf%vnorm_surf(isurf_end,2)                &
     &      + v4_start(3) * surf%vnorm_surf(isurf_end,3))               &
     &       * dble(iflag_dir) * dble(isurf_end / isurf_sign)

      if(flux .lt. 0) return
      if(isurf_sign .lt. 0) then
        isurf_org(1:2) =     surf%iele_4_surf(isurf_end,1,1:2)
      else
        isurf_org(1:2) =     surf%iele_4_surf(isurf_end,2,1:2)
      end if
!
      end subroutine find_backside_by_flux
!
!  ---------------------------------------------------------------------
!
      subroutine check_exit_in_double_number(surf, para_surf,           &
     &                                       isurf_org, isurf_org_dbl)
!
      use t_parallel_surface_indices
!
      type(surface_data), intent(in) :: surf
      type(paralell_surface_indices), intent(in) :: para_surf
      integer(kind = kint), intent(in) :: isurf_org(2)
!
      integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
!
      integer(kind = kint) :: isurf_end
!
!
      isurf_end = abs(surf%isf_4_ele(isurf_org(1),isurf_org(2)))
      if(para_surf%isf_4_ele_dbl(isurf_org(1),isurf_org(2),2)         &
     &                                                   .lt. 0) then
        isurf_org_dbl(1:3)                                            &
     &       = para_surf%iele_4_surf_dbl(isurf_end,1,1:3)
      else
        isurf_org_dbl(1:3)                                            &
     &       = para_surf%iele_4_surf_dbl(isurf_end,2,1:3)
      end if
!
      end subroutine check_exit_in_double_number
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine trace_to_element_wall(isf_org, iflag_dir, ele, surf,   &
     &          x4_ele, v4_ele, x4_start, v4_start,                     &
     &          isf_tgt_8, xi_surf_8, x4_tgt_8, v4_tgt_8)
!
      use cal_fline_in_cube
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: isf_org
      integer(kind = kint), intent(in) :: iflag_dir
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      real(kind = kreal), intent(in) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
!
      real(kind = kreal), intent(in) :: x4_start(4)
      real(kind = kreal), intent(in) :: v4_start(4)
!
      integer(kind = kint), intent(inout) :: isf_tgt_8
      real(kind = kreal), intent(inout) :: xi_surf_8(2)
      real(kind = kreal), intent(inout) :: x4_tgt_8(4)
      real(kind = kreal), intent(inout) :: v4_tgt_8(4)
!
      real(kind = kreal) :: v4_work(4,surf%nnod_4_surf)
!
!
      call find_line_end_in_ele_8(iflag_dir, isf_org,                   &
     &    ele%nnod_4_ele, surf%nnod_4_surf, surf%node_on_sf,            &
     &    v4_start, x4_start, x4_ele, isf_tgt_8, x4_tgt_8, xi_surf_8)
!
      call velocity_on_surf_on_one_ele(ele, surf, isf_tgt_8, v4_ele,    &
     &                                 v4_work)
      call cal_surf_field_value_2d(ifour, xi_surf_8, v4_work, v4_tgt_8)
!
      end subroutine trace_to_element_wall
!
!  ---------------------------------------------------------------------
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
      subroutine velocity_on_surf_on_one_ele(ele, surf,               &
     &          isf_in_ele, v4_ele, v4_work)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: isf_in_ele
      real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
!
      real(kind = kreal), intent(inout) :: v4_work(4,surf%nnod_4_surf)
!
      integer(kind = kint) :: k1, inod_lc
!
!
      do k1 = 1, 4
        inod_lc = surf%node_on_sf(k1,isf_in_ele)
        v4_work(1:4,k1) = v4_ele(1:4,inod_lc)
      end do
!
      end subroutine velocity_on_surf_on_one_ele
!
!  ---------------------------------------------------------------------
!
      end module trace_in_element

