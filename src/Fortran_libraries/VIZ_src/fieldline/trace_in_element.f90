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
!!      subroutine trace_to_element_wall                                &
!!     &         (isf_org, iflag_dir, ele, surf, viz_fields,            &
!!     &          x4_ele, v4_ele, c_ele, x4_start, v4_start,            &
!!     &          isf_tgt_8, x4_tgt_8, v4_tgt_8, c_tgt_8)
!!        integer(kind = kint), intent(in) :: isf_org
!!        integer(kind = kint), intent(in) :: iflag_dir
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(in) :: x4_ele(4,ele%nnod_4_ele)
!!        real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
!!        real(kind = kreal), intent(in)                                &
!!     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!!        real(kind = kreal), intent(in) :: x4_start(4)
!!        real(kind = kreal), intent(in) :: v4_start(4)
!!        integer(kind = kint), intent(inout) :: isf_tgt_8
!!        real(kind = kreal), intent(inout) :: x4_tgt_8(4)
!!        real(kind = kreal), intent(inout) :: v4_tgt_8(4)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: c_tgt_8(viz_fields%ntot_color_comp)
!!      subroutine update_fline_position(ratio, ntot_color_comp,        &
!!     &                                 x4_tgt, v4_tgt, c_tgt,         &
!!     &                                 x4_start, v4_start, c_field)
!!        real(kind = kreal), intent(in) :: ratio
!!        integer(kind = kint), intent(in) :: ntot_color_comp
!!        real(kind = kreal), intent(in) :: x4_tgt(4), v4_tgt(4)
!!        real(kind = kreal), intent(in) :: c_tgt(ntot_color_comp)
!!        real(kind = kreal), intent(inout) :: x4_start(4)
!!        real(kind = kreal), intent(inout) :: v4_start(4)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: c_field(ntot_color_comp)
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
      use t_paralell_surface_indices
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
      subroutine trace_to_element_wall                                  &
     &         (isf_org, iflag_dir, ele, surf, viz_fields,              &
     &          x4_ele, v4_ele, c_ele, x4_start, v4_start,              &
     &          isf_tgt_8, x4_tgt_8, v4_tgt_8, c_tgt_8)
!
      use cal_fline_in_cube
      use set_fields_after_tracing
!
      integer(kind = kint), intent(in) :: isf_org
      integer(kind = kint), intent(in) :: iflag_dir
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      real(kind = kreal), intent(in) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in)                                    &
     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!
      real(kind = kreal), intent(in) :: x4_start(4)
      real(kind = kreal), intent(in) :: v4_start(4)
!
      integer(kind = kint), intent(inout) :: isf_tgt_8
      real(kind = kreal), intent(inout) :: x4_tgt_8(4)
      real(kind = kreal), intent(inout) :: v4_tgt_8(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_tgt_8(viz_fields%ntot_color_comp)
!
      real(kind = kreal) :: xi_surf_8(2)
!

      call find_line_end_in_ele_8(iflag_dir, isf_org,                   &
     &    ele%nnod_4_ele, surf%nnod_4_surf, surf%node_on_sf,            &
     &    v4_start, x4_start, x4_ele, isf_tgt_8, x4_tgt_8, xi_surf_8)
!
      call fields_on_surf_from_one_ele                                  &
     &   (isf_tgt_8, xi_surf_8, ele, surf, viz_fields,                  &
     &    v4_ele, c_ele, x4_tgt_8, v4_tgt_8, c_tgt_8)
!
      end subroutine trace_to_element_wall
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine update_fline_position(ratio, ntot_color_comp,          &
     &                                 x4_tgt, v4_tgt, c_tgt,           &
     &                                 x4_start, v4_start, c_field)
!
      real(kind = kreal), intent(in) :: ratio
!
      integer(kind = kint), intent(in) :: ntot_color_comp
      real(kind = kreal), intent(in) :: x4_tgt(4), v4_tgt(4)
      real(kind = kreal), intent(in) :: c_tgt(ntot_color_comp)
!
      real(kind = kreal), intent(inout) :: x4_start(4)
      real(kind = kreal), intent(inout) :: v4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_field(ntot_color_comp)
!
       x4_start(1:4) = ratio * x4_tgt(1:4)                              &
     &               + (one - ratio) * x4_start(1:4)
       v4_start(1:4) = ratio * v4_tgt(1:4)                              &
     &               + (one - ratio) * v4_start(1:4)
       c_field(1:ntot_color_comp) =  ratio * c_tgt(1:ntot_color_comp)   &
     &                     + (one - ratio) * c_field(1:ntot_color_comp)
!
      end subroutine update_fline_position
!
!  ---------------------------------------------------------------------
!
      end module trace_in_element

