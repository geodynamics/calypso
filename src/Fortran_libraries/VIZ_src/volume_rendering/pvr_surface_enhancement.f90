!>@file  pvr_surface_enhancement.f90
!!       module pvr_surface_enhancement
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Set flag and opacities to enhance surfaces
!!
!!@verbatim
!!      subroutine set_pvr_bc_enhanse_flag                              &
!!     &         (surf_grp, num_enhanse_grp, enhanse_grp, draw_type,    &
!!     &          fixed_opacity, iflag_enhanse, enhansed_opacity)
!!      real(kind = kreal) function opacity_by_surf_grp                 &
!!     &                (isurf, surf, surf_grp, sf_grp_4_sf,            &
!!     &                 modelview_mat, iflag_enhanse, enhansed_opacity)
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!@endverbatim
!
      module pvr_surface_enhancement
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use t_surface_data
      use t_surf_grp_list_each_surf
      use t_group_data
      use t_surface_group_normals
      use t_surface_group_connect
      use t_control_params_4_pvr
!
      use calypso_mpi
!
      implicit  none
!
      integer(kind = kint), parameter :: IFLAG_NONE =           0
      integer(kind = kint), parameter :: IFLAG_SHOW_EDGE =      2
      integer(kind = kint), parameter :: IFLAG_SHOW_REVERSE =  -1
      integer(kind = kint), parameter :: IFLAG_SHOW_BOTH =     10
      integer(kind = kint), parameter :: IFLAG_SHOW_FORWARD =   1
!
      real(kind = kreal), parameter, private :: coef_op = 4.0e-2
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_bc_enhanse_flag                                &
     &         (surf_grp, num_enhanse_grp, enhanse_grp, draw_type,      &
     &          fixed_opacity, iflag_enhanse, enhansed_opacity)
!
      use t_control_params_4_pvr
      use m_pvr_control_labels
      use skip_comment_f
!
      type(surface_group_data), intent(in) :: surf_grp
      integer(kind = kint), intent(in) :: num_enhanse_grp
      character(len=kchara), intent(in) :: enhanse_grp(num_enhanse_grp)
      character(len=kchara), intent(in) :: draw_type(num_enhanse_grp)
      real(kind = kreal), intent(in) :: fixed_opacity(num_enhanse_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: iflag_enhanse(surf_grp%num_grp)
      real(kind = kreal), intent(inout)                                 &
     &                     :: enhansed_opacity(surf_grp%num_grp)
!
      integer(kind = kint) :: igrp, jgrp
!
!
      iflag_enhanse(1:surf_grp%num_grp) = IFLAG_NONE
      do jgrp = 1, num_enhanse_grp
        do igrp = 1, surf_grp%num_grp
          if(cmp_no_case(enhanse_grp(jgrp),                             &
     &                   surf_grp%grp_name(igrp))) then
            if(cmp_no_case(draw_type(jgrp), LABEL_EDGE)) then
              iflag_enhanse(igrp) = IFLAG_SHOW_EDGE
            else if(cmp_no_case(draw_type(jgrp), LABEL_BOTH)) then
              iflag_enhanse(igrp) = IFLAG_SHOW_BOTH
            else if(cmp_no_case(draw_type(jgrp), LABEL_FORWARD)) then
              iflag_enhanse(igrp) = IFLAG_SHOW_FORWARD
            else if(cmp_no_case(draw_type(jgrp), LABEL_REVERSE)) then
              iflag_enhanse(igrp) = IFLAG_SHOW_REVERSE
            end if
            enhansed_opacity(igrp) = fixed_opacity(jgrp)
            exit
          end if
        end do
      end do
!
      end subroutine set_pvr_bc_enhanse_flag
!
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function opacity_by_surf_grp                   &
     &                (isurf, surf, surf_grp, sf_grp_4_sf,              &
     &                 modelview_mat, iflag_enhanse, enhansed_opacity)
!
      use set_position_pvr_screen
!
      integer(kind = kint), intent(in) :: isurf
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
      integer(kind = kint), intent(in)                                  &
     &                     :: iflag_enhanse(surf_grp%num_grp)
      real(kind = kreal), intent(in)                                    &
     &                     :: enhansed_opacity(surf_grp%num_grp)
!
      integer(kind = kint) :: igrp, ist, ied, inum
      real(kind = kreal) :: size_v, ratio, arccos_sf
      real(kind = kreal) :: norm_sf_model(4), tmp_normal(3)
!
!
      opacity_by_surf_grp = zero
      ist = sf_grp_4_sf%istack_grp_surf(isurf-1) + 1
      ied = sf_grp_4_sf%istack_grp_surf(isurf  )
      if(ied .lt. ist)  return
!
      arccos_sf = zero
      do inum = ist, ied
        igrp = sf_grp_4_sf%igrp_4_surf(inum)
!
        tmp_normal(1:3) = surf%vnorm_surf(isurf,1:3)
        norm_sf_model(1:4) = zero
        call chenge_direction_pvr_modelview(modelview_mat,              &
     &      ione, tmp_normal(1), norm_sf_model(1))
        if(norm_sf_model(3) .eq. zero) norm_sf_model(3) =  1e-6
!
        size_v = sqrt(norm_sf_model(1)*norm_sf_model(1)                 &
     &              + norm_sf_model(2)*norm_sf_model(2)                 &
     &              + norm_sf_model(3)*norm_sf_model(3))
        ratio = coef_op * size_v / norm_sf_model(3)
!
        if(iflag_enhanse(igrp) .eq. IFLAG_SHOW_EDGE) then
          if(abs(ratio) .gt. ONE) then
            arccos_sf = max(enhansed_opacity(igrp), arccos_sf)
          end if
        else if(iflag_enhanse(igrp) .eq. IFLAG_SHOW_BOTH) then
          arccos_sf = max(enhansed_opacity(igrp), arccos_sf)
        else if(iflag_enhanse(igrp) .eq. IFLAG_SHOW_FORWARD) then
          if(ratio .lt. zero) then
            arccos_sf = max(enhansed_opacity(igrp), arccos_sf)
          end if
        else if(iflag_enhanse(igrp) .eq. IFLAG_SHOW_REVERSE) then
          if(ratio .ge. zero) then
            arccos_sf = max(enhansed_opacity(igrp), arccos_sf)
          end if
        end if
      end do
      opacity_by_surf_grp = arccos_sf
!
      end function opacity_by_surf_grp
!
!  ---------------------------------------------------------------------
!
      end module pvr_surface_enhancement
