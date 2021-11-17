!start_surface_by_gl_table.f90
!
!      module start_surface_by_gl_table
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_start_surface_by_gl_table                          &
!!     &         (ele, ele_grp, fln_prm, fln_src)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: ele_grp
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!
      module start_surface_by_gl_table
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
      use t_group_data
      use t_control_params_4_fline
      use t_source_of_filed_line
!
      implicit  none
!
      private :: cnt_start_surface_by_gl_table
      private :: set_start_surface_by_gl_table
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_start_surface_by_gl_table                            &
     &         (ele, ele_grp, fln_prm, fln_src)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
!
!
      fln_src%num_line_local                                            &
     &   = cnt_start_surface_by_gl_table(ele%numele, ele%iele_global,   &
     &    ele%interior_ele, ele_grp%num_grp, ele_grp%num_item,          &
     &    ele_grp%istack_grp, ele_grp%item_grp, fln_prm)
      call set_start_surface_by_gl_table                                &
     &     (ele%numele, ele%iele_global,  ele%interior_ele,             &
     &      ele_grp%num_grp, ele_grp%num_item,                          &
     &      ele_grp%istack_grp, ele_grp%item_grp, fln_prm)
!
      end subroutine s_start_surface_by_gl_table
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function cnt_start_surface_by_gl_table       &
     &         (numele, iele_global, interior_ele, num_mat, num_mat_bc, &
     &          mat_istack, mat_item, fln_prm)
!
      integer(kind=kint), intent(in) :: numele
      integer(kind=kint_gl), intent(in) :: iele_global(numele)
      integer(kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
      type(fieldline_paramter), intent(in) :: fln_prm
!
      integer(kind = kint) :: inum, jgrp
      integer(kind = kint) :: icou, jnum, jele, jg, jst, jed
      integer(kind = kint_gl) :: iele_g
!
!
      icou = 0
      do inum = 1, fln_prm%num_each_field_line
        iele_g = fln_prm%id_gl_surf_start_fline(1,inum)
        do jgrp = 1, fln_prm%nele_grp_area_fline
          jg = fln_prm%id_ele_grp_area_fline(jgrp)
          jst = mat_istack(jg-1) + 1
          jed = mat_istack(jg)
          do jnum = jst, jed
            jele = mat_item(jnum)
            if(iele_g.eq.iele_global(jele)                              &
     &         .and. interior_ele(jele) .gt. 0) then
              icou = icou + 1
              exit
            end if
          end do
        end do
      end do
      cnt_start_surface_by_gl_table = icou
!
      end function cnt_start_surface_by_gl_table
!
!  ---------------------------------------------------------------------
!
      subroutine set_start_surface_by_gl_table                          &
     &         (numele, iele_global, interior_ele,                      &
     &          num_mat, num_mat_bc, mat_istack, mat_item, fln_prm)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint_gl), intent(in) :: iele_global(numele)
      integer(kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      integer(kind = kint) :: inum, jgrp
      integer(kind = kint) :: icou, jnum, jele, jg, jst, jed
      integer(kind = kint_gl) :: iele_g
!
!
      icou = 0
      do inum = 1, fln_prm%num_each_field_line
        iele_g = fln_prm%id_gl_surf_start_fline(1,inum)
        do jgrp = 1, fln_prm%nele_grp_area_fline
          jg = fln_prm%id_ele_grp_area_fline(jgrp)
          jst = mat_istack(jg-1) + 1
          jed = mat_istack(jg)
          do jnum = jst, jed
            jele = mat_item(jnum)
            if(iele_g.eq.iele_global(jele)                              &
     &           .and. interior_ele(jele) .gt. 0) then
              icou = icou + 1
              fln_prm%id_surf_start_fline(1,icou) = jele
              fln_prm%id_surf_start_fline(2,icou)                       &
     &             = fln_prm%id_gl_surf_start_fline(2,inum)
              exit
            end if
          end do
        end do
      end do
!
      end subroutine set_start_surface_by_gl_table
!
!  ---------------------------------------------------------------------
!
      end module start_surface_by_gl_table
