!>@file  extend_field_line.f90
!!       module extend_field_line
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief extend field line in each domain
!!
!!@verbatim
!!      subroutine s_extend_field_line(node, ele, surf, para_surf,      &
!!     &          nod_fld, viz_fields, max_line_step, end_trace,        &
!!     &          iflag_used_ele, iglobal_fline, iflag_dir, i_fline,    &
!!     &          isurf_org_dbl, x4_start, v4_start, c_field,           &
!!     &          c_field, icount_line, iflag_comm, fline_lc)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(in) ::   end_trace
!!        integer(kind = kint_gl), intent(in) :: iglobal_fline
!!        integer(kind = kint), intent(in) :: iflag_dir, max_line_step
!!        integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!!        integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
!!        integer(kind = kint), intent(inout) :: icount_line, iflag_comm
!!        real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
!!        real(kind = kreal), intent(inout) ::   c_field(1)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!@endverbatim
!
      module extend_field_line
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
!
      use t_geometry_data
      use t_surface_data
      use t_paralell_surface_indices
      use t_phys_data
      use t_ctl_params_viz_fields
!
      implicit  none
!
      private :: fline_trace_in_element, ratio_of_trace_to_wall_fline
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_extend_field_line(node, ele, surf, para_surf,        &
     &          nod_fld, viz_fields, max_line_step, end_trace,          &
     &          iflag_used_ele, iglobal_fline, iflag_dir, i_fline,      &
     &          isurf_org_dbl, x4_start, v4_start, c_field,             &
     &          icount_line, trace_length, iflag_comm, fline_lc, inum)
!
      use t_local_fline
      use trace_in_element
      use set_fields_after_tracing
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      integer(kind = kint), intent(in) :: i_fline
      integer(kind = kint_gl), intent(in) :: iglobal_fline
!
      integer(kind = kint), intent(in) :: inum
      real(kind = kreal), intent(in) ::   end_trace
      integer(kind = kint), intent(in) :: iflag_dir, max_line_step
      integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!
      integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
      real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                    :: c_field(viz_fields%ntot_color_comp)
!
      type(local_fieldline), intent(inout) :: fline_lc
      real(kind = kreal), intent(inout) :: trace_length
      integer(kind = kint), intent(inout) :: icount_line, iflag_comm
!
      real(kind = kreal) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal)                                                &
     &           :: color_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
      integer(kind = kint) :: isf_tgt, jcou
      integer(kind = kint) :: isurf_org(2)
!
!
      if(isurf_org_dbl(2) .eq. 0) then
        iflag_comm = 0
!        write(*,*) 'Exit at initial tracing', my_rank, inum
        return
      end if
!
      isurf_org(1:2) = isurf_org_dbl(2:3)
      if(isurf_org(2) .gt. 0) then
        call find_backside_by_flux(surf, iflag_dir,                     &
     &                             v4_start, isurf_org)
      end if
!
      call add_fline_start(x4_start, v4_start,                          &
     &    viz_fields%ntot_color_comp, c_field(1), fline_lc)
!
      jcou = 0
      iflag_comm = 0
      do
        jcou = jcou + 1
        icount_line = icount_line + 1
        call fline_vector_at_one_element(isurf_org(1), node, ele,       &
     &                                  node%xx, x4_ele)
        call fline_vector_at_one_element(isurf_org(1), node, ele,       &
     &      nod_fld%d_fld(1,i_fline), v4_ele)
        call fline_colors_at_one_element(isurf_org(1), ele,             &
     &      nod_fld, viz_fields, color_ele)
!
!   extend in the middle of element
        call fline_trace_in_element(half, end_trace, trace_length,      &
     &      isurf_org(2), iflag_dir, ele, surf,                         &
     &      viz_fields, x4_ele, v4_ele, color_ele,                      &
     &      isf_tgt, x4_start, v4_start, c_field)
        if(isf_tgt .lt. 0) then
          iflag_comm = isf_tgt
!          write(*,*) 'Trace stops by zero vector', my_rank, inum,      &
!     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
!          write(*,*) 'Error at trace to mid point', my_rank, inum,     &
!     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if
        call add_fline_list(iglobal_fline, x4_start, v4_start,          &
     &      viz_fields%ntot_color_comp, c_field(1), fline_lc)
        if(trace_length.ge.end_trace .and. end_trace.gt.zero) return
!
!   extend to surface of element
        call fline_trace_in_element(one, end_trace, trace_length,       &
     &      izero, iflag_dir, ele, surf,                                &
     &      viz_fields, x4_ele, v4_ele, color_ele,                      &
     &      isf_tgt, x4_start, v4_start, c_field)
        if(isf_tgt .lt. 0) then
          iflag_comm = isf_tgt
!          write(*,*) 'Trace stops by zero vector', my_rank, inum,      &
!     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
!          write(*,*) 'Error at trace to end point', my_rank, inum,     &
!     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if
        call add_fline_list(iglobal_fline, x4_start, v4_start,          &
     &      viz_fields%ntot_color_comp, c_field(1), fline_lc)
        if(trace_length.ge.end_trace .and. end_trace.gt.zero) exit
!
        isurf_org(2) = isf_tgt
!
!   Check domain of new starting surface
        call check_exit_in_double_number(surf, para_surf,               &
     &                                   isurf_org, isurf_org_dbl)
        if(isurf_org_dbl(1) .ne. my_rank                                &
     &        .or. isurf_org_dbl(3) .eq. 0) then
            iflag_comm = 1
!            write(*,*) 'Exit for external surface', my_rank, inum
!       &            ': ', isurf_org_dbl(1:3), ': ',  &
!       &             para_surf%isf_4_ele_dbl(isurf_org(1),isf_tgt,2)
          exit
        end if
!
!   Check domain of backside element and surface
        call find_backside_by_flux(surf, iflag_dir,                     &
     &                             v4_start, isurf_org)
!
        if(max_line_step.gt.0 .and. icount_line.gt.max_line_step) then
            iflag_comm = 0
!            write(*,*) 'Exit by trace counts', my_rank, inum
            exit
        end if
        if(iflag_used_ele(isurf_org(1)) .eq. 0) then
          iflag_comm = 1
!          write(*,*) 'Exit from tracing area', my_rank, inum
          exit
        end if
        if(isurf_org(1) .eq. 0) then
          iflag_comm = -2
!          write(*,*) 'Trace leaves from domain', my_rank, inum
          exit
        end if
      end do
!
      end subroutine s_extend_field_line
!
!  ---------------------------------------------------------------------
!
      subroutine fline_trace_in_element                                 &
     &         (trace_ratio, end_trace, trace_length,                   &
     &          isf_org, iflag_dir, ele, surf,                          &
     &          viz_fields, x4_ele, v4_ele, c_ele,                      &
     &          isf_tgt, x4_start, v4_start, c_field)
!
      use coordinate_converter
      use convert_components_4_viz
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use trace_in_element
      use tracer_field_interpolate
!
      real(kind = kreal), intent(in) :: trace_ratio
      real(kind = kreal), intent(in) ::   end_trace
      real(kind = kreal), intent(inout) :: trace_length
!
      integer(kind = kint), intent(in) :: isf_org
      integer(kind = kint), intent(in) :: iflag_dir
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(in) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in)                                    &
     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: isf_tgt
      real(kind = kreal), intent(inout) :: x4_start(4)
      real(kind = kreal), intent(inout) :: v4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_field(viz_fields%ntot_color_comp)
!
      real(kind = kreal) :: v4_tgt(4), x4_tgt_8(4)
      real(kind = kreal) :: c_tgt(viz_fields%ntot_color_comp)
      real(kind = kreal) :: ratio
!
!
      if((v4_start(1)**2+v4_start(2)**2+v4_start(3)**2) .le. zero) then
        isf_tgt = -3
        return
      end if
!
      call trace_to_element_wall(isf_org, iflag_dir, ele, surf,         &
     &    viz_fields, x4_ele, v4_ele, c_ele, x4_start, v4_start,        &
     &    isf_tgt, x4_tgt_8, v4_tgt, c_tgt)
      if(isf_tgt .le. 0) return
!
      call ratio_of_trace_to_wall_fline(end_trace, trace_ratio,         &
     &                                  x4_tgt_8, x4_start,             &
     &                                  ratio, trace_length)
      call update_fline_position(ratio, viz_fields%ntot_color_comp,     &
     &                           x4_tgt_8, v4_tgt, c_tgt,               &
     &                           x4_start, v4_start, c_field)
!
      end subroutine fline_trace_in_element
!
!  ---------------------------------------------------------------------
!
      subroutine ratio_of_trace_to_wall_fline(end_trace, trace_ratio,   &
     &                                        x4_tgt, x4_start,         &
     &                                        ratio, trace_length)

      real(kind = kreal), intent(in) :: x4_tgt(4), x4_start(4)
      real(kind = kreal), intent(in) :: end_trace
      real(kind = kreal), intent(in) :: trace_ratio
      real(kind = kreal), intent(inout) :: ratio, trace_length
!
      real(kind = kreal) :: trip, rest_trace
!
!
      if(trace_length .ge. end_trace .and. end_trace .gt. zero) then
        rest_trace =  (end_trace - trace_length) 
        trip = sqrt((x4_tgt(1)-x4_start(1)) * (x4_tgt(1) - x4_start(1)) &
     &           + (x4_tgt(2)-x4_start(2)) * (x4_tgt(2) - x4_start(2))  &
     &           + (x4_tgt(3)-x4_start(3)) * (x4_tgt(3) - x4_start(3)))
        ratio = min(rest_trace/trip, trace_ratio)
        trace_length = trace_length + (one - ratio) * trip
      else
        ratio = trace_ratio
      end if
!
      end subroutine ratio_of_trace_to_wall_fline
!
!  ---------------------------------------------------------------------
!
      end module extend_field_line

