!>@file   trace_particle_in_element.f90
!!       module trace_particle_in_element
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief extend field line in each domain
!!
!!@verbatim
!!      subroutine s_trace_particle_in_element(dt, node, ele, surf,     &
!!     &          para_surf, nod_fld, v_prev, viz_fields,               &
!!     &          i_tracer, iflag_used_ele, isurf_org_dbl,              &
!!     &          x4_start, v4_start, c_field, progress,                &
!!     &          iflag_comm, inum)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!!        integer(kind = kint), intent(in) :: i_tracer
!!        integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
!!        real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: c_field(viz_fields%ntot_color_comp)
!!        real(kind = kreal), intent(inout) :: progress
!!        real(kind = kreal), intent(inout) :: dt
!!        real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
!!        integer(kind = kint), intent(inout) :: iflag_comm
!!@endverbatim
!
      module trace_particle_in_element
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
      private s_trace_in_element, ratio_of_trace_to_wall_tracer
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_trace_particle_in_element(dt, node, ele, surf,       &
     &          para_surf, nod_fld, v_prev, viz_fields,                 &
     &          i_tracer, iflag_used_ele, isurf_org_dbl,                &
     &          x4_start, v4_start, c_field, progress,                  &
     &          iflag_comm, inum)
!
      use t_local_fline
      use t_control_params_4_fline
      use trace_in_element
      use set_fields_after_tracing
!
      real(kind = kreal), intent(in) :: dt
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
      integer(kind = kint), intent(in) :: i_tracer
      integer(kind = kint), intent(in) :: inum
!
      integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
      real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                    :: c_field(viz_fields%ntot_color_comp)
      real(kind = kreal), intent(inout) :: progress
!
      real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
      integer(kind = kint), intent(inout) :: iflag_comm
!
      real(kind = kreal) :: v4_pre(4,ele%nnod_4_ele)
      real(kind = kreal) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal)                                                &
     &           :: color_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
      integer(kind = kint) :: isurf_org(2)
      integer(kind = kint) :: isf_tgt
      integer(kind = kint) :: jcou

!
      if(isurf_org_dbl(2) .eq. 0) then
        iflag_comm = 0
!        write(*,*) 'Exit at initial tracing', my_rank, inum
        return
      end if
!
      isurf_org(1:2) = isurf_org_dbl(2:3)
      if(isurf_org(2) .gt. 0) then
        call find_backside_by_flux(surf, iflag_forward_trace,           &
     &                             v4_start, isurf_org)
      end if
!
      jcou = 0
      iflag_comm = 0
      do
        jcou = jcou + 1
        call fline_vector_at_one_element(isurf_org(1), node, ele,       &
     &                                  node%xx, x4_ele)
        call fline_vector_at_one_element(isurf_org(1), node, ele,       &
     &                                  v_prev, v4_pre)
        call fline_vector_at_one_element(isurf_org(1), node, ele,       &
     &      nod_fld%d_fld(1,i_tracer), v4_ele)
        call fline_colors_at_one_element(isurf_org(1), ele,             &
     &      nod_fld, viz_fields, color_ele)
!
!   extend in the middle of element
        call s_trace_in_element(half, dt, isurf_org(2), ele, surf,      &
     &      viz_fields, x4_ele, v_prev, v4_ele, color_ele,              &
     &      isf_tgt, x4_start, v4_start, c_field, progress)
        if(isf_tgt .lt. 0) then
          iflag_comm = isf_tgt
          write(*,*) 'Trace stops by zero vector', my_rank, inum,       &
     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if
!
!   extend to surface of element
        call s_trace_in_element(one, dt, izero, ele, surf,              &
     &      viz_fields, x4_ele, v_prev, v4_ele, color_ele,              &
     &      isf_tgt, x4_start, v4_start, c_field, progress)
        if(progress .ge. 1.0d0) then
            iflag_comm = 0
!            write(*,*) 'Finish tracing', my_rank, inum
            exit
        end if
        if(isf_tgt .lt. 0) then
          iflag_comm = isf_tgt
!          write(*,*) 'Trace stops by zero vector', my_rank, inum,      &
!     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if

        isurf_org(2) = isf_tgt
        if(isurf_org(2) .gt. 0) then
!   set backside element and surface
          call check_exit_in_double_number(surf, para_surf,             &
     &                                     isurf_org, isurf_org_dbl)
          if(isurf_org_dbl(1) .ne. my_rank                              &
     &          .or. isurf_org_dbl(3) .eq. 0) then
            iflag_comm = 1
!            write(*,*) 'Exit for external surface', my_rank, inum
!       &          ': ', isurf_org_dbl(1:3), ': ',  &
!       &           para_surf%isf_4_ele_dbl(isurf_org(1),isurf_org(2),2)
            exit
          end if
!
          call find_backside_by_flux(surf, iflag_forward_trace,         &
     &                               v4_start, isurf_org)
        end if
!
        if(iflag_used_ele(isurf_org(1)) .eq. 0) then
!          isurf_org(2) = isf_tgt
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
      end subroutine s_trace_particle_in_element
!
!  ---------------------------------------------------------------------
!
      subroutine s_trace_in_element                                     &
     &         (trace_ratio, dt, isf_org, ele, surf, viz_fields,        &
     &          x4_ele, v4_pre, v4_ele, c_ele,                          &
     &          isf_tgt, x4_start, v4_start, c_field, progress)
!
      use coordinate_converter
      use convert_components_4_viz
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use trace_in_element
      use tracer_field_interpolate
!
      real(kind = kreal), intent(in) :: trace_ratio
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: isf_org
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(in) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in) :: v4_pre(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in)                                    &
     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: isf_tgt
      real(kind = kreal), intent(inout) :: x4_start(4)
      real(kind = kreal), intent(inout) :: v4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_field(viz_fields%ntot_color_comp)
      real(kind = kreal), intent(inout) :: progress
!
      real(kind = kreal) :: v4_current_e(4,ele%nnod_4_ele)
      real(kind = kreal) :: v4_tgt(4), x4_tgt(4)
      real(kind = kreal) :: c_tgt(viz_fields%ntot_color_comp)
      real(kind = kreal) :: ratio
!
!
      if((v4_start(1)**2+v4_start(2)**2+v4_start(3)**2) .le. zero) then
        isf_tgt = -3
        return
      end if
!
!$omp parallel workshare
      v4_current_e(1:4,1:ele%nnod_4_ele)                                &
     &   = (one - progress) * v4_pre(1:4,1:ele%nnod_4_ele)              &
     &           + progress * v4_ele(1:4,1:ele%nnod_4_ele)
!$omp end parallel workshare
!
      call trace_to_element_wall                                        &
     &   (isf_org, iflag_forward_line, ele, surf,                       &
     &    viz_fields, x4_ele, v4_current_e, c_ele, x4_start, v4_start,  &
     &    isf_tgt, x4_tgt, v4_tgt, c_tgt)
!
      call ratio_of_trace_to_wall_tracer(trace_ratio,                   &
     &    v4_start, x4_tgt, x4_start, dt, ratio, progress)
      call update_fline_position(ratio, viz_fields%ntot_color_comp,     &
     &                           x4_tgt, v4_tgt, c_tgt,                 &
     &                           x4_start, v4_start, c_field)
!
      end subroutine s_trace_in_element
!
!  ---------------------------------------------------------------------
!
      subroutine ratio_of_trace_to_wall_tracer(trace_ratio,             &
     &          v4_start, x4_tgt, x4_start, dt, ratio, progress)

      real(kind = kreal), intent(in) :: x4_tgt(4), x4_start(4)
      real(kind = kreal), intent(in) :: v4_start(4)
      real(kind = kreal), intent(in) :: dt, trace_ratio
      real(kind = kreal), intent(inout) :: ratio, progress
!
      real(kind = kreal) :: trip, dl, actual
!
      dl = dt * sqrt(v4_start(1) * v4_start(1)                          &
     &            +  v4_start(2) * v4_start(2)                          &
     &            +  v4_start(3) * v4_start(3))                         &
     &        * (one - progress)
      trip = sqrt((x4_tgt(1)-x4_start(1)) * (x4_tgt(1) - x4_start(1))   &
     &         + (x4_tgt(2)-x4_start(2)) * (x4_tgt(2) - x4_start(2))    &
     &         + (x4_tgt(3)-x4_start(3)) * (x4_tgt(3) - x4_start(3)))
!
      actual = trace_ratio * min(trip, dl)
      ratio =  actual / trip
      progress = progress + (one - progress) * actual / dl
!
      end subroutine ratio_of_trace_to_wall_tracer
!
!  ---------------------------------------------------------------------
!
      end module trace_particle_in_element

