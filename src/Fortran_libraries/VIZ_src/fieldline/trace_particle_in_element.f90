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
!!     &          para_surf, nod_fld, v_prev, i_tracer, iflag_used_ele, &
!!     &          isurf_org_dbl, xx4_start, xi4_start, progress,        &
!!     &          iflag_comm, itp_ele_work, inum)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!!        integer(kind = kint), intent(in) :: i_tracer
!!        integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
!!        real(kind = kreal), intent(inout) :: xx4_start(4)
!!        real(kind = kreal), intent(inout) :: xi4_start(4)
!!        real(kind = kreal), intent(inout) :: v4_start(4)
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
      use t_parallel_surface_indices
      use t_phys_data
      use t_ctl_params_viz_fields
      use t_find_interpolate_in_ele
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
     &          para_surf, nod_fld, v_prev, i_tracer, iflag_used_ele,   &
     &          isurf_org_dbl, xx4_start, xi4_start, progress,          &
     &          iflag_comm, itp_ele_work, inum)
!
      use t_local_fline
      use t_control_params_4_fline
      use trace_in_element
!
      real(kind = kreal), intent(in) :: dt
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
      integer(kind = kint), intent(in) :: i_tracer
      integer(kind = kint), intent(in) :: inum
!
      integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
      real(kind = kreal), intent(inout) :: xx4_start(4)
      real(kind = kreal), intent(inout) :: xi4_start(4)
      real(kind = kreal), intent(inout) :: progress
!
      real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
      integer(kind = kint), intent(inout) :: iflag_comm
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!
      real(kind = kreal) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal) :: v4_start(4)
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
      call find_valocity_at_tracer                                      &
     &   (node, ele, v_prev, nod_fld%d_fld(1,i_tracer), isurf_org(1),   &
     &    progress, xx4_start, xi4_start, v4_start, itp_ele_work)
!
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
!
!   extend in the middle of element
        call s_trace_in_element(i_tracer, half, dt, isurf_org(2),       &
     &      ele, surf, x4_ele, v4_start, isf_tgt, xx4_start, progress)
        if(isf_tgt .lt. 0) then
          iflag_comm = isf_tgt
          write(*,*) 'Trace stops by zero vector', my_rank, inum,       &
     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if
        call find_valocity_at_tracer                                    &
     &     (node, ele, v_prev, nod_fld%d_fld(1,i_tracer), isurf_org(1), &
     &      progress, xx4_start, xi4_start, v4_start, itp_ele_work)
!
!   extend to surface of element
        call s_trace_in_element(i_tracer, one, dt, izero,               &
     &      ele, surf, x4_ele, v4_start, isf_tgt, xx4_start, progress)
        if(progress .ge. 1.0d0) then
            iflag_comm = 0
            write(*,*) 'Finish tracing', my_rank, inum
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
        call find_valocity_at_tracer                                    &
     &     (node, ele, v_prev, nod_fld%d_fld(1,i_tracer), isurf_org(1), &
     &      progress, xx4_start, xi4_start, v4_start, itp_ele_work)
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
      subroutine s_trace_in_element(i_tracer, trace_ratio, dt, isf_org, &
     &                              ele, surf, x4_ele, v4_start,        &
     &                              isf_tgt, xx4_start, progress)
!
      use cal_fline_in_cube
      use tracer_field_interpolate
!
      integer(kind = kint), intent(in) :: i_tracer
      real(kind = kreal), intent(in) :: trace_ratio
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: isf_org
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      real(kind = kreal), intent(in) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in) :: v4_start(4)
!
      integer(kind = kint), intent(inout) :: isf_tgt
      real(kind = kreal), intent(inout) :: xx4_start(4)
      real(kind = kreal), intent(inout) :: progress
!
      real(kind = kreal) :: xi_surf_tgt(2)
      real(kind = kreal) :: x4_tgt(4)
      real(kind = kreal) :: ratio
!
!
      if((v4_start(1)**2+v4_start(2)**2+v4_start(3)**2) .le. zero) then
        isf_tgt = -3
        return
      end if
!
      call find_line_end_in_ele_8(iflag_forward_line, isf_org,          &
     &    ele%nnod_4_ele, surf%nnod_4_surf, surf%node_on_sf,            &
     &    v4_start, xx4_start, x4_ele, isf_tgt, x4_tgt, xi_surf_tgt)
      call ratio_of_trace_to_wall_tracer(trace_ratio,                   &
     &    v4_start, x4_tgt, xx4_start, dt, ratio, progress)
       xx4_start(1:4) = ratio * x4_tgt(1:4)                             &
     &               + (one - ratio) * xx4_start(1:4)
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
      dl = sqrt(v4_start(1)*v4_start(1) + v4_start(2)*v4_start(2)       &
     &        + v4_start(3)*v4_start(3)) * (one - progress) * dt
      trip = sqrt((x4_tgt(1)-x4_start(1)) * (x4_tgt(1) - x4_start(1))   &
     &          + (x4_tgt(2)-x4_start(2)) * (x4_tgt(2) - x4_start(2))   &
     &          + (x4_tgt(3)-x4_start(3)) * (x4_tgt(3) - x4_start(3)))
!
      actual = trace_ratio * min(trip, dl)
      ratio =  actual / trip
      progress = progress + (one - progress) * actual / dl
!
      end subroutine ratio_of_trace_to_wall_tracer
!
!  ---------------------------------------------------------------------
!
      subroutine find_valocity_at_tracer(node, ele, v_prev, d_velo,     &
     &          iele_start, progress, xx4_start, xi4_start, v4_current, &
     &          itp_ele_work)
!
      use t_mesh_data
      use t_control_params_4_fline
      use field_at_each_seed_point
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      real(kind = kreal), intent(in) :: v_prev(node%numnod,3)
      real(kind = kreal), intent(in) :: d_velo(node%numnod,3)
!
      integer(kind = kint), intent(in) :: iele_start(1)
      real(kind = kreal), intent(in) :: progress
      real(kind = kreal), intent(in) :: xx4_start(4)
!
      real(kind = kreal), intent(inout) :: xi4_start(4)
      real(kind = kreal), intent(inout) :: v4_current(4)
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!
      real(kind = kreal) :: v4_prev(4), v4_now(4)
      integer(kind = kint) :: ierr_inter, iflag
!
      integer(kind = kint), parameter :: maxitr = 20
      real(kind = kreal), parameter ::   eps_iter = 1.0d-9
      integer(kind = kint), parameter :: iflag_nomessage = 0
      real(kind = kreal), parameter ::   error_level = 1.0d-9
!
!
      call find_interpolate_in_ele(xx4_start(1), maxitr, eps_iter,      &
     &    my_rank, iflag_nomessage, error_level, node, ele,             &
     &    iele_start(1), itp_ele_work, xi4_start, ierr_inter)
      iflag = surface_mode_in_each_ele(error_level,xi4_start)
!
      call cal_each_seed_velocity_in_ele(ele, node%numnod, v_prev(1,1), &
     &    iele_start(1), xi4_start, v4_prev)
      call cal_each_seed_velocity_in_ele(ele, node%numnod, d_velo(1,1), &
     &    iele_start(1), xi4_start, v4_now)
!
!$omp parallel workshare
      v4_current(1:4) = (one - progress) * v4_prev(1:4)                 &
     &                        + progress * v4_now(1:4)
!$omp end parallel workshare
!
      end subroutine find_valocity_at_tracer
!
!  ---------------------------------------------------------------------
!
      subroutine pick_surf_position4_from_ele(ele, surf, isf_local,     &
     &                                        xx4_ele, xx4_surf)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: isf_local
      real(kind = kreal), intent(in) :: xx4_ele(4,ele%nnod_4_ele)
!
      real(kind = kreal), intent(inout) :: xx4_surf(4,surf%nnod_4_surf)
!
      integer(kind = kint) :: inod, k1
!
!
      do k1 = 1, surf%nnod_4_surf
        inod = surf%node_on_sf(k1,isf_local)
        xx4_surf(k1,1:4) = xx4_ele(1:4,inod)
      end do
!
      end subroutine pick_surf_position4_from_ele
!
!-----------------------------------------------------------------------
!
      end module trace_particle_in_element

