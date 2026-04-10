!>@file   pixel_rendering_tracers.f90
!!        module pixel_rendering_tracers
!!
!!@author H. Matsui
!!@date Programmed in 2024
!!
!>@brief Rendering trace particle and field lines
!!
!!@verbatim
!!      subroutine rendering_tracers(viewpoint_vec, color_param,        &
!!     &          tracer_pvr_prm, num_tracer, particle_lc,              &
!!     &          xx4_tgt, c_tgt, rgba_ray, rgba_now)
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        type(tracer_render_param), intent(in) :: tracer_pvr_prm
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        integer(kind = kint), intent(in) :: num_tracer
!!        type(local_fieldline), intent(in)                             &
!!     &                      :: particle_lc(np_smp,num_tracer)
!!        real(kind = kreal), intent(in) :: xx4_tgt(4)
!!        real(kind = kreal), intent(in) :: c_tgt(1)
!!        real(kind = kreal), intent(inout) :: rgba_ray(4), rgba_now(4)
!!      subroutine rendering_fieldlines(viewpoint_vec, color_param,     &
!!     &          fline_pvr_prm, num_fline, fline_lc,                   &
!!     &          xx4_tgt, c_tgt, rgba_ray, rgba_now)
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(tracer_render_param), intent(in) :: fline_pvr_prm
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(local_fieldline), intent(in) :: fline_lc(np_smp,num_fline)
!!        real(kind = kreal), intent(in) :: xx4_tgt(4)
!!        real(kind = kreal), intent(in) :: c_tgt(1)
!!        real(kind = kreal), intent(inout) :: rgba_ray(4), rgba_now(4)
!!@endverbatim
!
      module pixel_rendering_tracers
!
      use m_precision
      use m_machine_parameter
!
      use t_local_fline
      use t_pvr_colormap_parameter
      use t_ctl_param_tracer_render
!
      implicit none
!
      private :: distance_from_point, single_normalize_vector
      private :: distance_from_line_segment
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine rendering_tracers(viewpoint_vec, color_param,          &
     &          tracer_pvr_prm, num_tracer, particle_lc,                &
     &          xx4_tgt, c_tgt, rgba_ray, rgba_now)
!
      use set_rgba_4_each_pixel
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      type(tracer_render_param), intent(in) :: tracer_pvr_prm
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      integer(kind = kint), intent(in) :: num_tracer
      type(local_fieldline), intent(in)                                 &
     &                      :: particle_lc(np_smp,num_tracer)
!
      real(kind = kreal), intent(in) :: xx4_tgt(4)
      real(kind = kreal), intent(in) :: c_tgt(1)
!
      real(kind = kreal), intent(inout) :: rgba_ray(4), rgba_now(4)
!
      integer(kind = kint) :: i_fln, inum, increment, ip
      integer(kind = kint_gl) :: i_global
      real(kind = kreal) :: grad_tgt(3), radius, distance
      real(kind = kreal) :: rgb_color(3), opacity
      real(kind = kreal) :: xyzw(4)
!
!
      if(tracer_pvr_prm%num_pvr_tracer .le. 0) return
      do i_fln = 1, tracer_pvr_prm%num_pvr_tracer
        increment =      tracer_pvr_prm%increment(i_fln)
        radius =         tracer_pvr_prm%rendering_radius(i_fln)
        opacity =        tracer_pvr_prm%tracer_opacity(i_fln)
        rgb_color(1:3) = tracer_pvr_prm%tracer_RGB(1:3,i_fln)
        do ip = 1, np_smp
          do inum = 1, particle_lc(ip,i_fln)%nnod_line_l
            i_global = particle_lc(ip,i_fln)%iglobal_fline(inum)
            if(mod(i_global-1,increment) .ne. 0) cycle
!
            xyzw(1:3) = particle_lc(ip,i_fln)%xx_line_l(1:3,inum)
            xyzw(4) =   one
            distance = distance_from_point(xx4_tgt, xyzw(1))
            if(distance .ge. radius) cycle
!            opacity = opacity * (one - sqrt(distance / radius))
!
            grad_tgt(1:3) = xx4_tgt(1:3)                                &
     &                     - particle_lc(ip,i_fln)%xx_line_l(1:3,inum)
            call single_normalize_vector(grad_tgt)
!
            if(tracer_pvr_prm%iflag_color_mode(i_fln)                   &
     &                          .eq. iflag_single_color) then
              call surface_rendering_with_light                         &
     &           (viewpoint_vec, xx4_tgt, grad_tgt, rgb_color,          &
     &            opacity, color_param, rgba_ray, rgba_now)
            else
              call color_plane_with_light                               &
     &           (viewpoint_vec, xx4_tgt, c_tgt(1), grad_tgt,           &
     &            opacity, color_param, rgba_ray, rgba_now)
            end if
!
          end do
        end do
      end do
!
      end subroutine rendering_tracers
!
! ----------------------------------------------------------------------
!
      subroutine rendering_fieldlines(viewpoint_vec, color_param,       &
     &          fline_pvr_prm, num_fline, fline_lc,                     &
     &          xx4_tgt, c_tgt, rgba_ray, rgba_now)
!
      use set_rgba_4_each_pixel
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(tracer_render_param), intent(in) :: fline_pvr_prm
!
      integer(kind = kint), intent(in) :: num_fline
      type(local_fieldline), intent(in) :: fline_lc(np_smp,num_fline)
      real(kind = kreal), intent(in) :: xx4_tgt(4)
      real(kind = kreal), intent(in) :: c_tgt(1)
!
      real(kind = kreal), intent(inout) :: rgba_ray(4), rgba_now(4)
!
      integer(kind = kint) :: i_fln, iedge, inod, increment, ip
      integer(kind = kint) :: i1, i2
      integer(kind = kint_gl) :: i_global
      real(kind = kreal) :: grad_tgt(3), radius, distance
      real(kind = kreal) :: rgb_color(3), opacity
      real(kind = kreal) :: xyzw_1(4), xyzw_2(4)
!
!
      if(fline_pvr_prm%num_pvr_tracer .le. 0) return
      do i_fln = 1, fline_pvr_prm%num_pvr_tracer
        increment = fline_pvr_prm%increment(i_fln)
        radius =    fline_pvr_prm%rendering_radius(i_fln)
        opacity =   fline_pvr_prm%tracer_opacity(i_fln)
        rgb_color(1:3) = fline_pvr_prm%tracer_RGB(1:3,i_fln)
        do ip = 1, np_smp
          do iedge = 1, fline_lc(ip,i_fln)%nele_line_l
            inod =     fline_lc(ip,i_fln)%iedge_line_l(1,iedge)
            i_global = fline_lc(ip,i_fln)%iglobal_fline(inod)
            if(mod(i_global-1,increment) .ne. 0) cycle
!
            i1 = fline_lc(ip,i_fln)%iedge_line_l(1,iedge)
            i2 = fline_lc(ip,i_fln)%iedge_line_l(2,iedge)
            xyzw_1(1:3) = fline_lc(ip,i_fln)%xx_line_l(1:3,i1)
            xyzw_1(4) =   one
            xyzw_2(1:3) = fline_lc(ip,i_fln)%xx_line_l(1:3,i2)
            xyzw_2(4) =   one
            distance = distance_from_line_segment(xx4_tgt,              &
     &                                            xyzw_1(1), xyzw_2(1))
!
            if(distance .ge. radius) cycle
!            opacity = opacity * (one - sqrt(distance / radius))
            i1 = fline_lc(ip,i_fln)%iedge_line_l(1,iedge)
            i2 = fline_lc(ip,i_fln)%iedge_line_l(2,iedge)
            grad_tgt(1:3) = xx4_tgt(1:3)                                &
     &                 - half * (fline_lc(ip,i_fln)%xx_line_l(1:3,i1)   &
     &                         + fline_lc(ip,i_fln)%xx_line_l(1:3,i2))
            call single_normalize_vector(grad_tgt)
!
            if(fline_pvr_prm%iflag_color_mode(i_fln)                    &
     &                          .eq. iflag_single_color) then
              call surface_rendering_with_light                         &
     &           (viewpoint_vec, xx4_tgt, grad_tgt, rgb_color,          &
     &            opacity, color_param, rgba_ray, rgba_now)
            else
              call color_plane_with_light                               &
     &           (viewpoint_vec, xx4_tgt, c_tgt(1), grad_tgt,           &
     &            opacity, color_param, rgba_ray, rgba_now)
            end if
!
          end do
        end do
      end do
!
      end subroutine rendering_fieldlines
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      real(kind = kreal) function distance_from_point(point, xyzw1)
!
      use cal_vector_products
!
      real(kind = kreal), intent(in) :: point(4), xyzw1(4)
      real(kind = kreal) :: x_line(1:4)
!
      x_line(1:4) = xyzw1(1:4) - point(1:4)
      distance_from_point = single_dot_product(x_line(1), x_line(1))
!
      end function distance_from_point
!
! ----------------------------------------------------------------------
!
      subroutine single_normalize_vector(vector)
!
      use cal_vector_products
!
      real (kind=kreal), intent(inout) :: vector(3)
      real (kind=kreal) :: length
!
      length = max(single_dot_product(vector(1), vector(1)), TINY)
      vector(1:3) = vector(1:3) / length
!
      end subroutine single_normalize_vector
!
! ----------------------------------------------------------------------
!
      real(kind = kreal) function distance_from_line_segment            &
     &                  (point, xyzw1, xyzw2)
!
      use cal_vector_products
!
      real(kind = kreal), intent(in) :: point(4), xyzw1(4), xyzw2(4)
!
      real(kind = kreal) :: vec1(1:4), vec2(1:4)
      real(kind = kreal) :: x_line(1:4), c_prod(1:4)
      real(kind = kreal) :: dot1, dot2, area
      real(kind = kreal) :: seg_len, dist_line
!
      vec1(1:4) =   point(1:4) - xyzw1(1:4)
      vec2(1:4) =   xyzw2(1:4) - xyzw1(1:4)
      dot1 = single_dot_product(vec1(1), vec2(1))
      vec1(1:4) =   point(1:4) - xyzw2(1:4)
      vec2(1:4) =   xyzw1(1:4) - xyzw2(1:4)
      dot2 = single_dot_product(vec1(1), vec2(1))
!
      if     (dot1 .le. zero) then
        vec1(1:4) =   point(1:4) - xyzw1(1:4)
        dist_line = single_dot_product(vec1(1), vec1(1))
      else if(dot2 .le. zero) then
        vec2(1:4) =   point(1:4) - xyzw2(1:4)
        dist_line = single_dot_product(vec2(1), vec2(1))
      else
        x_line(1:4) = xyzw2(1:4) - xyzw1(1:4)
        seg_len = single_dot_product(x_line(1), x_line(1))
        call single_cross_product(vec1(1), vec2(1), c_prod)
        area =    single_dot_product(c_prod(1), c_prod(1))
        dist_line = area / seg_len
      end if
      distance_from_line_segment = dist_line
!
      end function distance_from_line_segment
!
! ----------------------------------------------------------------------
!
      end module pixel_rendering_tracers
