!>@file   xyz_plane_rendering.f90
!!@brief  module xyz_plane_rendering
!!
!!@author H. Matsui
!!@date Programmed in July, 2023
!
!>@brief Subroutines to draw lines on map
!!
!!@verbatim
!!      subroutine aitoff_projection_rendering(time_d, psf_nod, psf_ele,&
!!     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!!      subroutine s_xyz_plane_rendering(time_d, psf_nod, psf_ele,      &
!!     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!!        type(time_data), intent(in) :: time_d
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(phys_data), intent(in) :: psf_phys
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(map_rendering_data), intent(inout) :: map_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
      module xyz_plane_rendering
!
      use m_precision
      use m_constants
!
      use t_psf_patch_data
      use t_time_data
      use t_file_IO_parameter
      use t_map_patch_from_1patch
      use t_pvr_image_array
      use t_map_rendering_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine aitoff_projection_rendering(time_d, psf_nod, psf_ele,  &
     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!
      use set_ucd_data_to_type
      use ucd_IO_select
!
      use draw_aitoff_map
      use draw_lines_on_map
      use draw_pvr_colorbar
      use draw_pixels_on_map
      use draw_lines_on_map
!
      type(time_data), intent(in) :: time_d
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(phys_data), intent(in) :: psf_phys
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      type(map_rendering_data), intent(inout) :: map_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      type(map_patches_for_1patch) :: map_e1
      real(kind = kreal), parameter                                     &
     &                   :: black(4) = (/zero,zero,zero,one/)
      real(kind = kreal), parameter                                     &
     &                   :: white(4) = (/one,one,one,one/)
      real(kind = kreal) :: color_ref(4)
!
      real(kind = kreal), allocatable :: phi_shift(:)
      real(kind = kreal) :: pi
      integer(kind = kint) :: i
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!$omp parallel workshare
      pvr_rgb%rgba_real_gl(1:4,1:pvr_rgb%num_pixel_actual) = 0.0d0
!$omp end parallel workshare
!
      pi = four*atan(one)
      call alloc_map_patch_from_1patch(map_e1)
      if(map_data%fill_flag) then
        call set_scalar_on_map_image(color_param, psf_nod, psf_ele,     &
     &      psf_phys%d_fld(1,1), map_data, pvr_rgb, map_e1)
        if(map_data%flag_zeroline .and. (map_data%num_line.le.0)) then
          call draw_aitoff_map_zeroline                                 &
     &       (psf_nod, psf_ele, psf_phys%d_fld(1,1), map_data,          &
     &        black, pvr_rgb, map_e1)
        end if
      else
        call fill_map_one_color                                         &
     &      (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),              &
     &       color_param%bg_rgba_real, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%num_line .gt. 0) then
        call draw_aitoff_map_isolines                                   &
     &     (psf_nod, psf_ele, psf_phys%d_fld(1,2), map_data,            &
     &      color_param, pvr_rgb, map_e1)
!
        if(map_data%flag_zeroline                                       &
     &        .and. (map_data%fill_flag.eqv. .FALSE.)) then
          call set_flame_color                                          &
     &       (map_data%fill_flag, color_param%bg_rgba_real, color_ref)
          call draw_aitoff_map_zeroline                                 &
     &       (psf_nod, psf_ele, psf_phys%d_fld(1,2), map_data,          &
     &        white, pvr_rgb, map_e1)
        end if
      end if
!
      call draw_latitude_grid(psf_nod, psf_ele, map_data,               &
     &    color_param%bg_rgba_real, pvr_rgb, map_e1)
      if(map_data%flag_tangent_cylinder) then
        call draw_map_tangent_cyl_grid(psf_nod, psf_ele, map_data,      &
     &      color_param%bg_rgba_real, map_data%tangent_cylinder_theta,  &
     &      pvr_rgb, map_e1)
      end if
!
      allocate(phi_shift(psf_nod%numnod))
      do i = 1, psf_nod%numnod
        if(psf_nod%xx(i,2) .ge. 0) then
          phi_shift(i) = psf_nod%phi(i)
        else
          phi_shift(i) = two*pi - psf_nod%phi(i)
        end if
      end do
!
      call draw_longitude_grid(psf_nod, psf_ele, phi_shift, map_data,   &
     &    color_param%bg_rgba_real, pvr_rgb, map_e1)
      call draw_mapflame(psf_nod, psf_ele, phi_shift, map_data,         &
     &    color_param%bg_rgba_real, pvr_rgb, map_e1)
      deallocate(phi_shift)
      call dealloc_map_patch_from_1patch(map_e1)
!
      call fill_background                                              &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    color_param%bg_rgba_real, pvr_rgb%rgba_real_gl)
!
      if(cbar_param%flag_pvr_colorbar) then
        call set_pvr_colorbar(pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels, &
     &      color_param, cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
!
      if(cbar_param%flag_draw_time) then
        call set_pvr_timelabel                                          &
     &     (time_d%time, pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,      &
     &      cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
!
      end subroutine aitoff_projection_rendering
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_xyz_plane_rendering(time_d, psf_nod, psf_ele,        &
     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!
      use set_scalar_on_xyz_plane
      use draw_xyz_plane_isolines
      use draw_pvr_colorbar
      use draw_pixels_on_map
      use draw_lines_on_map
      use cal_mesh_position
!
      type(time_data), intent(in) :: time_d
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(phys_data), intent(in) :: psf_phys
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      type(map_rendering_data), intent(inout) :: map_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      type(map_patches_for_1patch) :: map_e1
      real(kind = kreal), parameter                                     &
     &                   :: black(4) = (/zero,zero,zero,one/)
      real(kind = kreal), parameter                                     &
     &                   :: white(4) = (/one,one,one,one/)
      real(kind = kreal) :: color_ref(4)
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!$omp parallel workshare
      pvr_rgb%rgba_real_gl(1:4,1:pvr_rgb%num_pixel_actual) = 0.0d0
!$omp end parallel workshare
!
      call alloc_map_patch_from_1patch(map_e1)
      if(map_data%fill_flag) then
        call sel_scalar_on_xyz_plane                                    &
     &     (color_param, psf_nod, psf_ele, psf_phys%d_fld(1,1),         &
     &      map_data, pvr_rgb, map_e1)
!
        if(map_data%flag_zeroline .and. (map_data%num_line.le.0)) then
          call draw_xyz_plane_zeroline                                  &
     &       (psf_nod, psf_ele, psf_phys%d_fld(1,1), map_data,          &
     &        black, pvr_rgb, map_e1)
        end if
      else
        call fill_map_one_color                                         &
     &      (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),              &
     &       color_param%bg_rgba_real, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%num_line .gt. 0) then
        call s_draw_xyz_plane_isolines                                  &
     &     (psf_nod, psf_ele, psf_phys%d_fld(1,2), map_data,            &
     &      color_param, pvr_rgb, map_e1)
        if(map_data%flag_zeroline                                       &
     &        .and. (map_data%fill_flag.eqv. .FALSE.)) then
          call set_flame_color                                          &
     &       (map_data%fill_flag, color_param%bg_rgba_real, color_ref)
          call draw_xyz_plane_zeroline                                  &
     &       (psf_nod, psf_ele, psf_phys%d_fld(1,2), map_data,          &
     &        white, pvr_rgb, map_e1)
        end if
      end if
!
      if(map_data%iflag_2d_projection_mode .eq. iflag_xz_plane          &
     &   .or. map_data%iflag_2d_projection_mode .eq. iflag_xz_plane     &
     &  ) then
        if(map_data%flag_tangent_cylinder) then
          call draw_med_tangent_cyl_grid                                &
     &       (psf_nod, psf_ele, map_data, color_param%bg_rgba_real,     &
     &        map_data%tangent_cylinder_radius(2), pvr_rgb, map_e1)
        end if
      end if
!
      call draw_radius_grid(psf_nod, psf_ele, map_data,                 &
     &   color_param%bg_rgba_real, map_data%tangent_cylinder_radius(2), &
     &   pvr_rgb, map_e1)
      call draw_radius_grid(psf_nod, psf_ele, map_data,                 &
     &   color_param%bg_rgba_real, map_data%tangent_cylinder_radius(1), &
     &   pvr_rgb, map_e1)
      call dealloc_map_patch_from_1patch(map_e1)
!
      call fill_background                                              &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    color_param%bg_rgba_real, pvr_rgb%rgba_real_gl)
!
      if(cbar_param%flag_pvr_colorbar) then
        call set_pvr_colorbar(pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels, &
     &      color_param, cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
!
      if(cbar_param%flag_draw_time) then
        call set_pvr_timelabel                                          &
     &     (time_d%time, pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,      &
     &      cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
!
      end subroutine s_xyz_plane_rendering
!
!  ---------------------------------------------------------------------
!
      end module xyz_plane_rendering
