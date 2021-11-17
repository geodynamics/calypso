!>@file  write_PVR_image.f90
!!       module write_PVR_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine rendering_image(istep_pvr, time, mesh, group,        &
!!     &          sf_grp_4_sf, color_param, cbar_param, field_pvr,      &
!!     &          draw_param, pvr_screen, pvr_start, pvr_stencil,       &
!!     &          pvr_rgb, SR_sig, SR_r)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_field_data), intent(in) :: field_pvr
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(pvr_view_parameter), intent(in) :: view_param
!!        type(pvr_projected_position), intent(in) :: pvr_screen
!!        type(pvr_ray_start_type), intent(inout) :: pvr_start
!!        type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!!        type(pvr_segmented_img), intent(inout) :: pvr_img
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine sel_write_pvr_image_file(istep_pvr, pvr_rgb)
!!      subroutine sel_write_pvr_local_img(index, istep_pvr, pvr_rgb)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!
!!      subroutine set_output_rot_sequence_image(istep_pvr,             &
!!     &          iflag_img_fmt, file_prefix, num_img, n_column_row,    &
!!     &          rot_rgb)
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        integer(kind = kint), intent(in) :: num_img
!!        integer(kind = kint), intent(in) :: n_column_row(2)
!!        integer(kind = kint), intent(in) :: iflag_img_fmt
!!        character(len=kchara), intent(in) :: file_prefix
!!        type(pvr_image_type), intent(in) :: rot_rgb(num_img)
!!@endverbatim
!
      module write_PVR_image
!
      use m_precision
      use m_work_time
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_MPI_quilt_bitmap_IO
!
      implicit  none
!
      type(MPI_quilt_bitmap_IO), private, save :: quilt_d
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_image(istep_pvr, time, mesh, group,          &
     &          sf_grp_4_sf, color_param, cbar_param, field_pvr,        &
     &          draw_param, pvr_screen, pvr_start, pvr_stencil,         &
     &          pvr_rgb, SR_sig, SR_r)
!
      use m_geometry_constants
      use m_elapsed_labels_4_VIZ
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_pvr_image_array
      use t_pvr_ray_startpoints
      use t_pvr_stencil_buffer
      use t_pvr_field_data
      use t_solver_SR
!
      use ray_trace_4_each_image
      use draw_pvr_colorbar
      use pvr_axis_label
!      use composit_by_segmentad_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(pvr_field_data), intent(in) :: field_pvr
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(pvr_projected_position), intent(in) :: pvr_screen
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!      type(pvr_segmented_img), intent(inout) :: pvr_img
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+3)
      if(iflag_debug .gt. 0) write(*,*) 's_ray_trace_4_each_image'
      call s_ray_trace_4_each_image(mesh, group, sf_grp_4_sf,           &
     &    field_pvr, pvr_screen, draw_param, color_param, pvr_start)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+3)
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+4)
      if(iflag_debug .gt. 0) write(*,*) 'collect_rendering_image'
      call collect_rendering_image(pvr_start,                           &
     &    pvr_rgb%num_pixel_actual, pvr_rgb%rgba_real_gl, pvr_stencil,  &
     &    SR_sig, SR_r)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+4)
!
!      call composit_by_segmentad_image                                 &
!     &   (istep_pvr, iflag_PVR_time, ist_elapsed_PVR,                  &
!     &    pvr_start, pvr_stencil, pvr_img, pvr_rgb)
!
      if(my_rank .eq. pvr_rgb%irank_image_file) then
        if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+3)
        if(cbar_param%iflag_pvr_colorbar) then
          call set_pvr_colorbar                                         &
     &       (pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,                 &
     &        color_param, cbar_param, pvr_rgb%rgba_real_gl)
        end if
!
        if(cbar_param%iflag_draw_time) then
          call set_pvr_timelabel                                        &
     &       (time, pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,           &
     &        cbar_param, pvr_rgb%rgba_real_gl)
        end if
!
        if(cbar_param%iflag_pvr_axis) then
          call set_pvr_axislabel                                        &
     &       (pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,                 &
     &        pvr_screen, pvr_rgb%rgba_real_gl)
        end if
        if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+3)
      end if
!
      end subroutine rendering_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_pvr_image_file(istep_pvr, pvr_rgb)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
      call cvt_double_rgba_to_char_rgb(pvr_rgb%num_pixel_xy,            &
     &    pvr_rgb%rgba_real_gl,  pvr_rgb%rgb_chara_gl)
!
      write(*,*) trim(pvr_rgb%pvr_prefix), ' is written from process ', &
     &          my_rank
      call sel_output_image_file(pvr_rgb%id_pvr_file_type,              &
     &    add_int_suffix(istep_pvr, pvr_rgb%pvr_prefix),                &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    pvr_rgb%rgb_chara_gl)
      if(pvr_rgb%iflag_monitoring .gt. 0) then
        call sel_output_image_file                                      &
     &     (pvr_rgb%id_pvr_file_type, pvr_rgb%pvr_prefix,               &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%rgb_chara_gl)
      end if
!
      end subroutine sel_write_pvr_image_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_write_pvr_local_img(index, istep_pvr, pvr_rgb)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      integer(kind = kint), intent(in) :: index, istep_pvr
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!>      Local real image data
      real(kind = kreal), allocatable :: rgba_real_lc(:,:)
!>      RGB byte image data
      character(len = 1), allocatable :: rgb_chara_lc(:,:)
!
      character(len=kchara) :: tmpchara, img_head
!
!
      if(istep_pvr .ge. 0) then
        tmpchara = add_int_suffix(istep_pvr, pvr_rgb%pvr_prefix)
      else
        tmpchara = pvr_rgb%pvr_prefix
      end if
      img_head = add_int_suffix(index, tmpchara)
!
      allocate(rgb_chara_lc(3,pvr_rgb%num_pixel_xy))
      allocate(rgba_real_lc(4,pvr_rgb%num_pixel_xy))
!
!$omp parallel workshare
      rgba_real_lc = 0.0d0
!$omp end parallel workshare
!
      call cvt_double_rgba_to_char_rgb(pvr_rgb%num_pixel_xy,            &
     &                                 rgba_real_lc, rgb_chara_lc)
!
      call sel_output_image_file(pvr_rgb%id_pvr_file_type,              &
     &    img_head, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),       &
     &    rgb_chara_lc)
      deallocate(rgba_real_lc, rgb_chara_lc)
!
      end subroutine sel_write_pvr_local_img
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_output_rot_sequence_image(istep_pvr,               &
     &          iflag_img_fmt, file_prefix, num_img, n_column_row,      &
     &          rot_rgb)
!
      use t_control_params_4_pvr
      use t_pvr_image_array
      use t_MPI_quilt_bitmap_IO
      use convert_real_rgb_2_bite
      use set_parallel_file_name
      use output_image_sel_4_png
      use mpi_write_quilt_BMP_file
!
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: num_img
      integer(kind = kint), intent(in) :: n_column_row(2)
      integer(kind = kint), intent(in) :: iflag_img_fmt
      character(len=kchara), intent(in) :: file_prefix
      type(pvr_image_type), intent(in) :: rot_rgb(num_img)
!
      integer(kind = kint) :: i_rot, icou
      character(len=kchara) :: file_tmp
!
!
      quilt_d%n_image = num_img
      quilt_d%n_column_row(1:2) = n_column_row(1:2)
!
      icou = 0
      do i_rot = 1, num_img
        if(my_rank .eq. rot_rgb(i_rot)%irank_image_file) icou = icou+1
      end do
      quilt_d%num_image_lc = icou
!
      if(iflag_img_fmt .eq. iflag_QUILT_BMP                             &
     &   .or. iflag_img_fmt .eq. iflag_QUILT_BMP_GZ) then
        write(file_tmp,'(2a)') trim(file_prefix), '_quilt'
      else
        file_tmp = file_prefix
      end if
!
      if(istep_pvr .ge. 0) then
        quilt_d%image_seq_prefix = add_int_suffix(istep_pvr, file_tmp)
      else
        quilt_d%image_seq_prefix = file_tmp
      end if
      quilt_d%image_seq_format = iflag_img_fmt
      quilt_d%npixel_xy(1:2) = rot_rgb(1)%num_pixels(1:2)
      call alloc_quilt_rgb_images(quilt_d)
!
      do icou = 1, quilt_d%num_image_lc
        quilt_d%images(icou)%image_format = quilt_d%image_seq_format
        call alloc_each_rgb_image                                       &
     &     (quilt_d%npixel_xy, quilt_d%images(icou))
      end do
!
!
      icou = 0
      do i_rot = 1, num_img
        if(my_rank .eq. rot_rgb(i_rot)%irank_image_file) then
          icou = icou + 1
          quilt_d%icou_each_pe(icou) = i_rot
          quilt_d%images(icou)%each_prefix                              &
     &         = add_int_suffix(i_rot, quilt_d%image_seq_prefix)
          call cvt_double_rgba_to_char_rgb                              &
     &       (rot_rgb(i_rot)%num_pixel_xy, rot_rgb(i_rot)%rgba_real_gl, &
     &        quilt_d%images(icou)%rgb(1,1,1))
        end if
      end do
!
      call sel_write_pvr_image_files(quilt_d)
      call dealloc_quilt_rgb_images(quilt_d)
!
      end subroutine set_output_rot_sequence_image
!
!  ---------------------------------------------------------------------
!
      end module write_PVR_image
