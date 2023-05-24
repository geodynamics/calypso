!>@file  write_PVR_image.f90
!!       module write_PVR_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine sel_write_pvr_image_file(istep_pvr, i_rot, pvr_rgb)
!!      subroutine sel_write_pvr_local_img(index, istep_pvr, pvr_rgb)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!
!!      subroutine set_output_rot_sequence_image(istep_pvr, i_rot,      &
!!     &          iflag_img_fmt, file_prefix, num_img, n_column_row,    &
!!     &          rot_rgb)
!!        integer(kind = kint), intent(in) :: istep_pvr, i_rot
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
      subroutine sel_write_pvr_image_file(istep_pvr, i_rot, pvr_rgb)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      integer(kind = kint), intent(in) :: istep_pvr, i_rot
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      character(len=kchara) :: file_prefix_w_index, fname_tmp
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
      call cvt_double_rgba_to_char_rgb(pvr_rgb%num_pixel_xy,            &
     &    pvr_rgb%rgba_real_gl,  pvr_rgb%rgb_chara_gl)
!
      fname_tmp = add_int_suffix(istep_pvr, pvr_rgb%pvr_prefix)
      if(i_rot .ge. 0) then
        file_prefix_w_index = add_int_suffix(i_rot, fname_tmp)
      else
        file_prefix_w_index = fname_tmp
      end if
!
      write(*,*) trim(file_prefix_w_index),                             &
     &          ' is written from process ', my_rank
      call sel_output_image_file                                        &
     &   (pvr_rgb%id_pvr_file_type, file_prefix_w_index,                &
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
      subroutine set_output_rot_sequence_image(istep_pvr, i_rot,        &
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
      integer(kind = kint), intent(in) :: istep_pvr, i_rot
      integer(kind = kint), intent(in) :: num_img
      integer(kind = kint), intent(in) :: n_column_row(2)
      integer(kind = kint), intent(in) :: iflag_img_fmt
      character(len=kchara), intent(in) :: file_prefix
      type(pvr_image_type), intent(in) :: rot_rgb(num_img)
!
      integer(kind = kint) :: i_img, icou
      character(len=kchara) :: file_tmp, file_tmp2
!
!
      quilt_d%n_image = num_img
      quilt_d%n_column_row(1:2) = n_column_row(1:2)
!
      icou = 0
      do i_img = 1, num_img
        if(my_rank .eq. rot_rgb(i_img)%irank_image_file) icou = icou+1
      end do
      quilt_d%num_image_lc = icou
!
      if(iflag_img_fmt .eq. iflag_QUILT_BMP                             &
     &   .or. iflag_img_fmt .eq. iflag_QUILT_BMP_GZ) then
        write(file_tmp2,'(2a)') trim(file_prefix), '_qs'
        file_tmp = append_index(quilt_d%n_column_row(1), file_tmp2)
        write(file_tmp2,'(a,a1)') trim(file_tmp), 'x'
        file_tmp = append_index(quilt_d%n_column_row(2), file_tmp2)
      else
        file_tmp = file_prefix
      end if
!
      if(istep_pvr .ge. 0) then
        file_tmp2 = add_int_suffix(istep_pvr, file_tmp)
      else
        file_tmp2 = file_tmp
      end if
      if(i_rot .ge. 0) then
        quilt_d%image_seq_prefix = add_int_suffix(i_rot, file_tmp2)
      else
        quilt_d%image_seq_prefix = file_tmp2
      end if
!
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
      do i_img = 1, num_img
        if(my_rank .eq. rot_rgb(i_img)%irank_image_file) then
          icou = icou + 1
          quilt_d%icou_each_pe(icou) = i_img
          quilt_d%images(icou)%each_prefix                              &
     &         = add_int_suffix(i_img, quilt_d%image_seq_prefix)
          call cvt_double_rgba_to_char_rgb                              &
     &       (rot_rgb(i_img)%num_pixel_xy, rot_rgb(i_img)%rgba_real_gl, &
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
