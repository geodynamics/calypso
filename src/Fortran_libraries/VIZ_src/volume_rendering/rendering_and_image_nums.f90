!>@file   rendering_and_image_nums.f90
!!@brief  module rendering_and_image_nums
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine count_num_rendering_and_images(num_pvr, pvr_param,   &
!!     &          num_pvr_images, istack_pvr_images)
!!      subroutine set_rendering_and_image_pes(num_pe, num_pvr, pvr_ctl,&
!!     &          pvr_sort, num_pvr_images, pvr_rgb)
!!        integer, intent(in) :: num_pe
!!        integer(kind = kint), intent(in) :: num_pvr
!!        integer(kind = kint), intent(in) :: num_pvr_images
!!        type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!!        type(sort_PVRs_by_type), intent(in) :: PVR_sort
!!        integer(kind = kint), intent(inout)                           &
!!       &              :: istack_pvr_images(0:num_pvr)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!!@endverbatim
!
      module rendering_and_image_nums
!
      use m_precision
      use calypso_mpi
!
      use t_control_data_4_pvr
      use t_rendering_vr_image
      use t_pvr_image_array
      use t_sort_PVRs_by_type
!
      implicit none
!
      private :: set_pvr_file_prefix, set_pvr_file_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_rendering_and_images(num_pvr, pvr_param,     &
     &          num_pvr_images, istack_pvr_images)
!
      integer(kind = kint), intent(in) :: num_pvr
      type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_images(0:num_pvr)
      integer(kind = kint), intent(inout) :: num_pvr_images
!
      integer(kind = kint) :: i_pvr
!
!
      istack_pvr_images(0) = 0
      do i_pvr = 1, num_pvr
        if(pvr_param(i_pvr)%stereo_def%flag_quilt) then
          istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1)         &
     &          + pvr_param(i_pvr)%stereo_def%n_column_row_view(1)      &
     &           * pvr_param(i_pvr)%stereo_def%n_column_row_view(2)
        else if(pvr_param(i_pvr)%stereo_def%flag_anaglyph) then
          istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 2
        else
          istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 1
        end if
      end do
      num_pvr_images =    istack_pvr_images(num_pvr)
!
      if(iflag_debug .eq. 0) return
      write(*,*) my_rank, 'num_pvr',           num_pvr
      write(*,*) my_rank, 'num_pvr_images',    num_pvr_images
      write(*,*) my_rank, 'num_pvr_images',    istack_pvr_images
!
      end subroutine count_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_rendering_and_image_pes(num_pe, num_pvr, pvr_ctl,  &
     &          PVR_sort, num_pvr_images, pvr_rgb)
!
      use m_error_IDs
      use set_composition_pe_range
      use set_parallel_file_name
      use delete_data_files
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr
      integer(kind = kint), intent(in) :: num_pvr_images
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
      type(sort_PVRs_by_type), intent(in) :: PVR_sort
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, i_ctl, ist, ied, i
      logical :: flag_error
!
!
      call s_set_composition_pe_range                                   &
     &   (num_pe, num_pvr, PVR_sort%istack_PVR_modes,                   &
     &    num_pvr_images, PVR_sort%istack_pvr_images, pvr_rgb)
!
      do i_ctl = 1, num_pvr
        i_pvr = PVR_sort%ipvr_sorted(i_ctl)
        ist = PVR_sort%istack_pvr_images(i_pvr-1) + 1
        ied = PVR_sort%istack_pvr_images(i_pvr  )
        do i = ist, ied
          call set_pvr_file_control(pvr_ctl(i_ctl),                     &
     &                            pvr_rgb(i)%iflag_monitoring,          &
     &                            pvr_rgb(i)%id_pvr_file_type)
          pvr_rgb(i_pvr)%id_pvr_transparent = 0
          pvr_rgb(i)%pvr_prefix = set_pvr_file_prefix(pvr_ctl(i_ctl))
        end do
      end do
!
      flag_error = .FALSE.
      do i = 1, PVR_sort%istack_pvr_images(num_pvr)
        if(check_file_writable(my_rank, pvr_rgb(i)%pvr_prefix)          &
     &                               .eqv. .FALSE.) flag_error = .TRUE.
      end do
      if(flag_error) call calypso_mpi_abort(ierr_VIZ,                   &
     &                  'Check Directory for PVR output')
!
!
      if(iflag_debug .eq. 0) return
!      if(my_rank .gt. 0) return
      write(*,*) 'ID, File, ouput_PE, end_composition_PE, Num_PE'
      do i_pvr = 1, num_pvr_images
        write(*,*) i_pvr, trim(pvr_rgb(i_pvr)%pvr_prefix), '  ',        &
     &             pvr_rgb(i_pvr)%irank_image_file, &
     &                               pvr_rgb(i_pvr)%irank_end_composit, &
     &                                 pvr_rgb(i_pvr)%npe_img_composit, &
     &                                 trim(pvr_rgb(i_pvr)%pvr_prefix)
      end do
!
      end subroutine set_rendering_and_image_pes
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      character(len = kchara) function set_pvr_file_prefix(pvr_ctl)
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!
!
      if(pvr_ctl%file_head_ctl%iflag .gt. 0) then
        set_pvr_file_prefix = pvr_ctl%file_head_ctl%charavalue
      else 
        set_pvr_file_prefix = 'pvr'
      end if
!
      end function set_pvr_file_prefix
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_file_control(pvr_ctl,                          &
     &          iflag_monitoring, id_pvr_file_type)
!
      use t_control_params_4_pvr
      use set_area_4_viz
      use skip_comment_f
      use output_image_sel_4_png
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      integer(kind = kint), intent(inout) :: iflag_monitoring
      integer(kind = kint), intent(inout) :: id_pvr_file_type
!
      character(len = kchara) :: tmpchara
!
!
      tmpchara = pvr_ctl%file_fmt_ctl%charavalue
      if(cmp_no_case(tmpchara, hd_PNG)) then
        id_pvr_file_type = iflag_PNG
      else if(cmp_no_case(tmpchara, hd_QUILT_BMP)) then
        id_pvr_file_type = iflag_QUILT_BMP
      else if(cmp_no_case(tmpchara, hd_QUILT_BMP_GZ)                    &
     &     .or. cmp_no_case(tmpchara, hd_QUILT_BMP_GZ2)                 &
     &     .or. cmp_no_case(tmpchara, hd_QUILT_BMP_GZ3)                 &
     &     .or. cmp_no_case(tmpchara, hd_QUILT_BMP_GZ4)) then
        id_pvr_file_type = iflag_QUILT_BMP_GZ
      else if(cmp_no_case(tmpchara, hd_BMP)) then
        id_pvr_file_type = iflag_BMP
      else
        id_pvr_file_type = iflag_BMP
      end if
!
      iflag_monitoring = 0
      if(yes_flag(pvr_ctl%monitoring_ctl%charavalue)) then
        iflag_monitoring = 1
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'id_pvr_file_type', id_pvr_file_type
      end if
!
      end subroutine set_pvr_file_control
!
!  ---------------------------------------------------------------------
!
      end module rendering_and_image_nums
