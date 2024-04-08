!>@file   t_ctl_data_quilt_image.f90
!!@brief  module t_ctl_data_quilt_image
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for PVR quilt_c from snapshot
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine init_quilt_image_ctl_label(hd_block, quilt_c)
!!      subroutine read_quilt_image_ctl                                 &
!!     &         (id_control, hd_block, quilt_c, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(quilt_image_ctl), intent(inout) :: quilt_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_quilt_image_ctl                                &
!!     &         (id_control, hd_block, quilt_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(quilt_image_ctl), intent(in) :: quilt_c
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine dup_quilt_image_ctl(org_quilt, new_quilt)
!!        type(quilt_image_ctl), intent(in) :: org_quilt
!!        type(quilt_image_ctl), intent(inout) :: new_quilt
!!      subroutine reset_quilt_image_ctl(quilt_c)
!!        type(quilt_image_ctl), intent(inout) :: quilt_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  begin quilt_image_ctl
!!
!!    array view_transform_ctl
!!      file  view_transform_ctl  control_view
!!
!!      begin view_transform_ctl
!!        ..
!!      end
!!    end array view_transform_ctl
!!  end quilt_image_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_quilt_image
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_integer
      use t_control_array_integer2
      use t_control_array_real2
      use t_ctl_data_view_transfers
      use skip_comment_f
!
      implicit  none
!
!
!>       Structure of quilt image controls
      type quilt_image_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'quilt_image_ctl'
!
!>        Structure of number of columns and row of image
        type(read_int2_item) :: num_column_row_ctl
!>        Structure of number of row and columns of image
        type(read_int2_item) :: num_row_column_ctl
!
!         Lists of multiple view parameters
        type(multi_modelview_ctl) :: mul_qmats_c
!
!         integer flag of used block
        integer (kind=kint) :: i_quilt_image = 0
      end type quilt_image_ctl
!
!     3rd level for rotation
!
      character(len=kchara), parameter, private                         &
     &             :: hd_column_row =    'num_column_row_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_row_column =    'num_row_column_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_qview_transform =   'view_transform_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_quilt_image_ctl                                   &
     &         (id_control, hd_block, quilt_c, c_buf)
!
      use ctl_file_pvr_modelview_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(quilt_image_ctl), intent(inout) :: quilt_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if (quilt_c%i_quilt_image.gt.0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_integer2_ctl_type(c_buf, hd_column_row,               &
     &      quilt_c%num_column_row_ctl)
        call read_integer2_ctl_type(c_buf, hd_row_column,               &
     &      quilt_c%num_row_column_ctl)
!
        call read_mul_view_transfer_ctl                                 &
     &     (id_control, hd_qview_transform, quilt_c%mul_qmats_c, c_buf)
      end do
      quilt_c%i_quilt_image = 1
!
      end subroutine read_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_quilt_image_ctl                                  &
     &         (id_control, hd_block, quilt_c, level)
!
      use ctl_file_pvr_modelview_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(quilt_image_ctl), intent(in) :: quilt_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(quilt_c%i_quilt_image .le. 0) return
!
      maxlen = len_trim(hd_column_row)
      maxlen = max(maxlen, len_trim(hd_row_column))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_integer2_ctl_type(id_control, level, maxlen,           &
     &    quilt_c%num_column_row_ctl)
      call write_integer2_ctl_type(id_control, level, maxlen,           &
     &    quilt_c%num_row_column_ctl)
!
      call write_mul_view_transfer_ctl                                  &
     &   (id_control, hd_qview_transform, quilt_c%mul_qmats_c, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_quilt_image_ctl_label(hd_block, quilt_c)
!
      use ctl_file_pvr_modelview_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(quilt_image_ctl), intent(inout) :: quilt_c
!
!
      quilt_c%block_name = hd_block
      call init_multi_modeview_ctl(hd_qview_transform,                  &
     &                             quilt_c%mul_qmats_c)
!
        call init_integer2_ctl_item_label(hd_column_row,                &
     &      quilt_c%num_column_row_ctl)
        call init_integer2_ctl_item_label(hd_row_column,                &
     &      quilt_c%num_row_column_ctl)
!
      end subroutine init_quilt_image_ctl_label
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_quilt_image_ctl(org_quilt, new_quilt)
!
      type(quilt_image_ctl), intent(in) :: org_quilt
      type(quilt_image_ctl), intent(inout) :: new_quilt
!
!
      call dup_mul_view_trans_ctl(org_quilt%mul_qmats_c,                &
     &                            new_quilt%mul_qmats_c)
!
      call copy_integer2_ctl(org_quilt%num_column_row_ctl,              &
     &                       new_quilt%num_column_row_ctl)
      call copy_integer2_ctl(org_quilt%num_row_column_ctl,              &
     &                       new_quilt%num_row_column_ctl)
!
      new_quilt%i_quilt_image = org_quilt%i_quilt_image
      new_quilt%block_name =    org_quilt%block_name
!
      end subroutine dup_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_quilt_image_ctl(quilt_c)
!
      type(quilt_image_ctl), intent(inout) :: quilt_c
!
!
      call dealloc_multi_modeview_ctl(quilt_c%mul_qmats_c)
!
      quilt_c%num_column_row_ctl%iflag = 0
      quilt_c%num_row_column_ctl%iflag = 0
!
      quilt_c%i_quilt_image = 0
!
      end subroutine reset_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_quilt_image
