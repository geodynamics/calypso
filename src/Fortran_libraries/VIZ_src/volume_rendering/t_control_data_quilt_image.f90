!>@file   t_control_data_quilt_image.f90
!!@brief  module t_control_data_quilt_image
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for PVR quilt_c from snapshot
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_quilt_image_ctl                                 &
!!     &         (id_control, hd_block, quilt_c, c_buf)
!!        type(quilt_image_ctl), intent(inout) :: quilt_c
!!
!!      subroutine dup_quilt_image_ctl(org_quilt, new_quilt)
!!        type(quilt_image_ctl), intent(in) :: org_quilt
!!        type(quilt_image_ctl), intent(inout) :: new_quilt
!!      subroutine reset_quilt_image_ctl(quilt_c)
!!        type(quilt_image_ctl), intent(inout) :: quilt_c
!!
!!      subroutine bcast_quilt_image_ctl(quilt_c)
!!        type(quilt_image_ctl), intent(inout) :: quilt_c
!!
!!      integer(kind = kint) function num_label_quilt_image()
!!      subroutine set_label_quilt_image(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    Avaiable parameters for quilt_mode_ctl: BITMAP
!!
!!  begin quilt_image_ctl
!!    quilt_mode_ctl           BITMAP
!!    num_column_row_ctl       9    5
!!    num_row_column_ctl       9    5
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
!!    quilt_mode_ctl:   view_matrices, rotation, apature, LIC_kernel
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_quilt_image
!
      use m_precision
      use calypso_mpi
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
!>        Structure of quilt mode control
        type(read_character_item) :: quilt_mode_ctl
!
!>        Structure of number of columns and row of image
        type(read_int2_item) :: num_column_row_ctl
!>        Structure of number of row and columns of image
        type(read_int2_item) :: num_row_column_ctl
!
!         Lists of multiple view parameters
        type(multi_modeview_ctl) :: mul_qmats_c
!
!         integer flag of used block
        integer (kind=kint) :: i_quilt_image = 0
      end type quilt_image_ctl
!
!     3rd level for rotation
!
      character(len=kchara), parameter, private                         &
     &             :: hd_quilt_mode =  'quilt_mode_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_column_row =    'num_column_row_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_row_column =    'num_row_column_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_qview_transform =   'view_transform_ctl'
!
      integer(kind = kint), parameter :: n_label_quilt_ctl =   4
      private :: n_label_quilt_ctl
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
      use read_control_pvr_modelview
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(quilt_image_ctl), intent(inout) :: quilt_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (quilt_c%i_quilt_image.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type(c_buf, hd_quilt_mode,                  &
     &      quilt_c%quilt_mode_ctl)
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
!  ---------------------------------------------------------------------
!
      subroutine dup_quilt_image_ctl(org_quilt, new_quilt)
!
      use bcast_dup_view_transfer_ctl
!
      type(quilt_image_ctl), intent(in) :: org_quilt
      type(quilt_image_ctl), intent(inout) :: new_quilt
!
!
      call dup_mul_view_trans_ctl(org_quilt%mul_qmats_c,                &
     &                            new_quilt%mul_qmats_c)
!
      call copy_chara_ctl(org_quilt%quilt_mode_ctl,                     &
     &                    new_quilt%quilt_mode_ctl)
      call copy_integer2_ctl(org_quilt%num_column_row_ctl,              &
     &                       new_quilt%num_column_row_ctl)
      call copy_integer2_ctl(org_quilt%num_row_column_ctl,              &
     &                       new_quilt%num_row_column_ctl)
!
      new_quilt%i_quilt_image = org_quilt%i_quilt_image
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
      quilt_c%quilt_mode_ctl%iflag =     0
      quilt_c%num_column_row_ctl%iflag = 0
      quilt_c%num_row_column_ctl%iflag = 0
!
      quilt_c%i_quilt_image = 0
!
      end subroutine reset_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_quilt_image_ctl(quilt_c)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
      use bcast_dup_view_transfer_ctl
!
      type(quilt_image_ctl), intent(inout) :: quilt_c
!
!
      call calypso_mpi_bcast_one_int(quilt_c%i_quilt_image, 0)
!
      call bcast_ctl_type_c1(quilt_c%quilt_mode_ctl)
      call bcast_ctl_type_i2(quilt_c%num_column_row_ctl)
      call bcast_ctl_type_i2(quilt_c%num_row_column_ctl)
!
      call bcast_mul_view_trans_ctl(quilt_c%mul_qmats_c)
!
      end subroutine bcast_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_quilt_image()
      num_label_quilt_image = n_label_quilt_ctl
      return
      end function num_label_quilt_image
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_quilt_image(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_quilt_ctl)
!
!
      call set_control_labels(hd_quilt_mode, names( 1))
      call set_control_labels(hd_column_row, names( 2))
      call set_control_labels(hd_row_column, names( 3))
      call set_control_labels(hd_qview_transform, names( 4))
!
      end subroutine set_label_quilt_image
!
! ----------------------------------------------------------------------
!
      end module t_control_data_quilt_image
