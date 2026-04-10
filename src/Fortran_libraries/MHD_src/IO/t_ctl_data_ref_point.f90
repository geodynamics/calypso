!>@file   t_ctl_data_ref_point.f90
!!@brief  module t_ctl_data_ref_point
!!
!!@author H. Matsui
!>@brief   Control of reference temperature for dynamo
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on Oct., 2007
!!
!!@verbatim
!!      subroutine reset_ref_value_ctl(ref_ctl)
!!        type(reference_point_control), intent(inout) :: ref_ctl
!!
!!      subroutine read_ref_temp_ctl                                    &
!!     &         (id_control, hd_block, ref_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(reference_point_control), intent(inout) :: ref_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_ref_temp_ctl                                   &
!!     &         (id_control, hd_block, ref_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(reference_point_control), intent(in) :: ref_ctl
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine init_ref_temp_ctl_label(hd_block, ref_ctl)
!!        character(len=kchara), intent(in) :: hd_block
!!        type(reference_point_control), intent(inout) :: ref_ctl
!!
!!      subroutine read_ref_comp_ctl                                    &
!!     &         (id_control, hd_block, ref_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(reference_point_control), intent(inout) :: ref_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_ref_comp_ctl                                   &
!!     &         (id_control, hd_block, ref_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(reference_point_control), intent(in) :: ref_ctl
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine init_ref_comp_ctl_label(hd_block, ref_ctl)
!!        character(len=kchara), intent(in) :: hd_block
!!        type(reference_point_control), intent(inout) :: ref_ctl
!!
!!!!!!!!! Reference temperature model !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!      begin low_temp_ctl
!!           depth         1.5384615384615384
!!           temperature   0.0d0
!!      end  low_temp_ctl
!!      begin high_temp_ctl
!!           depth         0.5384615384615384
!!           temperature   1.0d0
!!      end  high_temp_ctl
!!
!!      begin low_comp_ctl
!!           depth         1.5384615384615384
!!           composition   0.0d0
!!      end  low_comp_ctl
!!      begin high_comp_ctl
!!           depth         0.5384615384615384
!!           composition   1.0d0
!!      end  high_comp_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_ref_point
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_ctl_data_stratified_model
      use t_ctl_data_valuable_diffuse
      use skip_comment_f
!
      implicit  none
!
!
      type reference_point_control
!>        Block name
        character(len=kchara) :: block_name  = 'low_temp_ctl'

        type(read_real_item) :: value
        type(read_real_item) :: depth
!
        integer (kind=kint) :: i_referenced = 0
      end type reference_point_control
!
!    5th level for higher temp position
!
      character(len=kchara), parameter, private                         &
     &       :: hd_position =  'depth'
      character(len=kchara), parameter, private                         &
     &       :: hd_temp_value = 'temperature'
      character(len=kchara), parameter                                  &
     &       :: hd_comp_value = 'composition'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine reset_ref_value_ctl(ref_ctl)
!
      type(reference_point_control), intent(inout) :: ref_ctl
!
      ref_ctl%depth%iflag = 0
      ref_ctl%value%iflag = 0
!
      ref_ctl%i_referenced = 0
!
      end subroutine reset_ref_value_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ref_temp_ctl                                      &
     &         (id_control, hd_block, ref_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(reference_point_control), intent(inout) :: ref_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(ref_ctl%i_referenced .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type(c_buf, hd_position, ref_ctl%depth)
        call read_real_ctl_type(c_buf, hd_temp_value, ref_ctl%value)
      end do
      ref_ctl%i_referenced = 1
!
      end subroutine read_ref_temp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_ref_temp_ctl                                     &
     &         (id_control, hd_block, ref_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(reference_point_control), intent(in) :: ref_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(ref_ctl%i_referenced .le. 0) return
!
      maxlen = len_trim(hd_position)
      maxlen = max(maxlen, len_trim(hd_temp_value))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    ref_ctl%depth)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    ref_ctl%value)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_ref_temp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine init_ref_temp_ctl_label(hd_block, ref_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(reference_point_control), intent(inout) :: ref_ctl
!
      ref_ctl%block_name = hd_block
        call init_real_ctl_item_label(hd_position, ref_ctl%depth)
        call init_real_ctl_item_label(hd_temp_value, ref_ctl%value)
      end subroutine init_ref_temp_ctl_label
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ref_comp_ctl                                      &
     &         (id_control, hd_block, ref_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(reference_point_control), intent(inout) :: ref_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(ref_ctl%i_referenced .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type(c_buf, hd_position, ref_ctl%depth)
        call read_real_ctl_type(c_buf, hd_comp_value, ref_ctl%value)
      end do
      ref_ctl%i_referenced = 1
!
      end subroutine read_ref_comp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_ref_comp_ctl                                     &
     &         (id_control, hd_block, ref_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(reference_point_control), intent(in) :: ref_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(ref_ctl%i_referenced .le. 0) return
!
      maxlen = len_trim(hd_position)
      maxlen = max(maxlen, len_trim(hd_comp_value))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    ref_ctl%depth)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    ref_ctl%value)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_ref_comp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine init_ref_comp_ctl_label(hd_block, ref_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(reference_point_control), intent(inout) :: ref_ctl
!
      ref_ctl%block_name = hd_block
      call init_real_ctl_item_label(hd_position, ref_ctl%depth)
      call init_real_ctl_item_label(hd_comp_value, ref_ctl%value)
      end subroutine init_ref_comp_ctl_label
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_ref_point
