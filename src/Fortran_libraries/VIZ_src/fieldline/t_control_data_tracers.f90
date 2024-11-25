!>@file   t_control_data_tracers.f90
!!@brief  module t_control_data_tracers
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine dealloc_tracer_controls(tracer_ctls)
!!        type(tracers_control), intent(inout) :: tracer_ctls
!!      subroutine add_flds_4_tracers_to_fld_ctl(tracer_ctls, field_ctl)
!!        type(tracers_control), intent(in) :: tracer_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!      subroutine read_tracer_controls                                 &
!!     &         (id_control, hd_block, tracer_ctls, c_buf)
!!        integer(kind = kint), intent(in) :: id_control 
!!        character(len=kchara), intent(in) :: hd_block
!!        type(tracers_control), intent(inout) :: tracer_ctls
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_tracer_controls(id_control, tracer_ctls, level)
!!        integer(kind = kint), intent(in) :: id_control 
!!        type(tracers_control), intent(in) :: tracer_ctls
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine init_tracers_ctl_label(hd_block, tracer_ctls)
!!        character(len=kchara), intent(in) :: hd_block
!!        type(tracers_control), intent(inout) :: tracer_ctls
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin tracers_control
!!    array  tracer_ctl
!!      file  tracer_ctl
!!
!!      begin tracer_ctl
!!        ...
!!      end   tracer_ctl
!!    end array tracer_ctl
!!  end  tracers_control
!!
!!    delta_t_tracer_output            1.0e-1
!!    i_step_tracer_output             400
!!  end tracers_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module t_control_data_tracers
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_flines
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
!
      implicit  none
!
!>        Structures of visualization controls
      type tracers_control
!>        Block name
        character(len=kchara) :: block_name = 'tracers_control'
!>        Structures of tracer controls
        type(fieldline_controls) :: tracer_controls
!
!>   Increment for field line
        type(read_integer_item) :: i_step_tracer_out_ctl
!>   time interval for field line
        type(read_real_item) ::    delta_t_tracer_out_ctl
!
        integer (kind=kint) :: i_tracers_control = 0
      end type tracers_control
!
      character(len=kchara), parameter, private                         &
     &       :: hd_tracer_ctl =             'tracer_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_tracer =          'i_step_tracer_output'
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_tracer =         'delta_t_tracer_output'
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_tracer_controls(tracer_ctls)
!
      type(tracers_control), intent(inout) :: tracer_ctls
!
!
      call dealloc_fline_ctl_struct(tracer_ctls%tracer_controls)
!
      tracer_ctls%delta_t_tracer_out_ctl%iflag = 0
      tracer_ctls%i_step_tracer_out_ctl%iflag = 0
!
      tracer_ctls%i_tracers_control = 0
!
      end subroutine dealloc_tracer_controls
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_flds_4_tracers_to_fld_ctl(tracer_ctls, field_ctl)
!
      use t_control_array_character3
!
      type(tracers_control), intent(in) :: tracer_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      if(tracer_ctls%tracer_controls%num_fline_ctl .gt. 0) then
        call add_fields_4_flines_to_fld_ctl                             &
     &     (tracer_ctls%tracer_controls, field_ctl)
      end if
!
      end subroutine add_flds_4_tracers_to_fld_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_tracer_controls                                   &
     &         (id_control, hd_block, tracer_ctls, c_buf)
!
      use t_read_control_elements
      use ctl_file_fieldlines_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
!
      type(tracers_control), intent(inout) :: tracer_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(tracer_ctls%i_tracers_control .gt. 0) return
      tracer_ctls%block_name = trim(hd_block)
      call init_fline_ctl_struct(hd_tracer_ctl,                         &
     &                           tracer_ctls%tracer_controls)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_files_4_fline_ctl(id_control, hd_tracer_ctl,          &
     &                              tracer_ctls%tracer_controls, c_buf)
!
        call read_integer_ctl_type(c_buf, hd_i_step_tracer,             &
     &                             tracer_ctls%i_step_tracer_out_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_tracer,               &
     &                          tracer_ctls%delta_t_tracer_out_ctl)
      end do
      tracer_ctls%i_tracers_control = 1
!
      end subroutine read_tracer_controls
!
!  ---------------------------------------------------------------------
!
      subroutine write_tracer_controls(id_control, tracer_ctls, level)
!
      use t_read_control_elements
      use ctl_file_fieldlines_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      type(tracers_control), intent(in) :: tracer_ctls
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(tracer_ctls%i_tracers_control .le. 0) return
!
      maxlen = len_trim(hd_delta_t_tracer)
      maxlen = max(maxlen, len_trim(hd_i_step_tracer))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 tracer_ctls%block_name)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    tracer_ctls%delta_t_tracer_out_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    tracer_ctls%i_step_tracer_out_ctl)
      call write_files_4_fline_ctl(id_control, hd_tracer_ctl,           &
     &                             tracer_ctls%tracer_controls, level)
!
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                             tracer_ctls%block_name)
!
      end subroutine write_tracer_controls
!
!  ---------------------------------------------------------------------
!
      subroutine init_tracers_ctl_label(hd_block, tracer_ctls)
!
      use ctl_file_fieldlines_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(tracers_control), intent(inout) :: tracer_ctls
!
!
      tracer_ctls%block_name = trim(hd_block)
      call init_fline_ctl_struct(hd_tracer_ctl,                         &
      &                          tracer_ctls%tracer_controls)
!
      call init_int_ctl_item_label(hd_i_step_tracer,                    &
     &                             tracer_ctls%i_step_tracer_out_ctl)
      call init_real_ctl_item_label(hd_delta_t_tracer,                  &
     &                             tracer_ctls%delta_t_tracer_out_ctl)
!
      end subroutine init_tracers_ctl_label
!
!  ---------------------------------------------------------------------
!
      subroutine tracer_step_ctls_to_time_ctl(tracer_ctls, tctl)
!
      use t_ctl_data_4_time_steps
!
      type(tracers_control), intent(in) :: tracer_ctls
      type(time_data_control), intent(inout) :: tctl
!
      if(tracer_ctls%i_step_tracer_out_ctl%iflag .gt. 0) then
        call copy_integer_ctl(tracer_ctls%i_step_tracer_out_ctl,        &
     &     tctl%i_step_tracer_output_ctl)
      end if
      if(tracer_ctls%delta_t_tracer_out_ctl%iflag .gt. 0) then
        call copy_real_ctl(tracer_ctls%delta_t_tracer_out_ctl,          &
     &                     tctl%delta_t_tracer_output_ctl)
      end if
!
      end subroutine tracer_step_ctls_to_time_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_tracers
