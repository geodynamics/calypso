!
!>@file   ctl_data_temp_model_IO
!!@brief  module ctl_data_temp_model_IO
!!
!!@author H. Matsui
!>@brief   Control of reference temperature for dynamo
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on Oct., 2007
!!
!!@verbatim
!!      subroutine read_reftemp_ctl                                     &
!!     &         (id_control, hd_block, reft_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(reference_temperature_ctl), intent(inout) :: reft_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_reftemp_ctl                                    &
!!     &         (id_control, hd_block, reft_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(reference_temperature_ctl), intent(in) :: reft_ctl
!!        integer(kind = kint), intent(inout) :: level
!!!!!!!!! model for stratification !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ref_temp_ctl: none           (No reference of temperature)
!!                  spherical_shell ( for spherical shell model)
!!                  takepiro        ( takepiro model )
!!                  numerical_solution ( Get numerical solution)
!!                  linear_x        ( propotional to x-direction )
!!                  linear_y        ( propotional to x-direction )
!!                  linear_z        ( propotional to x-direction )
!!
!!
!!    stratified_ctl:   0...off  1...on
!!     stratified_sigma_ctl: intense ofstratification
!!     stratified_width_ctl: width of stratification
!!     stratified_outer_r_ctl: outer boundary of stratification
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    begin temperature_define
!!      filtered_advection_ctl       Off
!!      ICB_diffusivity_reduction_ctl     0.1
!!
!!      ref_temp_ctl              spherical_shell
!!      ref_field_file_name      'reference_temp.dat'
!!      begin low_temp_ctl
!!           depth         1.5384615384615384
!!           temperature   0.0d0
!!      end  low_temp_ctl
!!      begin high_temp_ctl
!!           depth         0.5384615384615384
!!           temperature   1.0d0
!!      end  high_temp_ctl
!!
!!      stratified_ctl            Off
!!      begin takepiro_model_ctl
!!        stratified_sigma_ctl         0.000   end
!!        stratified_width_ctl         0.000   end
!!        stratified_outer_r_ctl       0.000   end
!!      end  takepiro_model_ctl
!!    end  temperature_define
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_data_temp_model_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_ctl_data_temp_model
      use skip_comment_f
!
      implicit  none
!
!   4th level for temperature define
!
      character(len=kchara), parameter, private                         &
     &    :: hd_filterd_advection = 'filtered_advection_ctl'
      character(len=kchara), parameter, private                         &
     &    :: hd_diffusivity_reduction = 'ICB_diffusivity_reduction_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_ref_temp =       'ref_temp_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_ref_field_file = 'ref_field_file_name'
      character(len=kchara), parameter, private                         &
     &       :: hd_low_temp =    'low_temp_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_high_temp =   'high_temp_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_start_ctl =   'stratified_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_takepiro_ctl = 'takepiro_model_ctl'
!
!    5th level for higher temp position
!
      character(len=kchara), parameter, private                         &
     &       :: hd_position =  'depth'
      character(len=kchara), parameter, private                         &
     &       :: hd_temp_value = 'temperature'
!
      private :: read_ref_temp_ctl, write_ref_temp_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_reftemp_ctl                                       &
     &         (id_control, hd_block, reft_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(reference_temperature_ctl), intent(inout) :: reft_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(reft_ctl%i_temp_def .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_ref_temp_ctl                                          &
     &     (id_control, hd_low_temp, reft_ctl%low_ctl, c_buf)
        call read_ref_temp_ctl                                          &
     &     (id_control, hd_high_temp, reft_ctl%high_ctl, c_buf)
!
        call read_takepiro_ctl(id_control, hd_takepiro_ctl,             &
     &      reft_ctl%takepiro_ctl, c_buf)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_filterd_advection, reft_ctl%filterd_advect_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_ref_temp, reft_ctl%reference_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_start_ctl, reft_ctl%stratified_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_ref_field_file, reft_ctl%ref_file_ctl)
!
        call read_real_ctl_type(c_buf, hd_diffusivity_reduction,        &
     &                          reft_ctl%ICB_diffuse_reduction_ctl)
      end do
      reft_ctl%i_temp_def = 1
!
      end subroutine read_reftemp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_reftemp_ctl                                      &
     &         (id_control, hd_block, reft_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(reference_temperature_ctl), intent(in) :: reft_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(reft_ctl%i_temp_def .le. 0) return
!
      maxlen = len_trim(hd_filterd_advection)
      maxlen = max(maxlen, len_trim(hd_diffusivity_reduction))
      maxlen = max(maxlen, len_trim(hd_ref_temp))
      maxlen = max(maxlen, len_trim(hd_start_ctl))
      maxlen = max(maxlen, len_trim(hd_ref_field_file))
!
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_filterd_advection, reft_ctl%filterd_advect_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_diffusivity_reduction,                                     &
     &    reft_ctl%ICB_diffuse_reduction_ctl)
!
      write(id_control,'(a1)') '!'
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_ref_temp, reft_ctl%reference_ctl)
      call write_ref_temp_ctl                                           &
     &   (id_control, hd_low_temp, reft_ctl%low_ctl, level)
      call write_ref_temp_ctl                                           &
     &   (id_control, hd_high_temp, reft_ctl%high_ctl, level)
!
      write(id_control,'(a1)') '!'
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_start_ctl, reft_ctl%stratified_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_ref_field_file, reft_ctl%ref_file_ctl)
      call write_takepiro_ctl(id_control, hd_takepiro_ctl,              &
     &    reft_ctl%takepiro_ctl, level)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_reftemp_ctl
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
        call load_one_line_from_control(id_control, c_buf)
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
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_position, ref_ctl%depth)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_temp_value, ref_ctl%value)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_ref_temp_ctl
!
!   --------------------------------------------------------------------
!
      end module ctl_data_temp_model_IO
