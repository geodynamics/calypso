!>@file   t_ctl_data_temp_model.f90
!!@brief  module t_ctl_data_temp_model
!!
!!@author H. Matsui
!>@brief   Control of reference temperature for dynamo
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on Oct., 2007
!!
!!@verbatim
!!      subroutine reset_ref_scalar_ctl(refs_ctl)
!!        type(reference_temperature_ctl), intent(inout) :: refs_ctl
!!
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
!!
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
!!
!!    begin composition_define
!!      filtered_advection_ctl    Off
!!
!!      ref_comp_ctl              spherical_shell
!!      ref_field_file_name      'reference_comp.dat'
!!      begin low_comp_ctl
!!           depth         1.5384615384615384
!!           composition   0.0d0
!!      end  low_comp_ctl
!!      begin high_comp_ctl
!!           depth         0.5384615384615384
!!           composition   1.0d0
!!      end  high_comp_ctl
!!
!!      stratified_ctl            Off
!!      begin takepiro_model_ctl
!!        stratified_sigma_ctl         0.000   end
!!        stratified_width_ctl         0.000   end
!!        stratified_outer_r_ctl       0.000   end
!!      end  takepiro_model_ctl
!!    end  composition_define
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_temp_model
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_ctl_data_stratified_model
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
      type reference_temperature_ctl
!>        Block name
        character(len=kchara) :: block_name                             &
     &                          = 'temperature_define'

        type(read_character_item) :: filterd_advect_ctl
        type(read_character_item) :: reference_ctl
        type(read_character_item) :: stratified_ctl
        type(read_character_item) :: ref_file_ctl
!
        type(read_real_item) :: ICB_diffuse_reduction_ctl
!
        type(reference_point_control) :: low_ctl
        type(reference_point_control) :: high_ctl
        type(takepiro_model_control) :: takepiro_ctl
!
        integer (kind=kint) :: i_temp_def = 0
      end type reference_temperature_ctl
!
      private :: reset_ref_value_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine reset_ref_scalar_ctl(refs_ctl)
!
      type(reference_temperature_ctl), intent(inout) :: refs_ctl
!
      call reset_ref_value_ctl(refs_ctl%low_ctl)
      call reset_ref_value_ctl(refs_ctl%high_ctl)
      call reset_takepiro_ctl(refs_ctl%takepiro_ctl)
!
      refs_ctl%ICB_diffuse_reduction_ctl%iflag = 0
      refs_ctl%filterd_advect_ctl%iflag =   0
      refs_ctl%reference_ctl%iflag =        0
      refs_ctl%stratified_ctl%iflag =       0
      refs_ctl%ref_file_ctl%iflag =         0
!
      refs_ctl%i_temp_def = 0
!
      end subroutine reset_ref_scalar_ctl
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
!
      end module t_ctl_data_temp_model
