!
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
!!      subroutine read_reftemp_ctl                                     &
!!     &         (id_control, hd_block, reft_ctl, c_buf)
!!      subroutine read_refcomp_ctl                                     &
!!     &          (id_control, hd_block, refc_ctl, c_buf)
!!      subroutine bcast_ref_scalar_ctl(refs_ctl)
!!        type(reference_temperature_ctl), intent(inout) :: refs_ctl
!!
!!!!!!!!! model for stratification !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ref_temp_ctl: none           (No reference of temperature)
!!                  spherical_shell ( for spherical shell model)
!!                  takepiro        ( takepiro model )
!!                  numrical_solution ( Get numerical solution)
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
      use skip_comment_f
      use bcast_control_arrays
!
      implicit  none
!
!
      type reference_point_control
        type(read_real_item) :: value
        type(read_real_item) :: depth
!
        integer (kind=kint) :: i_referenced = 0
      end type reference_point_control
!
      type takepiro_model_control
        type(read_real_item) :: stratified_sigma_ctl
        type(read_real_item) :: stratified_width_ctl
        type(read_real_item) :: stratified_outer_r_ctl
!
        integer (kind=kint) :: i_takepiro_t_ctl = 0
      end type takepiro_model_control
!
      type reference_temperature_ctl
        type(read_character_item) :: filterd_advect_ctl
        type(read_character_item) :: reference_ctl
        type(read_character_item) :: stratified_ctl
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
!   4th level for temperature define
!
      character(len=kchara), parameter                                  &
     &       :: hd_ref_temp =    'ref_temp_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_low_temp =    'low_temp_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_high_temp =   'high_temp_ctl'
!
      character(len=kchara), parameter                                  &
     &    :: hd_filterd_advection = 'filtered_advection_ctl'
      character(len=kchara), parameter                                  &
     &    :: hd_diffusivity_reduction = 'ICB_diffusivity_reduction_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_ref_comp =    'ref_comp_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_low_comp =    'low_comp_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_high_comp =   'high_comp_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_strat_ctl =   'stratified_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_takepiro_ctl = 'takepiro_model_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_strat_sigma = 'stratified_sigma_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_strat_width = 'stratified_width_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_strat_outer = 'stratified_outer_r_ctl'
!
!    5th level for higher temp position
!
      character(len=kchara), parameter                                  &
     &       :: hd_position =  'depth'
      character(len=kchara), parameter                                  &
     &       :: hd_temp_value = 'temperature'
      character(len=kchara), parameter                                  &
     &       :: hd_comp_value = 'composition'
!
      private :: hd_filterd_advection, hd_diffusivity_reduction
      private :: hd_ref_temp, hd_ref_comp
      private :: hd_strat_ctl, hd_strat_sigma
      private :: hd_strat_width, hd_strat_outer
      private :: hd_low_temp, hd_high_temp, hd_low_comp, hd_high_comp
      private :: hd_position, hd_temp_value, hd_comp_value
      private :: hd_takepiro_ctl
!
      private :: read_ref_temp_ctl, read_ref_comp_ctl
      private :: read_takepiro_ctl
      private :: bcast_ref_value_ctl, bcast_takepiro_ctl
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
     &     (c_buf, hd_strat_ctl, reft_ctl%stratified_ctl)
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
      subroutine read_refcomp_ctl                                       &
     &          (id_control, hd_block, refc_ctl, c_buf)
!
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(reference_temperature_ctl), intent(inout) :: refc_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(refc_ctl%i_temp_def .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_ref_comp_ctl                                          &
     &     (id_control, hd_low_comp, refc_ctl%low_ctl, c_buf)
        call read_ref_comp_ctl                                          &
     &     (id_control, hd_high_comp, refc_ctl%high_ctl, c_buf)
        call read_takepiro_ctl                                          &
     &     (id_control, hd_takepiro_ctl, refc_ctl%takepiro_ctl, c_buf)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_filterd_advection, refc_ctl%filterd_advect_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_ref_comp, refc_ctl%reference_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_strat_ctl, refc_ctl%stratified_ctl)
!
        call read_real_ctl_type(c_buf, hd_diffusivity_reduction,        &
     &                          refc_ctl%ICB_diffuse_reduction_ctl)
      end do
      refc_ctl%i_temp_def = 1
!
      end subroutine read_refcomp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ref_scalar_ctl(refs_ctl)
!
      use calypso_mpi_int
!
      type(reference_temperature_ctl), intent(inout) :: refs_ctl
!
!
      call bcast_ref_value_ctl(refs_ctl%low_ctl)
      call bcast_ref_value_ctl(refs_ctl%high_ctl)
      call bcast_takepiro_ctl(refs_ctl%takepiro_ctl)
!
      call bcast_ctl_type_c1(refs_ctl%filterd_advect_ctl)
      call bcast_ctl_type_c1(refs_ctl%reference_ctl)
      call bcast_ctl_type_c1(refs_ctl%stratified_ctl)
!
      call bcast_ctl_type_r1(refs_ctl%ICB_diffuse_reduction_ctl)
!
      call calypso_mpi_bcast_one_int(refs_ctl%i_temp_def, 0)
!
      end subroutine bcast_ref_scalar_ctl
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
        call load_one_line_from_control(id_control, c_buf)
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
      subroutine read_takepiro_ctl                                      &
     &         (id_control, hd_block, takepiro_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(takepiro_model_control), intent(inout) :: takepiro_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(takepiro_ctl%i_takepiro_t_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_strat_sigma, takepiro_ctl%stratified_sigma_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_strat_width, takepiro_ctl%stratified_width_ctl)
        call read_real_ctl_type(c_buf, hd_strat_outer,                  &
     &      takepiro_ctl%stratified_outer_r_ctl)
      end do
      takepiro_ctl%i_takepiro_t_ctl = 1
!
      end subroutine read_takepiro_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_ref_value_ctl(ref_ctl)
!
      use calypso_mpi_int
!
      type(reference_point_control), intent(inout) :: ref_ctl
!
!
      call bcast_ctl_type_r1(ref_ctl%depth)
      call bcast_ctl_type_r1(ref_ctl%value)
!
      call calypso_mpi_bcast_one_int(ref_ctl%i_referenced, 0)
!
      end subroutine bcast_ref_value_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_takepiro_ctl(takepiro_ctl)
!
      use calypso_mpi_int
!
      type(takepiro_model_control), intent(inout) :: takepiro_ctl
!
!
      call bcast_ctl_type_r1(takepiro_ctl%stratified_sigma_ctl)
      call bcast_ctl_type_r1(takepiro_ctl%stratified_width_ctl)
      call bcast_ctl_type_r1(takepiro_ctl%stratified_outer_r_ctl)
!
      call calypso_mpi_bcast_one_int(takepiro_ctl%i_takepiro_t_ctl, 0)
!
      end subroutine bcast_takepiro_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_temp_model
