!>@file   t_control_data_surfacings.f90
!!@brief  module t_control_data_surfacings
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine dealloc_surfacing_controls(surfacing_ctls)
!!       type(surfacing_controls), intent(inout) :: surfacing_ctls
!!       type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine section_step_ctls_to_time_ctl(surfacing_ctls, tctl)
!!        type(surfacing_controls), intent(in) :: surfacing_ctls
!!        type(time_data_control), intent(inout) :: tctl
!!
!!      subroutine add_fields_4_scts_to_fld_ctl(surfacing_ctls,         &
!!     &                                        field_ctl)
!!        type(surfacing_controls), intent(in) :: surfacing_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin visual_control
!!    delta_t_sectioning_ctl   1.0e-3
!!    i_step_sectioning_ctl    400
!!    array  cross_section_ctl  1
!!      ....
!!    end array cross_section_ctl
!!
!!    delta_t_isosurface_ctl   1.0e-3
!!    i_step_isosurface_ctl    400
!!    array  isosurface_ctl  1
!!      ....
!!    end array isosurface_ctl
!!
!!    delta_t_field_ctl        1.0e-3
!!    i_step_field_ctl         800
!!    output_field_file_fmt_ctl   'VTK'
!!  end visual_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_surfacings
!
      use m_precision
      use m_machine_parameter
!
      use t_control_data_sections
      use t_control_data_isosurfaces
      use t_control_array_real
      use t_control_array_character
      use t_control_array_integer
!
      implicit  none
!
!>        Structures of surfacing controls
      type surfacing_controls
!>        Control block name
        character(len = kchara) :: block_name = 'visual_control'
!
!>        Structures of setioning controls
        type(section_controls) :: psf_s_ctls
!>        Structures of isosurface controls
        type(isosurf_controls) :: iso_s_ctls
!
!>   time interval for sectioning
        type(read_real_item) :: delta_t_psf_s_ctl
!>   time interval for isosurface
        type(read_real_item) :: delta_t_iso_s_ctl
!>   time interval for field data output
        type(read_real_item) :: delta_t_ucd_s_ctl
!
!>   Increment for sectioning
        type(read_integer_item) :: i_step_psf_s_ctl
!>   Increment for isosurface
        type(read_integer_item) :: i_step_iso_s_ctl
!>   Increment for field data output
        type(read_integer_item) :: i_step_ucd_s_ctl
!
!>   Putput field file format
        type(read_character_item) :: output_ucd_fmt_s_ctl
!
        integer (kind=kint) :: i_surfacing_control = 0
      end type surfacing_controls
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_surfacing_controls(surfacing_ctls)
!
      type(surfacing_controls), intent(inout) :: surfacing_ctls
!
!
      call dealloc_psf_ctl_stract(surfacing_ctls%psf_s_ctls)
      call dealloc_iso_ctl_stract(surfacing_ctls%iso_s_ctls)
!
      surfacing_ctls%delta_t_psf_s_ctl%iflag =   0
      surfacing_ctls%delta_t_iso_s_ctl%iflag =   0
      surfacing_ctls%delta_t_ucd_s_ctl%iflag =   0
!
      surfacing_ctls%i_step_psf_s_ctl%iflag =   0
      surfacing_ctls%i_step_iso_s_ctl%iflag =   0
      surfacing_ctls%i_step_ucd_s_ctl%iflag =   0
!
      surfacing_ctls%output_ucd_fmt_s_ctl%iflag = 0
!
      surfacing_ctls%i_surfacing_control = 0
!
      end subroutine dealloc_surfacing_controls
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine section_step_ctls_to_time_ctl(surfacing_ctls, tctl)
!
      use t_ctl_data_4_time_steps
      use t_control_array_real
      use t_control_array_character
      use t_control_array_integer
!
      type(surfacing_controls), intent(in) :: surfacing_ctls
      type(time_data_control), intent(inout) :: tctl
!
!
      if(surfacing_ctls%i_step_psf_s_ctl%iflag .gt. 0) then
        call copy_integer_ctl                                           &
     &     (surfacing_ctls%i_step_psf_s_ctl, tctl%i_step_psf_ctl)
      end if
      if(surfacing_ctls%i_step_iso_s_ctl%iflag .gt. 0) then
        call copy_integer_ctl                                           &
     &     (surfacing_ctls%i_step_iso_s_ctl, tctl%i_step_iso_ctl)
      end if
      if(surfacing_ctls%i_step_ucd_s_ctl%iflag .gt. 0) then
        call copy_integer_ctl                                           &
     &     (surfacing_ctls%i_step_ucd_s_ctl, tctl%i_step_ucd_ctl)
      end if
!
      if(surfacing_ctls%delta_t_psf_s_ctl%iflag .gt. 0) then
        call copy_real_ctl                                              &
     &     (surfacing_ctls%delta_t_psf_s_ctl, tctl%delta_t_psf_ctl)
      end if
      if(surfacing_ctls%delta_t_iso_s_ctl%iflag .gt. 0) then
        call copy_real_ctl                                              &
     &     (surfacing_ctls%delta_t_iso_s_ctl, tctl%delta_t_iso_ctl)
      end if
      if(surfacing_ctls%delta_t_ucd_s_ctl%iflag .gt. 0) then
        call copy_real_ctl                                              &
     &     (surfacing_ctls%delta_t_ucd_s_ctl, tctl%delta_t_field_ctl)
      end if
!
      end subroutine section_step_ctls_to_time_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_fields_4_scts_to_fld_ctl(surfacing_ctls,           &
     &                                        field_ctl)
!
      use t_control_array_character3
!
      type(surfacing_controls), intent(in) :: surfacing_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(surfacing_ctls%psf_s_ctls%num_psf_ctl .gt. 0) then
        call add_fields_4_psfs_to_fld_ctl(surfacing_ctls%psf_s_ctls,    &
     &                                    field_ctl)
      end if
!
      if(surfacing_ctls%iso_s_ctls%num_iso_ctl .gt. 0) then
        call add_fields_4_isos_to_fld_ctl(surfacing_ctls%iso_s_ctls,    &
     &                                    field_ctl)
      end if
!
      end subroutine add_fields_4_scts_to_fld_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_surfacings
