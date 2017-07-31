!>@file   t_VIZ_step_parameter.f90
!!@brief  module t_VIZ_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!!
!!@verbatim
!!      subroutine accum_output_flag_4_viz(i_step, viz_step, iflag_field)
!!      integer(kind = kint) function viz_file_step_4_flex              &
!!     &                            (time_d, viz_step)
!!      integer(kind = kint) function viz_file_step_4_fix               &
!!     &                            (i_step, viz_step)
!!
!!      subroutine viz_fixed_time_step_params(dt, tctl, viz_step)
!!      subroutine viz_flex_time_step_controls(tctl, dt, viz_step)
!!        integer(kind=kint ), intent(inout) :: visval
!!@endverbatim
!
      module t_VIZ_step_parameter
!
!
      use m_precision
      use m_constants
      use t_time_data
      use t_IO_step_parameter
!
      implicit  none
!
      type VIZ_step_params
!>        time step paremters for sectioning
        type(IO_step_param) :: PSF_t
!>        time step paremters for isosurface
        type(IO_step_param) :: ISO_t
!>        time step paremters for volume rendering
        type(IO_step_param) :: PVR_t
!>        time step paremters for field lines
        type(IO_step_param) :: FLINE_t
      end type VIZ_step_params
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine accum_output_flag_4_viz(i_step, viz_step, iflag_field)
!
      integer (kind =kint), intent(in) :: i_step
      type(VIZ_step_params), intent(in) :: viz_step
!
      integer (kind =kint), intent(inout) :: iflag_field
!
!
      call accum_output_flag(i_step, viz_step%PSF_t, iflag_field)
      call accum_output_flag(i_step, viz_step%ISO_t, iflag_field)
!
      call accum_output_flag(i_step, viz_step%PVR_t, iflag_field)
      call accum_output_flag(i_step, viz_step%FLINE_t, iflag_field)
!
      end subroutine accum_output_flag_4_viz
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function viz_file_step_4_fix                 &
     &                            (i_step, viz_step)
!
      integer (kind =kint), intent(in) :: i_step
!
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      viz_file_step_4_fix = ione
      call accum_flag_to_visualization(i_step, viz_step%PSF_t,          &
     &    viz_file_step_4_fix)
      call accum_flag_to_visualization(i_step, viz_step%ISO_t,          &
     &    viz_file_step_4_fix)
      call accum_flag_to_visualization(i_step, viz_step%PVR_t,          &
     &    viz_file_step_4_fix)
      call accum_flag_to_visualization(i_step, viz_step%FLINE_t,        &
     &    viz_file_step_4_fix)
!
      end function viz_file_step_4_fix
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function viz_file_step_4_flex                &
     &                            (time_d, viz_step)
!
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(inout) :: viz_step
!
      integer(kind=kint ) :: ivis_pvr, ivis_psf, ivis_iso, ivis_fline
!
!
      call set_viz_flex_file_step(time_d, viz_step%PSF_t, ivis_psf)
      call set_viz_flex_file_step(time_d, viz_step%ISO_t, ivis_iso)
      call set_viz_flex_file_step(time_d, viz_step%PVR_t, ivis_pvr)
      call set_viz_flex_file_step(time_d, viz_step%FLINE_t,ivis_fline)
!
      viz_file_step_4_flex                                              &
     &    = ivis_psf * ivis_iso * ivis_pvr * ivis_fline
!
      end function viz_file_step_4_flex
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine viz_fixed_time_step_params(dt, tctl, viz_step)
!
      use t_ctl_data_4_time_steps
!
      real(kind=kreal), intent(in) :: dt
      type(time_data_control), intent(in) :: tctl
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      call set_output_step_4_fixed_step                                 &
     &   (izero, dt, tctl%i_step_psf_ctl, tctl%delta_t_psf_ctl,         &
     &    viz_step%PSF_t)
!
      call set_output_step_4_fixed_step                                 &
     &   (izero, dt, tctl%i_step_iso_ctl, tctl%delta_t_iso_ctl,         &
     &    viz_step%ISO_t)
!
      call set_output_step_4_fixed_step                                 &
     &   (izero, dt, tctl%i_step_pvr_ctl,   tctl%delta_t_pvr_ctl,       &
     &    viz_step%PVR_t)
!
      call set_output_step_4_fixed_step                                 &
     &   (izero, dt, tctl%i_step_fline_ctl, tctl%delta_t_fline_ctl,     &
     &    viz_step%FLINE_t)
!
      end subroutine viz_fixed_time_step_params
!
! -----------------------------------------------------------------------
!
      subroutine viz_flex_time_step_controls(tctl, dt, viz_step)
!
      use t_ctl_data_4_time_steps
!
      real(kind=kreal), intent(in) :: dt
      type(time_data_control), intent(in) :: tctl
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      call set_output_step_4_flex_step                                  &
     &   (izero, dt, tctl%i_step_psf_ctl, tctl%delta_t_psf_ctl,         &
     &    viz_step%PSF_t)
!
      call set_output_step_4_flex_step                                  &
     &   (izero, dt, tctl%i_step_iso_ctl, tctl%delta_t_iso_ctl,         &
     &    viz_step%ISO_t)
!
      call set_output_step_4_flex_step                                  &
     &   (izero, dt, tctl%i_step_pvr_ctl, tctl%delta_t_pvr_ctl,         &
     &    viz_step%PVR_t)
!
      call set_output_step_4_flex_step                                  &
     &   (izero, dt, tctl%i_step_fline_ctl, tctl%delta_t_fline_ctl,     &
     &    viz_step%FLINE_t)
!
      end subroutine viz_flex_time_step_controls
!
! -----------------------------------------------------------------------
!
      end module  t_VIZ_step_parameter
