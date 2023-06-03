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
!!      logical function iflag_vizs_w_fix_step(i_step, viz_step)
!!      logical function iflag_vizs_w_flex_step(time_d, viz_step)
!!      subroutine istep_viz_w_fix_dt(i_step, viz_step)
!!      subroutine istep_viz_w_flex_dt(time_d, viz_step)
!!        type(VIZ_step_params), intent(inout) :: viz_step
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
!>        time step paremters for map projection
        type(IO_step_param) :: MAP_t
!>        time step paremters for volume rendering
        type(IO_step_param) :: PVR_t
!>        time step paremters for LIC volume rendering
        type(IO_step_param) :: LIC_t
!>        time step paremters for field lines
        type(IO_step_param) :: FLINE_t
!
!>        step number for sectioning file
        integer(kind = kint) :: istep_psf = 0
!>        step number for isosurface file
        integer(kind = kint) :: istep_iso = 0
!>        step number for map projection file
        integer(kind = kint) :: istep_map = 0
!>        step number for volume rendering file
        integer(kind = kint) :: istep_pvr = 0
!>        step number for LIC volume rendering file
        integer(kind = kint) :: istep_lic = 0
!>        step number for field lines file
        integer(kind = kint) :: istep_fline = 0
      end type VIZ_step_params
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      logical function iflag_vizs_w_fix_step(i_step, viz_step)
!
      integer(kind = kint), intent(in) :: i_step
      type(VIZ_step_params), intent(in) :: viz_step
!
!
      iflag_vizs_w_fix_step = output_IO_flag(i_step, viz_step%PSF_t)    &
     &                   .or. output_IO_flag(i_step, viz_step%ISO_t)    &
     &                   .or. output_IO_flag(i_step, viz_step%MAP_t)    &
     &                   .or. output_IO_flag(i_step, viz_step%PVR_t)    &
     &                   .or. output_IO_flag(i_step, viz_step%FLINE_t)  &
     &                   .or. output_IO_flag(i_step, viz_step%LIC_t)
!
      end function iflag_vizs_w_fix_step
!
!-----------------------------------------------------------------------
!
      logical function iflag_vizs_w_flex_step(time_d, viz_step)
!
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
!
!
      iflag_vizs_w_flex_step                                            &
     &      = iflag_viz_flex_step(time_d, viz_step%PSF_t)               &
     &   .or. iflag_viz_flex_step(time_d, viz_step%ISO_t)               &
     &   .or. iflag_viz_flex_step(time_d, viz_step%MAP_t)               &
     &   .or. iflag_viz_flex_step(time_d, viz_step%PVR_t)               &
     &   .or. iflag_viz_flex_step(time_d, viz_step%FLINE_t)             &
     &   .or. iflag_viz_flex_step(time_d, viz_step%LIC_t)
!
      end function iflag_vizs_w_flex_step
!
!-----------------------------------------------------------------------
!
      subroutine istep_viz_w_fix_dt(i_step, viz_step)
!
      integer(kind = kint), intent(in) :: i_step
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      viz_step%istep_psf = istep_file_w_fix_dt(i_step, viz_step%PSF_t)
      viz_step%istep_iso = istep_file_w_fix_dt(i_step, viz_step%ISO_t)
      viz_step%istep_map = istep_file_w_fix_dt(i_step, viz_step%MAP_t)
      viz_step%istep_pvr = istep_file_w_fix_dt(i_step, viz_step%PVR_t)
      viz_step%istep_lic = istep_file_w_fix_dt(i_step, viz_step%LIC_t)
      viz_step%istep_fline                                              &
     &          = istep_file_w_fix_dt(i_step, viz_step%FLINE_t)
!
      end subroutine istep_viz_w_fix_dt
!
!-----------------------------------------------------------------------
!
      subroutine istep_viz_w_flex_dt(time_d, viz_step)
!
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      viz_step%istep_psf = istep_file_w_flex_dt(time_d, viz_step%PSF_t)
      viz_step%istep_iso = istep_file_w_flex_dt(time_d, viz_step%ISO_t)
      viz_step%istep_map = istep_file_w_flex_dt(time_d, viz_step%MAP_t)
      viz_step%istep_pvr = istep_file_w_flex_dt(time_d, viz_step%PVR_t)
      viz_step%istep_lic = istep_file_w_flex_dt(time_d, viz_step%LIC_t)
      viz_step%istep_fline                                              &
     &          = istep_file_w_flex_dt(time_d, viz_step%FLINE_t)
!
      end subroutine istep_viz_w_flex_dt
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
      call output_step_4_fixed_step_ctl                                 &
     &   (izero, dt, tctl%i_step_psf_ctl, tctl%delta_t_psf_ctl,         &
     &    viz_step%PSF_t)
!
      call output_step_4_fixed_step_ctl                                 &
     &   (izero, dt, tctl%i_step_iso_ctl, tctl%delta_t_iso_ctl,         &
     &    viz_step%ISO_t)
!
      call output_step_4_fixed_step_ctl                                 &
     &   (izero, dt, tctl%i_step_map_ctl, tctl%delta_t_map_ctl,         &
     &    viz_step%MAP_t)
!
      call output_step_4_fixed_step_ctl                                 &
     &   (izero, dt, tctl%i_step_pvr_ctl,   tctl%delta_t_pvr_ctl,       &
     &    viz_step%PVR_t)
!
      call output_step_4_fixed_step_ctl                                 &
     &   (izero, dt, tctl%i_step_fline_ctl, tctl%delta_t_fline_ctl,     &
     &    viz_step%FLINE_t)
!
      call output_step_4_fixed_step_ctl                                 &
     &   (izero, dt, tctl%i_step_lic_ctl, tctl%delta_t_lic_ctl,         &
     &    viz_step%LIC_t)
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
      call output_step_4_flex_step_ctl                                  &
     &   (izero, dt, tctl%i_step_psf_ctl, tctl%delta_t_psf_ctl,         &
     &    viz_step%PSF_t)
!
      call output_step_4_flex_step_ctl                                  &
     &   (izero, dt, tctl%i_step_iso_ctl, tctl%delta_t_iso_ctl,         &
     &    viz_step%ISO_t)
!
      call output_step_4_flex_step_ctl                                  &
     &   (izero, dt, tctl%i_step_map_ctl, tctl%delta_t_map_ctl,         &
     &    viz_step%MAP_t)
!
      call output_step_4_flex_step_ctl                                  &
     &   (izero, dt, tctl%i_step_pvr_ctl, tctl%delta_t_pvr_ctl,         &
     &    viz_step%PVR_t)
!
      call output_step_4_flex_step_ctl                                  &
     &   (izero, dt, tctl%i_step_fline_ctl, tctl%delta_t_fline_ctl,     &
     &    viz_step%FLINE_t)
!
      call output_step_4_flex_step_ctl                                  &
     &   (izero, dt, tctl%i_step_lic_ctl, tctl%delta_t_lic_ctl,         &
     &    viz_step%LIC_t)
!
      end subroutine viz_flex_time_step_controls
!
! -----------------------------------------------------------------------
!
      end module  t_VIZ_step_parameter
