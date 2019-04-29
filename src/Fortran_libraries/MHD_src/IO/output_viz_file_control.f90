!
!     module output_viz_file_control
!
!     programmed by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      integer(kind = kint) function lead_field_data_flag              &
!!     &                   (i_step, MHD_step)
!!        type(MHD_step_param), intent(in) :: MHD_step
!!      subroutine MHD_viz_routine_flag_and_step                        &
!!     &         (flex_p, time_d, viz_step, viz_flag)
!!        type(time_data), intent(in) :: time_d
!!        type(flexible_stepping_parameter), intent(in) :: flex_p
!!        type(VIZ_step_params), intent(inout) :: viz_step
!
      module output_viz_file_control
!
      use m_machine_parameter
      use m_precision
!
      use m_constants
      use t_IO_step_parameter
      use t_MHD_step_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function lead_field_data_flag                &
     &                   (i_step, MHD_step)
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_step_param), intent(in) :: MHD_step
!
!
      lead_field_data_flag                                              &
     &      = iflag_vizs_w_fix_step(i_step, MHD_step%viz_step)          &
     &       * output_IO_flag(i_step, MHD_step%rst_step)                &
     &       * output_IO_flag(i_step, MHD_step%ucd_step)                &
     &       * output_IO_flag(i_step, MHD_step%rms_step)                &
     &       * output_IO_flag(i_step, MHD_step%point_step)              &
     &       * output_IO_flag(i_step, MHD_step%sgs_IO_step)
!
      if (iflag_debug.eq.1) then
        write(*,*) 'irst: ', output_IO_flag(i_step, MHD_step%rst_step)
        write(*,*) 'i_udt: ', output_IO_flag(i_step, MHD_step%ucd_step)
        write(*,*) 'i_monitor: ',                                       &
     &            output_IO_flag(i_step, MHD_step%point_step)
        write(*,*) 'i_rms: ',                                           &
     &            output_IO_flag(i_step, MHD_step%rms_step)
        write(*,*) 'i_model_coef: ',                                    &
     &            output_IO_flag(i_step, MHD_step%sgs_IO_step)
      end if
!
      end function lead_field_data_flag
!
!-----------------------------------------------------------------------
!
      subroutine MHD_viz_routine_flag_and_step                          &
     &         (flex_p, time_d, viz_step, viz_flag)
!
      type(time_data), intent(in) :: time_d
      type(flexible_stepping_parameter), intent(in) :: flex_p
      type(VIZ_step_params), intent(inout) :: viz_step
      integer(kind = kint), intent(inout) :: viz_flag
!
!
      if(flex_p%iflag_flexible_step .eq. iflag_flex_step) then
        viz_flag = iflag_vizs_w_flex_step(time_d, viz_step)
        call istep_viz_w_flex_dt(time_d, viz_step)
      else
        viz_flag = iflag_vizs_w_fix_step(flex_p%istep_max_dt, viz_step)
        call istep_viz_w_fix_dt(flex_p%istep_max_dt, viz_step)
      end if
!
      end subroutine MHD_viz_routine_flag_and_step
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      integer(kind = kint) function iflag_viz_output                    &
     &              (iflag_flexible_step, i_step_fix, time_d, IO_step)
!
      use t_time_data
      use t_flex_delta_t_data
!
      integer(kind = kint), intent(in) :: iflag_flexible_step
      integer(kind = kint), intent(in) :: i_step_fix
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: IO_step
!
!
      if(iflag_flexible_step .eq. iflag_flex_step) then
        iflag_viz_output = iflag_viz_flex_step(time_d, IO_step)
      else
        iflag_viz_output = output_IO_flag(i_step_fix, IO_step)
      end if
!
      end function iflag_viz_output
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function viz_time_step                       &
     &              (iflag_flexible_step, i_step_fix, time_d, IO_step)
!
      use t_time_data
      use t_flex_delta_t_data
!
      integer(kind = kint), intent(in) :: iflag_flexible_step
      integer(kind = kint), intent(in) :: i_step_fix
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: IO_step
!
!
      if(iflag_flexible_step .eq. iflag_flex_step) then
        viz_time_step = iflag_viz_flex_step(time_d, IO_step)
      else
        viz_time_step = output_IO_flag(i_step_fix, IO_step)
      end if
!
      end function viz_time_step
!
! -----------------------------------------------------------------------
!
      end module output_viz_file_control
