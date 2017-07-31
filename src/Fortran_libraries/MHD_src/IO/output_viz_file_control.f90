!
!     module output_viz_file_control
!
!     programmed by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      integer(kind = kint) function lead_field_data_flag              &
!!     &                   (i_step, MHD_step)
!!        type(MHD_step_param), intent(in) :: MHD_step
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
      integer (kind =kint) :: i_monitor, i_bulk, i_udt, i_coef, irst
!
!
      lead_field_data_flag = 1
      call accum_output_flag_4_viz                                      &
     &   (i_step, MHD_step%viz_step, lead_field_data_flag)
!
      irst =      output_IO_flag(i_step, MHD_step%rst_step)
      i_udt =     output_IO_flag(i_step, MHD_step%ucd_step)
      i_bulk =    output_IO_flag(i_step, MHD_step%rms_step)
      i_monitor = output_IO_flag(i_step, MHD_step%point_step)
!
      i_coef =    output_IO_flag(i_step, MHD_step%sgs_IO_step)
!
      lead_field_data_flag = lead_field_data_flag                       &
     &                     * irst * i_udt * i_monitor * i_bulk * i_coef
!
      if (iflag_debug.eq.1) then
        write(*,*) 'irst: ',         i_udt
        write(*,*) 'i_udt: ',        i_udt
        write(*,*) 'i_monitor: ',    i_monitor
        write(*,*) 'i_bulk: ',       i_bulk
        write(*,*) 'i_model_coef: ', i_coef
      end if
!
      end function lead_field_data_flag
!
!-----------------------------------------------------------------------
!
      end module output_viz_file_control
