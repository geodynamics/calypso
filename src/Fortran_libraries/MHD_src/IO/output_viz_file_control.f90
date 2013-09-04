!
!     module output_viz_file_control
!
!     programmed by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine set_lead_physical_values_flag(iflag_set_field)
!      subroutine output_viz_file_4_flex(istep_psf, istep_iso,          &
!     &          istep_pvr, istep_fline, , visval)
!
      module output_viz_file_control
!
      use m_machine_parameter
      use m_precision
!
      use m_constants
      use m_t_step_parameter
!
      implicit none
!
      private :: set_viz_flex_file_step
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_lead_physical_values_flag(iflag_set_field)
!
      use set_exit_flag_4_visualizer
!
      integer (kind =kint), intent(inout) :: iflag_set_field
!
      integer (kind =kint) :: i_monitor, i_bulk, i_udt, i_coef
!
!
      iflag_set_field = 1
      call set_output_flag_4_viz(istep_max_dt, iflag_set_field)
!
      call set_output_flag(i_bulk, istep_max_dt, i_step_check)
      call set_output_flag(i_udt, istep_max_dt, i_step_output_ucd)
      call set_output_flag(i_monitor, istep_max_dt,                     &
     &    i_step_output_monitor)
!
      call set_output_flag(i_coef, istep_max_dt, i_step_sgs_output)
!
      iflag_set_field = iflag_set_field * i_udt * i_monitor * i_bulk
      iflag_set_field = iflag_set_field * i_coef
!
      if (iflag_debug.eq.1) then
        write(*,*) 'i_udt: ', i_udt
        write(*,*) 'i_monitor: ', i_monitor
        write(*,*) 'i_bulk: ', i_bulk
        write(*,*) 'i_coef: ', i_coef
      end if
!
      end subroutine set_lead_physical_values_flag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine output_viz_file_4_flex(istep_psf, istep_iso,           &
     &          istep_pvr, istep_fline, visval)
!
      use m_t_int_parameter
!
      integer(kind=kint ), intent(inout) :: visval
      integer(kind=kint ), intent(inout) :: istep_psf, istep_iso
      integer(kind=kint ), intent(inout) :: istep_pvr, istep_fline
!
      integer(kind=kint ) :: ivis_pvr, ivis_psf, ivis_iso, ivis_fline
!
!
      call set_viz_flex_file_step(time, dt, delta_t_output_psf,         &
     &    ivis_psf, istep_psf )
      call set_viz_flex_file_step(time, dt, delta_t_output_iso,         &
     &    ivis_iso, istep_iso)
      call set_viz_flex_file_step(time, dt, delta_t_output_pvr,         &
     &    ivis_pvr, istep_pvr)
      call set_viz_flex_file_step(time, dt, delta_t_output_fline,       &
     &    ivis_fline, istep_fline)
!
      visval = ivis_psf * ivis_iso
!
      end subroutine output_viz_file_4_flex
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_viz_flex_file_step(time, dt, dt_viz, iviz, i_cnt)
!
      real(kind = kreal), intent(in) :: time, dt, dt_viz
      integer(kind = kint), intent(inout) :: iviz, i_cnt
!
      integer(kind = kint) :: istep, iref
!
!
      istep = int(time / dt)
      if ( dt_viz .eq. zero) then
        iviz =   ione
        i_cnt = -ione
      else
         iref =  int(dt_viz / dt)
         iviz = mod(istep, iref)
        if (iviz .eq. izero) then
          i_cnt = istep / iref
        else 
          i_cnt = -ione
        end if
      end if
!
      end subroutine set_viz_flex_file_step
!
! -----------------------------------------------------------------------
!
      end module output_viz_file_control
