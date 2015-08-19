!>@file   set_exit_flag_4_visualizer.f90
!!@brief  module set_exit_flag_4_visualizer
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief Set exit loop flag for visualization
!!
!!@verbatim
!!      subroutine set_output_flag_4_viz(i_step, iflag_set_field)
!!      subroutine set_flag_to_visualization(i_step,                    &
!!     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!!      subroutine set_flag_to_visualization(count, visval)
!!
!!      subroutine set_output_flag(i_flag, i_step, interval)
!!      subroutine set_viz_file_step(istep_ref, i_step_viz, iviz, i_cnt)
!!@endverbatim
!
      module set_exit_flag_4_visualizer
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_output_flag_4_viz(i_step, iflag_set_field)
!
      use m_t_step_parameter
!
      integer (kind =kint), intent(in) :: i_step
      integer (kind =kint), intent(inout) :: iflag_set_field
      integer (kind =kint) :: i_pvr, i_psf, i_iso, i_fline
!
!
      call set_output_flag(i_pvr,   i_step, i_step_output_pvr)
      call set_output_flag(i_fline, i_step, i_step_output_fline)
!
      call set_output_flag(i_psf, i_step, i_step_output_psf)
      call set_output_flag(i_iso, i_step, i_step_output_iso)
!
      iflag_set_field = iflag_set_field * i_pvr*i_fline*i_psf*i_iso
!
      end subroutine set_output_flag_4_viz
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_flag_to_visualization(i_step,                      &
     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      use m_t_step_parameter
!
      integer (kind =kint), intent(in) :: i_step
      integer(kind=kint ), intent(inout) :: visval
      integer(kind=kint ), intent(inout) :: istep_psf, istep_iso
      integer(kind=kint ), intent(inout) :: istep_pvr, istep_fline
!
      integer(kind=kint ) :: ivis_pvr, ivis_psf, ivis_iso, ivis_fline
!
!
      call set_viz_file_step(i_step, i_step_output_psf,                 &
     &    ivis_psf, istep_psf)
      call set_viz_file_step(i_step, i_step_output_iso,                 &
     &    ivis_iso, istep_iso)
      call set_viz_file_step(i_step, i_step_output_pvr,                 &
     &    ivis_pvr, istep_pvr)
      call set_viz_file_step(i_step, i_step_output_fline,               &
     &    ivis_fline, istep_fline)
!
      visval = ivis_psf * ivis_iso * ivis_pvr * ivis_fline
!
      end subroutine set_flag_to_visualization
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_output_flag(i_flag, i_step, interval)
!
      integer (kind = kint) :: i_flag, i_step, interval
!
      if ( interval .eq. 0) then
        i_flag = ione
      else
        i_flag = mod(i_step,interval)
      end if
!
      end subroutine set_output_flag
!
! -----------------------------------------------------------------------
!
      subroutine set_viz_file_step(istep_ref, i_step_viz, iviz, i_cnt)
!
      integer(kind = kint), intent(in) :: istep_ref, i_step_viz
      integer(kind = kint), intent(inout) :: iviz, i_cnt
!
!
      if ( i_step_viz .eq. izero ) then
        iviz = ione
        i_cnt =-ione
      else 
        iviz = mod(istep_ref,i_step_viz)
        if (iviz .eq. izero) then
          i_cnt = istep_ref / i_step_viz
        else 
          i_cnt = -ione
        end if
      end if
!
      end subroutine set_viz_file_step
!
! -----------------------------------------------------------------------
!
      end module set_exit_flag_4_visualizer
