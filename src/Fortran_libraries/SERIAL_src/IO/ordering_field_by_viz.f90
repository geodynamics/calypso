!>@file   ordering_field_by_viz.f90
!!@brief  module ordering_field_by_viz
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on  Aug., 2006
!
!>@brief  Ordering field data by visualization flag
!!
!!@verbatim
!!      subroutine s_ordering_field_by_viz(num_phys, num_phys_viz,      &
!!     &          num_component, phys_name, iflag_monitor)
!!      subroutine ordering_field_by_comp_viz(num_phys, num_phys_viz,   &
!!     &          num_component, phys_name,iflag_monitor)
!!
!!      subroutine set_istack_4_nodal_field(num_phys, num_phys_viz,     &
!!     &          num_component, ntot_phys, ntot_phys_viz,              &
!!     &          istack_component)
!!      subroutine count_field_4_monitor(num_phys, num_component,       &
!!     &          iflag_monitor, num_field_monitor, ntot_comp_monitor)
!!@endverbatim
!
      module ordering_field_by_viz
!
      use m_precision
      use m_constants
!
      implicit  none
!
      private :: mark_vis_checked_fields
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_ordering_field_by_viz(num_phys, num_phys_viz,        &
     &          num_component, phys_name, iflag_monitor)
!
      use m_ctl_data_4_fields
      use set_nodal_field_name
!
      integer(kind = kint), intent(in) :: num_phys
      integer(kind = kint), intent(inout) :: num_phys_viz
      integer(kind = kint), intent(inout) :: num_component(num_phys)
      integer(kind = kint), intent(inout) :: iflag_monitor(num_phys)
      character (len=kchara), intent(inout) :: phys_name(num_phys)
!
      integer(kind = kint) :: i, i0, iflag
      integer(kind = kint) :: iflag_viz
!
!
      num_component = 0
!
      i0 = 0
      do i = 1, num_phys
        call check_vis_control_flag(field_ctl%c2_tbl(i), iflag_viz)
!
        if (iflag_viz .eq. 1) then
          iflag = 0
          call set_vector_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
          call set_scalar_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
          call set_tensor_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
!
          call check_monitor_control_flag(iflag, field_ctl%c3_tbl(i),   &
     &        iflag_monitor(i0))
!
        end if
      end do
      num_phys_viz = i0
!
      do i = 1, num_phys
        call mark_vis_checked_fields(num_phys, num_phys_viz, phys_name, &
     &      field_ctl%c1_tbl(i), iflag)
!
        if ( iflag .eq. 0 ) then
          call set_vector_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
          call set_scalar_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
          call set_tensor_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
!
          call check_monitor_control_flag(iflag, field_ctl%c3_tbl(i),   &
     &         iflag_monitor(i0))
        end if
      end do
!
      end subroutine s_ordering_field_by_viz
!
! -----------------------------------------------------------------------
!
      subroutine ordering_field_by_comp_viz(num_phys, num_phys_viz,     &
     &          num_component, phys_name, iflag_monitor)
!
      use m_ctl_data_4_fields
      use set_nodal_field_name
!
      integer(kind = kint), intent(in) :: num_phys
      integer(kind = kint), intent(inout) :: num_phys_viz
      integer(kind = kint), intent(inout) :: num_component(num_phys)
      integer(kind = kint), intent(inout) :: iflag_monitor(num_phys)
      character (len=kchara), intent(inout) :: phys_name(num_phys)
!
      integer(kind = kint) :: i, i0, iflag
      integer(kind = kint) :: iflag_viz
!
!
      num_component = 0
!
      i0 = 0
      do i = 1, num_phys
        call check_vis_control_flag(field_ctl%c2_tbl(i), iflag_viz)
!
        if (iflag_viz .eq. 1) then
          iflag = 0
          call set_vector_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
          call check_monitor_control_flag(iflag, field_ctl%c3_tbl(i),   &
     &        iflag_monitor(i0))
!
        end if
      end do
!
      do i = 1, num_phys
        call check_vis_control_flag(field_ctl%c2_tbl(i), iflag_viz)
!
        if (iflag_viz .eq. 1) then
          iflag = 0
          call set_scalar_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
          call check_monitor_control_flag(iflag, field_ctl%c3_tbl(i),   &
     &        iflag_monitor(i0))
!
        end if
      end do
!
      do i = 1, num_phys
        call check_vis_control_flag(field_ctl%c2_tbl(i), iflag_viz)
!
        if (iflag_viz .eq. 1) then
          iflag = 0
          call set_tensor_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
          call check_monitor_control_flag(iflag, field_ctl%c3_tbl(i),   &
     &        iflag_monitor(i0))
!
        end if
      end do
!
!
      num_phys_viz = i0
!
      do i = 1, num_phys
        call mark_vis_checked_fields(num_phys, num_phys_viz, phys_name, &
     &      field_ctl%c1_tbl(i), iflag)
!
        if (iflag .eq. 0) then
          call set_vector_field_name(field_ctl%c1_tbl(i), i0,           &
     &       phys_name(i0+1), num_component(i0+1), iflag)
          call check_monitor_control_flag(iflag, field_ctl%c3_tbl(i),   &
     &       iflag_monitor(i0))
        end if
      end do
!
      do i = 1, num_phys
        call mark_vis_checked_fields(num_phys, num_phys_viz, phys_name, &
     &      field_ctl%c1_tbl(i), iflag)
!
        if (iflag .eq. 0) then
          call set_scalar_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
          call check_monitor_control_flag(iflag, field_ctl%c3_tbl(i),   &
     &        iflag_monitor(i0))
        end if
      end do
!
      do i = 1, num_phys
        call mark_vis_checked_fields(num_phys, num_phys_viz, phys_name, &
     &      field_ctl%c1_tbl(i), iflag)
!
        if (iflag .eq. 0) then
          call set_tensor_field_name(field_ctl%c1_tbl(i), i0,           &
     &        phys_name(i0+1), num_component(i0+1), iflag)
          call check_monitor_control_flag(iflag, field_ctl%c3_tbl(i),   &
     &        iflag_monitor(i0))
        end if
      end do
!
      end subroutine ordering_field_by_comp_viz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_istack_4_nodal_field(num_phys, num_phys_viz,       &
     &          num_component, ntot_phys, ntot_phys_viz,                &
     &          istack_component)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: num_phys
      integer(kind = kint), intent(in) :: num_phys_viz
      integer(kind = kint), intent(in) :: num_component(num_phys)
!
      integer(kind = kint), intent(inout) :: ntot_phys
      integer(kind = kint), intent(inout) :: ntot_phys_viz
      integer(kind = kint), intent(inout) :: istack_component(0:num_phys)
!
!
      call s_cal_total_and_stacks(num_phys, num_component,              &
     &    izero, istack_component, ntot_phys)
      ntot_phys_viz = istack_component(num_phys_viz)
!
      end subroutine set_istack_4_nodal_field
!
! -----------------------------------------------------------------------
!
      subroutine count_field_4_monitor(num_phys, num_component,         &
     &          iflag_monitor, num_field_monitor, ntot_comp_monitor)
!
      integer(kind = kint), intent(in) :: num_phys
      integer(kind = kint), intent(in) :: num_component(num_phys)
      integer(kind = kint), intent(in) :: iflag_monitor(num_phys)
!
      integer(kind = kint), intent(inout) :: num_field_monitor
      integer(kind = kint), intent(inout) :: ntot_comp_monitor
!
      integer(kind = kint) :: i
!
!    count number of components for monitoring
!
      num_field_monitor = 0
      ntot_comp_monitor = 0
      do i = 1, num_phys
        num_field_monitor = num_field_monitor + iflag_monitor(i)
        ntot_comp_monitor = ntot_comp_monitor                           &
     &                       + iflag_monitor(i)*num_component(i)
      end do
!
      end subroutine count_field_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mark_vis_checked_fields(num_phys, num_phys_viz,        &
     &          phys_name, phys_nod_name_ctl, iflag)
!
      integer(kind = kint), intent(in) :: num_phys
      integer(kind = kint), intent(in) :: num_phys_viz
      character (len=kchara), intent(in) :: phys_name(num_phys)
!
      character(len = kchara), intent(in) :: phys_nod_name_ctl
      integer (kind = kint), intent(inout) :: iflag
      integer (kind = kint) :: i1
!
      iflag = 0
      do i1 = 1, num_phys_viz
        if (phys_nod_name_ctl .eq. phys_name(i1) ) then
          iflag = 1
          exit
        end if
      end do
!
      end subroutine mark_vis_checked_fields
!
! -----------------------------------------------------------------------
!
      end module ordering_field_by_viz
