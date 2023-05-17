!>@file   set_nodal_field_name.f90
!!@brief  module set_nodal_field_name
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2008
!!@n        modified by H.Matsui on Oct.,  2009
!!@n        modified by H.Matsui on June., 2012
!
!>@brief  Set field names from control data
!!
!!@verbatim
!!      subroutine ordering_field_by_viz(field_ctl, fld)
!!      subroutine ordering_field_by_comp_viz(field_ctl, fld)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!        type(phys_data), intent(inout) :: fld
!!@endverbatim
!
      module set_nodal_field_name
!
      use m_precision
      use t_phys_data
      use t_control_array_character3
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine ordering_field_by_viz(field_ctl, fld)
!
      use add_nodal_fields_ctl
      use set_each_field_name
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i
      logical :: flag
!
!
      fld%num_phys = 0
      call alloc_phys_name(fld)
!
      do i = 1, field_ctl%icou
        flag = .FALSE.
        if(flag) cycle
        call set_vector_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
        if(flag) cycle
        call set_scalar_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
        if(flag) cycle
        call set_tensor_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      end subroutine ordering_field_by_viz
!
! -----------------------------------------------------------------------
!
      subroutine ordering_field_by_comp_viz(field_ctl, fld)
!
      use add_nodal_fields_ctl
      use set_each_field_name
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i
      logical :: flag
!
!
      fld%num_phys = 0
      call alloc_phys_name(fld)
!
      do i = 1, field_ctl%icou
        flag = .FALSE.
        if(flag) cycle
        call set_vector_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      do i = 1, field_ctl%icou
        if(flag) cycle
        call set_scalar_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      do i = 1, field_ctl%icou
        if(flag) cycle
        call set_tensor_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      end subroutine ordering_field_by_comp_viz
!
! -----------------------------------------------------------------------
!
      end module set_nodal_field_name
