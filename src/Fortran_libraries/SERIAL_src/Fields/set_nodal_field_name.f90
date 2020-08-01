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
!!      subroutine count_field_4_monitor                                &
!!     &         (fld, num_field_monitor, ntot_comp_monitor)
!!        type(phys_data), intent(in) :: fld
!!
!!      logical function check_vis_control_flag(visualize_ctl)
!!      logical function check_monitor_control_flag(monitor_ctl)
!!
!!      subroutine set_vis_control_flag(iflag_viz, visualize_ctl)
!!      subroutine set_monitor_control_flag                             &
!!     &         (iflag_fld_monitor, monitor_ctl)
!!@endverbatim
!
      module set_nodal_field_name
!
      use m_precision
      use m_phys_labels
      use t_phys_data
      use t_control_array_character3
!
      implicit  none
!
      character(len = kchara), parameter :: cflag_viz_on =  'Viz_On'
      character(len = kchara), parameter :: cflag_viz_off = 'Viz_Off'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_monitor_on =  'Monitor_On'
      character(len = kchara), parameter                                &
     &                        :: cflag_monitor_off = 'Monitor_Off'
!
      private :: set_vector_field_name, set_scalar_field_name
      private :: set_tensor_field_name
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine ordering_field_by_viz(field_ctl, fld)
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i
      logical :: flag
!
!
      fld%num_phys = 0
      call alloc_phys_name_type(fld)
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
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i
      logical :: flag
!
!
      fld%num_phys = 0
      call alloc_phys_name_type(fld)
!
      do i = 1, field_ctl%icou
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
      subroutine count_field_4_monitor                                  &
     &         (fld, num_field_monitor, ntot_comp_monitor)
!
      type(phys_data), intent(in) :: fld
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
      do i = 1, fld%num_phys
        if(fld%flag_monitor(i)) then
          num_field_monitor = num_field_monitor + 1
          ntot_comp_monitor = ntot_comp_monitor + fld%num_component(i)
        end if
      end do
!
      end subroutine count_field_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vector_field_name                                  &
     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!
      use set_MHD_field_address
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical, intent(in) :: flag_viz, flag_monitor
      type(phys_data), intent(inout) :: fld
      logical, intent(inout) :: flag
!
!
      flag =  check_vector_fields(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, n_vector,            &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      if(phys_name_ctl .eq. geostrophic_balance%name) then
        flag = .TRUE.
        call append_field_name_list(rest_of_geostrophic%name, n_vector, &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      end subroutine set_vector_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_scalar_field_name                                  &
     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!
      use m_energy_flux_labels
      use set_MHD_field_address
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical, intent(in) :: flag_viz, flag_monitor
      type(phys_data), intent(inout) :: fld
      logical, intent(inout) :: flag
!
!
      flag =  check_scalar_fields(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, n_scalar,            &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
!   Old field label... Should be deleted later!!
      if(phys_name_ctl .eq. buoyancy_work%name) then
        flag = .TRUE.
        call append_field_name_list(buoyancy_flux%name, n_scalar,       &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      end subroutine set_scalar_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_tensor_field_name                                  &
     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!
      use set_MHD_field_address
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical, intent(in) :: flag_viz, flag_monitor
      type(phys_data), intent(inout) :: fld
      logical, intent(inout) :: flag
!
!
      flag =  check_sym_tensor_fields(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, n_sym_tensor,        &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      flag =  check_asym_tensor_fields(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, ithree,              &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      end subroutine set_tensor_field_name
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function check_vis_control_flag(visualize_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: visualize_ctl
!
      check_vis_control_flag = cmp_no_case(visualize_ctl, cflag_viz_on)
!
      end function check_vis_control_flag
!
! -----------------------------------------------------------------------
!
      logical function check_monitor_control_flag(monitor_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: monitor_ctl
!
      check_monitor_control_flag                                        &
     &      = cmp_no_case(monitor_ctl, cflag_monitor_on)
!
      end function check_monitor_control_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vis_control_flag(iflag_viz, visualize_ctl)
!
      integer (kind = kint), intent(in) :: iflag_viz
      character(len = kchara), intent(inout) :: visualize_ctl
!
      if(iflag_viz .gt. 0) then
        visualize_ctl = cflag_viz_on
      else
        visualize_ctl = cflag_viz_off
      end if
!
      end subroutine set_vis_control_flag
!
! -----------------------------------------------------------------------
!
      subroutine set_monitor_control_flag                               &
     &         (iflag_fld_monitor, monitor_ctl)
!
      integer (kind = kint), intent(in) :: iflag_fld_monitor
      character(len = kchara), intent(inout) :: monitor_ctl
!
      if(iflag_fld_monitor .gt. 0) then
        monitor_ctl = cflag_monitor_on
      else
        monitor_ctl = cflag_monitor_off
      end if
!
      end subroutine set_monitor_control_flag
!
! -----------------------------------------------------------------------
!
      end module set_nodal_field_name
