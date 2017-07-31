!>@file   set_control_sph_data.f90
!!@brief  module set_control_sph_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2007
!!@date Modified in 2008
!
!> @brief Load control data for sphrical dynamo
!!
!!@verbatim
!!      subroutine s_set_control_sph_data(field_ctl, rj_fld, ierr)
!!      subroutine ordering_sph_field_by_viz_comp(rj_fld)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module set_control_sph_data
!
      use m_precision
      use m_constants
!
      use t_phys_data
!
      implicit  none
!
      private :: ordering_sph_field_by_viz
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_sph_data(field_ctl, rj_fld, ierr)
!
      use m_machine_parameter
      use t_read_control_arrays
      use cal_minmax_and_stacks
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: rj_fld
      integer (kind = kint), intent(inout) :: ierr
!
!
!   set physical values
!
      ierr = 0
      if(field_ctl%icou .eq. 0) then
        e_message = 'Set field for simulation'
        ierr = 90
        return
      else
        rj_fld%num_phys = field_ctl%num
      end if
!
!    set spectr data
!
      if ( rj_fld%num_phys .ne. 0 ) then
        call alloc_phys_name_type(rj_fld)
        call ordering_sph_field_by_viz(field_ctl, rj_fld)
!
        if (iflag_debug .gt. 0) then
          call check_nodal_field_name_type(6, rj_fld)
        end if
      end if
!
      end subroutine s_set_control_sph_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ordering_sph_field_by_viz(field_ctl, rj_fld)
!
      use t_read_control_arrays
      use ordering_field_by_viz
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: rj_fld
!
!
      call s_ordering_field_by_viz                                      &
     &   (field_ctl, rj_fld%num_phys, rj_fld%num_phys_viz,              &
     &    rj_fld%num_component, rj_fld%phys_name, rj_fld%iflag_monitor)
!
      call set_istack_4_nodal_field                                     &
     &   (rj_fld%num_phys, rj_fld%num_phys_viz,                         &
     &    rj_fld%num_component, rj_fld%ntot_phys,                       &
     &    rj_fld%ntot_phys_viz, rj_fld%istack_component)
!
      end subroutine ordering_sph_field_by_viz
!
! -----------------------------------------------------------------------
!
      subroutine ordering_sph_field_by_viz_comp(field_ctl, rj_fld)
!
      use t_read_control_arrays
      use ordering_field_by_viz
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: rj_fld
!
!
      call ordering_field_by_comp_viz                                   &
     &   (field_ctl, rj_fld%num_phys, rj_fld%num_phys_viz,              &
     &    rj_fld%num_component, rj_fld%phys_name,                       &
     &    rj_fld%iflag_monitor)
!
      call set_istack_4_nodal_field                                     &
     &   (rj_fld%num_phys, rj_fld%num_phys_viz,                         &
     &    rj_fld%num_component, rj_fld%ntot_phys,                       &
     &    rj_fld%ntot_phys_viz, rj_fld%istack_component)
!
      end subroutine ordering_sph_field_by_viz_comp
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_data
