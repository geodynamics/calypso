!
!      module set_control_sph_data
!
!        programmed by H.Matsui on Dec., 2007
!
!     subroutine s_set_control_sph_data(ierr)
!
      module set_control_sph_data
!
      use m_precision
      use m_constants
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
      subroutine s_set_control_sph_data(ierr)
!
      use m_machine_parameter
      use m_ctl_data_4_fields
      use m_sph_spectr_data
      use cal_minmax_and_stacks
!
      integer (kind = kint), intent(inout) :: ierr
!
!
!   set physical values
!
      ierr = 0
      if(i_num_nod_phys .eq. 0) then
        e_message = 'Set field for simulation'
        ierr = 90
        return
      else
        num_phys_rj = num_nod_phys_ctl
      end if
!
!    set spectr data
!
      if ( num_phys_rj .ne. 0 ) then
        call allocate_phys_rj_name
        call ordering_sph_field_by_viz
!
        if (iflag_debug .gt. 0) call check_rj_spectr_name
      end if
!
      end subroutine s_set_control_sph_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ordering_sph_field_by_viz
!
      use m_sph_spectr_data
      use ordering_field_by_viz
!
!
      call s_ordering_field_by_viz(num_phys_rj, num_phys_rj_vis,        &
     &    num_phys_comp_rj, phys_name_rj, iflag_monitor_rj)
!
      call set_istack_4_nodal_field(num_phys_rj, num_phys_rj_vis,       &
     &    num_phys_comp_rj, ntot_phys_rj, ntot_comp_rj_vis,             &
     &    istack_phys_comp_rj)
!
      end subroutine ordering_sph_field_by_viz
!
! -----------------------------------------------------------------------
!
      subroutine ordering_sph_field_by_viz_comp
!
      use m_sph_spectr_data
      use ordering_field_by_viz
!
!
      call ordering_field_by_comp_viz(num_phys_rj, num_phys_rj_vis,     &
     &    num_phys_comp_rj, phys_name_rj, iflag_monitor_rj)
!
      call set_istack_4_nodal_field(num_phys_rj, num_phys_rj_vis,       &
     &    num_phys_comp_rj, ntot_phys_rj, ntot_comp_rj_vis,             &
     &    istack_phys_comp_rj)
!
      end subroutine ordering_sph_field_by_viz_comp
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_data
