!>@file   bcast_4_sph_monitor_ctl.f90
!!        module bcast_4_sph_monitor_ctl
!!
!! @author H. Matsui
!! @date   Programmed in 2016
!!
!
!> @brief Control data for spectr data monitoring
!!
!!@verbatim
!!      subroutine bcast_sph_monitoring_ctl(smonitor_ctl)
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!@endverbatim
!
      module bcast_4_sph_monitor_ctl
!
      use m_precision
!
      use calypso_mpi
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_sph_vol_spectr
      use t_ctl_data_pick_sph_spectr
      use bcast_control_arrays
!
      implicit  none
!
      private :: bcast_pickup_spectr_ctl, bcast_gauss_spectr_ctl
      private :: bcast_each_vol_spectr_ctl, bcast_layerd_spectr_ctl
      private :: bcast_mid_eq_monitor_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_monitoring_ctl(smonitor_ctl)
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
      integer(kind = kint) :: i
!
!
      call bcast_ctl_type_c1(smonitor_ctl%volume_average_prefix)
      call bcast_ctl_type_c1(smonitor_ctl%volume_pwr_spectr_prefix)
      call bcast_ctl_type_c1(smonitor_ctl%Nusselt_file_prefix)
!
      call bcast_pickup_spectr_ctl(smonitor_ctl%pspec_ctl)
      call bcast_gauss_spectr_ctl(smonitor_ctl%g_pwr)
!
      call bcast_layerd_spectr_ctl(smonitor_ctl%lp_ctl)
!
      call bcast_mid_eq_monitor_ctl(smonitor_ctl%meq_ctl)
!
!
      call MPI_BCAST(smonitor_ctl%num_vspec_ctl, 1,                     &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(smonitor_ctl%num_vspec_ctl .gt. 0 .and. my_rank .gt. 0) then
        allocate(smonitor_ctl%v_pwr(smonitor_ctl%num_vspec_ctl))
      end if
!
      do i = 1, smonitor_ctl%num_vspec_ctl
        call bcast_each_vol_spectr_ctl(smonitor_ctl%v_pwr(i))
      end do
!
      end subroutine bcast_sph_monitoring_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_pickup_spectr_ctl(pspec_ctl)
!
      type(pick_spectr_control), intent(inout) :: pspec_ctl
!
!
      call bcast_ctl_array_i1(pspec_ctl%idx_pick_layer_ctl)
!
      call bcast_ctl_array_i2(pspec_ctl%idx_pick_sph_ctl)
      call bcast_ctl_array_i1(pspec_ctl%idx_pick_sph_l_ctl)
      call bcast_ctl_array_i1(pspec_ctl%idx_pick_sph_m_ctl)
!
      call bcast_ctl_type_c1(pspec_ctl%picked_mode_head_ctl)
!
      end subroutine bcast_pickup_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_gauss_spectr_ctl(g_pwr)
!
      type(gauss_spectr_control), intent(inout) :: g_pwr
!
!
      call bcast_ctl_array_i2(g_pwr%idx_gauss_ctl)
      call bcast_ctl_array_i1(g_pwr%idx_gauss_l_ctl)
      call bcast_ctl_array_i1(g_pwr%idx_gauss_m_ctl)
!
      call bcast_ctl_type_r1(g_pwr%gauss_coefs_radius_ctl)
      call bcast_ctl_type_c1(g_pwr%gauss_coefs_prefix)
!
      end subroutine bcast_gauss_spectr_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_each_vol_spectr_ctl(v_pwr)
!
      type(volume_spectr_control), intent(inout) :: v_pwr
!
!
      call bcast_ctl_type_c1(v_pwr%volume_spec_file_ctl)
      call bcast_ctl_type_c1(v_pwr%volume_ave_file_ctl)
      call bcast_ctl_type_r1(v_pwr%inner_radius_ctl)
      call bcast_ctl_type_r1(v_pwr%outer_radius_ctl)
!
      end subroutine bcast_each_vol_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_layerd_spectr_ctl(lp_ctl)
!
      type(layerd_spectr_control), intent(inout) :: lp_ctl
!
!
      call bcast_ctl_array_i1(lp_ctl%idx_spec_layer_ctl)
!
      call bcast_ctl_type_c1(lp_ctl%layered_pwr_spectr_prefix)
!
      call bcast_ctl_type_c1(lp_ctl%degree_spectr_switch)
      call bcast_ctl_type_c1(lp_ctl%order_spectr_switch)
      call bcast_ctl_type_c1(lp_ctl%diff_lm_spectr_switch)
      call bcast_ctl_type_c1(lp_ctl%axis_spectr_switch)
!
      end subroutine bcast_layerd_spectr_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_mid_eq_monitor_ctl(meq_ctl)
!
      type(mid_equator_control), intent(inout) :: meq_ctl
!
!
      call bcast_ctl_type_r1(meq_ctl%pick_s_ctl)
      call bcast_ctl_type_r1(meq_ctl%pick_z_ctl)
!
      call bcast_ctl_type_i1(meq_ctl%nphi_mid_eq_ctl)
!
      call bcast_ctl_type_c1(meq_ctl%pick_circle_coord_ctl)
!
      end subroutine bcast_mid_eq_monitor_ctl
!
! -----------------------------------------------------------------------
!
      end module bcast_4_sph_monitor_ctl
