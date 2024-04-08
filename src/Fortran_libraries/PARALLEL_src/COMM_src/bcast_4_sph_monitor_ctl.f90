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
!
      implicit  none
!
      private :: bcast_pickup_spectr_ctl, bcast_gauss_spectr_ctl
      private :: bcast_each_vol_spectr_ctl, bcast_layerd_spectr_ctl
      private :: bcast_mid_eq_monitor_ctl
      private :: bcast_ctl_data_dynamobench, bcast_sph_dipolarity_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_monitoring_ctl(smonitor_ctl)
!
      use transfer_to_long_integers
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
!
      call calypso_mpi_bcast_character                                  &
     &   (smonitor_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(smonitor_ctl%i_sph_monitor, 0)
!
      call bcast_ctl_type_c1(smonitor_ctl%volume_average_prefix)
      call bcast_ctl_type_c1(smonitor_ctl%volume_pwr_spectr_prefix)
      call bcast_ctl_type_c1(smonitor_ctl%volume_work_spectr_prefix)
      call bcast_ctl_type_c1(smonitor_ctl%volume_pwr_spectr_format)
!
      call bcast_ctl_type_c1(smonitor_ctl%degree_v_spectra_switch)
      call bcast_ctl_type_c1(smonitor_ctl%order_v_spectra_switch)
      call bcast_ctl_type_c1(smonitor_ctl%diff_v_lm_spectra_switch)
      call bcast_ctl_type_c1(smonitor_ctl%axis_v_power_switch)
!
      call bcast_ctl_type_c1(smonitor_ctl%heat_Nusselt_file_prefix)
      call bcast_ctl_type_c1(smonitor_ctl%heat_Nusselt_file_format)
      call bcast_ctl_type_c1(smonitor_ctl%comp_Nusselt_file_prefix)
      call bcast_ctl_type_c1(smonitor_ctl%comp_Nusselt_file_format)
      call bcast_ctl_type_c1(smonitor_ctl%typ_scale_file_prefix_ctl)
      call bcast_ctl_type_c1(smonitor_ctl%typ_scale_file_format_ctl)
!
      call bcast_pickup_spectr_ctl(smonitor_ctl%pspec_ctl)
      call bcast_gauss_spectr_ctl(smonitor_ctl%g_pwr)
!
      call bcast_layerd_spectr_ctl(smonitor_ctl%lp_ctl)
!
      call bcast_sph_dipolarity_ctl(smonitor_ctl%fdip_ctl)
      call bcast_ctl_data_dynamobench(smonitor_ctl%dbench_ctl)
!
!
      call calypso_mpi_bcast_character                                  &
     &   (smonitor_ctl%v_pwr_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(smonitor_ctl%num_vspec_ctl, 0)
      if(smonitor_ctl%num_vspec_ctl .gt. 0 .and. my_rank .gt. 0) then
        allocate(smonitor_ctl%v_pwr(smonitor_ctl%num_vspec_ctl))
      end if
!
      call bcast_each_vol_spectr_ctl                                    &
     &   (smonitor_ctl%num_vspec_ctl, smonitor_ctl%v_pwr)
!
!
      call calypso_mpi_bcast_character                                  &
     &   (smonitor_ctl%d_circ_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(smonitor_ctl%num_circ_ctl, 0)
      if(smonitor_ctl%num_circ_ctl .gt. 0 .and. my_rank .gt. 0) then
        allocate(smonitor_ctl%meq_ctl(smonitor_ctl%num_circ_ctl))
      end if
!
      call bcast_mid_eq_monitor_ctl(smonitor_ctl%num_circ_ctl,          &
     &                              smonitor_ctl%meq_ctl)
!
      end subroutine bcast_sph_monitoring_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_pickup_spectr_ctl(pspec_ctl)
!
      use transfer_to_long_integers
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
!
      type(pick_spectr_control), intent(inout) :: pspec_ctl
!
!
      call bcast_ctl_array_i1(pspec_ctl%idx_pick_layer_ctl)
      call bcast_ctl_array_r1(pspec_ctl%pick_radius_ctl)
!
      call bcast_ctl_array_i2(pspec_ctl%idx_pick_sph_ctl)
      call bcast_ctl_array_i1(pspec_ctl%idx_pick_sph_l_ctl)
      call bcast_ctl_array_i1(pspec_ctl%idx_pick_sph_m_ctl)
!
      call bcast_ctl_type_c1(pspec_ctl%picked_mode_head_ctl)
      call bcast_ctl_type_c1(pspec_ctl%picked_mode_fmt_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (pspec_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(pspec_ctl%i_pick_sph, 0)
!
      end subroutine bcast_pickup_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_gauss_spectr_ctl(g_pwr)
!
      use t_ctl_data_gauss_coefs
      use transfer_to_long_integers
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
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
      call bcast_ctl_type_c1(g_pwr%gauss_coefs_format)
!
      call calypso_mpi_bcast_character                                  &
     &   (g_pwr%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(g_pwr%i_gauss_coef_ctl, 0)
!
      end subroutine bcast_gauss_spectr_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_each_vol_spectr_ctl(num_vspec_ctl, v_pwr)
!
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
      use t_ctl_data_sph_vol_spectr
!
      integer(kind = kint), intent(in) :: num_vspec_ctl
      type(volume_spectr_control), intent(inout)                        &
     &                            :: v_pwr(num_vspec_ctl)
!
      integer(kind = kint) :: i
!
      do i = 1, num_vspec_ctl
        call bcast_ctl_type_c1(v_pwr(i)%volume_spec_file_ctl)
        call bcast_ctl_type_c1(v_pwr(i)%volume_ave_file_ctl)
        call bcast_ctl_type_c1(v_pwr(i)%volume_lor_spec_file_ctl)
        call bcast_ctl_type_c1(v_pwr(i)%volume_spec_format_ctl)
!
        call bcast_ctl_type_c1(v_pwr(i)%degree_v_spectra_switch)
        call bcast_ctl_type_c1(v_pwr(i)%order_v_spectra_switch)
        call bcast_ctl_type_c1(v_pwr(i)%diff_v_lm_spectra_switch)
        call bcast_ctl_type_c1(v_pwr(i)%axis_v_power_switch)
!
        call bcast_ctl_type_r1(v_pwr(i)%inner_radius_ctl)
        call bcast_ctl_type_r1(v_pwr(i)%outer_radius_ctl)
        call calypso_mpi_bcast_character                                &
     &     (v_pwr(i)%block_name, cast_long(kchara), 0)
        call calypso_mpi_bcast_one_int(v_pwr(i)%i_vol_spectr_ctl, 0)
      end do
!
      end subroutine bcast_each_vol_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_layerd_spectr_ctl(lp_ctl)
!
      use transfer_to_long_integers
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use t_ctl_data_sph_layer_spectr
!
      type(layerd_spectr_control), intent(inout) :: lp_ctl
!
!
      call bcast_ctl_array_i1(lp_ctl%idx_spec_layer_ctl)
      call bcast_ctl_array_r1(lp_ctl%layer_radius_ctl)
!
      call bcast_ctl_type_c1(lp_ctl%layered_pwr_spectr_prefix)
      call bcast_ctl_type_c1(lp_ctl%layered_work_spectr_prefix)
      call bcast_ctl_type_c1(lp_ctl%layered_pwr_spectr_format)
!
      call bcast_ctl_type_c1(lp_ctl%degree_spectra_switch)
      call bcast_ctl_type_c1(lp_ctl%order_spectra_switch)
      call bcast_ctl_type_c1(lp_ctl%diff_lm_spectra_switch)
      call bcast_ctl_type_c1(lp_ctl%axis_power_switch)
!
      call calypso_mpi_bcast_character                                  &
     &   (lp_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(lp_ctl%i_layer_spectr_ctl, 0)
!
      end subroutine bcast_layerd_spectr_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_dipolarity_ctl(fdip_ctl)
!
      use transfer_to_long_integers
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use t_ctl_data_sph_dipolarity
!
      type(sph_dipolarity_control), intent(inout) :: fdip_ctl
!
!
      call bcast_ctl_array_i1(fdip_ctl%fdip_truncation_ctl)
      call bcast_ctl_type_c1(fdip_ctl%fdip_file_prefix_ctl)
      call bcast_ctl_type_c1(fdip_ctl%fdip_file_format_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (fdip_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(fdip_ctl%i_dipolarity_ctl, 0)
!
      end subroutine bcast_sph_dipolarity_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_mid_eq_monitor_ctl(num_circ_ctl, meq_ctl)
!
      use transfer_to_long_integers
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use t_ctl_data_mid_equator
!
      integer(kind = kint), intent(in) :: num_circ_ctl
      type(mid_equator_control), intent(inout) :: meq_ctl(num_circ_ctl)
      integer(kind = kint) :: i
!
!
      do i = 1, num_circ_ctl
        call bcast_ctl_type_r1(meq_ctl(i)%pick_s_ctl)
        call bcast_ctl_type_r1(meq_ctl(i)%pick_z_ctl)
!
        call bcast_ctl_type_i1(meq_ctl(i)%nphi_mid_eq_ctl)
!
        call bcast_ctl_type_c1(meq_ctl(i)%pick_circle_coord_ctl)
!
        call bcast_ctl_type_c1(meq_ctl(i)%circle_field_file_ctl)
        call bcast_ctl_type_c1(meq_ctl(i)%circle_spectr_file_ctl)
        call bcast_ctl_type_c1(meq_ctl(i)%circle_file_format_ctl)
!
        call calypso_mpi_bcast_character                                &
     &     (meq_ctl(i)%block_name, cast_long(kchara), 0)
        call calypso_mpi_bcast_one_int(meq_ctl(i)%i_mid_equator_ctl, 0)
      end do
!
      end subroutine bcast_mid_eq_monitor_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_dynamobench(dbench_ctl)
!
      use transfer_to_long_integers
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use t_ctl_data_dynamobench
!
      type(dynamobench_control), intent(inout) :: dbench_ctl
!
!
      call bcast_ctl_type_i1(dbench_ctl%nphi_mid_eq_ctl)
      call bcast_ctl_type_c1(dbench_ctl%dynamobench_file_ctl)
      call bcast_ctl_type_c1(dbench_ctl%dynamobench_format_ctl)
!
      call bcast_ctl_type_c1(dbench_ctl%detailed_dbench_file_ctl)
      call bcast_ctl_type_c1(dbench_ctl%dbench_field_file_ctl)
      call bcast_ctl_type_c1(dbench_ctl%dbench_spectr_file_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (dbench_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(dbench_ctl%i_dynamobench_ctl, 0)
!
      end subroutine bcast_ctl_data_dynamobench
!
! -----------------------------------------------------------------------
!
      end module bcast_4_sph_monitor_ctl
