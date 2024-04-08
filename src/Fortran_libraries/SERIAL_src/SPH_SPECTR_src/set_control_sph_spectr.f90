!>@file   set_control_sph_spectr.f90
!!        module set_control_sph_spectr
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Set control parameter for monitoring spectrum
!!
!!@verbatim
!!      subroutine set_ctl_params_layered_spectr(lp_ctl, pwr)
!!        type(layerd_spectr_control), intent(in) :: lp_ctl
!!        type(sph_mean_squares), intent(inout) :: pwr
!!      subroutine set_ctl_params_sph_spectr(smonitor_ctl, pwr)
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(sph_mean_squares), intent(inout) :: pwr
!!
!!      subroutine set_ctl_params_base_vol_spectr(smonitor_ctl, v_spectr)
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(sph_vol_mean_squares), intent(inout) :: v_spectr
!!      subroutine set_ctl_params_vol_sph_spectr(v_pwr_ctl, v_spectr)
!!      subroutine set_ctl_prm_vol_sph_spectr(v_pwr_ctl, v_spectr)
!!        type(volume_spectr_control), intent(in) :: v_pwr_ctl
!!        type(sph_vol_mean_squares), intent(inout) :: v_spectr
!!@endverbatim
!!
      module set_control_sph_spectr
!
      use m_precision
      use t_rms_4_sph_spectr
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_layered_spectr(lp_ctl, pwr)
!
      use t_ctl_data_sph_layer_spectr
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
!
      use m_file_format_labels
      use skip_comment_f
!
      type(layerd_spectr_control), intent(in) :: lp_ctl
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      pwr%iflag_layer_rms_spec = lp_ctl%layered_pwr_spectr_prefix%iflag
      if(pwr%iflag_layer_rms_spec .gt. 0) then
        pwr%fhead_rms_layer                                             &
     &        = lp_ctl%layered_pwr_spectr_prefix%charavalue
      end if
!
      call set_ctl_prm_layered_spectr(lp_ctl, pwr)
!
      end subroutine set_ctl_params_layered_spectr
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_sph_spectr(smonitor_ctl, pwr)
!
      use t_ctl_data_4_sph_monitor
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
!
      use m_file_format_labels
      use skip_comment_f
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: num_vspec, inum
!
!
      num_vspec = 1
      if(smonitor_ctl%num_vspec_ctl .gt. 0) then
        num_vspec = smonitor_ctl%num_vspec_ctl + num_vspec
      end if

      call alloc_volume_spectr_data(num_vspec, pwr)
!
      call set_ctl_params_base_vol_spectr(smonitor_ctl,                 &
     &                                    pwr%v_spectr(1))
      call set_ctl_prm_base_vol_spectr(smonitor_ctl, pwr%v_spectr(1))
!
      do inum = 1, smonitor_ctl%num_vspec_ctl
        call set_ctl_params_vol_sph_spectr(smonitor_ctl%v_pwr(inum),    &
     &                                     pwr%v_spectr(inum+1))
      end do
!
      end subroutine set_ctl_params_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_base_vol_spectr(smonitor_ctl, v_spectr)
!
      use t_ctl_data_4_sph_monitor
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
!
      use m_file_format_labels
      use skip_comment_f
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(sph_vol_mean_squares), intent(inout) :: v_spectr
!
!
      v_spectr%iflag_volume_rms_spec                                    &
     &        = smonitor_ctl%volume_pwr_spectr_prefix%iflag
      if(v_spectr%iflag_volume_rms_spec .gt. 0) then
        v_spectr%fhead_rms_v                                            &
     &        = smonitor_ctl%volume_pwr_spectr_prefix%charavalue
      end if
!
      v_spectr%iflag_volume_ave_sph                                     &
     &         =  smonitor_ctl%volume_average_prefix%iflag
      if(v_spectr%iflag_volume_ave_sph .gt. 0) then
        v_spectr%fhead_ave                                              &
     &         = smonitor_ctl%volume_average_prefix%charavalue
      end if
!
      end subroutine set_ctl_params_base_vol_spectr
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_vol_sph_spectr(v_pwr_ctl, v_spectr)
!
      use t_ctl_data_4_sph_monitor
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
!
      use m_file_format_labels
      use skip_comment_f
!
      type(volume_spectr_control), intent(in) :: v_pwr_ctl
      type(sph_vol_mean_squares), intent(inout) :: v_spectr
!
      character(len = kchara) :: input_flag
!
!
      v_spectr%iflag_volume_rms_spec                                    &
     &      = v_pwr_ctl%volume_spec_file_ctl%iflag
      if(v_spectr%iflag_volume_rms_spec .gt. 0) then
        v_spectr%fhead_rms_v                                            &
     &     = v_pwr_ctl%volume_spec_file_ctl%charavalue
      end if
!
      v_spectr%iflag_volume_ave_sph                                     &
     &      = v_pwr_ctl%volume_ave_file_ctl%iflag
      if(v_spectr%iflag_volume_ave_sph .gt. 0) then
        v_spectr%fhead_ave = v_pwr_ctl%volume_ave_file_ctl%charavalue
      end if
!
      v_spectr%gzip_flag_vol_spec = .FALSE.
      if(v_pwr_ctl%volume_spec_format_ctl%iflag .gt. 0) then
        input_flag = v_pwr_ctl%volume_spec_format_ctl%charavalue
        if(check_mul_flags(input_flag, gzip_flags))                     &
     &                   v_spectr%gzip_flag_vol_spec = .TRUE.
      end if
!
      call set_ctl_prm_vol_sph_spectr(v_pwr_ctl, v_spectr)
      call set_ctl_vol_sph_spectr_range(v_pwr_ctl, v_spectr)
!
      end subroutine set_ctl_params_vol_sph_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ctl_prm_layered_spectr(lp_ctl, pwr)
!
      use t_ctl_data_sph_layer_spectr
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
!
      use m_file_format_labels
      use skip_comment_f
!
      type(layerd_spectr_control), intent(in) :: lp_ctl
      type(sph_mean_squares), intent(inout) :: pwr
!
      character(len = kchara) :: input_flag
      integer(kind = kint) :: num_layer, i, ist
!
!
      pwr%gzip_flag_rms_layer = .FALSE.
      if(pwr%iflag_layer_rms_spec .gt. 0) then
        if(lp_ctl%layered_pwr_spectr_format%iflag .gt. 0) then
          input_flag = lp_ctl%layered_pwr_spectr_format%charavalue
          if(check_mul_flags(input_flag, gzip_flags))                   &
     &                             pwr%gzip_flag_rms_layer = .TRUE.
        end if
      end if
!
      pwr%flag_skip_spectr_l =  .TRUE.
      if(no_flag(lp_ctl%degree_spectra_switch%charavalue))              &
     &                              pwr%flag_skip_spectr_l = .FALSE.
      pwr%flag_skip_spectr_m =  .TRUE.
      if(no_flag(lp_ctl%order_spectra_switch%charavalue))               &
     &                              pwr%flag_skip_spectr_m = .FALSE.
      pwr%flag_skip_spectr_lm = .TRUE.
      if(no_flag(lp_ctl%diff_lm_spectra_switch%charavalue))             &
     &                              pwr%flag_skip_spectr_lm = .FALSE.
      pwr%flag_skip_spectr_m0 = .TRUE.
      if(no_flag(lp_ctl%axis_power_switch%charavalue))                  &
     &                              pwr%flag_skip_spectr_m0 = .FALSE.
!
!   set pickup layer
      num_layer = 0
      if(pwr%iflag_layer_rms_spec .eq. 0                                &
     &     .and. lp_ctl%layer_radius_ctl%num .eq. 0) then
        pwr%nri_rms = -1
        return
      else if(lp_ctl%idx_spec_layer_ctl%num .eq. 1                      &
        .and. lp_ctl%idx_spec_layer_ctl%ivec(1) .lt. 0) then
        pwr%nri_rms = -1
        return
      else if(pwr%iflag_layer_rms_spec .eq. 0) then
        num_layer = 0
      else
        num_layer = lp_ctl%idx_spec_layer_ctl%num
      end if
      if(lp_ctl%layer_radius_ctl%num .gt. 0) then
        num_layer = num_layer + lp_ctl%layer_radius_ctl%num
      end if
!
      call alloc_num_spec_layer(num_layer, pwr)
!
      do i = 1, lp_ctl%idx_spec_layer_ctl%num
        pwr%kr_4_rms(i,1) = lp_ctl%idx_spec_layer_ctl%ivec(i)
        pwr%r_4_rms(i,1) = -one
      end do
      ist = lp_ctl%idx_spec_layer_ctl%num
      do i = 1, lp_ctl%layer_radius_ctl%num
        pwr%kr_4_rms(i+ist,1) = 0
        pwr%r_4_rms(i+ist,1) =  lp_ctl%layer_radius_ctl%vect(i)
      end do
!
      end subroutine set_ctl_prm_layered_spectr
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_prm_base_vol_spectr(smonitor_ctl, v_spectr)
!
      use t_ctl_data_4_sph_monitor
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
!
      use m_file_format_labels
      use skip_comment_f
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(sph_vol_mean_squares), intent(inout) :: v_spectr
!
      character(len = kchara) :: input_flag
!
!
      v_spectr%gzip_flag_vol_spec = .FALSE.
      if(smonitor_ctl%volume_pwr_spectr_format%iflag .gt. 0) then
        input_flag = smonitor_ctl%volume_pwr_spectr_format%charavalue
        if(check_mul_flags(input_flag, gzip_flags))                     &
     &                     v_spectr%gzip_flag_vol_spec = .TRUE.
      end if
!
      v_spectr%flag_skip_v_spec_l =  .FALSE.
      if(no_flag(smonitor_ctl%degree_v_spectra_switch%charavalue))      &
     &                           v_spectr%flag_skip_v_spec_l = .TRUE.
      v_spectr%flag_skip_v_spec_m =  .FALSE.
      if(no_flag(smonitor_ctl%order_v_spectra_switch%charavalue))       &
     &                           v_spectr%flag_skip_v_spec_m = .TRUE.
      v_spectr%flag_skip_v_spec_lm = .FALSE.
      if(no_flag(smonitor_ctl%diff_v_lm_spectra_switch%charavalue))     &
     &                           v_spectr%flag_skip_v_spec_lm = .TRUE.
       v_spectr%flag_skip_v_spec_m0 = .FALSE.
      if(no_flag(smonitor_ctl%axis_v_power_switch%charavalue))          &
     &                           v_spectr%flag_skip_v_spec_m0 = .TRUE.
!
      v_spectr%r_inside =  -1.0
      v_spectr%r_outside = -1.0
!
      end subroutine set_ctl_prm_base_vol_spectr
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_prm_vol_sph_spectr(v_pwr_ctl, v_spectr)
!
      use t_ctl_data_4_sph_monitor
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
!
      use m_file_format_labels
      use skip_comment_f
!
      type(volume_spectr_control), intent(in) :: v_pwr_ctl
      type(sph_vol_mean_squares), intent(inout) :: v_spectr
!
      character(len = kchara) :: input_flag
!
!
      v_spectr%flag_skip_v_spec_l =  .FALSE.
      if(no_flag(v_pwr_ctl%degree_v_spectra_switch%charavalue))         &
     &                           v_spectr%flag_skip_v_spec_l = .TRUE.
      v_spectr%flag_skip_v_spec_m =  .FALSE.
      if(no_flag(v_pwr_ctl%order_v_spectra_switch%charavalue))          &
     &                           v_spectr%flag_skip_v_spec_m = .TRUE.
      v_spectr%flag_skip_v_spec_lm = .FALSE.
      if(no_flag(v_pwr_ctl%diff_v_lm_spectra_switch%charavalue))        &
     &                           v_spectr%flag_skip_v_spec_lm = .TRUE.
       v_spectr%flag_skip_v_spec_m0 = .FALSE.
      if(no_flag(v_pwr_ctl%axis_v_power_switch%charavalue))             &
     &                           v_spectr%flag_skip_v_spec_m0 = .TRUE.
!
      end subroutine set_ctl_prm_vol_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_vol_sph_spectr_range(v_pwr_ctl, v_spectr)
!
      use t_ctl_data_4_sph_monitor
!
      use m_file_format_labels
      use skip_comment_f
!
      type(volume_spectr_control), intent(in) :: v_pwr_ctl
      type(sph_vol_mean_squares), intent(inout) :: v_spectr
!
!
      if(v_pwr_ctl%inner_radius_ctl%iflag .gt. 0) then
        v_spectr%r_inside = v_pwr_ctl%inner_radius_ctl%realvalue
      else
        v_spectr%r_inside = -1.0
      end if
!
      if(v_pwr_ctl%outer_radius_ctl%iflag .gt. 0) then
        v_spectr%r_outside = v_pwr_ctl%outer_radius_ctl%realvalue
      else
        v_spectr%r_outside = -1.0
      end if
!
      end subroutine set_ctl_vol_sph_spectr_range
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_spectr
