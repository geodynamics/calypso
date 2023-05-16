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
      character(len = kchara) :: input_flag
!
!
      if(no_flag(lp_ctl%degree_spectr_switch%charavalue))               &
     &                                      pwr%iflag_spectr_l = 0
      if(no_flag(lp_ctl%order_spectr_switch%charavalue))                &
     &                                      pwr%iflag_spectr_m = 0
      if(no_flag(lp_ctl%diff_lm_spectr_switch%charavalue))              &
     &                                      pwr%iflag_spectr_lm = 0
      if(no_flag(lp_ctl%axis_spectr_switch%charavalue))                 &
     &                                      pwr%iflag_spectr_m0 = 0
!
!
      pwr%iflag_layer_rms_spec = lp_ctl%layered_pwr_spectr_prefix%iflag
      if(pwr%iflag_layer_rms_spec .gt. 0) then
        pwr%fhead_rms_layer                                             &
     &        = lp_ctl%layered_pwr_spectr_prefix%charavalue
      end if
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
!   set pickup layer
      if(pwr%iflag_layer_rms_spec .eq. 0) then
        call alloc_num_spec_layer(izero, pwr)
      else if(lp_ctl%idx_spec_layer_ctl%num .eq. 1                      &
        .and. lp_ctl%idx_spec_layer_ctl%ivec(1) .lt. 0) then
        pwr%nri_rms = -1
      else if(lp_ctl%idx_spec_layer_ctl%num .gt. 0) then
        call alloc_num_spec_layer(lp_ctl%idx_spec_layer_ctl%num, pwr)
!
        pwr%kr_4_rms(1:pwr%nri_rms)                                     &
     &         = lp_ctl%idx_spec_layer_ctl%ivec(1:pwr%nri_rms)
      else
        pwr%nri_rms = -1
      end if
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
      character(len = kchara) :: input_flag
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
      character(len = kchara) :: input_flag
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
      v_spectr%gzip_flag_vol_spec = .FALSE.
      if(smonitor_ctl%volume_pwr_spectr_format%iflag .gt. 0) then
        input_flag = smonitor_ctl%volume_pwr_spectr_format%charavalue
        if(check_mul_flags(input_flag, gzip_flags))                     &
     &                     v_spectr%gzip_flag_vol_spec = .TRUE.
      end if
!
      v_spectr%r_inside =  -1.0
      v_spectr%r_outside = -1.0
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
      end subroutine set_ctl_params_vol_sph_spectr
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_spectr
