!>@file   set_control_4_pickup_sph.f90
!!        module set_control_4_pickup_sph
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Set control parameter for monitoring spectrum
!!
!!@verbatim
!!      subroutine set_ctl_params_sph_spectr(smonitor_ctl, pwr)
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(sph_mean_squares), intent(inout) :: pwr
!!      subroutine set_ctl_params_layered_spectr(lp_ctl, pwr)
!!        type(layerd_spectr_control), intent(in) :: lp_ctl
!!        type(sph_mean_squares), intent(inout) :: pwr
!!      subroutine set_ctl_params_pick_sph                              &
!!     &         (pspec_ctl, pick_list, picked_sph)
!!        type(pick_spectr_control), intent(in) :: pspec_ctl
!!        type(pickup_mode_list), intent(inout) :: pick_list
!!        type(picked_spectrum_data), intent(inout) :: picked_sph
!!      subroutine set_ctl_params_pick_gauss                            &
!!     &         (g_pwr, gauss_list, gauss_coef)
!!        type(gauss_spectr_control), intent(in) :: g_pwr
!!        type(pickup_mode_list), intent(inout) :: gauss_list
!!        type(picked_spectrum_data), intent(inout) :: gauss_coef
!!@endverbatim
!!
      module set_control_4_pickup_sph
!
      use m_precision
!
      implicit  none
!
      private :: set_ctl_params_vol_sph_spectr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_sph_spectr(smonitor_ctl, pwr)
!
      use t_ctl_data_4_sph_monitor
      use t_pickup_sph_spectr_data
      use t_rms_4_sph_spectr
      use t_multi_flag_labels
!
      use m_file_format_labels
      use skip_comment_f
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: num_vspec
      character(len = kchara) :: input_flag
!
!
      if(smonitor_ctl%num_vspec_ctl .lt. 0) then
        num_vspec = 1
      else
        num_vspec = smonitor_ctl%num_vspec_ctl + 1
      end if
      call alloc_volume_spectr_data(num_vspec, pwr)
!
      pwr%v_spectr(1)%iflag_volume_rms_spec                             &
     &        = smonitor_ctl%volume_pwr_spectr_prefix%iflag
      if(pwr%v_spectr(1)%iflag_volume_rms_spec .gt. 0) then
        pwr%v_spectr(1)%fhead_rms_v                                     &
     &        = smonitor_ctl%volume_pwr_spectr_prefix%charavalue
      end if
!
      pwr%v_spectr(1)%iflag_volume_ave_sph                              &
     &         =  smonitor_ctl%volume_average_prefix%iflag
      if(pwr%v_spectr(1)%iflag_volume_ave_sph .gt. 0) then
        pwr%v_spectr(1)%fhead_ave                                       &
     &         = smonitor_ctl%volume_average_prefix%charavalue
      end if
!
      pwr%v_spectr(1)%gzip_flag_vol_spec = .FALSE.
      if(smonitor_ctl%volume_pwr_spectr_format%iflag .gt. 0) then
        input_flag = smonitor_ctl%volume_pwr_spectr_format%charavalue
        if(check_mul_flags(input_flag, gzip_flags))                     &
     &                     pwr%v_spectr(1)%gzip_flag_vol_spec = .TRUE.
      end if
!
      pwr%v_spectr(1)%r_inside =  -1.0
      pwr%v_spectr(1)%r_outside = -1.0
!
      call set_ctl_params_vol_sph_spectr                                &
     &   (smonitor_ctl%num_vspec_ctl, smonitor_ctl%v_pwr,               &
     &    pwr%num_vol_spectr, pwr%v_spectr)
!
      end subroutine set_ctl_params_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_vol_sph_spectr                          &
     &         (num_vspec_ctl, v_pwr_ctl, num_vspec, v_spectr)
!
      use t_ctl_data_4_sph_monitor
      use t_pickup_sph_spectr_data
      use t_rms_4_sph_spectr
      use t_multi_flag_labels
!
      use m_file_format_labels
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_vspec_ctl
      type(volume_spectr_control), intent(in)                           &
     &                            :: v_pwr_ctl(num_vspec_ctl)
      integer(kind = kint), intent(in) :: num_vspec
      type(sph_vol_mean_squares), intent(inout) :: v_spectr(num_vspec)
!
      integer(kind = kint) :: i, j
      character(len = kchara) :: input_flag
!
!
      do i = 1, num_vspec_ctl
        j = i + 1
        v_spectr(j)%iflag_volume_rms_spec                               &
     &        = v_pwr_ctl(i)%volume_spec_file_ctl%iflag
        if(v_spectr(j)%iflag_volume_rms_spec .gt. 0) then
          v_spectr(j)%fhead_rms_v                                       &
     &       = v_pwr_ctl(i)%volume_spec_file_ctl%charavalue
        end if
!
        v_spectr(j)%iflag_volume_ave_sph                                &
     &        = v_pwr_ctl(i)%volume_ave_file_ctl%iflag
        if(v_spectr(j)%iflag_volume_ave_sph .gt. 0) then
          v_spectr(j)%fhead_ave                                         &
     &       = v_pwr_ctl(i)%volume_ave_file_ctl%charavalue
        end if
!
        v_spectr(j)%gzip_flag_vol_spec = .FALSE.
        if(v_pwr_ctl(i)%volume_spec_format_ctl%iflag .gt. 0) then
          input_flag                                                    &
     &        = v_pwr_ctl(i)%volume_spec_format_ctl%charavalue
          if(check_mul_flags(input_flag, gzip_flags))                   &
     &                     v_spectr(j)%gzip_flag_vol_spec = .TRUE.
        end if
!
!
        if(v_pwr_ctl(i)%inner_radius_ctl%iflag .gt. 0) then
          v_spectr(j)%r_inside                                          &
     &        = v_pwr_ctl(i)%inner_radius_ctl%realvalue
        else
          v_spectr(j)%r_inside = -1.0
        end if
!
        if(v_pwr_ctl(i)%outer_radius_ctl%iflag .gt. 0) then
          v_spectr(j)%r_outside                                         &
     &        = v_pwr_ctl(i)%outer_radius_ctl%realvalue
        else
          v_spectr(j)%r_outside = -1.0
        end if
      end do
!
      end subroutine set_ctl_params_vol_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_layered_spectr(lp_ctl, pwr)
!
      use t_ctl_data_sph_layer_spectr
      use t_pickup_sph_spectr_data
      use t_rms_4_sph_spectr
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
      subroutine set_ctl_params_pick_sph                                &
     &         (pspec_ctl, pick_list, picked_sph)
!
      use t_ctl_data_pick_sph_spectr
      use t_pickup_sph_spectr_data
      use t_rms_4_sph_spectr
      use t_multi_flag_labels
!
      use m_base_field_labels
      use m_file_format_labels
      use skip_comment_f
!
      type(pick_spectr_control), intent(in) :: pspec_ctl
      type(pickup_mode_list), intent(inout) :: pick_list
      type(picked_spectrum_data), intent(inout) :: picked_sph
!
      integer(kind = kint) :: inum
      character(len = kchara) :: input_flag
!
!   Define spectr pick up
!
      if(pspec_ctl%picked_mode_head_ctl%iflag .gt. 0) then
        picked_sph%file_prefix                                          &
     &        = pspec_ctl%picked_mode_head_ctl%charavalue
!
        picked_sph%flag_gzip = .FALSE.
        if(pspec_ctl%picked_mode_fmt_ctl%iflag .gt. 0) then
          input_flag = pspec_ctl%picked_mode_fmt_ctl%charavalue
          if(check_mul_flags(input_flag, gzip_flags))                   &
     &                           picked_sph%flag_gzip = .TRUE.
        end if
      else
        pick_list%num_modes =  0
        pick_list%num_degree = 0
        pick_list%num_order =  0
        picked_sph%num_layer = 0
        call alloc_pick_sph_mode(pick_list)
        call alloc_pick_sph_l(pick_list)
        call alloc_pick_sph_m(pick_list)
        call alloc_num_pick_layer(picked_sph)
        return
      end if
!
!   set pickup mode
!
      pick_list%num_modes = pspec_ctl%idx_pick_sph_ctl%num
      call alloc_pick_sph_mode(pick_list)
!
      do inum = 1, pick_list%num_modes
        pick_list%idx_pick_mode(inum,1)                                 &
     &        = pspec_ctl%idx_pick_sph_ctl%int1(inum)
        pick_list%idx_pick_mode(inum,2)                                 &
     &        = pspec_ctl%idx_pick_sph_ctl%int2(inum)
      end do
!
      pick_list%num_order = pspec_ctl%idx_pick_sph_m_ctl%num
      call alloc_pick_sph_m(pick_list)
!
      do inum = 1, pick_list%num_order
        pick_list%idx_pick_m(inum)                                      &
     &        = pspec_ctl%idx_pick_sph_m_ctl%ivec(inum)
      end do
!
!
      pick_list%num_degree = pspec_ctl%idx_pick_sph_l_ctl%num
      if(pick_list%num_degree .gt. 0) then
        call alloc_pick_sph_l(pick_list)
!
        do inum = 1, pick_list%num_degree
          pick_list%idx_pick_l(inum)                                    &
     &          = pspec_ctl%idx_pick_sph_l_ctl%ivec(inum)
        end do
      else if(pspec_ctl%picked_mode_head_ctl%iflag .gt. 0               &
     &   .and. pick_list%num_order .le. 0                               &
     &   .and. pick_list%num_modes .le. 0) then
        pick_list%num_degree = -9999
      else 
        call alloc_pick_sph_l(pick_list)
      end if
!
!
!   set pickup layer
      picked_sph%num_layer = 0
      if(pspec_ctl%idx_pick_layer_ctl%num .gt. 0) then
        picked_sph%num_layer = pspec_ctl%idx_pick_layer_ctl%num
        call alloc_num_pick_layer(picked_sph)
!
        do inum = 1, picked_sph%num_layer
          picked_sph%id_radius(inum)                                    &
     &          = pspec_ctl%idx_pick_layer_ctl%ivec(inum)
        end do
      end if
!
      end subroutine set_ctl_params_pick_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_pick_gauss                              &
     &         (g_pwr, gauss_list, gauss_coef)
!
      use t_ctl_data_gauss_coefs
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
      use m_file_format_labels
!
      type(gauss_spectr_control), intent(in) :: g_pwr
      type(pickup_mode_list), intent(inout) :: gauss_list
      type(picked_spectrum_data), intent(inout) :: gauss_coef
!
      integer(kind = kint) :: inum
      character(len = kchara) :: input_flag
!
!
!   set pickup gauss coefficients
!
      if(g_pwr%gauss_coefs_prefix%iflag .gt. 0) then
        gauss_coef%file_prefix = g_pwr%gauss_coefs_prefix%charavalue
      else
        gauss_list%num_modes =  0
        gauss_list%num_degree = 0
        gauss_list%num_order =  0
        call alloc_pick_sph_mode(gauss_list)
        call alloc_pick_sph_l(gauss_list)
        call alloc_pick_sph_m(gauss_list)
        return
      end if
!
      gauss_coef%flag_gzip = .FALSE.
      if(g_pwr%gauss_coefs_format%iflag .gt. 0) then
        input_flag = g_pwr%gauss_coefs_format%charavalue
        if(check_mul_flags(input_flag, gzip_flags))                     &
     &                     gauss_coef%flag_gzip = .TRUE.
      end if
!
      gauss_coef%num_layer = 1
      call alloc_num_pick_layer(gauss_coef)
      gauss_coef%radius_gl(1) = 2.82
!
      if(g_pwr%gauss_coefs_radius_ctl%iflag .gt. 0) then
        gauss_coef%radius_gl(1)                                         &
     &        = g_pwr%gauss_coefs_radius_ctl%realvalue
      end if
!
      gauss_list%num_modes = g_pwr%idx_gauss_ctl%num
      call alloc_pick_sph_mode(gauss_list)
!
      do inum = 1, gauss_list%num_modes
        gauss_list%idx_pick_mode(inum,1)                                &
     &        = g_pwr%idx_gauss_ctl%int1(inum)
        gauss_list%idx_pick_mode(inum,2)                                &
     &        = g_pwr%idx_gauss_ctl%int2(inum)
      end do
!
      gauss_list%num_order = g_pwr%idx_gauss_m_ctl%num
      call alloc_pick_sph_m(gauss_list)
!
      do inum = 1, gauss_list%num_order
        gauss_list%idx_pick_m(inum)                                     &
     &        = g_pwr%idx_gauss_m_ctl%ivec(inum)
      end do
!
!
      gauss_list%num_degree = g_pwr%idx_gauss_l_ctl%num
      if(gauss_list%num_degree .gt. 0) then
        call alloc_pick_sph_l(gauss_list)
!
        do inum = 1, gauss_list%num_degree
          gauss_list%idx_pick_l(inum)                                   &
     &          = g_pwr%idx_gauss_l_ctl%ivec(inum)
        end do
      else if(g_pwr%gauss_coefs_prefix%iflag .gt. 0                     &
     &   .and. gauss_list%num_order .le. 0                              &
     &   .and. gauss_list%num_modes .le. 0) then
       gauss_list%num_degree = -9999
      end if
!
      end subroutine set_ctl_params_pick_gauss
!
! -----------------------------------------------------------------------
!
      end module set_control_4_pickup_sph
