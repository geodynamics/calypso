!>@file   set_control_4_pickup_sph.f90
!!        module set_control_4_pickup_sph
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!
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
!!
!!      subroutine set_ctl_params_no_heat_Nu                            &
!!     &         (Nusselt_file_prefix, rj_fld, Nu_type)
!!        type(read_character_item), intent(in) :: Nusselt_file_prefix
!!        type(phys_data), intent(in) :: rj_fld
!!        type(nusselt_number_data), intent(inout) :: Nu_type
!!@endverbatim
!!
      module set_control_4_pickup_sph
!
      use m_precision
!
      implicit  none
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
      use output_sph_m_square_file
      use skip_comment_f
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: i, j, num_vspec
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
      pwr%v_spectr(1)%r_inside =  -1.0
      pwr%v_spectr(1)%r_outside = -1.0
!
!
      do j = 2, num_vspec
        i = j - 1
        pwr%v_spectr(j)%iflag_volume_rms_spec                           &
     &        = smonitor_ctl%v_pwr(i)%volume_spec_file_ctl%iflag
        if(pwr%v_spectr(j)%iflag_volume_rms_spec .gt. 0) then
          pwr%v_spectr(j)%fhead_rms_v                                   &
     &       = smonitor_ctl%v_pwr(i)%volume_spec_file_ctl%charavalue
        end if
!
        pwr%v_spectr(j)%iflag_volume_ave_sph                            &
     &        =  smonitor_ctl%v_pwr(i)%volume_ave_file_ctl%iflag
        if(pwr%v_spectr(j)%iflag_volume_ave_sph .gt. 0) then
          pwr%v_spectr(j)%fhead_ave                                     &
     &        = smonitor_ctl%v_pwr(i)%volume_ave_file_ctl%charavalue
        end if
!
        if(smonitor_ctl%v_pwr(i)%inner_radius_ctl%iflag .gt. 0) then
          pwr%v_spectr(j)%r_inside                                      &
     &        = smonitor_ctl%v_pwr(i)%inner_radius_ctl%realvalue
        else
          pwr%v_spectr(j)%r_inside = -1.0
        end if
!
        if(smonitor_ctl%v_pwr(i)%outer_radius_ctl%iflag .gt. 0) then
          pwr%v_spectr(j)%r_outside                                     &
     &        = smonitor_ctl%v_pwr(i)%outer_radius_ctl%realvalue
        else
          pwr%v_spectr(j)%r_outside = -1.0
        end if
      end do
!
      end subroutine set_ctl_params_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_layered_spectr(lp_ctl, pwr)
!
      use t_ctl_data_sph_vol_spectr
      use t_pickup_sph_spectr_data
      use t_rms_4_sph_spectr
      use output_sph_m_square_file
      use skip_comment_f
!
      type(layerd_spectr_control), intent(in) :: lp_ctl
      type(sph_mean_squares), intent(inout) :: pwr
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
!   set pickup layer
      if(pwr%iflag_layer_rms_spec .eq. 0) then
        pwr%nri_rms = 0
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
      use output_sph_m_square_file
      use skip_comment_f
!
      type(pick_spectr_control), intent(in) :: pspec_ctl
      type(pickup_mode_list), intent(inout) :: pick_list
      type(picked_spectrum_data), intent(inout) :: picked_sph
!
      integer(kind = kint) :: inum
!
!   Define spectr pick up
!
      if(pspec_ctl%picked_mode_head_ctl%iflag .gt. 0) then
        picked_sph%file_prefix                                          &
     &        = pspec_ctl%picked_mode_head_ctl%charavalue
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
      use t_ctl_data_pick_sph_spectr
      use t_pickup_sph_spectr_data
!
      type(gauss_spectr_control), intent(in) :: g_pwr
      type(pickup_mode_list), intent(inout) :: gauss_list
      type(picked_spectrum_data), intent(inout) :: gauss_coef
!
      integer(kind = kint) :: inum
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
      subroutine set_ctl_params_no_heat_Nu                              &
     &         (Nusselt_file_prefix, rj_fld, Nu_type)
!
      use m_phys_labels
      use t_no_heat_Nusselt
      use t_phys_data
      use t_control_elements
!
      type(read_character_item), intent(in) :: Nusselt_file_prefix
      type(phys_data), intent(in) :: rj_fld
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      integer(kind = kint) :: i
!
!    Turn On Nusselt number if temperature gradient is there
      Nu_type%iflag_no_source_Nu = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. fhd_grad_temp) then
          Nu_type%iflag_no_source_Nu = 1
          exit
        end if
      end do
!
      if(Nusselt_file_prefix%iflag .gt. 0) then
        Nu_type%iflag_no_source_Nu = 1
        Nu_type%Nusselt_file_head = Nusselt_file_prefix%charavalue
      else
        Nu_type%iflag_no_source_Nu = 0
      end if
!
!    Turn Off Nusselt number if heat source is there
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. fhd_heat_source) then
          Nu_type%iflag_no_source_Nu = 0
          exit
        end if
      end do
!
      end subroutine set_ctl_params_no_heat_Nu
!
! -----------------------------------------------------------------------
!
      end module set_control_4_pickup_sph
