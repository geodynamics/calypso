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
      use t_pickup_sph_spectr_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_pick_sph                                &
     &         (pspec_ctl, pick_list, picked_sph)
!
      use t_ctl_data_pick_sph_spectr
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
