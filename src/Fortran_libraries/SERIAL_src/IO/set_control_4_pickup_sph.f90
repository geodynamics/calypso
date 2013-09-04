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
!!      subroutine set_ctl_params_pick_sph
!!      subroutine set_ctl_params_pick_gauss
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
      subroutine set_ctl_params_pick_sph
!
      use m_ctl_data_4_pickup_sph
      use m_pickup_sph_spectr_data
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: inum, l, m
!
!
      iflag_layer_rms_spec =  i_layer_rms_head
      if(iflag_layer_rms_spec .gt. 0) then
        fhead_rms_layer = layered_pwr_spectr_prefix
      end if      
!
      iflag_volume_rms_spec = i_voume_rms_head
      if(iflag_volume_rms_spec .gt. 0) then
        fhead_rms_vol = volume_pwr_spectr_prefix
      end if
!
      iflag_volume_ave_sph =  i_voume_ave_head
      if(iflag_volume_ave_sph .gt. 0) then
        fhead_ave_vol = volume_average_prefix
      end if
!
!
!
      if(i_picked_mode_head .gt. 0) then
        pickup_sph_head = picked_mode_head_ctl
      else
        num_pick_sph = 0
        num_pick_sph_l = 0
        num_pick_sph_m = 0
        num_pick_layer = 0
        call allocate_pick_sph_mode
        call allocate_pick_sph_l
        call allocate_pick_sph_m
        call allocate_num_pick_layer
        return
      end if
!
!   set pickup mode
!
      num_pick_sph = num_pick_sph_mode_ctl
      call allocate_pick_sph_mode
!
      do inum = 1, num_pick_sph
        l = idx_pick_sph_mode_ctl(inum,1)
        m = idx_pick_sph_mode_ctl(inum,2)
        idx_pick_sph_mode(inum) = l*(l+1) + m
      end do
      if(num_pick_sph .gt. 0) call deallocate_pick_sph_ctl
!
      num_pick_sph_l = num_pick_sph_l_ctl
      call allocate_pick_sph_l
!
      do inum = 1, num_pick_sph_l
        idx_pick_sph_l(inum) = idx_pick_sph_l_ctl(inum)
      end do
      if(num_pick_sph_l .gt. 0) call deallocate_pick_sph_l_ctl
!
      num_pick_sph_m = num_pick_sph_m_ctl
      call allocate_pick_sph_m
!
      do inum = 1, num_pick_sph_m
        idx_pick_sph_m(inum) = idx_pick_sph_m_ctl(inum)
      end do
      if(num_pick_sph_m .gt. 0) call deallocate_pick_sph_m_ctl
!
!   set pickup layer
!
      if (i_num_pick_layer .gt. 0) then
        if(num_pick_layer_ctl .gt. 0) then
          num_pick_layer = num_pick_layer_ctl
          call allocate_num_pick_layer
!
          id_pick_layer(1:num_pick_layer)                               &
     &      = id_pick_layer_ctl(1:num_pick_layer)
!
          call deallocate_num_pick_layer_ctl
        else
          num_pick_layer = 0
        end if
      else
        num_pick_layer = 0
      end if
!
      end subroutine set_ctl_params_pick_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_pick_gauss
!
      use m_ctl_data_4_pickup_sph
      use m_gauss_coefs_monitor_data
!
      integer(kind = kint) :: inum, l, m
!
!
!   set pickup gauss coefficients
!
      if(i_gauss_coefs_head .gt. 0) then
        gauss_coefs_file_head = gauss_coefs_prefix
      else
        num_pick_gauss_coefs = 0
        num_pick_gauss_l = 0
        num_pick_gauss_m = 0
        call allocate_pick_gauss
        call allocate_pick_gauss_l
        call allocate_pick_gauss_m
        return
      end if
!
      if(i_gauss_coefs_r .gt. 0) then
        r_4_gauss_coefs = gauss_coefs_radius_ctl
      end if
!
      num_pick_gauss_coefs = num_pick_gauss_coefs_ctl
      call allocate_pick_gauss
!
      do inum = 1, num_pick_gauss_coefs
        l = idx_pick_gauss_mode_ctl(inum,1)
        m = idx_pick_gauss_mode_ctl(inum,2)
        idx_pick_gauss_mode(inum) = l*(l+1) + m
      end do
!
      if(num_pick_gauss_coefs .gt. 0) call deallocate_pick_gauss_ctl
!
      num_pick_gauss_l = num_pick_gauss_l_ctl
      call allocate_pick_gauss_l
!
      do inum = 1, num_pick_gauss_l
        idx_pick_gauss_l(inum) = idx_pick_gauss_l_ctl(inum)
      end do
      if(num_pick_gauss_l .gt. 0) call deallocate_pick_gauss_l_ctl
!
      num_pick_gauss_m = num_pick_gauss_m_ctl
      call allocate_pick_gauss_m
!
      do inum = 1, num_pick_gauss_m
        idx_pick_gauss_m(inum) = idx_pick_gauss_m_ctl(inum)
      end do
      if(num_pick_gauss_m .gt. 0) call deallocate_pick_gauss_m_ctl
!
      end subroutine set_ctl_params_pick_gauss
!
! -----------------------------------------------------------------------
!
      end module set_control_4_pickup_sph
