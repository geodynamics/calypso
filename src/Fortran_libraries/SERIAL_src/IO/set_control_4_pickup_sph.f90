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
!!
!!      subroutine set_ctl_params_no_heat_Nu
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
      integer(kind = kint) :: inum
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
      num_pick_sph = idx_pick_sph_ctl%num
      call allocate_pick_sph_mode
!
      do inum = 1, num_pick_sph
        idx_pick_sph_mode(inum,1) = idx_pick_sph_ctl%int1(inum)
        idx_pick_sph_mode(inum,2) = idx_pick_sph_ctl%int2(inum)
      end do
      call deallocate_pick_sph_ctl
!
      num_pick_sph_m = idx_pick_sph_m_ctl%num
      call allocate_pick_sph_m
!
      do inum = 1, num_pick_sph_m
        idx_pick_sph_m(inum) = idx_pick_sph_m_ctl%ivec(inum)
      end do
      call deallocate_pick_sph_m_ctl
!
!
      num_pick_sph_l = idx_pick_sph_l_ctl%num
      if(num_pick_sph_l .gt. 0) then
        call allocate_pick_sph_l
!
        do inum = 1, num_pick_sph_l
          idx_pick_sph_l(inum) = idx_pick_sph_l_ctl%ivec(inum)
        end do
      call deallocate_pick_sph_l_ctl
      else if( i_picked_mode_head .gt. 0                                &
     &   .and. num_pick_sph_m .le. 0  .and. num_pick_sph .le. 0) then
        num_pick_sph_l = -9999
      else 
        call allocate_pick_sph_l
      end if
!
!   set pickup layer
!
      num_pick_layer = 0
      if(idx_pick_layer_ctl%num .gt. 0) then
        num_pick_layer = idx_pick_layer_ctl%num
        call allocate_num_pick_layer
!
        do inum = 1, num_pick_layer
          id_pick_layer(inum) = idx_pick_layer_ctl%ivec(inum)
        end do
!
        call deallocate_num_pick_layer_ctl
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
      integer(kind = kint) :: inum
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
      num_pick_gauss_coefs = idx_gauss_ctl%num
      call allocate_pick_gauss
!
      do inum = 1, num_pick_gauss_coefs
        idx_pick_gauss_mode(inum,1) = idx_gauss_ctl%int1(inum)
        idx_pick_gauss_mode(inum,2) = idx_gauss_ctl%int2(inum)
      end do
!
      if(num_pick_gauss_coefs .gt. 0) call deallocate_pick_gauss_ctl
!
!
      num_pick_gauss_m = idx_gauss_m_ctl%num
      call allocate_pick_gauss_m
!
      do inum = 1, num_pick_gauss_m
        idx_pick_gauss_m(inum) = idx_gauss_m_ctl%ivec(inum)
      end do
      call deallocate_pick_gauss_m_ctl
!
!
      num_pick_gauss_l = idx_gauss_l_ctl%num
      if(num_pick_gauss_l .gt. 0) then
        call allocate_pick_gauss_l
!
        do inum = 1, num_pick_gauss_l
          idx_pick_gauss_l(inum) = idx_gauss_l_ctl%ivec(inum)
        end do
        call deallocate_pick_gauss_l_ctl
      else if( i_gauss_coefs_head .gt. 0                                &
     &   .and. num_pick_gauss_m .le. 0                                  &
     &   .and. num_pick_gauss_coefs .le. 0) then
       num_pick_gauss_l = -9999
      end if
!
      end subroutine set_ctl_params_pick_gauss
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_no_heat_Nu
!
      use m_ctl_data_4_pickup_sph
      use m_sph_spectr_data
      use m_phys_labels
      use m_no_heat_Nusselt_num
!
      integer(kind = kint) :: i
!
!    Turn On Nusselt number if temperature gradient is there
      iflag_no_source_Nu = 0
      do i = 1, num_phys_rj
        if(phys_name_rj(i) .eq. fhd_grad_temp) then
          iflag_no_source_Nu = 1
          exit
        end if
      end do
!
      if(i_Nusselt_file_head .gt. 0) then
        iflag_no_source_Nu = 1
        Nusselt_file_head = Nusselt_file_prefix
      else
        iflag_no_source_Nu = 0
      end if
!
!    Turn Off Nusselt number if heat source is there
      do i = 1, num_phys_rj
        if(phys_name_rj(i) .eq. fhd_heat_source) then
          iflag_no_source_Nu = 0
          exit
        end if
      end do
!
      end subroutine set_ctl_params_no_heat_Nu
!
! -----------------------------------------------------------------------
!
      end module set_control_4_pickup_sph
