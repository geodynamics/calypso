!>@file   cal_typical_scale.f90
!!@brief      module cal_typical_scale
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate lengh scale data
!!
!!@verbatim
!!      subroutine set_ctl_typical_scale_params                         &
!!     &         (scale_file_prefix_t, typ_scale_file_fmt, rj_fld, tsl)
!!        type(read_character_item), intent(in) :: scale_file_prefix_t
!!        type(read_character_item), intent(in) :: typ_scale_file_fmt
!!        type(phys_data), intent(in) :: rj_fld
!!        type(typical_scale_data), intent(inout) :: tsl
!!      subroutine cal_typical_scales(pwr, tsl)
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(typical_scale_data), intent(inout) :: tsl
!!@endverbatim
!
      module cal_typical_scale
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_sph_typical_scales
      use t_field_labels
      use t_phys_data
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_boundary_params_sph_MHD
!
      implicit none
!
      private :: find_rms_address_4_kene, find_rms_address_4_mene
      private :: s_cal_typical_scale
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_typical_scale_params                           &
     &         (scale_file_prefix_t, typ_scale_file_fmt, rj_fld, tsl)
!
      use t_phys_data
      use t_control_array_character
      use t_multi_flag_labels
      use m_base_field_labels
      use m_field_file_format_labels
!
      type(read_character_item), intent(in) :: scale_file_prefix_t
      type(read_character_item), intent(in) :: typ_scale_file_fmt
      type(phys_data), intent(in) :: rj_fld
      type(typical_scale_data), intent(inout) :: tsl
!
      integer(kind = kint) :: i
      character(len = kchara) :: input_flag
!
!    Turn On Nusselt number if temperature gradient is there
      tsl%iflag_ub_scales = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. magnetic_field%name) then
          tsl%iflag_ub_scales = 1
          exit
        end if
        if(rj_fld%phys_name(i) .eq. velocity%name) then
          tsl%iflag_ub_scales = 1
          exit
        end if
      end do
!
      if(scale_file_prefix_t%iflag .gt. 0) then
        tsl%scale_prefix = scale_file_prefix_t%charavalue
      else
        tsl%iflag_ub_scales = 0
      end if
!
      tsl%flag_gzip_scale = .FALSE.
      if(tsl%iflag_ub_scales .gt. 0) then
        if(typ_scale_file_fmt%iflag .gt. 0) then
          input_flag = typ_scale_file_fmt%charavalue
          if(check_mul_flags(input_flag, gzip_flags))                   &
     &                           tsl%flag_gzip_scale = .TRUE.
        end if
      end if
!
      end subroutine set_ctl_typical_scale_params
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_typical_scales(rj_fld, pwr, tsl)
!
      use t_rms_4_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(typical_scale_data), intent(inout) :: tsl
!
      integer(kind = kint) :: icou
!
!
      if(tsl%icomp_kene .le. 0)                                         &
     &          tsl%icomp_kene = find_rms_address_4_kene(pwr, rj_fld)
      if(tsl%icomp_mene .le. 0)                                         &
     &          tsl%icomp_mene = find_rms_address_4_mene(pwr, rj_fld)
!
      icou = 0
      if(tsl%icomp_kene .gt. 0) icou = icou + 3
      if(tsl%icomp_mene .gt. 0) icou = icou + 3
      call alloc_typical_scale_data(icou, tsl)
!
      icou = 0
      if(tsl%icomp_kene .gt. 0) then
        tsl%lscale_name(icou+1) = 'lscale_flow_degree'
        tsl%lscale_name(icou+2) = 'lscale_flow_order'
        tsl%lscale_name(icou+3) = 'lscale_flow_diff_lm'
        icou = icou + 3
      end if
      if(tsl%icomp_mene .gt. 0) then
        tsl%lscale_name(icou+1) = 'lscale_magnetic_degree'
        tsl%lscale_name(icou+2) = 'lscale_magnetic_order'
        tsl%lscale_name(icou+3) = 'lscale_magnetic_diff_lm'
        icou = icou + 3
      end if
!
      end subroutine init_typical_scales
!
! -----------------------------------------------------------------------
!
      subroutine cal_typical_scales(pwr, tsl)
!
      use t_rms_4_sph_spectr
!
      type(sph_mean_squares), intent(in) :: pwr
      type(typical_scale_data), intent(inout) :: tsl
!
!
      if(tsl%num_lscale .le. 0) return
      if(tsl%iflag_ub_scales .le. izero) return
!
!
      if(tsl%icomp_kene .gt. 0) then
        call s_cal_typical_scale(tsl%icomp_kene, pwr%v_spectr(1),       &
     &                           tsl%dl_kin, tsl%dm_kin, tsl%dlm_kin)
      end if
      if(tsl%icomp_mene .gt. 0) then
        call s_cal_typical_scale(tsl%icomp_mene, pwr%v_spectr(1),       &
     &                           tsl%dl_mag, tsl%dm_mag, tsl%dlm_mag)
      end if
!
      end subroutine cal_typical_scales
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint)                                              &
     &          function find_rms_address_4_kene(pwr, rj_fld)
!
      use m_base_field_labels
      use t_rms_4_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: j_fld, i_fld
!
      find_rms_address_4_kene = 0
      do j_fld = 1, pwr%num_fld_sq
        i_fld = pwr%id_field(j_fld)
!
        if(rj_fld%phys_name(i_fld) .eq. velocity%name) then
          find_rms_address_4_kene =  pwr%istack_comp_sq(j_fld-1) + 1
          exit
        end if
      end do
      end function find_rms_address_4_kene
!
! -----------------------------------------------------------------------
!
      integer(kind = kint)                                              &
     &          function find_rms_address_4_mene(pwr, rj_fld)
!
      use m_base_field_labels
      use t_rms_4_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: j_fld, i_fld
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld = pwr%id_field(j_fld)
!
        find_rms_address_4_mene = 0
        if(rj_fld%phys_name(i_fld) .eq. magnetic_field%name) then
          find_rms_address_4_mene = pwr%istack_comp_sq(j_fld-1) + 1
          exit
        end if
      end do
!
      end function find_rms_address_4_mene
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_typical_scale(icomp_mene, v_pwr,                 &
     &                               dl_mag, dm_mag, dlm_mag)
!
      use t_sph_volume_mean_square
      use calypso_mpi_real
!
      integer(kind = kint), intent(in) :: icomp_mene
      type(sph_vol_mean_squares), intent(in) :: v_pwr
      real(kind = kreal), intent(inout) :: dl_mag, dm_mag, dlm_mag
!
      integer(kind = kint) :: l, m, lm
!
!
      dl_mag = 0.0d0
      dm_mag = 0.0d0
      dlm_mag = 0.0d0
      if(my_rank .eq. v_pwr%irank_l) then
        do l = 1, v_pwr%ltr
          dl_mag =  dl_mag +  dble(l) * v_pwr%v_l(l,icomp_mene+2)
        end do
      end if
      if(my_rank .eq. v_pwr%irank_lm) then
        do lm = 1, v_pwr%ltr
          dlm_mag = dlm_mag + dble(lm) * v_pwr%v_lm(lm,icomp_mene+2)
        end do
      end if
      if(my_rank .eq. v_pwr%irank_m) then
        do m = 1, v_pwr%ltr
          dm_mag =  dm_mag +  dble(m) * v_pwr%v_m(m,icomp_mene+2)
        end do
      end if
      call calypso_mpi_bcast_one_real(dl_mag, v_pwr%irank_l)
      call calypso_mpi_bcast_one_real(dlm_mag, v_pwr%irank_lm)
!
      if(my_rank .eq. v_pwr%irank_m) then
        dm_mag =  dm_mag /  v_pwr%v_sq(icomp_mene+2)
        dl_mag =  dl_mag /  v_pwr%v_sq(icomp_mene+2)
        dlm_mag = dlm_mag / v_pwr%v_sq(icomp_mene+2)
      end if
!
      end subroutine s_cal_typical_scale
!
! -----------------------------------------------------------------------
!
      end module cal_typical_scale
