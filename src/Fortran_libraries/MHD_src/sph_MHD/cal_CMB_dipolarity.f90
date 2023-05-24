!>@file   cal_CMB_dipolarity.f90
!!@brief      module cal_CMB_dipolarity
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate dipolarity at CMB
!!
!!@verbatim
!!      subroutine set_ctl_dipolarity_params                            &
!!     &         (fdip_file_prefix, fdip_file_format, fdip_truncation,  &
!!     &          rj_fld, dip)
!!        type(read_character_item), intent(in) :: fdip_file_prefix
!!        type(read_character_item), intent(in) :: fdip_file_format
!!        type(ctl_array_int), intent(in) :: fdip_truncation
!!        type(phys_data), intent(in) :: rj_fld
!!        type(dipolarity_data), intent(inout) :: dip
!!      subroutine init_dipolarity_4_sph_spectr(sph_params, pwr, dip)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(dipolarity_data), intent(inout) :: dip
!!      subroutine s_cal_CMB_dipolarity(id_rank, rj_fld, pwr, dip)
!!        integer, intent(in) :: id_rank
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(in) :: pwr
!!        real(kind = kreal), intent(inout) :: f_dip
!!@endverbatim
!
      module cal_CMB_dipolarity
!
      use m_precision
      use m_constants
!
      use t_phys_data
      use t_CMB_dipolarity
      use t_rms_4_sph_spectr
      use t_spheric_parameter
!
      implicit none
!
      private :: find_rms_address_4_mene
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_dipolarity_params                              &
     &         (fdip_file_prefix, fdip_file_format, fdip_truncation,    &
     &          rj_fld, dip)
!
      use t_control_array_character
      use t_control_array_integer
      use t_multi_flag_labels
      use m_file_format_labels
      use m_base_field_labels
      use set_parallel_file_name
!
      type(read_character_item), intent(in) :: fdip_file_prefix
      type(read_character_item), intent(in) ::  fdip_file_format
      type(ctl_array_int), intent(in) :: fdip_truncation
      type(phys_data), intent(in) :: rj_fld
      type(dipolarity_data), intent(inout) :: dip
!
      integer(kind = kint) :: i, num
      character(len = kchara) :: input_flag
!
!    Turn On Nusselt number if temperature gradient is there
      dip%iflag_dipolarity = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. magnetic_field%name) then
          dip%iflag_dipolarity = 1
          exit
        end if
      end do
!
      if(fdip_file_prefix%iflag .gt. 0) then
        dip%iflag_dipolarity = 1
        dip%dipolarity_file_name                                        &
     &              = add_dat_extension(fdip_file_prefix%charavalue)
      else
        dip%iflag_dipolarity = 0
      end if
!
      dip%flag_gzip_dipolarity = .FALSE.
      if(dip%iflag_dipolarity .gt. 0) then
        if(fdip_file_format%iflag .gt. 0) then
          input_flag = fdip_file_format%charavalue
          if(check_mul_flags(input_flag, gzip_flags))                   &
     &                           dip%flag_gzip_dipolarity = .TRUE.
        end if
!
        num = 1
        if(fdip_truncation%num .gt. 0) then
          num = fdip_truncation%num + 1
        end if
        call alloc_dipolarity_data(num, dip)
!
        dip%ltr_max(1) = -1
        do i = 2, dip%num_dip
          dip%ltr_max(i) = fdip_truncation%ivec(i-1)
        end do
      end if
!
      end subroutine set_ctl_dipolarity_params
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_dipolarity_4_sph_spectr(sph_params, pwr, dip)
!
      use set_parallel_file_name
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_mean_squares), intent(in) :: pwr
!
      type(dipolarity_data), intent(inout) :: dip
!
      integer(kind = kint) :: i, knum
!
!
      if(dip%iflag_dipolarity .gt. 0) then
        do i = 1, dip%num_dip
          if(dip%ltr_max(i).le.0                                        &
     &            .or. dip%ltr_max(i).gt.sph_params%l_truncation) then
            dip%ltr_max(i) = sph_params%l_truncation
          end if
          dip%dip_name(i) = append_index(dip%ltr_max(i), dip_ltr_label) 
        end do
!
        do knum = 1, pwr%nri_rms
          if(pwr%kr_4_rms(knum) .eq. sph_params%nlayer_CMB) then
            dip%krms_CMB = knum
            dip%rdip_CMB = sph_params%radius_CMB
          end if
        end do
      end if
!
      end subroutine init_dipolarity_4_sph_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_cal_CMB_dipolarity(id_rank, rj_fld, pwr, dip)
!
      use t_spheric_parameter
      use t_spheric_rj_data
!
      integer, intent(in) :: id_rank
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(dipolarity_data), intent(inout) :: dip
!
!>      magnetic energy at CMB
      real(kind = kreal) :: me_cmb_d
!>      dipole component of magnetic energy at CMB
      real(kind = kreal) :: pwr_g10
!
      integer(kind = kint) :: i, l
!
!
      if(dip%iflag_dipolarity .le. izero) return
!
      if(dip%icomp_mene .le. 0)                                         &
     &          dip%icomp_mene = find_rms_address_4_mene(pwr, rj_fld)
      if(dip%icomp_mene .le. 0) return
!
      if(id_rank .eq. pwr%irank_l) then
        pwr_g10 = pwr%shl_l(dip%krms_CMB,1,dip%icomp_mene)
!
        do i = 1, dip%num_dip
          me_cmb_d = 0.0d0
!$omp parallel do reduction(+:me_cmb_d)
          do l = 1, dip%ltr_max(i)
            me_cmb_d = me_cmb_d                                         &
     &                + pwr%shl_l(dip%krms_CMB,l,dip%icomp_mene)
          end do
!$omp end parallel do
          dip%f_dip(i) = pwr_g10 / me_cmb_d
        end do
      end if
!
      end subroutine s_cal_CMB_dipolarity
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
      end module cal_CMB_dipolarity
