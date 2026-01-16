!>@file   pick_CMB_average.f90
!!@brief      module pick_CMB_average
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2026
!
!> @brief  Evaluate dipolarity at CMB
!!
!!@verbatim
!!      subroutine set_ctl_CMB_average_params                           &
!!     &         (CMB_ave_file_prefix, CMB_ave_file_format, ave_CMB)
!!        type(read_character_item), intent(in) :: CMB_ave_file_prefix
!!        type(read_character_item), intent(in) :: CMB_ave_file_format
!!        type(CMB_average_data), intent(inout) :: ave_CMB
!!      subroutine init_CMB_average_field_list                          &
!!     &         (my_rank, sph_params, sph_rj, ipol, pwr, ave_CMB)
!!        integer, intent(in) :: my_rank
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(CMB_average_data), intent(inout) :: ave_CMB
!!      subroutine s_pick_CMB_average(my_rank, sph_rj, ipol,            &
!!     &                              rj_fld, ave_CMB)
!!        integer, intent(in) :: my_rank
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(CMB_average_data), intent(inout) :: ave_CMB
!!@endverbatim
!
      module pick_CMB_average
!
      use m_precision
      use m_constants
      use t_CMB_average_data
!
      implicit none
!
      private :: count_CMB_average_data, set_CMB_average_address
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_CMB_average_params                             &
     &         (CMB_ave_file_prefix, CMB_ave_file_format, ave_CMB)
!
      use t_control_array_character
      use m_file_format_labels
      use set_parallel_file_name
!
      type(read_character_item), intent(in) :: CMB_ave_file_prefix
      type(read_character_item), intent(in) :: CMB_ave_file_format
!
      type(CMB_average_data), intent(inout) :: ave_CMB
!
      character(len = kchara) :: input_flag
!
!
      ave_CMB%iflag_CMB_average = 0
      if(CMB_ave_file_prefix%iflag .gt. 0) then
        ave_CMB%iflag_CMB_average = 1
        ave_CMB%CMB_average_file_name                                   &
     &              = add_dat_extension(CMB_ave_file_prefix%charavalue)
      end if
!
      ave_CMB%flag_gzip_CMB_average = .FALSE.
      if(ave_CMB%iflag_CMB_average .le. 0) return
!
      if(CMB_ave_file_format%iflag .gt. 0) then
        input_flag = CMB_ave_file_format%charavalue
        if(check_mul_flags(input_flag, gzip_flags))                     &
     &                         ave_CMB%flag_gzip_CMB_average = .TRUE.
      end if
!
      end subroutine set_ctl_CMB_average_params
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_CMB_average_field_list                            &
     &         (my_rank, sph_params, sph_rj, ipol, pwr, ave_CMB)
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_address
      use t_rms_4_sph_spectr
!
      integer, intent(in) :: my_rank
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_mean_squares), intent(in) :: pwr
!
      type(CMB_average_data), intent(inout) :: ave_CMB
!
      integer(kind = kint) :: knum, num
!
!
      if(ave_CMB%iflag_CMB_average .le. 0) return
      num = count_CMB_average_data(ipol%base)
      call alloc_CMB_average_data(num, ave_CMB)
      if(ave_CMB%num_CMB_ave .le. 0) then
        ave_CMB%num_CMB_ave = 0
        ave_CMB%iflag_CMB_average = 0
        return
      end if
!
      if(sph_rj%idx_rj_degree_zero .gt. 0) then
        ave_CMB%irank_CMB_ave = my_rank
      end if
!
      call set_CMB_average_address(ipol%base, ave_CMB%num_CMB_ave,      &
     &    ave_CMB%CMB_ave_name, ave_CMB%imonitor_CMB)
!
      do knum = 1, pwr%nri_rms
        if(pwr%kr_4_rms(knum,1) .eq. sph_params%nlayer_CMB) then
          ave_CMB%kr_ave_CMB = knum
          ave_CMB%rave_CMB =   sph_params%radius_CMB
        end if
      end do
!
      end subroutine init_CMB_average_field_list
!
! -----------------------------------------------------------------------
!
      subroutine s_pick_CMB_average(my_rank, sph_rj, ipol,              &
     &                              rj_fld, ave_CMB)
!
      use t_spheric_rj_data
      use t_phys_data
!
      integer, intent(in) :: my_rank
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(CMB_average_data), intent(inout) :: ave_CMB
!
      integer(kind = kint) :: inod
!
!
      if(my_rank .ne. ave_CMB%irank_CMB_ave)   return
      if(ave_CMB%iflag_CMB_average .le. izero) return
      if(ave_CMB%num_CMB_ave .le. izero)       return
      if(sph_rj%idx_rj_degree_zero .le. izero) return
!
      inod = sph_rj%idx_rj_degree_zero                                  &
     &      + (ave_CMB%kr_ave_CMB-1) * sph_rj%nidx_rj(2)
!
      if(ave_CMB%imonitor_CMB%i_temp .gt. 0) then
        ave_CMB%ave_data(ave_CMB%imonitor_CMB%i_temp)                   &
     &    = rj_fld%d_fld(inod,ipol%base%i_temp)
      end if
      if(ave_CMB%imonitor_CMB%i_light .gt. 0) then
        ave_CMB%ave_data(ave_CMB%imonitor_CMB%i_light)                  &
     &    = rj_fld%d_fld(inod,ipol%base%i_light)
      end if
      if(ave_CMB%imonitor_CMB%i_entropy .gt. 0) then
        ave_CMB%ave_data(ave_CMB%imonitor_CMB%i_entropy)                &
     &    = rj_fld%d_fld(inod,ipol%base%i_entropy)
      end if
      if(ave_CMB%imonitor_CMB%i_density .gt. 0) then
        ave_CMB%ave_data(ave_CMB%imonitor_CMB%i_density)                &
     &    = rj_fld%d_fld(inod,ipol%base%i_density)
      end if
      if(ave_CMB%imonitor_CMB%i_press .gt. 0) then
        ave_CMB%ave_data(ave_CMB%imonitor_CMB%i_press)                  &
     &    = rj_fld%d_fld(inod,ipol%base%i_press)
      end if
!
      end subroutine s_pick_CMB_average
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function count_CMB_average_data(ipol_base)
!
      use t_base_field_labels
!
      type(base_field_address), intent(in) :: ipol_base
!
      integer(kind = kint) :: icou
!
!
      icou = 0
      if(ipol_base%i_temp .gt. 0)    icou = icou + 1
      if(ipol_base%i_light .gt. 0)   icou = icou + 1
      if(ipol_base%i_entropy .gt. 0) icou = icou + 1
      if(ipol_base%i_density .gt. 0) icou = icou + 1
      if(ipol_base%i_press .gt. 0)   icou = icou + 1
      count_CMB_average_data = icou
!
      end function count_CMB_average_data
!
! -----------------------------------------------------------------------
!
      subroutine set_CMB_average_address(ipol_base, num_CMB_ave,        &
     &                                   CMB_ave_name, imonitor_CMB)
!
      use t_base_field_labels
      use m_base_field_labels
!
      type(base_field_address), intent(in) :: ipol_base
      integer(kind = kint), intent(in) :: num_CMB_ave
!
      character(len = kchara), intent(inout)                            &
     &                        :: CMB_ave_name(num_CMB_ave)
      type(base_field_address), intent(inout) :: imonitor_CMB
!
      integer(kind = kint) :: icou
!
!
      icou = 0
      if(ipol_base%i_temp .gt. 0) then
        icou = icou + 1
        imonitor_CMB%i_temp = icou
        CMB_ave_name(icou) = temperature%name
      end if
      if(ipol_base%i_light .gt. 0) then
        icou = icou + 1
        imonitor_CMB%i_light = icou
        CMB_ave_name(icou) = composition%name
      end if
      if(ipol_base%i_entropy .gt. 0) then
        icou = icou + 1
        imonitor_CMB%i_entropy = icou
        CMB_ave_name(icou) = entropy%name
      end if
      if(ipol_base%i_density .gt. 0)  then
        icou = icou + 1
        imonitor_CMB%i_density = icou
        CMB_ave_name(icou) = density%name
      end if
      if(ipol_base%i_press .gt. 0) then
        icou = icou + 1
        imonitor_CMB%i_press = icou
        CMB_ave_name(icou) = pressure%name
      end if
!
      end subroutine set_CMB_average_address
!
! -----------------------------------------------------------------------
!
      end module pick_CMB_average
