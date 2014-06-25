!>@file   m_ctl_data_termal_norm.f90
!!@brief  module m_ctl_data_termal_norm
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!!@date Modified in July, 2013
!
!>@brief  Thermal equation parameters to read
!!
!!@verbatim
!!      subroutine deallocate_coef_4_termal_ctl
!!      subroutine deallocate_coef_4_t_diffuse_ctl
!!      subroutine deallocate_coef_4_h_source_ctl
!!
!!      subroutine read_thermal_ctl
!!
!!   --------------------------------------------------------------------
!! example of control block
!!
!!      begin thermal
!!        array coef_4_termal_ctl     1
!!          coef_4_termal_ctl            One                   1.0
!!        end array coef_4_termal_ctl
!!
!!        array coef_4_t_diffuse_ctl  1
!!          coef_4_t_diffuse_ctl         Prandtl_number       -1.0
!!        end array coef_4_t_diffuse_ctl
!!
!!        array coef_4_heat_source_ctl  1
!!          coef_4_heat_source_ctl       One                   1.0
!!        end array coef_4_heat_source_ctl
!!      end  thermal
!!
!!   --------------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_termal_norm
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
!
!>      Structure for number and power to construct heat flux
!!@n      coef_4_heat_flux_ctl%c_tbl:  Name of number 
!!@n      coef_4_heat_flux_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_heat_flux_ctl
!
!>      Structure for number and power to construct thermal diffusion
!!@n      coef_4_t_diffuse_ctl%c_tbl:  Name of number 
!!@n      coef_4_t_diffuse_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_t_diffuse_ctl
!
!>      Structure for number and power to construct heat source
!!@n      coef_4_heat_src_ctl%c_tbl:  Name of number 
!!@n      coef_4_heat_src_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_heat_src_ctl
!
!   entry label
!
      character(len=kchara), parameter :: hd_thermal = 'thermal'
      integer (kind=kint) :: i_thermal = 0
!
!   5th level for coefs for thermal
!
      character(len=kchara) :: hd_n_thermal = 'coef_4_termal_ctl'
      character(len=kchara) :: hd_n_t_diff =  'coef_4_t_diffuse_ctl'
      character(len=kchara) :: hd_n_h_src =  'coef_4_heat_source_ctl'
!
      private :: hd_thermal, i_thermal
      private :: hd_n_thermal, hd_n_t_diff, hd_n_h_src
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_coef_4_termal_ctl
!
      call dealloc_control_array_c_r(coef_4_heat_flux_ctl)
!
      end subroutine deallocate_coef_4_termal_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_t_diffuse_ctl
!
      call dealloc_control_array_c_r(coef_4_t_diffuse_ctl)
!
      end subroutine deallocate_coef_4_t_diffuse_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_h_source_ctl
!
      call dealloc_control_array_c_r(coef_4_heat_src_ctl)
!
      end subroutine deallocate_coef_4_h_source_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_thermal_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_thermal) .eq. 0) return
      if (i_thermal .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_thermal, i_thermal)
        if(i_thermal .gt. 0) exit
!
!
        call read_control_array_c_r(hd_n_thermal, coef_4_heat_flux_ctl)
        call read_control_array_c_r(hd_n_t_diff, coef_4_t_diffuse_ctl)
        call read_control_array_c_r(hd_n_h_src, coef_4_heat_src_ctl)
      end do
!
      end subroutine read_thermal_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_termal_norm
