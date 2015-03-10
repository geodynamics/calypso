!>@file   m_ctl_data_composite_norm.f90
!!@brief  module m_ctl_data_composite_norm
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!!@date Modified in July, 2013
!
!>@brief  composition equation parameters to read
!!
!!@verbatim
!!      subroutine deallocate_coef_4_dscalar_ctl
!!      subroutine deallocate_coef_4_dsc_diff_ctl
!!      subroutine deallocate_coef_4_dsc_src_ctl
!!
!!      subroutine read_composition_eq_ctl
!!
!!   --------------------------------------------------------------------
!! example of control block
!!
!!     begin composition
!!        array coef_4_composition_ctl     1
!!          coef_4_composition_ctl           One       1.0
!!        end array coef_4_composition_ctl
!!
!!        array coef_4_c_diffuse_ctl  1
!!          coef_4_c_diffuse_ctl    Composite_Prandtl_number  -1.0
!!        end array coef_4_c_diffuse_ctl
!!
!!        array coef_4_light_source_ctl  1
!!          coef_4_light_source_ctl     One                    1.0
!!        end array coef_4_light_source_ctl
!!      end  composition
!!
!!   --------------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_composite_norm
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
!
!>      Structure for number and power to construct composiitonal flux
!!@n      coef_4_comp_flux_ctl%c_tbl:  Name of number 
!!@n      coef_4_comp_flux_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_comp_flux_ctl
!
!>      Structure for number and power
!!            to construct composiitonal diffusion
!!@n      coef_4_c_diffuse_ctl%c_tbl:  Name of number 
!!@n      coef_4_c_diffuse_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_c_diffuse_ctl
!
!>      Structure for number and power to construct compositional source
!!@n      coef_4_comp_src_ctl%c_tbl:  Name of number 
!!@n      coef_4_comp_src_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_comp_src_ctl
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &        :: hd_dsc_diff_adv = 'composition'
      integer (kind=kint) :: i_dsc_diff_adv = 0
!
!   5th level for coefs for compositional scalar
!
      character(len=kchara), parameter                                  &
     &         :: hd_n_dscalar =  'coef_4_composition_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_n_dsc_diff = 'coef_4_c_diffuse_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_n_dsc_src =    'coef_4_light_source_ctl'
!
      private :: hd_dsc_diff_adv, i_dsc_diff_adv
      private :: hd_n_dscalar, hd_n_dsc_diff, hd_n_dsc_src
!
!   --------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_dscalar_ctl
!
      call dealloc_control_array_c_r(coef_4_comp_flux_ctl)
!
      end subroutine deallocate_coef_4_dscalar_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_dsc_diff_ctl
!
      call dealloc_control_array_c_r(coef_4_c_diffuse_ctl)
!
      end subroutine deallocate_coef_4_dsc_diff_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_dsc_src_ctl
!
      call dealloc_control_array_c_r(coef_4_comp_src_ctl)
!
      end subroutine deallocate_coef_4_dsc_src_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_composition_eq_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_dsc_diff_adv) .eq. 0) return
      if (i_dsc_diff_adv .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_dsc_diff_adv, i_dsc_diff_adv)
        if(i_dsc_diff_adv .gt. 0) exit
!
!
        call read_control_array_c_r(hd_n_dscalar, coef_4_comp_flux_ctl)
        call read_control_array_c_r(hd_n_dsc_diff, coef_4_c_diffuse_ctl)
        call read_control_array_c_r(hd_n_dsc_src, coef_4_comp_src_ctl)
      end do
!
      end subroutine read_composition_eq_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_composite_norm
