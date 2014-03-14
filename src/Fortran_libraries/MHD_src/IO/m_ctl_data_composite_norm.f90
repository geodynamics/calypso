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
!
      implicit  none
!
!
      integer(kind=kint) :: num_coef_4_c_diffuse_ctl =  0
      character(len=kchara),allocatable :: coef_4_c_diff_name_ctl(:)
      real (kind = kreal), allocatable :: coef_4_c_diff_power_ctl(:)
!
      integer(kind=kint) :: num_coef_4_composit_ctl =   0
      character(len=kchara),allocatable :: coef_4_composit_name_ctl(:)
      real (kind = kreal), allocatable :: coef_4_composit_power_ctl(:)
!
      integer(kind=kint) :: num_coef_4_c_src_ctl =   0
      character(len=kchara),allocatable :: coef_4_c_src_name_ctl(:)
      real (kind = kreal), allocatable :: coef_4_c_src_power_ctl(:)
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
      integer (kind=kint) :: i_n_dscalar =     0
      integer (kind=kint) :: i_n_dsc_diff =    0
      integer (kind=kint) :: i_n_dsc_src =     0
!
      private :: hd_dsc_diff_adv, i_dsc_diff_adv
      private :: hd_n_dscalar, hd_n_dsc_diff, hd_n_dsc_src
!
      private :: allocate_coef_4_dscalar_ctl
      private :: allocate_coef_4_dsc_diff_ctl
      private :: allocate_coef_4_dsc_src_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_dscalar_ctl
!
      allocate(coef_4_composit_name_ctl(num_coef_4_composit_ctl))
      allocate(coef_4_composit_power_ctl(num_coef_4_composit_ctl))
      coef_4_composit_power_ctl = 0.0d0
!
      end subroutine allocate_coef_4_dscalar_ctl
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_dsc_diff_ctl
!
      allocate(coef_4_c_diff_name_ctl(num_coef_4_c_diffuse_ctl))
      allocate(coef_4_c_diff_power_ctl(num_coef_4_c_diffuse_ctl))
      coef_4_c_diff_power_ctl = 0.0d0
!
      end subroutine allocate_coef_4_dsc_diff_ctl
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_dsc_src_ctl
!
      allocate(coef_4_c_src_name_ctl(num_coef_4_c_src_ctl))
      allocate(coef_4_c_src_power_ctl(num_coef_4_c_src_ctl))
      coef_4_c_src_power_ctl = 0.0d0
!
      end subroutine allocate_coef_4_dsc_src_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_dscalar_ctl
!
      deallocate(coef_4_composit_name_ctl, coef_4_composit_power_ctl)
!
      end subroutine deallocate_coef_4_dscalar_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_dsc_diff_ctl
!
      deallocate(coef_4_c_diff_name_ctl, coef_4_c_diff_power_ctl)
!
      end subroutine deallocate_coef_4_dsc_diff_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_dsc_src_ctl
!
      deallocate(coef_4_c_src_name_ctl, coef_4_c_src_power_ctl)
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
        call find_control_array_flag(hd_n_dscalar,                      &
     &      num_coef_4_composit_ctl)
        if(num_coef_4_composit_ctl.gt.0 .and. i_n_dscalar.eq.0) then
          call allocate_coef_4_dscalar_ctl
          call read_control_array_vect_list(hd_n_dscalar,               &
     &        num_coef_4_composit_ctl, i_n_dscalar,                     &
     &        coef_4_composit_name_ctl, coef_4_composit_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_dsc_diff,                     &
     &      num_coef_4_c_diffuse_ctl)
        if(num_coef_4_c_diffuse_ctl.gt.0 .and. i_n_dsc_diff.eq.0) then
          call allocate_coef_4_dsc_diff_ctl
          call read_control_array_vect_list(hd_n_dsc_diff,              &
     &        num_coef_4_c_diffuse_ctl, i_n_dsc_diff,                   &
     &        coef_4_c_diff_name_ctl, coef_4_c_diff_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_dsc_src,                      &
     &      num_coef_4_c_src_ctl)
        if(num_coef_4_c_src_ctl.gt.0 .and. i_n_dsc_src.eq.0) then
          call allocate_coef_4_dsc_src_ctl
          call read_control_array_vect_list(hd_n_dsc_src,               &
     &        num_coef_4_c_src_ctl, i_n_dsc_src,                        &
     &        coef_4_c_src_name_ctl, coef_4_c_src_power_ctl)
        end if
      end do
!
      end subroutine read_composition_eq_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_composite_norm
