!>@file   m_more_component_flags.f90
!!        module m_more_component_flags
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief More flags of components in control
!!
!!@verbatim
!!      subroutine set_primary_componnet_flag(input_flag)
!!
!!      subroutine init_more_componnet_flags()
!!      subroutine init_more_componnet_flags()
!!@endverbatim
!
      module m_more_component_flags
!
      use m_precision
      use m_phys_constants
      use t_field_labels
      use t_multi_flag_labels
!
      implicit  none
!!
      character(len=kchara), parameter :: mag_flags(3)                  &
     &     = (/'amplitude ',  'magnitude ', 'norm      ' /)
!
      character(len=kchara), parameter :: r_flags(2)                    &
     &     = (/'r      ',     'radial '/)
      character(len=kchara), parameter :: t_flags(2)                    &
     &     = (/'theta     ',  'elevation '/)
      character(len=kchara), parameter :: p_flags(2)                    &
     &     = (/'phi     ',    'azimuth '/)
      character(len=kchara), parameter :: s_flags(2)                    &
     &     = (/'s          ', 'cylinder_r '/)
!
      character(len=kchara), parameter :: mag_sym_tsr_flags(3)          &
     &     = (/'magnitude       ',  'norm_sym_tensor ',                 &
     &         'mag_sym_tensor  '/)
      character(len=kchara), parameter :: mag_asym_tsr_flags(3)         &
     &     = (/'magnitude        ',  'norm_asym_tensor ',               &
     &         'mag_asym_tensor  '/)
!
!>      flags for amplitude of vector
!!        'amplitude ',  'magnitude ', 'norm     ' 
      type(multi_flag_labels), save :: magnitude_flags
!
!>      flags for radial component
!!        'r      ',     'radial ' 
      type(multi_flag_labels), save :: radial_flags
!>      flags for theta component
!!        'theta     ',  'elevation '
      type(multi_flag_labels), save :: theta_flags
!>      flags for phi component
!!        'phi     ',    'azimuth '
      type(multi_flag_labels), save :: phi_flags
!>      flags for cytrindrical radius component
!!        's          ', 'cylinder_r '
      type(multi_flag_labels), save :: cyrindrical_r_flags
!
!>      flags for amplitude of symmetric tensor
!!        'magnitude       ',  'norm_sym_tensor ',  'mag_sym_tensor  '
      type(multi_flag_labels), save :: sym_tensor_magnitude_flags
!>      flags for amplitude of asymmetric tensor
!!        'magnitude        ',  'norm_asym_tensor ', 'mag_asym_tensor  '
      type(multi_flag_labels), save :: asym_tensor_magnitude_flags
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_primary_componnet_flag(input_flag)
!
      use m_component_flags
!
      character(len=kchara), intent(inout) :: input_flag
!
      character(len=kchara) :: primary_flag
!
!
      call init_more_componnet_flags()
      if     (check_mul_flags(input_flag, magnitude_flags)) then
        primary_flag = magnitude%name
!
      else if(check_mul_flags(input_flag, radial_flags)) then
        primary_flag = V_r%name
      else if(check_mul_flags(input_flag, theta_flags)) then
        primary_flag = V_theta%name
      else if(check_mul_flags(input_flag, phi_flags)) then
        primary_flag = V_phi%name
      else if(check_mul_flags(input_flag, cyrindrical_r_flags)) then
        primary_flag = V_s%name
!
      else if(check_mul_flags(input_flag, sym_tensor_magnitude_flags))  &
     &    then
        primary_flag = magnitude%name
      else if(check_mul_flags(input_flag, asym_tensor_magnitude_flags)) &
     &    then
        primary_flag = magnitude%name
      end if
      input_flag = trim(primary_flag) // char(0)
      call dealloc_more_componnet_flags
!
      end subroutine set_primary_componnet_flag
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_more_componnet_flags()
!
      call init_multi_flags_by_labels                                   &
     &   (ithree, mag_flags, magnitude_flags)
!
      call init_multi_flags_by_labels                                   &
     &   (itwo, r_flags,     radial_flags)
      call init_multi_flags_by_labels                                   &
     &   (itwo, t_flags,     theta_flags)
      call init_multi_flags_by_labels                                   &
     &   (itwo, p_flags,     phi_flags)
      call init_multi_flags_by_labels                                   &
     &   (itwo, s_flags,     cyrindrical_r_flags)
!
      call init_multi_flags_by_labels                                   &
     &   (ithree, mag_sym_tsr_flags,  sym_tensor_magnitude_flags)
      call init_multi_flags_by_labels                                   &
     &   (ithree, mag_asym_tsr_flags, asym_tensor_magnitude_flags)
!
      end subroutine init_more_componnet_flags
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_more_componnet_flags()
!
      call dealloc_multi_flags(magnitude_flags)
!
      call dealloc_multi_flags(radial_flags)
      call dealloc_multi_flags(theta_flags)
      call dealloc_multi_flags(phi_flags)
      call dealloc_multi_flags(cyrindrical_r_flags)
!
      call dealloc_multi_flags(sym_tensor_magnitude_flags)
      call dealloc_multi_flags(asym_tensor_magnitude_flags)
!
      end subroutine dealloc_more_componnet_flags
!
! ----------------------------------------------------------------------
!
      end module m_more_component_flags
