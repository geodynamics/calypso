!>@file   m_section_coef_flags.f90
!!@brief  module m_section_coef_flags
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  Set coefficients for cross section function
!!
!!@verbatim
!!      subroutine set_coefs_4_psf                                      &
!!     &         (num_const, c_name_psf, const_psf_ctl, c_surf)
!!        integer(kind = kint), intent(in) :: num_const
!!        character(len=kchara), intent(in) :: c_name_psf(num_const)
!!        real(kind = kreal), intent(in) :: const_psf_ctl(num_const)
!!        real(kind = kreal), intent(inout) :: c_surf(10)
!!      subroutine set_parameter_2_vectors                              &
!!     &         (num_vect, ctl_name, vect_ctl, vector)
!!        integer(kind = kint), intent(in) :: num_vect
!!        character(len=kchara), intent(in) :: ctl_name(num_vect)
!!        real(kind = kreal), intent(in) :: vect_ctl(num_vect)
!!        real(kind = kreal), intent(inout) :: vector(3)
!!      subroutine set_primary_section_coef_flag(dir_ctl)
!!        character(len=kchara), intent(inout) :: dir_ctl
!!
!!      subroutine psf_coef_label_array(array_c)
!!      subroutine psf_dirs_label_array(array_c)
!!      subroutine iso_type_label_array(array_c)
!!      subroutine psf_def_type_label_array(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
!!@endverbatim
!
      module m_section_coef_flags
!
      use m_precision
      use m_constants
      use t_multi_flag_labels
!
      implicit  none
!
      character(len=kchara), parameter                                  &
     &        :: x_sq_name(3) = (/'XX  ', 'X2  ', 'X^2 '/)
      character(len=kchara), parameter                                  &
     &        :: y_sq_name(3) = (/'YY  ', 'Y2  ', 'Y^2 '/)
      character(len=kchara), parameter                                  &
     &        :: z_sq_name(3) = (/'ZZ  ', 'Z2  ', 'Z^2 '/)
!
      character(len=kchara), parameter                                  &
     &        :: xy_name(2) = (/'XY  ', 'YX  '/)
      character(len=kchara), parameter                                  &
     &        :: yz_name(2) = (/'YZ  ', 'ZY  '/)
      character(len=kchara), parameter                                  &
     &        :: zx_name(2) = (/'ZX  ', 'XZ  '/)
!
      character(len=kchara), parameter                                  &
     &        :: x_name(3) = (/'X   ', 'X1  ', 'X^1 '/)
      character(len=kchara), parameter                                  &
     &        :: y_name(3) = (/'Y   ', 'Y1  ', 'Y^1 '/)
      character(len=kchara), parameter                                  &
     &        :: z_name(3) = (/'Z   ', 'Z1  ', 'Z^1 '/)
!
      character(len=kchara), parameter                                  &
     &        :: c_name(2) = (/'Const ', 'C     '/)
!
!>      flags for square of X: 'XX', 'X2', 'X^2 '
      type(multi_flag_labels), save :: x_sq_labels
!>      flags for square of Y: 'YY', 'Y2', 'Y^2 '
      type(multi_flag_labels), save :: y_sq_labels
!>      flags for square of Z: 'ZZ', 'Z2', 'Z^2 '
      type(multi_flag_labels), save :: z_sq_labels
!
!>      flags for xy: 'XY', 'YX'
      type(multi_flag_labels), save :: xy_labels
!>      flags for yz: 'YZ', 'ZY'
      type(multi_flag_labels), save :: yz_labels
!>      flags for zx: 'ZX', 'XZ'
      type(multi_flag_labels), save :: zx_labels
!
!>      flags for x: 'X', 'X1', 'X^1'
      type(multi_flag_labels), save :: x_labels
!>      flags for y: 'Y', 'Y1', 'Y^1'
      type(multi_flag_labels), save :: y_labels
!>      flags for z: 'Z', 'Z1', 'z^1'
      type(multi_flag_labels), save :: z_labels
!
!>      flags for Constant: 'Const', 'C'
      type(multi_flag_labels), save :: c_labels
!
!
      character(len = kchara), parameter :: cflag_eq =  'equation'
      character(len = kchara), parameter :: cflag_pln = 'plane'
      character(len = kchara), parameter :: cflag_sph = 'sphere'
      character(len = kchara), parameter :: cflag_elp = 'ellipsoid'
      character(len = kchara), parameter :: cflag_hyp = 'hyperboloid'
      character(len = kchara), parameter :: cflag_prb = 'paraboloid'
      character(len = kchara), parameter :: cflag_grp = 'group'
!
!
      character(len=kchara), parameter :: cflag_const_iso = 'constant'
      character(len=kchara), parameter :: cflag_field_iso = 'field'
!
!
      private :: x_sq_name, y_sq_name, z_sq_name
      private :: xy_name, yz_name, zx_name
      private :: x_name, y_name, z_name, c_name
!
      private :: init_section_coef_flags, dealloc_section_coef_flags
      private :: set_each_coef_4_psf, set_each_param_2_vector
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_psf                                        &
     &         (num_const, c_name_psf, const_psf_ctl, c_surf)
!
      integer(kind = kint), intent(in) :: num_const
      character(len=kchara), intent(in) :: c_name_psf(num_const)
      real(kind = kreal), intent(in) :: const_psf_ctl(num_const)
!
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      integer(kind = kint) :: i
!
!
      call init_section_coef_flags()
      do i = 1, num_const
        call set_each_coef_4_psf                                        &
     &     (c_name_psf(i), const_psf_ctl(i), c_surf)
      end do
      call dealloc_section_coef_flags()
!
      end subroutine set_coefs_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_parameter_2_vectors                                &
     &         (num_vect, ctl_name, vect_ctl, vector)
!
      integer(kind = kint), intent(in) :: num_vect
      character(len=kchara), intent(in) :: ctl_name(num_vect)
      real(kind = kreal), intent(in) :: vect_ctl(num_vect)
!
      real(kind = kreal), intent(inout) :: vector(3)
!
      integer(kind = kint) :: i
!
!
      vector(1:3) = zero
      do i = 1, num_vect
        call set_each_param_2_vector(ctl_name(i), vect_ctl(i), vector)
      end do
!
      end subroutine set_parameter_2_vectors
!
!  ---------------------------------------------------------------------
!
      subroutine set_primary_section_coef_flag(dir_ctl)
!
      character(len=kchara), intent(inout) :: dir_ctl
!
      character(len=kchara) :: prim_name
!
!
      call init_section_coef_flags()
!
      if     (check_mul_flags(dir_ctl, x_sq_labels)) then
        prim_name = x_sq_name(3)
      else if(check_mul_flags(dir_ctl, y_sq_labels)) then
        prim_name = y_sq_name(3)
      else if(check_mul_flags(dir_ctl, z_sq_labels)) then
        prim_name = z_sq_name(3)
      else if(check_mul_flags(dir_ctl, xy_labels)) then
        prim_name = xy_name(1)
      else if(check_mul_flags(dir_ctl, yz_labels)) then
        prim_name = yz_name(1)
      else if(check_mul_flags(dir_ctl, zx_labels)) then
        prim_name = zx_name(1)
      else if(check_mul_flags(dir_ctl, x_labels)) then
        prim_name = x_name(1)
      else if(check_mul_flags(dir_ctl, y_labels)) then
        prim_name = y_name(1)
      else if(check_mul_flags(dir_ctl, z_labels)) then
        prim_name = z_name(1)
      else if(check_mul_flags(dir_ctl, c_labels)) then
        prim_name = c_name(1)
      end if
      dir_ctl = trim(prim_name) // char(0)
!
      call dealloc_section_coef_flags()
!
      end subroutine set_primary_section_coef_flag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine psf_coef_label_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      character(len=kchara) :: tmpchara
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      tmpchara = x_sq_name(3)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = y_sq_name(3)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = z_sq_name(3)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = xy_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = yz_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = zx_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = x_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = y_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = z_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = c_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
!
      end subroutine psf_coef_label_array
!
! ----------------------------------------------------------------------
!
      subroutine psf_dirs_label_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      character(len=kchara) :: tmpchara
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      tmpchara = x_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = y_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = z_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
!
      end subroutine psf_dirs_label_array
!
! ----------------------------------------------------------------------
!
      subroutine iso_type_label_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(cflag_const_iso, array_c)
      call append_c_to_ctl_array(cflag_field_iso, array_c)
!
      end subroutine iso_type_label_array
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_section_coef_flags()
!
!
      call init_multi_flags_by_labels(ithree, x_sq_name, x_sq_labels)
      call init_multi_flags_by_labels(ithree, y_sq_name, y_sq_labels)
      call init_multi_flags_by_labels(ithree, z_sq_name, z_sq_labels)
      call init_multi_flags_by_labels(itwo,   xy_name, xy_labels)
      call init_multi_flags_by_labels(itwo,   yz_name, yz_labels)
      call init_multi_flags_by_labels(itwo,   zx_name, zx_labels)
      call init_multi_flags_by_labels(ithree, x_name,  x_labels)
      call init_multi_flags_by_labels(ithree, y_name,  y_labels)
      call init_multi_flags_by_labels(ithree, z_name,  z_labels)
      call init_multi_flags_by_labels(itwo,   c_name,  c_labels)
!
      end subroutine init_section_coef_flags
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_section_coef_flags()
!
!
      call dealloc_multi_flags(x_sq_labels)
      call dealloc_multi_flags(y_sq_labels)
      call dealloc_multi_flags(z_sq_labels)
      call dealloc_multi_flags(xy_labels)
      call dealloc_multi_flags(yz_labels)
      call dealloc_multi_flags(zx_labels)
      call dealloc_multi_flags(x_labels)
      call dealloc_multi_flags(y_labels)
      call dealloc_multi_flags(z_labels)
      call dealloc_multi_flags(c_labels)
!
      end subroutine dealloc_section_coef_flags
!
!  ---------------------------------------------------------------------
!
      subroutine psf_def_type_label_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(cflag_eq,  array_c)
      call append_c_to_ctl_array(cflag_pln, array_c)
      call append_c_to_ctl_array(cflag_sph, array_c)
      call append_c_to_ctl_array(cflag_elp, array_c)
      call append_c_to_ctl_array(cflag_hyp, array_c)
      call append_c_to_ctl_array(cflag_prb, array_c)
      call append_c_to_ctl_array(cflag_grp, array_c)
!
      end subroutine psf_def_type_label_array
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_each_coef_4_psf(dir_ctl, coef_ctl, c_surf)
!
      character(len=kchara), intent(in) :: dir_ctl
      real(kind = kreal), intent(in) :: coef_ctl
!
      real(kind = kreal), intent(inout) :: c_surf(10)
!
!
      if     (check_mul_flags(dir_ctl, x_sq_labels)) then
        c_surf( 1) = coef_ctl
      else if(check_mul_flags(dir_ctl, y_sq_labels)) then
        c_surf( 2) = coef_ctl
      else if(check_mul_flags(dir_ctl, z_sq_labels)) then
        c_surf( 3) = coef_ctl
      else if(check_mul_flags(dir_ctl, xy_labels)) then
        c_surf( 4) = coef_ctl
      else if(check_mul_flags(dir_ctl, yz_labels)) then
        c_surf( 5) = coef_ctl
      else if(check_mul_flags(dir_ctl, zx_labels)) then
        c_surf( 6) = coef_ctl
      else if(check_mul_flags(dir_ctl, x_labels)) then
        c_surf( 7) = coef_ctl
      else if(check_mul_flags(dir_ctl, y_labels)) then
        c_surf( 8) = coef_ctl
      else if(check_mul_flags(dir_ctl, z_labels)) then
        c_surf( 9) = coef_ctl
      else if(check_mul_flags(dir_ctl, c_labels)) then
        c_surf(10) = coef_ctl
      end if
!
      end subroutine set_each_coef_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_each_param_2_vector(ctl_name, vect_ctl, vector)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: ctl_name
      real(kind = kreal), intent(in) :: vect_ctl
!
      real(kind = kreal), intent(inout) :: vector(3)
!
!
      if      (cmp_no_case(ctl_name, x_name(1))) then
        vector(1) = vect_ctl
      else if (cmp_no_case(ctl_name, y_name(1))) then
        vector(2) = vect_ctl
      else if (cmp_no_case(ctl_name, z_name(1))) then
        vector(3) = vect_ctl
      end if
!
      end subroutine set_each_param_2_vector
!
!  ---------------------------------------------------------------------
!
      end module m_section_coef_flags
