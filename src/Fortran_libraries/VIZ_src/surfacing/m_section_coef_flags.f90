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
!!      integer(kind = kint) function num_label_psf_coefs()
!!      integer(kind = kint) function num_label_psf_dirs()
!!      subroutine set_label_psf_coefs(names)
!!      subroutine set_label_psf_dirs(names)
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
!>      primary flag for square of X: 'X^2 '
      character(len=kchara), save :: cflag_x_sq
!>      primary flag for square of Y: 'Y^2 '
      character(len=kchara), save :: cflag_y_sq
!>      primary flag for square of Z: 'Z^2 '
      character(len=kchara), save :: cflag_z_sq
!
!>      primary flag for xy: 'XY'
      character(len=kchara), save :: cflag_xy
!>      primary flag for yz: 'YZ'
      character(len=kchara), save :: cflag_yz
!>      primary flag for zx: 'ZX'
      character(len=kchara), save :: cflag_zx
!
!>      primary flag for x: 'X'
      character(len=kchara), save :: cflag_x
!>      primary flag for y: 'Y'
      character(len=kchara), save :: cflag_y
!>      primary flag for z: 'Z'
      character(len=kchara), save :: cflag_z
!
!>      primary flag for Constant: 'Const'
      character(len=kchara), save :: cflag_const
!
      integer(kind = kint), parameter :: n_label_psf_coefs = 10
      integer(kind = kint), parameter :: n_label_psf_dirs =   3
!
      private :: x_sq_name, y_sq_name, z_sq_name
      private :: xy_name, yz_name, zx_name
      private :: x_name, y_name, z_name, c_name
!
      private :: init_section_coef_flags, dealloc_section_coef_flags
      private :: set_each_coef_4_psf, set_each_param_2_vector
      private :: init_primary_section_coef_flag
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
      call init_primary_section_coef_flag()
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
      call init_primary_section_coef_flag()
!
      if     (check_mul_flags(dir_ctl, x_sq_labels)) then
        prim_name = cflag_x_sq
      else if(check_mul_flags(dir_ctl, y_sq_labels)) then
        prim_name = cflag_y_sq
      else if(check_mul_flags(dir_ctl, z_sq_labels)) then
        prim_name = cflag_z_sq
      else if(check_mul_flags(dir_ctl, xy_labels)) then
        prim_name = cflag_xy
      else if(check_mul_flags(dir_ctl, yz_labels)) then
        prim_name = cflag_yz
      else if(check_mul_flags(dir_ctl, zx_labels)) then
        prim_name = cflag_zx
      else if(check_mul_flags(dir_ctl, x_labels)) then
        prim_name = cflag_x
      else if(check_mul_flags(dir_ctl, y_labels)) then
        prim_name = cflag_y
      else if(check_mul_flags(dir_ctl, z_labels)) then
        prim_name = cflag_z
      else if(check_mul_flags(dir_ctl, c_labels)) then
        prim_name = cflag_const
      end if
      dir_ctl = trim(prim_name) // char(0)
!
      call dealloc_section_coef_flags()
!
      end subroutine set_primary_section_coef_flag
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_psf_coefs()
      num_label_psf_coefs = n_label_psf_coefs
      return
      end function num_label_psf_coefs
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_label_psf_dirs()
      num_label_psf_dirs = n_label_psf_dirs
      return
      end function num_label_psf_dirs
!
! ----------------------------------------------------------------------
!
      subroutine set_label_psf_coefs(names)
!
      use t_read_control_elements
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_psf_coefs)
!
!
      call init_primary_section_coef_flag()
      call set_control_labels(cflag_x_sq,  names( 1))
      call set_control_labels(cflag_y_sq,  names( 2))
      call set_control_labels(cflag_z_sq,  names( 3))
      call set_control_labels(cflag_xy,    names( 4))
      call set_control_labels(cflag_yz,    names( 5))
      call set_control_labels(cflag_zx,    names( 6))
      call set_control_labels(cflag_x,     names( 7))
      call set_control_labels(cflag_y,     names( 8))
      call set_control_labels(cflag_z,     names( 9))
      call set_control_labels(cflag_const, names(10))
!
      end subroutine set_label_psf_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_psf_dirs(names)
!
      use t_read_control_elements
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_psf_dirs)
!
!
      call init_primary_section_coef_flag()
      call set_control_labels(cflag_x,     names( 1))
      call set_control_labels(cflag_y,     names( 2))
      call set_control_labels(cflag_z,     names( 3))
!
      end subroutine set_label_psf_dirs
!
!  ---------------------------------------------------------------------
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
      if      (cmp_no_case(ctl_name, cflag_x)) then
        vector(1) = vect_ctl
      else if (cmp_no_case(ctl_name, cflag_y)) then
        vector(2) = vect_ctl
      else if (cmp_no_case(ctl_name, cflag_z)) then
        vector(3) = vect_ctl
      end if
!
      end subroutine set_each_param_2_vector
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_primary_section_coef_flag
!
!
      cflag_x_sq =  x_sq_name(3)
      cflag_y_sq =  y_sq_name(3)
      cflag_z_sq =  z_sq_name(3)
      cflag_xy =    xy_name(1)
      cflag_yz =    yz_name(1)
      cflag_zx =    zx_name(1)
      cflag_x =     x_name(1)
      cflag_y =     y_name(1)
      cflag_z =     z_name(1)
      cflag_const = c_name(1)
!
      end subroutine init_primary_section_coef_flag
!
!  ---------------------------------------------------------------------
!
      end module m_section_coef_flags
