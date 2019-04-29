!>@file   set_cross_section_coefs.f90
!!@brief  module set_cross_section_coefs
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  Set coefficients for cross section function
!!
!!@verbatim
!!      subroutine set_coefs_4_psf(num_const, c_name_psf, const_psf_ctl,&
!!                c_surf)
!!      subroutine set_coefs_4_plane(psf_c, c_surf)
!!      subroutine set_coefs_4_sphere(psf_c, c_surf)
!!      subroutine set_coefs_4_ellipsode(psf_c, c_surf)
!!      subroutine set_coefs_4_hyperboloide(psf_c, c_surf)
!!      subroutine set_coefs_4_parabolic(psf_c, c_surf)
!!@endverbatim
!
      module set_cross_section_coefs
!
      use m_precision
!
      use m_constants
      use t_control_data_4_psf
!
      implicit  none
!
      private :: set_parameter_2_vectors
!
      character(len=kchara), parameter :: x_sq1 = 'XX'
      character(len=kchara), parameter :: x_sq2 = 'X2'
      character(len=kchara), parameter :: x_sq3 = 'X^2'
!
      character(len=kchara), parameter :: y_sq1 = 'YY'
      character(len=kchara), parameter :: y_sq2 = 'Y2'
      character(len=kchara), parameter :: y_sq3 = 'Y^2'
!
      character(len=kchara), parameter :: z_sq1 = 'ZZ'
      character(len=kchara), parameter :: z_sq2 = 'Z2'
      character(len=kchara), parameter :: z_sq3 = 'Z^2'
!
      character(len=kchara), parameter :: xy_1 = 'XY'
      character(len=kchara), parameter :: xy_2 = 'YX'
!
      character(len=kchara), parameter :: yz_1 = 'YZ'
      character(len=kchara), parameter :: yz_2 = 'ZY'
!
      character(len=kchara), parameter :: zx_1 = 'ZX'
      character(len=kchara), parameter :: zx_2 = 'XZ'
!
      character(len=kchara), parameter :: x_ln1 = 'X'
      character(len=kchara), parameter :: x_ln2 = 'X1'
      character(len=kchara), parameter :: x_ln3 = 'X^1'
!
      character(len=kchara), parameter :: y_ln1 = 'Y'
      character(len=kchara), parameter :: y_ln2 = 'Y1'
      character(len=kchara), parameter :: y_ln3 = 'Y^1'
!
      character(len=kchara), parameter :: z_ln1 = 'Z'
      character(len=kchara), parameter :: z_ln2 = 'Z1'
      character(len=kchara), parameter :: z_ln3 = 'Z^1'
!
      character(len=kchara), parameter :: const1 = 'Const'
      character(len=kchara), parameter :: const2 = 'C'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_psf(num_const, c_name_psf, const_psf_ctl,  &
     &          c_surf)
!
      use skip_comment_f
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
      do i = 1, num_const
        if(      cmp_no_case(c_name_psf(i), x_sq1)                      &
     &      .or. cmp_no_case(c_name_psf(i), x_sq2)                      &
     &      .or. cmp_no_case(c_name_psf(i), x_sq3)) then
          c_surf(1) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), y_sq1)                      &
     &      .or. cmp_no_case(c_name_psf(i), y_sq2)                      &
     &      .or. cmp_no_case(c_name_psf(i), y_sq3)) then
          c_surf(2) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), z_sq1)                      &
     &      .or. cmp_no_case(c_name_psf(i), z_sq2)                      &
     &      .or. cmp_no_case(c_name_psf(i), z_sq3)) then
          c_surf(3) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), xy_1)                       &
     &      .or. cmp_no_case(c_name_psf(i), xy_2)) then
          c_surf(4) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), yz_1)                       &
     &      .or. cmp_no_case(c_name_psf(i), yz_2)) then
          c_surf(5) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), zx_1)                       &
     &      .or. cmp_no_case(c_name_psf(i), zx_2)) then
          c_surf(6) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), x_ln1)                      &
     &      .or. cmp_no_case(c_name_psf(i), x_ln2)                      &
     &      .or. cmp_no_case(c_name_psf(i), x_ln3)) then
          c_surf(7) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), y_ln1)                      &
     &      .or. cmp_no_case(c_name_psf(i), y_ln2)                      &
     &      .or. cmp_no_case(c_name_psf(i), y_ln3)) then
          c_surf(8) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), z_ln1)                      &
     &      .or. cmp_no_case(c_name_psf(i), z_ln2)                      &
     &      .or. cmp_no_case(c_name_psf(i), z_ln3)) then
          c_surf(9) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), const1)                     &
     &      .or. cmp_no_case(c_name_psf(i), const2)) then
          c_surf(10) = const_psf_ctl(i)
        end if
      end do
!
      end subroutine set_coefs_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_plane(psf_c, c_surf)
!
      type(psf_ctl), intent(in) :: psf_c
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf_c%psf_normal_ctl%num,            &
     &    psf_c%psf_normal_ctl%c_tbl, psf_c%psf_normal_ctl%vect, axc)
      call set_parameter_2_vectors(psf_c%psf_center_ctl%num,            &
     &    psf_c%psf_center_ctl%c_tbl, psf_c%psf_center_ctl%vect, xc)
!
      c_surf( 1) =  zero
      c_surf( 2) =  zero
      c_surf( 3) =  zero
      c_surf( 4) =  zero
      c_surf( 5) =  zero
      c_surf( 6) =  zero
      c_surf( 7) =  axc(1)
      c_surf( 8) =  axc(2)
      c_surf( 9) =  axc(3)
      c_surf(10) = -axc(1)*xc(1) - axc(2)*xc(2) - axc(3)*xc(3)
!
      end subroutine set_coefs_4_plane
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_sphere(psf_c, c_surf)
!
      type(psf_ctl), intent(in) :: psf_c
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), r
!
!
      call set_parameter_2_vectors(psf_c%psf_center_ctl%num,            &
     &    psf_c%psf_center_ctl%c_tbl, psf_c%psf_center_ctl%vect, xc)
!
      r = psf_c%radius_psf_ctl%realvalue
!
      c_surf( 1) =  one
      c_surf( 2) =  one
      c_surf( 3) =  one
      c_surf( 4) = zero
      c_surf( 5) = zero
      c_surf( 6) = zero
      c_surf( 7) = -two*xc(1)
      c_surf( 8) = -two*xc(2)
      c_surf( 9) = -two*xc(3)
      c_surf(10) = xc(1)*xc(1) + xc(2)*xc(2) + xc(3)*xc(3) - (r*r)
!
      end subroutine set_coefs_4_sphere
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_ellipsode(psf_c, c_surf)
!
      type(psf_ctl), intent(in) :: psf_c
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf_c%psf_center_ctl%num,            &
     &    psf_c%psf_center_ctl%c_tbl, psf_c%psf_center_ctl%vect, xc)
      call set_parameter_2_vectors(psf_c%psf_axis_ctl%num,              &
     &    psf_c%psf_axis_ctl%c_tbl, psf_c%psf_axis_ctl%vect, axc)
!
      c_surf(1:9) = zero
      c_surf(10) =  -one
!
      if(axc(1) .gt. zero) then
        c_surf( 1) =  one / (axc(1)*axc(1))
        c_surf( 7) = -two*xc(1) / (axc(1)*axc(1))
        c_surf(10) =  c_surf(10) + xc(1)*xc(1) / (axc(1)*axc(1))
      end if
!
      if(axc(2) .gt. zero) then
        c_surf( 2) =  one / (axc(2)*axc(2))
        c_surf( 8) = -two*xc(2) / (axc(2)*axc(2))
        c_surf(10) =  c_surf(10) + xc(2)*xc(2) / (axc(2)*axc(2))
      end if
!
      if(axc(3) .gt. zero) then
        c_surf( 3) =  one / (axc(3)*axc(3))
        c_surf( 9) = -two*xc(3) / (axc(3)*axc(3))
        c_surf(10) =  c_surf(10) + xc(3)*xc(3) / (axc(3)*axc(3))
      end if
!
      end subroutine set_coefs_4_ellipsode
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_hyperboloide(psf_c, c_surf)
!
      type(psf_ctl), intent(in) :: psf_c
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf_c%psf_center_ctl%num,            &
     &    psf_c%psf_center_ctl%c_tbl, psf_c%psf_center_ctl%vect, xc)
      call set_parameter_2_vectors(psf_c%psf_axis_ctl%num,              &
     &    psf_c%psf_axis_ctl%c_tbl, psf_c%psf_axis_ctl%vect, axc)
!
      c_surf( 1) =  one / (axc(1)*axc(1))
      c_surf( 2) =  one / (axc(2)*axc(2))
      c_surf( 3) = -one / (axc(3)*axc(3))
      c_surf( 4) = zero
      c_surf( 5) = zero
      c_surf( 6) = zero
      c_surf( 7) = -two*xc(1) / (axc(1)*axc(1))
      c_surf( 8) = -two*xc(2) / (axc(2)*axc(2))
      c_surf( 9) =  two*xc(3) / (axc(3)*axc(3))
      c_surf(10) =  xc(1)*xc(1) / (axc(1)*axc(1))                       &
     &            + xc(2)*xc(2) / (axc(2)*axc(2))                       &
     &            - xc(3)*xc(3) / (axc(3)*axc(3))                       &
     &            - one
!
      end subroutine set_coefs_4_hyperboloide
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_parabolic(psf_c, c_surf)
!
      type(psf_ctl), intent(in) :: psf_c
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf_c%psf_center_ctl%num,            &
     &    psf_c%psf_center_ctl%c_tbl, psf_c%psf_center_ctl%vect, xc)
      call set_parameter_2_vectors(psf_c%psf_axis_ctl%num,              &
     &    psf_c%psf_axis_ctl%c_tbl, psf_c%psf_axis_ctl%vect, axc)
!
      c_surf( 1) =  one / (axc(1)*axc(1))
      c_surf( 2) = -one / (axc(2)*axc(2))
      c_surf( 3) =  zero
      c_surf( 4) = zero
      c_surf( 5) = zero
      c_surf( 6) = zero
      c_surf( 7) = -two*xc(1) / (axc(1)*axc(1))
      c_surf( 8) =  two*xc(2) / (axc(2)*axc(2))
      c_surf( 9) = -one
      c_surf(10) =  xc(1)*xc(1) / (axc(1)*axc(1))                       &
     &            - xc(2)*xc(2) / (axc(2)*axc(2))                       &
     &            + xc(3)
!
      end subroutine set_coefs_4_parabolic
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_parameter_2_vectors(num_vect, ctl_name, vect_ctl,  &
     &          vector)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_vect
      character(len=kchara), intent(in) :: ctl_name(num_vect)
      real(kind = kreal), intent(in) :: vect_ctl(num_vect)
!
      real(kind = kreal), intent(inout) :: vector(num_vect)
!
      integer(kind = kint) :: i
!
!
      vector(1:3) = zero
      do i = 1, num_vect
        if      (cmp_no_case(ctl_name(i), x_ln1)) then
          vector(1) = vect_ctl(i)
        else if (cmp_no_case(ctl_name(i), y_ln1)) then
          vector(2) = vect_ctl(i)
        else if (cmp_no_case(ctl_name(i), z_ln1)) then
          vector(3) = vect_ctl(i)
        end if
      end do
!
      end subroutine set_parameter_2_vectors
!
!  ---------------------------------------------------------------------
!
      end module set_cross_section_coefs
