!>@file   set_cross_section_coefs.f90
!!@brief  module set_cross_section_coefs
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  Set coefficients for cross section function
!!
!!@verbatim
!!      subroutine set_coefs_4_plane(psf_def_c, c_surf)
!!      subroutine set_coefs_4_sphere(psf_def_c, c_surf)
!!      subroutine set_coefs_4_ellipsode(psf_def_c, c_surf)
!!      subroutine set_coefs_4_hyperboloide(psf_def_c, c_surf)
!!      subroutine set_coefs_4_parabolic(psf_def_c, c_surf)
!!        type(psf_define_ctl), intent(in) :: psf_def_c
!!@endverbatim
!
      module set_cross_section_coefs
!
      use m_precision
!
      use m_constants
      use t_control_data_4_psf_def
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_plane(psf_def_c, c_surf)
!
      use m_section_coef_flags
!
      type(psf_define_ctl), intent(in) :: psf_def_c
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf_def_c%psf_normal_ctl%num,        &
     &    psf_def_c%psf_normal_ctl%c_tbl,                               &
     &    psf_def_c%psf_normal_ctl%vect, axc)
      call set_parameter_2_vectors(psf_def_c%psf_center_ctl%num,        &
     &    psf_def_c%psf_center_ctl%c_tbl,                               &
     &    psf_def_c%psf_center_ctl%vect, xc)
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
      subroutine set_coefs_4_sphere(psf_def_c, c_surf)
!
      use m_section_coef_flags
!
      type(psf_define_ctl), intent(in) :: psf_def_c
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), r
!
!
      call set_parameter_2_vectors(psf_def_c%psf_center_ctl%num,        &
     &    psf_def_c%psf_center_ctl%c_tbl,                               &
     &    psf_def_c%psf_center_ctl%vect, xc)
!
      r = psf_def_c%radius_psf_ctl%realvalue
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
      subroutine set_coefs_4_ellipsode(psf_def_c, c_surf)
!
      use m_section_coef_flags
!
      type(psf_define_ctl), intent(in) :: psf_def_c
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf_def_c%psf_center_ctl%num,        &
     &    psf_def_c%psf_center_ctl%c_tbl,                               &
     &    psf_def_c%psf_center_ctl%vect, xc)
      call set_parameter_2_vectors(psf_def_c%psf_axis_ctl%num,          &
     &    psf_def_c%psf_axis_ctl%c_tbl, psf_def_c%psf_axis_ctl%vect,    &
     &    axc)
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
      subroutine set_coefs_4_hyperboloide(psf_def_c, c_surf)
!
      use m_section_coef_flags
!
      type(psf_define_ctl), intent(in) :: psf_def_c
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf_def_c%psf_center_ctl%num,        &
     &    psf_def_c%psf_center_ctl%c_tbl,                               &
     &    psf_def_c%psf_center_ctl%vect, xc)
      call set_parameter_2_vectors(psf_def_c%psf_axis_ctl%num,          &
     &    psf_def_c%psf_axis_ctl%c_tbl, psf_def_c%psf_axis_ctl%vect,    &
     &    axc)
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
      subroutine set_coefs_4_parabolic(psf_def_c, c_surf)
!
      use m_section_coef_flags
!
      type(psf_define_ctl), intent(in) :: psf_def_c
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf_def_c%psf_center_ctl%num,        &
     &    psf_def_c%psf_center_ctl%c_tbl,                               &
     &    psf_def_c%psf_center_ctl%vect, xc)
      call set_parameter_2_vectors(psf_def_c%psf_axis_ctl%num,          &
     &    psf_def_c%psf_axis_ctl%c_tbl, psf_def_c%psf_axis_ctl%vect,    &
     &    axc)
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
!
      end module set_cross_section_coefs
