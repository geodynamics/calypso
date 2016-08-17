!set_cross_section_coefs.f90
!      module set_cross_section_coefs
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine set_coefs_4_psf(num_const, c_name_psf, const_psf_ctl, &
!                c_surf)
!      subroutine set_coefs_4_sphere(psf, c_surf)
!      subroutine set_coefs_4_ellipsode(psf, c_surf)
!      subroutine set_coefs_4_hyperboloide(psf, c_surf)
!      subroutine set_coefs_4_parabolic(psf, c_surf)
!
      module set_cross_section_coefs
!
      use m_precision
!
      use m_constants
      use m_control_data_4_psf
!
      implicit  none
!
      private :: set_parameter_2_vectors
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
        if(      cmp_no_case(c_name_psf(i), 'XX')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'X2')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'X^2')) then
          c_surf(1) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), 'YY')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'Y2')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'Y^2')) then
          c_surf(2) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), 'ZZ')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'Z2')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'Z^2')) then
          c_surf(3) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), 'XY')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'YZ')) then
          c_surf(4) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), 'YZ')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'ZY')) then
          c_surf(5) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), 'ZX')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'XZ')) then
          c_surf(6) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), 'X')                        &
     &      .or. cmp_no_case(c_name_psf(i), 'X1')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'X^1')) then
          c_surf(7) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), 'Y')                        &
     &      .or. cmp_no_case(c_name_psf(i), 'Y1')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'Y^1')) then
          c_surf(8) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), 'Z')                        &
     &      .or. cmp_no_case(c_name_psf(i), 'Z1')                       &
     &      .or. cmp_no_case(c_name_psf(i), 'Z^1')) then
          c_surf(9) = const_psf_ctl(i)
!
        else if( cmp_no_case(c_name_psf(i), 'Const')                    &
     &      .or. cmp_no_case(c_name_psf(i), 'C')) then
          c_surf(10) = const_psf_ctl(i)
        end if
      end do
!
      end subroutine set_coefs_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_sphere(psf, c_surf)
!
      type(psf_ctl), intent(in) :: psf
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), r
!
!
      call set_parameter_2_vectors(psf%psf_center_ctl%num,              &
     &    psf%psf_center_ctl%c_tbl, psf%psf_center_ctl%vect, xc)
!
      r = psf%radius_psf_ctl%realvalue
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
      subroutine set_coefs_4_ellipsode(psf, c_surf)
!
      type(psf_ctl), intent(in) :: psf
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf%psf_center_ctl%num,              &
     &    psf%psf_center_ctl%c_tbl, psf%psf_center_ctl%vect, xc)
      call set_parameter_2_vectors(psf%psf_axis_ctl%num,                &
     &    psf%psf_axis_ctl%c_tbl, psf%psf_axis_ctl%vect, axc)
!
      c_surf( 1) =  one / (axc(1)*axc(1))
      c_surf( 2) =  one / (axc(2)*axc(2))
      c_surf( 3) =  one / (axc(3)*axc(3))
      c_surf( 4) = zero
      c_surf( 5) = zero
      c_surf( 6) = zero
      c_surf( 7) = -two*xc(1) / (axc(1)*axc(1))
      c_surf( 8) = -two*xc(2) / (axc(2)*axc(2))
      c_surf( 9) = -two*xc(3) / (axc(3)*axc(3))
      c_surf(10) =  xc(1)*xc(1) / (axc(1)*axc(1))                       &
     &            + xc(2)*xc(2) / (axc(2)*axc(2))                       &
     &            + xc(3)*xc(3) / (axc(3)*axc(3))                       &
     &            - one
!
      end subroutine set_coefs_4_ellipsode
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_hyperboloide(psf, c_surf)
!
      type(psf_ctl), intent(in) :: psf
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf%psf_center_ctl%num,              &
     &    psf%psf_center_ctl%c_tbl, psf%psf_center_ctl%vect, xc)
      call set_parameter_2_vectors(psf%psf_axis_ctl%num,                &
     &    psf%psf_axis_ctl%c_tbl, psf%psf_axis_ctl%vect, axc)
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
      subroutine set_coefs_4_parabolic(psf, c_surf)
!
      type(psf_ctl), intent(in) :: psf
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf%psf_center_ctl%num,              &
     &    psf%psf_center_ctl%c_tbl, psf%psf_center_ctl%vect, xc)
      call set_parameter_2_vectors(psf%psf_axis_ctl%num,                &
     &    psf%psf_axis_ctl%c_tbl, psf%psf_axis_ctl%vect, axc)
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
      integer(kind = kint), intent(in) :: num_vect
      character(len=kchara), intent(in) :: ctl_name(num_vect)
      real(kind = kreal), intent(in) :: vect_ctl(num_vect)
!
      real(kind = kreal), intent(inout) :: vector(num_vect)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_vect
        if      (ctl_name(i).eq.'x' .or. ctl_name(i).eq.'X') then
          vector(1) = vect_ctl(i)
        else if (ctl_name(i).eq.'y' .or. ctl_name(i).eq.'Y') then
          vector(2) = vect_ctl(i)
        else if (ctl_name(i).eq.'z' .or. ctl_name(i).eq.'Z') then
          vector(3) = vect_ctl(i)
        end if
      end do
!
      end subroutine set_parameter_2_vectors
!
!  ---------------------------------------------------------------------
!
      end module set_cross_section_coefs
