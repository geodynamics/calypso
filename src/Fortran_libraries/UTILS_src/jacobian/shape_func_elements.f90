!
!      module shape_func_elements
!
!        modified by H. Matsui on Aug., 2006
!
!      subroutine s_shape_elenents_aw_3d(xi_nega, ei_nega, zi_nega,     &
!     &          xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,  &
!     &          xi, ei, zi)
!
!      subroutine s_shape_elenents_aw_2d(xi_nega, ei_nega,              &
!     &          xi_posi, ei_posi, xi_sqre, ei_sqre, xi, ei)
!
!      subroutine s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi)
!
      module shape_func_elements
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_shape_elenents_aw_3d(xi_nega, ei_nega, zi_nega,      &
     &          xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,   &
     &          xi, ei, zi)
!
      real (kind=kreal), intent(in) :: xi, ei, zi
      real (kind=kreal), intent(inout) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(inout) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal), intent(inout) :: xi_sqre, ei_sqre, zi_sqre
!
      call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi)
      call s_shape_elenents_aw_1d(ei_nega, ei_posi, ei_sqre, ei)
      call s_shape_elenents_aw_1d(zi_nega, zi_posi, zi_sqre, zi)
!
      end subroutine s_shape_elenents_aw_3d
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_shape_elenents_aw_2d(xi_nega, ei_nega,               &
     &          xi_posi, ei_posi, xi_sqre, ei_sqre, xi, ei)
!
      real (kind=kreal), intent(in) :: xi, ei
      real (kind=kreal), intent(inout) :: xi_nega, ei_nega
      real (kind=kreal), intent(inout) :: xi_posi, ei_posi
      real (kind=kreal), intent(inout) :: xi_sqre, ei_sqre
!
      call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi)
      call s_shape_elenents_aw_1d(ei_nega, ei_posi, ei_sqre, ei)
!
      end subroutine s_shape_elenents_aw_2d
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi)
!
      real(kind = kreal), intent(in) :: xi
      real(kind = kreal), intent(inout) :: xi_nega, xi_posi, xi_sqre
!
!
      xi_nega = one - xi
      xi_posi = one + xi
      xi_sqre = one - xi * xi
!
      end subroutine s_shape_elenents_aw_1d
!
!-----------------------------------------------------------------------
!
      end module shape_func_elements
