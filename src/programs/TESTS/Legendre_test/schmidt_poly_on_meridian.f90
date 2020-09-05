!
!      module schmidt_poly_on_meridian
!
!      Written by H. Matsui on June, 2007
!
!!      subroutine cal_full_legendre_on_med(nth, lst, led, leg_d)
!!        type(gauss_legendre_data), intent(inout) :: leg_d
!
      module schmidt_poly_on_meridian
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: dint_p(:)
!
      private :: copy_gauss_colatitude
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_full_legendre_on_med(nth, lst, led, leg_d)
!
      use t_schmidt_poly_on_gauss
      use t_schmidt_polynomial
      use t_spherical_harmonics
      use t_gauss_points
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: nth, lst, led
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      integer(kind = kint) :: i, j, m, l
      type(gauss_points) :: gauss_med
      type(legendre_polynomials) :: leg_1pt
      type(sph_1point_type) :: sph_1pt
!
!
!
!     set gauss colatitudes
!
      call const_gauss_colatitude(nth, gauss_med)
!
      call alloc_gauss_colat_med(gauss_med%n_point, leg_d)
      call copy_gauss_colatitude(gauss_med, leg_d)
!
      call dealloc_gauss_colatitude(gauss_med)
!
!     set Legendre polynomials
!
      call init_sph_indices(leg_d%ltr_g, leg_1pt, sph_1pt)
!
      leg_d%jmax_g = sph_1pt%jmax_tri
!
      call alloc_schmidt_poly_med(leg_d)
      call alloc_legendre_med(leg_d)
!
      allocate(dint_p(0:leg_d%jmax_g))
      dint_p = 0.0d0
!
      do i = lst, led
        call full_norm_legendre(leg_d%g_colat_med(i), leg_1pt)
!
        do j = 0, leg_d%jmax_g
          l = sph_1pt%idx(j,1)
          m = abs( sph_1pt%idx(j,2) )
          leg_d%P_smdt(i,j) =    leg_1pt%p(m,l)
          leg_d%dPdt_smdt(i,j) = leg_1pt%dp(m,l)
        end do
!
        call dlad(leg_d%g_colat_med(i), leg_1pt)
        do j = 0, leg_d%jmax_g
          l = sph_1pt%idx(j,1)
          m = abs( sph_1pt%idx(j,2) )
          leg_d%P_org(i,j) = leg_1pt%dplm(m,l)
        end do
      end do
!
      dint_p = 0.0d0
      do j = 0, leg_d%jmax_g
        l = sph_1pt%idx(j,1)
        m = abs( sph_1pt%idx(j,2) )
        do i = 1, leg_d%nth_g
          dint_p(j) = dint_p(j)                                         &
     &               + leg_d%weight_med(i) * leg_d%P_smdt(i,j)**2
        end do
      end do
!
      call finalize_sph_indices(leg_1pt, sph_1pt)
!
      end subroutine cal_full_legendre_on_med
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_gauss_colatitude(gauss, leg_d)
!
      use t_gauss_points
      use t_schmidt_poly_on_gauss
!
      type(gauss_points), intent(in) :: gauss
      type(gauss_legendre_data), intent(inout) :: leg_d
!
!
      leg_d%g_point_med(1:leg_d%nth_g) = gauss%point(1:leg_d%nth_g)
      leg_d%g_colat_med(1:leg_d%nth_g) = gauss%colat(1:leg_d%nth_g)
      leg_d%weight_med(1:leg_d%nth_g) =  gauss%weight(1:leg_d%nth_g)
!
      end subroutine copy_gauss_colatitude
!
! -----------------------------------------------------------------------
!
      end module schmidt_poly_on_meridian
