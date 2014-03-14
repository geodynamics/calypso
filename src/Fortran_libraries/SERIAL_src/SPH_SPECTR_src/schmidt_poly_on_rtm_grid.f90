!>@file   schmidt_poly_on_rtm_grid.f90
!!@brief  module schmidt_poly_on_rtm_grid
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!
!>@brief  Copy Legendre polynomials for spherical transform
!!
!!
!!@verbatim
!!      subroutine s_cal_schmidt_poly_rtm
!!
!!      subroutine copy_sph_normalization_2_rlm
!!      subroutine copy_sph_normalization_2_rj
!!@endverbatim
!
      module schmidt_poly_on_rtm_grid
!
      use m_precision
!
      implicit none
!
      private :: set_gauss_points_rtm
      private :: set_lagender_4_rlm, set_lagender_pole_rlm
      private :: copy_sph_normalization_2_rlm
      private :: copy_sph_normalization_2_rj
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_schmidt_poly_rtm
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_gauss_points
      use m_schmidt_polynomial
      use m_spherical_harmonics
!
      use spherical_harmonics
!
!
      n_point = nidx_rtm(2)
      nth = l_truncation
!
      call allocate_gauss_colat_rtm
      call allocate_gauss_points
      call allocate_gauss_colatitude
!
      call set_gauss_points_rtm
!
      call deallocate_gauss_colatitude
      call deallocate_gauss_points
!
!     set Legendre polynomials
!
      call allocate_index_4_sph(nth)
      call allocate_schmidt_polynomial
      call idx28
!
      call allocate_schmidt_poly_rtm
!
      call copy_sph_normalization_2_rlm
      call copy_sph_normalization_2_rj
!
      call set_lagender_4_rlm
!
      call allocate_schmidt_p_rtm_pole
      call set_lagender_pole_rlm
!
      call deallocate_index_4_sph
      call deallocate_schmidt_polynomial
!
      end subroutine s_cal_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_gauss_points_rtm
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_gauss_points
!
      integer(kind = kint) :: i
!
!
!     set gauss colatitudes
!
      call construct_gauss_coefs
!
      call set_gauss_colatitude
!
      do i = 1, n_point
        g_point_rtm(i) = w_point(i)
        g_colat_rtm(i) = w_colat(i)
        weight_rtm(i) =  w_coefs(i)
      end do
!
      end subroutine set_gauss_points_rtm
!
! -----------------------------------------------------------------------
!
      subroutine copy_sph_normalization_2_rlm
!
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_spherical_harmonics
!
      integer(kind = kint) :: j, jj
!
      do j = 1, nidx_rlm(2)
        jj = idx_gl_1d_rlm_j(j,1)
        g_sph_rlm(j,1) =  g(jj,1)
        g_sph_rlm(j,2) =  g(jj,2)
        g_sph_rlm(j,3) =  g(jj,3)
        g_sph_rlm(j,4) =  g(jj,4)
        g_sph_rlm(j,5) =  g(jj,5)
        g_sph_rlm(j,6) =  g(jj,6)
        g_sph_rlm(j,7) =  g(jj,7)
        g_sph_rlm(j,8) =  g(jj,8)
        g_sph_rlm(j,9) =  g(jj,9)
        g_sph_rlm(j,10) = g(jj,10)
        g_sph_rlm(j,11) = g(jj,11)
        g_sph_rlm(j,12) = g(jj,12)
        g_sph_rlm(j,13) = g(jj,13)
        g_sph_rlm(j,14) = g(jj,14)
        g_sph_rlm(j,15) = g(jj,15)
        g_sph_rlm(j,16) = g(jj,16)
        g_sph_rlm(j,17) = g(jj,17)
!
        if(jj .eq. 0) g_sph_rlm(j,3) = half
      end do
!
      end subroutine copy_sph_normalization_2_rlm
!
! -----------------------------------------------------------------------
!
      subroutine copy_sph_normalization_2_rj
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_spherical_harmonics
!
      integer(kind = kint) :: j, jj
!
      do j = 1, nidx_rj(2)
        jj = idx_gl_1d_rj_j(j,1)
        g_sph_rj(j,1) =  g(jj,1)
        g_sph_rj(j,2) =  g(jj,2)
        g_sph_rj(j,3) =  g(jj,3)
        g_sph_rj(j,4) =  g(jj,4)
        g_sph_rj(j,5) =  g(jj,5)
        g_sph_rj(j,6) =  g(jj,6)
        g_sph_rj(j,7) =  g(jj,7)
        g_sph_rj(j,8) =  g(jj,8)
        g_sph_rj(j,9) =  g(jj,9)
        g_sph_rj(j,10) = g(jj,10)
        g_sph_rj(j,11) = g(jj,11)
        g_sph_rj(j,12) = g(jj,12)
        g_sph_rj(j,13) = g(jj,13)
      end do
!
      end subroutine copy_sph_normalization_2_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_lagender_4_rlm
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_schmidt_polynomial
!
      integer(kind = kint) :: i, j, l, mm, jj
!
!
      do i = 1, nidx_rtm(2)
        dth = g_colat_rtm(i)
        call dschmidt
!
        do j = 1, nidx_rlm(2)
          jj = idx_gl_1d_rlm_j(j,1)
          l =  idx_gl_1d_rlm_j(j,2)
          mm = abs( idx_gl_1d_rlm_j(j,3) )
          P_rtm(i,j) =    p(mm,l)
          dPdt_rtm(i,j) = dp(mm,l)
        end do
!
      end do
!
      end subroutine set_lagender_4_rlm
!
! -----------------------------------------------------------------------
!
      subroutine set_lagender_pole_rlm
!
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_schmidt_polynomial
!
      integer(kind = kint) :: j, l, mm, jj
      real(kind = kreal) :: pi
!
!
      pi = four * atan(one)
!
      dth = zero
      call dschmidt
!
      do j = 1, nidx_rlm(2)
        jj = idx_gl_1d_rlm_j(j,1)
        l =  idx_gl_1d_rlm_j(j,2)
        mm = abs( idx_gl_1d_rlm_j(j,3) )
        P_pole_rtm(1,j) =    p(mm,l)
        dPdt_pole_rtm(1,j) = dp(mm,l)
      end do
!
      dth = pi
      call dschmidt
!
      do j = 1, nidx_rlm(2)
        jj = idx_gl_1d_rlm_j(j,1)
        l =  idx_gl_1d_rlm_j(j,2)
        mm = abs( idx_gl_1d_rlm_j(j,3) )
        P_pole_rtm(2,j) =    p(mm,l)
        dPdt_pole_rtm(2,j) = dp(mm,l)
      end do
!
      end subroutine set_lagender_pole_rlm
!
! -----------------------------------------------------------------------
!
      end module schmidt_poly_on_rtm_grid
