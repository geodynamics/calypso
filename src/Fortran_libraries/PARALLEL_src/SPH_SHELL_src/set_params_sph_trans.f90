!>@file   set_params_sph_trans.f90
!!@brief  module set_params_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Initialize spherical harmonics transform
!!
!!@verbatim
!!      subroutine const_conatitude_rtp(sph_rtm, sph_rtp, leg)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(legendre_4_sph_trans), intent(inout) :: leg
!!      subroutine const_sin_theta_rtp(leg, sph_rtm, sph_rtp)
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(sph_rtm_grid), intent(in) ::    sph_rtm
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!      subroutine set_mdx_rlm_rtm(l_truncation, nidx_rtm, nidx_rlm,    &
!!     &          idx_gl_1d_rtm_m, idx_gl_1d_rlm_j,                     &
!!     &          mdx_p_rlm_rtm, mdx_n_rlm_rtm, maxdegree_rlm,          &
!!     &          lstack_rlm)
!!      subroutine set_sin_theta_rtm                                    &
!!     &         (nth_rtm, g_colat_rtm, asin_theta_1d_rtm)
!!      subroutine radial_4_sph_trans(sph_rtp, sph_rtm, sph_rlm, sph_rj)
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!@endverbatim
!
      module set_params_sph_trans
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: set_colatitude_rtp, set_sin_theta_rtp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_conatitude_rtp(sph_rtm, sph_rtp, leg)
!
      use t_spheric_rtp_data
      use t_spheric_rtm_data
      use t_schmidt_poly_on_rtm
!
      type(sph_rtm_grid), intent(in) ::    sph_rtm
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(inout) :: leg
!
!
      call alloc_gauss_colatitude_rtp(sph_rtp%nidx_rtp(2), leg)
      call set_colatitude_rtp                                           &
     &   (sph_rtp%nidx_rtp(2), sph_rtm%nidx_rtm(2), leg%g_colat_rtm,    &
     &    sph_rtp%idx_gl_1d_rtp_t, leg%g_colat_rtp)
!
      end subroutine const_conatitude_rtp
!
! -----------------------------------------------------------------------
!
      subroutine const_sin_theta_rtp(leg, sph_rtm, sph_rtp)
!
      use t_spheric_rtp_data
      use t_spheric_rtm_data
      use t_schmidt_poly_on_rtm
!
      type(legendre_4_sph_trans), intent(in) :: leg
      type(sph_rtm_grid), intent(in) ::    sph_rtm
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
!
      call alloc_theta_4_rtp(sph_rtp)
      call set_sin_theta_rtp                                            &
     &   (sph_rtp%nidx_rtp(2), sph_rtm%nidx_rtm(2), leg%g_colat_rtm,    &
     &    sph_rtp%idx_gl_1d_rtp_t, sph_rtp%cos_theta_1d_rtp,            &
     &    sph_rtp%sin_theta_1d_rtp, sph_rtp%cot_theta_1d_rtp)
!
      end subroutine const_sin_theta_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_mdx_rlm_rtm(l_truncation, nidx_rtm, nidx_rlm,      &
     &          idx_gl_1d_rtm_m, idx_gl_1d_rlm_j,                       &
     &          mdx_p_rlm_rtm, mdx_n_rlm_rtm, maxdegree_rlm,            &
     &          lstack_rlm)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in)                                  &
     &             :: idx_gl_1d_rtm_m(nidx_rtm(3),2)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in)                                  &
     &             :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
!
      integer(kind = kint), intent(inout) :: mdx_p_rlm_rtm(nidx_rlm(2))
      integer(kind = kint), intent(inout) :: mdx_n_rlm_rtm(nidx_rlm(2))
      integer(kind = kint), intent(inout) :: maxdegree_rlm
      integer(kind = kint), intent(inout) :: lstack_rlm(0:nidx_rtm(3))
!
      integer(kind = kint) :: m, mm, j, mst, med
      integer(kind = kint), allocatable :: mdx_rlm_rtm(:)
      integer(kind = kint), allocatable :: mp_rlm(:), mn_rlm(:)
!
      allocate(mp_rlm( nidx_rlm(2)))
      allocate(mn_rlm( nidx_rlm(2)))
!
      allocate(mdx_rlm_rtm(-l_truncation:l_truncation))
      mdx_rlm_rtm(-l_truncation:l_truncation) = 0
!
      mdx_p_rlm_rtm(1:nidx_rlm(2)) = 0
      mdx_n_rlm_rtm(1:nidx_rlm(2)) = 0
      maxdegree_rlm = 0
      lstack_rlm(0) = 0
      do m = 1, nidx_rtm(3)
        mm = idx_gl_1d_rtm_m(m,2)
        mdx_rlm_rtm(mm) = m
        lstack_rlm(m) = lstack_rlm(m-1) + (l_truncation - abs(mm) + 1)
        maxdegree_rlm = max(maxdegree_rlm,(l_truncation - abs(mm) + 1))
      end do
!
      do m = 1, nidx_rtm(3)
        mst = lstack_rlm(m-1)+1
        med = lstack_rlm(m)
        do j = mst, med
          mp_rlm(j) = m
          mn_rlm(j) = nidx_rtm(3) - m + 1
        end do
      end do
!
      do j = 1, nidx_rlm(2)
        m = idx_gl_1d_rlm_j(j,3)
        mdx_p_rlm_rtm(j) = mdx_rlm_rtm( m)
        mdx_n_rlm_rtm(j) = mdx_rlm_rtm(-m)
      end do
! 
!      write(50+my_rank,*) 'j, mdx_p_rlm_rtm(j), mp_rlm(j)'
!      do j = 1, nidx_rlm(2)
!        write(50+my_rank,*) j, mdx_p_rlm_rtm(j), mp_rlm(j)
!      end do
!
!      write(50+my_rank,*) 'j, mdx_n_rlm_rtm(j), mn_rlm(j)'
!      do j = 1, nidx_rlm(2)
!        write(50+my_rank,*) j, mdx_n_rlm_rtm(j), mn_rlm(j)
!      end do
!
!      write(50+my_rank,*) 'm, lstack_rlm(m)'
!      do m = 1, nidx_rtm(3)
!        write(50+my_rank,*) m, lstack_rlm(m)
!      end do
!
      deallocate(mdx_rlm_rtm, mp_rlm, mn_rlm)
!
      end subroutine set_mdx_rlm_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_sin_theta_rtm                                      &
     &         (nth_rtm, g_colat_rtm, asin_theta_1d_rtm)
!
      integer(kind = kint), intent(in) :: nth_rtm
      real(kind= kreal), intent(in) :: g_colat_rtm(nth_rtm)
      real(kind= kreal), intent(inout) :: asin_theta_1d_rtm(nth_rtm)
!
      integer(kind = kint) :: l_rtm
!
!
!$omp parallel do
      do l_rtm = 1, nth_rtm
        asin_theta_1d_rtm(l_rtm) = one / sin(g_colat_rtm(l_rtm))
      end do
!$omp end parallel do
!
      end subroutine set_sin_theta_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_colatitude_rtp                                     &
     &         (nth_rtp, nth_rtm, g_colat_rtm, idx_gl_1d_rtp_t,         &
     &          g_colat_rtp)
!
      integer(kind = kint), intent(in) :: nth_rtp, nth_rtm
      integer(kind = kint), intent(in) :: idx_gl_1d_rtp_t(nth_rtp)
      real(kind= kreal), intent(in) :: g_colat_rtm(nth_rtm)
!
      real(kind= kreal), intent(inout) :: g_colat_rtp(nth_rtp)
!
      integer(kind = kint) :: l_rtp, l_rtm
!
!
!$omp parallel do private(l_rtm)
      do l_rtp = 1, nth_rtp
        l_rtm = idx_gl_1d_rtp_t(l_rtp)
        g_colat_rtp(l_rtp) = g_colat_rtm(l_rtm)
      end do
!$omp end parallel do
!
      end subroutine set_colatitude_rtp
!
! -----------------------------------------------------------------------
!
      subroutine set_sin_theta_rtp                                      &
     &         (nth_rtp, nth_rtm, g_colat_rtm, idx_gl_1d_rtp_t,         &
     &          cos_theta_1d_rtp, sin_theta_1d_rtp, cot_theta_1d_rtp)
!
      integer(kind = kint), intent(in) :: nth_rtp, nth_rtm
      integer(kind = kint), intent(in) :: idx_gl_1d_rtp_t(nth_rtp)
      real(kind= kreal), intent(in) :: g_colat_rtm(nth_rtm)
!
      real(kind= kreal), intent(inout) :: cos_theta_1d_rtp(nth_rtp)
      real(kind= kreal), intent(inout) :: sin_theta_1d_rtp(nth_rtp)
      real(kind= kreal), intent(inout) :: cot_theta_1d_rtp(nth_rtp)
!
      integer(kind = kint) :: l_rtp, l_rtm
!
!
!$omp parallel do private(l_rtm)
      do l_rtp = 1, nth_rtp
        l_rtm = idx_gl_1d_rtp_t(l_rtp)
        cos_theta_1d_rtp(l_rtp) = cos(g_colat_rtm(l_rtm))
        sin_theta_1d_rtp(l_rtp) = sin(g_colat_rtm(l_rtm))
        cot_theta_1d_rtp(l_rtp) = cos_theta_1d_rtp(l_rtp)               &
     &                           / sin_theta_1d_rtp(l_rtp)
      end do
!$omp end parallel do
!
      end subroutine set_sin_theta_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine radial_4_sph_trans(sph_rtp, sph_rtm, sph_rlm, sph_rj)
!
      use t_spheric_parameter
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
!
!$omp parallel workshare
      sph_rtp%a_r_1d_rtp_r(1:sph_rtp%nidx_rtp(1))                       &
     &      = one / sph_rtp%radius_1d_rtp_r(1:sph_rtp%nidx_rtp(1))
      sph_rtm%a_r_1d_rtm_r(1:sph_rtm%nidx_rtm(1))                       &
     &      = one / sph_rtm%radius_1d_rtm_r(1:sph_rtm%nidx_rtm(1))
      sph_rlm%a_r_1d_rlm_r(1:sph_rlm%nidx_rlm(1))                       &
     &      = one / sph_rlm%radius_1d_rlm_r(1:sph_rlm%nidx_rlm(1))
      sph_rj%a_r_1d_rj_r(1:sph_rj%nidx_rj(1))                           &
     &      = one / sph_rj%radius_1d_rj_r(1:sph_rj%nidx_rj(1))
!$omp end parallel workshare
!
      end subroutine radial_4_sph_trans
!
! -----------------------------------------------------------------------
!
      end module set_params_sph_trans
