!>@file   set_params_sph_trans.f90
!!@brief  module set_params_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Initialize spherical harmonics transform
!!
!!@verbatim
!!      subroutine set_mdx_rlm_rtm
!!      subroutine set_sin_theta_rtm
!!      subroutine set_sin_theta_rtp
!!      subroutine radial_4_sph_trans
!!@endverbatim
!
      module set_params_sph_trans
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_mdx_rlm_rtm
!
      use calypso_mpi
      use m_spheric_parameter
      use m_work_4_sph_trans
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
      subroutine set_sin_theta_rtm
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      integer(kind = kint) :: l_rtm
!
!
      do l_rtm = 1, nidx_rtm(2)
        asin_theta_1d_rtm(l_rtm) = one / sin(g_colat_rtm(l_rtm))
      end do
!
      end subroutine set_sin_theta_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_sin_theta_rtp
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      integer(kind = kint) :: l_rtp, l_gl
!
!
      do l_rtp = 1, nidx_rtp(2)
        l_gl = idx_gl_1d_rtp_t(l_rtp)
        cos_theta_1d_rtp(l_rtp) = cos(g_colat_rtm(l_gl))
        sin_theta_1d_rtp(l_rtp) = sin(g_colat_rtm(l_gl))
        cot_theta_1d_rtp(l_rtp) = cos_theta_1d_rtp(l_rtp)               &
     &                           / sin_theta_1d_rtp(l_rtp)
      end do
!
      end subroutine set_sin_theta_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine radial_4_sph_trans
!
      use m_spheric_parameter
!
!
      a_r_1d_rtp_r(1:nidx_rtp(1)) = one/radius_1d_rtp_r(1:nidx_rtp(1))
      a_r_1d_rtm_r(1:nidx_rtm(1)) = one/radius_1d_rtm_r(1:nidx_rtm(1))
      a_r_1d_rlm_r(1:nidx_rlm(1)) = one/radius_1d_rlm_r(1:nidx_rlm(1))
      a_r_1d_rj_r(1:nidx_rj(1)) =   one/radius_1d_rj_r(1:nidx_rj(1))
!
      end subroutine radial_4_sph_trans
!
! -----------------------------------------------------------------------
!
      end module set_params_sph_trans
