!>@file   set_local_sphere_param.f90
!!@brief  module set_local_sphere_param
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Copy number of global spherical harmonics indices
!!        to local data
!!
!!
!!@verbatim
!!      subroutine set_global_sph_rtp_id
!!      subroutine set_global_sph_rj_id
!!      subroutine set_global_sph_4_rtm
!!      subroutine set_global_sph_4_rlm
!!@endverbatim
!
      module set_local_sphere_param
!
      use m_precision
!
      use m_spheric_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_rtp_id
!
      use m_spheric_global_ranks
      use m_sph_1d_global_index
!
      integer(kind = kint) :: inod, k, l, m
      integer(kind = kint) :: k_gl, l_gl, m_gl
      integer(kind = kint) :: ndom_r, ndom_t, nsize_r, nsize_t
!
!
      ndom_r = ndomain_rtp(1)
      ndom_t = ndomain_rtp(2)
      nsize_r = istack_idx_local_rtp_r(ndom_r)
      nsize_t = istack_idx_local_rtp_t(ndom_t)
!
      inod = 0
      do m = 1, nidx_rtp(3)
        m_gl = idx_gl_1d_rtp_p(m,1)
        do l = 1, nidx_rtp(2)
          l_gl = idx_gl_1d_rtp_t(l)
          do k = 1, nidx_rtp(1)
            k_gl = idx_gl_1d_rtp_r(k)
!
            inod = inod + 1
            idx_global_rtp(inod,1) = k_gl
            idx_global_rtp(inod,2) = l_gl
            idx_global_rtp(inod,3) = m_gl
          end do
        end do
      end do
!
      end subroutine set_global_sph_rtp_id
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_rj_id
!
      use m_spheric_global_ranks
      use m_sph_1d_global_index
!
      integer(kind = kint) :: j, k, inod
      integer(kind = kint) :: ndom_r, nsize_r
!
      ndom_r = ndomain_rj(1)
      nsize_r = istack_idx_local_rj_r(ndom_r)
!
      inod = 0
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          inod = inod + 1
          idx_global_rj(inod,1) = idx_gl_1d_rj_r(k)
          idx_global_rj(inod,2) = idx_gl_1d_rj_j(j,1)
        end do
      end do
!
      if(inod_rj_center .eq. 0) return
      idx_global_rj(nnod_rj,1) = 0
      idx_global_rj(nnod_rj,2) = 0
!
      end subroutine set_global_sph_rj_id
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_4_rtm
!
      use m_spheric_global_ranks
      use m_sph_1d_global_index
!
      integer(kind = kint) :: inod, k, l, m
      integer(kind = kint) :: ndom_r, ndom_t, nsize_r, nsize_t
!
      ndom_r = ndomain_rtm(1)
      ndom_t = ndomain_rtm(2)
      nsize_r = istack_idx_local_rtm_r(ndom_r)
      nsize_t = istack_idx_local_rtm_t(ndom_t)
!
!
      inod = 0
      do m = 1, nidx_rtm(3)
        do k = 1, nidx_rtm(1)
          do l = 1, nidx_rtm(2)
            inod = inod + 1
            idx_global_rtm(inod,1) = idx_gl_1d_rtm_r(k)
            idx_global_rtm(inod,2) = idx_gl_1d_rtm_t(l)
            idx_global_rtm(inod,3) = idx_gl_1d_rtm_m(m,1)
          end do
        end do
      end do
!
      end subroutine set_global_sph_4_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_4_rlm
!
      use m_spheric_global_ranks
      use m_sph_1d_global_index
!
      integer(kind = kint) :: j, k, inod
      integer(kind = kint) :: ndom_r, nsize_r
!
      ndom_r = ndomain_rlm(1)
      nsize_r = istack_idx_local_rlm_r(ndom_r)
!
      inod = 0
      do k = 1, nidx_rlm(1)
        do j = 1, nidx_rlm(2)
          inod = inod + 1
          idx_global_rlm(inod,1) = idx_gl_1d_rlm_r(k)
          idx_global_rlm(inod,2) = idx_gl_1d_rlm_j(j,1)
          end do
        end do
!
      end subroutine set_global_sph_4_rlm
!
! -----------------------------------------------------------------------!
      end module set_local_sphere_param
