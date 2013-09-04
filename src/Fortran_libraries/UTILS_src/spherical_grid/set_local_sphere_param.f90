!
!      module set_local_sphere_param
!
!     Written by H. Matsui on July, 2007
!
!      subroutine set_global_sph_rtp_id
!      subroutine set_global_sph_rj_id
!      subroutine set_global_sph_4_rtm
!      subroutine set_global_sph_4_rlm
!
      module set_local_sphere_param
!
      use m_precision
!
      use m_spheric_parameter
      use m_sph_1d_global_index
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
      integer(kind = kint) :: inod, k, l, m
      integer(kind = kint) :: inod_gl, k_gl, l_gl, m_gl
      integer(kind = kint) :: ndom_r, ndom_t, nsize_r, nsize_t
      integer(kind = kint) :: kst_gl, lst_gl, mst_gl
!
!
      ndom_r = ndomain_rtp(1)
      ndom_t = ndomain_rtp(2)
      nsize_r = istack_idx_local_rtp_r(ndom_r)                          &
     &         - istack_idx_local_rtp_r(0)
      nsize_t = istack_idx_local_rtp_t(ndom_t)                          &
     &         - istack_idx_local_rtp_t(0)
      kst_gl = istack_idx_local_rtp_r(0)
      lst_gl = istack_idx_local_rtp_t(0)
      mst_gl = istack_idx_local_rtp_p(0)
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
            inod_gl =   k_gl - kst_gl                                   &
     &               + (l_gl - lst_gl - 1) * nsize_r                    &
     &               + (m_gl - mst_gl - 1) * nsize_r*nsize_t
!
            idx_global_rtp(inod,1) = k_gl
            idx_global_rtp(inod,2) = l_gl
            idx_global_rtp(inod,3) = m_gl
            inod_global_rtp(inod) = inod_gl
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
      integer(kind = kint) :: j, k, inod
      integer(kind = kint) :: ndom_r, nsize_r, kst_gl, jst_gl
!
      ndom_r = ndomain_rj(1)
      nsize_r = istack_idx_local_rj_r(ndom_r)                          &
     &         - istack_idx_local_rj_r(0)
      kst_gl = istack_idx_local_rj_r(0)
      jst_gl = istack_idx_local_rj_j(0)
!
      inod = 0
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
            inod = inod + 1
            idx_global_rj(inod,1) = idx_gl_1d_rj_r(k)
            idx_global_rj(inod,2) = idx_gl_1d_rj_j(j,1)
            inod_global_rj(inod)                                        &
     &            =   idx_global_rj(inod,1) - kst_gl                    &
     &             + (idx_global_rj(inod,2) - jst_gl - 1) * nsize_r
          end do
        end do
!
      end subroutine set_global_sph_rj_id
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_4_rtm
!
      integer(kind = kint) :: inod, k, l, m
      integer(kind = kint) :: ndom_r, ndom_t, nsize_r, nsize_t
      integer(kind = kint) :: kst_gl, lst_gl, mst_gl
!
      ndom_r = ndomain_rtm(1)
      ndom_t = ndomain_rtm(2)
      nsize_r = istack_idx_local_rtm_r(ndom_r)                          &
     &         - istack_idx_local_rtm_r(0)
      nsize_t = istack_idx_local_rtm_t(ndom_t)                          &
     &         - istack_idx_local_rtm_t(0)
      kst_gl = istack_idx_local_rtm_r(0)
      lst_gl = istack_idx_local_rtm_t(0)
      mst_gl = istack_idx_local_rtm_m(0)
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
            inod_global_rtm(inod)                                       &
     &       =   idx_global_rtm(inod,1) - kst_gl                        &
     &        + (idx_global_rtm(inod,2) - lst_gl - 1) * nsize_r         &
     &        + (idx_global_rtm(inod,3) - mst_gl - 1) * nsize_r*nsize_t
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
      integer(kind = kint) :: j, k, inod
      integer(kind = kint) :: ndom_r, nsize_r, kst_gl, jst_gl
!
      ndom_r = ndomain_rlm(1)
      nsize_r = istack_idx_local_rlm_r(ndom_r)                          &
     &         - istack_idx_local_rlm_r(0)
      kst_gl = istack_idx_local_rlm_r(0)
      jst_gl = istack_idx_local_rlm_j(0)
!
      inod = 0
      do k = 1, nidx_rlm(1)
        do j = 1, nidx_rlm(2)
          inod = inod + 1
          idx_global_rlm(inod,1) = idx_gl_1d_rlm_r(k)
          idx_global_rlm(inod,2) = idx_gl_1d_rlm_j(j,1)
          inod_global_rlm(inod) =  idx_global_rlm(inod,1) - kst_gl      &
     &                          + (idx_global_rlm(inod,2) - jst_gl - 1) &
     &                           * (nsize_r)
          end do
        end do
!
      end subroutine set_global_sph_4_rlm
!
! -----------------------------------------------------------------------!
      end module set_local_sphere_param
