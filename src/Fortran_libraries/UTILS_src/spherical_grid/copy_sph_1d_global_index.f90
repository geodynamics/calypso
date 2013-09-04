!
!      module copy_sph_1d_global_index
!
      module copy_sph_1d_global_index
!
!     Written by H. Matsui on July, 2007
!
      use m_precision
!
      use m_spheric_parameter
      use m_sph_1d_global_index
!
      implicit none
!
!      subroutine copy_sph_1d_gl_idx_rtp
!      subroutine copy_sph_1d_gl_idx_rtm
!      subroutine copy_sph_1d_gl_idx_rlm
!      subroutine copy_sph_1d_gl_idx_rj
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_1d_gl_idx_rtp
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, nidx_rtp(1)
        j = i - 1 + ist_rtp(1)
        idx_gl_1d_rtp_r(i) = idx_global_rtp_r(j)
      end do
      do i = 1, nidx_rtp(1)
        j = idx_gl_1d_rtp_r(i)
        radius_1d_rtp_r(i) = radius_1d_gl(j)
      end do
!
      do i = 1, nidx_rtp(2)
        j = i - 1 + ist_rtp(2)
        idx_gl_1d_rtp_t(i) = idx_global_rtp_t(j)
      end do
!
      do i = 1, nidx_rtp(3)
        j = i - 1 + ist_rtp(3)
        idx_gl_1d_rtp_p(i,1) = idx_global_rtp_p(j,1)
        idx_gl_1d_rtp_p(i,2) = idx_global_rtp_p(j,2)
      end do
!
      end subroutine copy_sph_1d_gl_idx_rtp
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_1d_gl_idx_rtm
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, nidx_rtm(1)
        j = i - 1 + ist_rtm(1)
        idx_gl_1d_rtm_r(i) = idx_global_rtm_r(j)
      end do
      do i = 1, nidx_rtm(1)
        j = idx_gl_1d_rtm_r(i)
        radius_1d_rtm_r(i) = radius_1d_gl(j)
      end do
!
      do i = 1, nidx_rtm(2)
        j = i - 1 + ist_rtm(2)
        idx_gl_1d_rtm_t(i) = idx_global_rtm_t(j)
      end do
!
      do i = 1, nidx_rtm(3)
        j = i - 1 + ist_rtm(3)
        idx_gl_1d_rtm_m(i,1) = idx_global_rtm_m(j,1)
        idx_gl_1d_rtm_m(i,2) = idx_global_rtm_m(j,2)
      end do
!
      end subroutine copy_sph_1d_gl_idx_rtm
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_1d_gl_idx_rlm
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, nidx_rlm(1)
        j = i - 1 + ist_rlm(1)
        idx_gl_1d_rlm_r(i) = idx_global_rlm_r(j)
      end do
      do i = 1, nidx_rlm(1)
        j = idx_gl_1d_rlm_r(i)
        radius_1d_rlm_r(i) = radius_1d_gl(j)
      end do
!
      do i = 1, nidx_rlm(2)
        j = i - 1 + ist_rlm(2)
        idx_gl_1d_rlm_j(i,1) = idx_global_rlm_j(j,1)
        idx_gl_1d_rlm_j(i,2) = idx_global_rlm_j(j,2)
        idx_gl_1d_rlm_j(i,3) = idx_global_rlm_j(j,3)
      end do
!
      end subroutine copy_sph_1d_gl_idx_rlm
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_1d_gl_idx_rj
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, nidx_rj(1)
        j = i - 1 + ist_rj(1)
        idx_gl_1d_rj_r(i) = idx_global_rj_r(j)
      end do
      do i = 1, nidx_rj(1)
        j = idx_gl_1d_rj_r(i)
        radius_1d_rj_r(i) = radius_1d_gl(j)
      end do
!
      do i = 1, nidx_rj(2)
        j = i - 1 + ist_rj(2)
        idx_gl_1d_rj_j(i,1) = idx_global_rj_j(j,1)
        idx_gl_1d_rj_j(i,2) = idx_global_rj_j(j,2)
        idx_gl_1d_rj_j(i,3) = idx_global_rj_j(j,3)
      end do
!
      end subroutine copy_sph_1d_gl_idx_rj
!
! ----------------------------------------------------------------------
!
      end module copy_sph_1d_global_index
