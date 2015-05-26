!>@file   copy_sph_1d_global_index.f90
!!@brief  module copy_sph_1d_global_index
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Copy global spherical harmonics indices in local array
!!
!!
!!@verbatim
!!      subroutine copy_sph_1d_gl_idx_rtp
!!      subroutine copy_sph_1d_gl_idx_rtm
!!      subroutine copy_sph_1d_gl_idx_rlm
!!      subroutine copy_sph_1d_gl_idx_rj
!!
!!      subroutine add_center_mode_rj
!!@endverbatim
!
!
      module copy_sph_1d_global_index
!
      use m_precision
      use m_constants
!
      use m_spheric_parameter
      use m_sph_1d_global_index
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_1d_gl_idx_rtp
!
      use m_sph_mesh_1d_connect
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
      use m_sph_mesh_1d_connect
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
      use m_sph_mesh_1d_connect
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
      use m_sph_mesh_1d_connect
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
! ----------------------------------------------------------------------
!
      subroutine add_center_mode_rj
!
      integer(kind = kint) :: i, j
!
!
      inod_rj_center = 0
      if(iflag_rj_center .eq. izero) return
!
      do i = 1, nidx_rj(2)
        j = i - 1 + ist_rj(2)
        if(idx_global_rj_j(j,1) .eq. 0) then
          write(*,*) 'Add center mode!!'
          nnod_rj = nnod_rj + 1
          inod_rj_center = 1
          exit
        end if
      end do
!
      end subroutine add_center_mode_rj
!
! ----------------------------------------------------------------------
!
      end module copy_sph_1d_global_index
