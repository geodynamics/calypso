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
!!      subroutine copy_sph_1d_gl_idx_rtp                               &
!!     &         (s3d_radius, sph_gl1d, sph_rtp)
!!        type(spheric_global_radius), intent(in) :: s3d_radius
!!        type(sph_1d_global_index), intent(in) :: sph_gl1d
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!      subroutine copy_sph_1d_gl_idx_rtm                               &
!!     &         (s3d_radius, sph_gl1d, sph_rtm)
!!        type(spheric_global_radius), intent(in) :: s3d_radius
!!        type(sph_1d_global_index), intent(in) :: sph_gl1d
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!      subroutine copy_sph_1d_gl_idx_rlm                               &
!!     &         (s3d_radius, sph_gl1d, sph_rlm)
!!        type(spheric_global_radius), intent(in) :: s3d_radius
!!        type(sph_1d_global_index), intent(in) :: sph_gl1d
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!      subroutine copy_sph_1d_gl_idx_rj                                &
!!     &         (s3d_radius, sph_gl1d, sph_rj)
!!        type(spheric_global_radius), intent(in) :: s3d_radius
!!        type(sph_1d_global_index), intent(in) :: sph_gl1d
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!
!!      subroutine add_center_mode_rj(ip_rank, sph_gl1d, sph_rj)
!!        type(sph_1d_global_index), intent(in) :: sph_gl1d
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!@endverbatim
!
!
      module copy_sph_1d_global_index
!
      use m_precision
      use m_constants
!
      use t_spheric_rtp_data
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_spheric_rj_data
      use t_spheric_global_ranks
      use t_sph_1d_global_index
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_1d_gl_idx_rtp                                 &
     &         (s3d_radius, sph_gl1d, sph_rtp)
!
      type(spheric_global_radius), intent(in) :: s3d_radius
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, sph_rtp%nidx_rtp(1)
        j = i - 1 + sph_rtp%ist_rtp(1)
        sph_rtp%idx_gl_1d_rtp_r(i) = sph_gl1d%idx_global_rtp_r(j)
      end do
      do i = 1, sph_rtp%nidx_rtp(1)
        j = sph_rtp%idx_gl_1d_rtp_r(i)
        sph_rtp%radius_1d_rtp_r(i) = s3d_radius%radius_1d_gl(j)
      end do
!
      do i = 1, sph_rtp%nidx_rtp(2)
        j = i - 1 + sph_rtp%ist_rtp(2)
        sph_rtp%idx_gl_1d_rtp_t(i) = sph_gl1d%idx_global_rtp_t(j)
      end do
!
      do i = 1, sph_rtp%nidx_rtp(3)
        j = i - 1 + sph_rtp%ist_rtp(3)
        sph_rtp%idx_gl_1d_rtp_p(i,1) = sph_gl1d%idx_global_rtp_p(j,1)
        sph_rtp%idx_gl_1d_rtp_p(i,2) = sph_gl1d%idx_global_rtp_p(j,2)
      end do
!
      end subroutine copy_sph_1d_gl_idx_rtp
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_1d_gl_idx_rtm                                 &
     &         (s3d_radius, sph_gl1d, sph_rtm)
!
      type(spheric_global_radius), intent(in) :: s3d_radius
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, sph_rtm%nidx_rtm(1)
        j = i - 1 + sph_rtm%ist_rtm(1)
        sph_rtm%idx_gl_1d_rtm_r(i) = sph_gl1d%idx_global_rtm_r(j)
      end do
      do i = 1, sph_rtm%nidx_rtm(1)
        j = sph_rtm%idx_gl_1d_rtm_r(i)
        sph_rtm%radius_1d_rtm_r(i) = s3d_radius%radius_1d_gl(j)
      end do
!
      do i = 1, sph_rtm%nidx_rtm(2)
        j = i - 1 + sph_rtm%ist_rtm(2)
        sph_rtm%idx_gl_1d_rtm_t(i) = sph_gl1d%idx_global_rtm_t(j)
      end do
!
      do i = 1, sph_rtm%nidx_rtm(3)
        j = i - 1 + sph_rtm%ist_rtm(3)
        sph_rtm%idx_gl_1d_rtm_m(i,1) = sph_gl1d%idx_global_rtm_m(j,1)
        sph_rtm%idx_gl_1d_rtm_m(i,2) = sph_gl1d%idx_global_rtm_m(j,2)
      end do
!
      end subroutine copy_sph_1d_gl_idx_rtm
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_1d_gl_idx_rlm                                 &
     &         (s3d_radius, sph_gl1d, sph_rlm)
!
      type(spheric_global_radius), intent(in) :: s3d_radius
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, sph_rlm%nidx_rlm(1)
        j = i - 1 + sph_rlm%ist_rlm(1)
        sph_rlm%idx_gl_1d_rlm_r(i) = sph_gl1d%idx_global_rlm_r(j)
      end do
      do i = 1, sph_rlm%nidx_rlm(1)
        j = sph_rlm%idx_gl_1d_rlm_r(i)
        sph_rlm%radius_1d_rlm_r(i) = s3d_radius%radius_1d_gl(j)
      end do
!
      do i = 1, sph_rlm%nidx_rlm(2)
        j = i - 1 + sph_rlm%ist_rlm(2)
        sph_rlm%idx_gl_1d_rlm_j(i,1) = sph_gl1d%idx_global_rlm_j(j,1)
        sph_rlm%idx_gl_1d_rlm_j(i,2) = sph_gl1d%idx_global_rlm_j(j,2)
        sph_rlm%idx_gl_1d_rlm_j(i,3) = sph_gl1d%idx_global_rlm_j(j,3)
      end do
!
      end subroutine copy_sph_1d_gl_idx_rlm
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_1d_gl_idx_rj                                  &
     &         (s3d_radius, sph_gl1d, sph_rj)
!
      type(spheric_global_radius), intent(in) :: s3d_radius
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, sph_rj%nidx_rj(1)
        j = i - 1 + sph_rj%ist_rj(1)
        sph_rj%idx_gl_1d_rj_r(i) = sph_gl1d%idx_global_rj_r(j)
      end do
      do i = 1, sph_rj%nidx_rj(1)
        j = sph_rj%idx_gl_1d_rj_r(i)
        sph_rj%radius_1d_rj_r(i) = s3d_radius%radius_1d_gl(j)
      end do
!
      do i = 1, sph_rj%nidx_rj(2)
        j = i - 1 + sph_rj%ist_rj(2)
        sph_rj%idx_gl_1d_rj_j(i,1) = sph_gl1d%idx_global_rj_j(j,1)
        sph_rj%idx_gl_1d_rj_j(i,2) = sph_gl1d%idx_global_rj_j(j,2)
        sph_rj%idx_gl_1d_rj_j(i,3) = sph_gl1d%idx_global_rj_j(j,3)
      end do
!
      end subroutine copy_sph_1d_gl_idx_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_center_mode_rj(ip_rank, sph_gl1d, sph_rj)
!
      integer(kind = kint), intent(in) :: ip_rank
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      integer(kind = kint) :: i, j
!
!
      sph_rj%inod_rj_center = 0
      if(sph_rj%iflag_rj_center .eq. izero) return
!
      do i = 1, sph_rj%nidx_rj(2)
        j = i - 1 + sph_rj%ist_rj(2)
        if(sph_gl1d%idx_global_rj_j(j,1) .eq. 0) then
          write(*,*) 'Add center mode at domain ', ip_rank
          sph_rj%nnod_rj = sph_rj%nnod_rj + 1
          sph_rj%inod_rj_center = 1
          exit
        end if
      end do
!
      end subroutine add_center_mode_rj
!
! ----------------------------------------------------------------------
!
      end module copy_sph_1d_global_index
