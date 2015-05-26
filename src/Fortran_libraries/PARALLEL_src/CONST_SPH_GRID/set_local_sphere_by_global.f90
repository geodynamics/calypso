!>@file   set_local_sphere_by_global.f90
!!@brief  module set_local_sphere_by_global
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Copy number of global spherical harmonics indices
!!        to local data
!!
!!
!!@verbatim
!!      subroutine copy_gl_2_local_sph_param(ip_rank)
!!
!!      subroutine copy_gl_2_local_rj_param(ip_rank)
!!      subroutine copy_gl_2_local_rlm_param(ip_rank)
!!      subroutine copy_gl_2_local_rtm_param(ip_rank)
!!      subroutine copy_gl_2_local_rtp_param(ip_rank)
!!@endverbatim
!
      module set_local_sphere_by_global
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_gl_2_local_sph_param(ip_rank)
!
      use m_spheric_parameter
      use m_sph_global_parameter
      use m_spheric_global_ranks
      use m_sph_1d_global_index
!
      integer(kind = kint), intent(in) :: ip_rank
!
!
      call copy_gl_2_local_rtp_param(ip_rank)
      call copy_gl_2_local_rtm_param(ip_rank)
      call copy_gl_2_local_rlm_param(ip_rank)
      call copy_gl_2_local_rj_param(ip_rank)
!
      end subroutine copy_gl_2_local_sph_param
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_gl_2_local_rj_param(ip_rank)
!
      use m_spheric_parameter
      use m_sph_global_parameter
      use m_spheric_global_ranks
      use m_sph_1d_global_index
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint) :: i1, i2
!
!
      sph_rank_rj(1:2) =  iglobal_rank_rj(1:2,ip_rank)
!
      nnod_rj =  nnod_local_rj(ip_rank+1)
!
      nidx_rj(1:2) =  nidx_local_rj(ip_rank+1,1:2)
!
      i1 = sph_rank_rj(1) + 1
      i2 = sph_rank_rj(2) + 1
      ist_rj(1) =  istack_idx_local_rj_r(i1-1) + 1
      ist_rj(2) =  istack_idx_local_rj_j(i2-1) + 1
      ied_rj(1) =  istack_idx_local_rj_r(i1)
      ied_rj(2) =  istack_idx_local_rj_j(i2)
!
      end subroutine copy_gl_2_local_rj_param
!
! -----------------------------------------------------------------------
!
      subroutine copy_gl_2_local_rlm_param(ip_rank)
!
      use m_spheric_parameter
      use m_sph_global_parameter
      use m_spheric_global_ranks
      use m_sph_1d_global_index
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint) :: i1, i2
!
!
      sph_rank_rlm(1:2) = iglobal_rank_rlm(1:2,ip_rank)
!
      nnod_rlm = nnod_local_rlm(ip_rank+1)
!
      nidx_rlm(1:2) = nidx_local_rlm(ip_rank+1,1:2)
!
      i1 = sph_rank_rlm(1) + 1
      i2 = sph_rank_rlm(2) + 1
      ist_rlm(1) = istack_idx_local_rlm_r(i1-1) + 1
      ist_rlm(2) = istack_idx_local_rlm_j(i2-1) + 1
      ied_rlm(1) = istack_idx_local_rlm_r(i1)
      ied_rlm(2) = istack_idx_local_rlm_j(i2)
!
      end subroutine copy_gl_2_local_rlm_param
!
! -----------------------------------------------------------------------
!
      subroutine copy_gl_2_local_rtm_param(ip_rank)
!
      use m_spheric_parameter
      use m_sph_global_parameter
      use m_spheric_global_ranks
      use m_sph_1d_global_index
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint) :: i1, i2, i3
!
!
      sph_rank_rtm(1:3) = iglobal_rank_rtm(1:3,ip_rank)
!
      nnod_rtm = nnod_local_rtm(ip_rank+1)
!
      nidx_rtm(1:3) = nidx_local_rtm(ip_rank+1,1:3)
!
      i1 = sph_rank_rtm(1) + 1
      i2 = sph_rank_rtm(2) + 1
      i3 = sph_rank_rtm(3) + 1
      ist_rtm(1) = istack_idx_local_rtm_r(i1-1) + 1
      ist_rtm(2) = istack_idx_local_rtm_t(i2-1) + 1
      ist_rtm(3) = istack_idx_local_rtm_m(i3-1) + 1
      ied_rtm(1) = istack_idx_local_rtm_r(i1)
      ied_rtm(2) = istack_idx_local_rtm_t(i2)
      ied_rtm(3) = istack_idx_local_rtm_m(i3)
!
      end subroutine copy_gl_2_local_rtm_param
!
! -----------------------------------------------------------------------
!
      subroutine copy_gl_2_local_rtp_param(ip_rank)
!
      use m_spheric_parameter
      use m_sph_global_parameter
      use m_spheric_global_ranks
      use m_sph_1d_global_index
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint) :: i1, i2, i3
!
!
      sph_rank_rtp(1:3) = iglobal_rank_rtp(1:3,ip_rank)
!
      nnod_rtp = nnod_local_rtp(ip_rank+1)
!
      nidx_rtp(1:3) = nidx_local_rtp(ip_rank+1,1:3)
!
      i1 = sph_rank_rtp(1) + 1
      i2 = sph_rank_rtp(2) + 1
      i3 = sph_rank_rtp(3) + 1
      ist_rtp(1) = istack_idx_local_rtp_r(i1-1) + 1
      ist_rtp(2) = istack_idx_local_rtp_t(i2-1) + 1
      ist_rtp(3) = istack_idx_local_rtp_p(i3-1) + 1
      ied_rtp(1) = istack_idx_local_rtp_r(i1)
      ied_rtp(2) = istack_idx_local_rtp_t(i2)
      ied_rtp(3) = istack_idx_local_rtp_p(i3)
!
      end subroutine copy_gl_2_local_rtp_param
!
! -----------------------------------------------------------------------
!
      end module set_local_sphere_by_global
