!
!      module set_sph_1d_global_index
!
!     Written by H. Matsui on July, 2007
!
!>@file   set_sph_1d_global_index.f90
!!@brief  module set_sph_1d_global_index
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set global indices for spherical harominics transform
!!
!!@verbatim
!!      subroutine set_sph_1d_global_idx_rtp                            &
!!     &         (m_folding, nphi, mdx_ispack,                          &
!!     &          s3d_ranks, sph_dbc, stk_lc1d, sph_gl1d)
!!      subroutine set_sph_1d_global_idx_rtm                            &
!!     &         (m_folding, nphi, mtbl_fft_2_lgd, mdx_4_lgd,           &
!!     &          s3d_ranks, sph_dbc, stk_lc1d, sph_gl1d)
!!      subroutine set_sph_1d_global_idx_rlm(jmax, jtbl_fsph, sph_gl1d)
!!      subroutine set_sph_1d_global_idx_rj(jmax, jtbl_rj, sph_gl1d)
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!        type(sph_local_default_BC), intent(in) :: sph_dbc
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_1d_global_index), intent(inout) :: sph_gl1d
!!@endverbatim
!
      module set_sph_1d_global_index
!
      use m_precision
!
      use t_spheric_global_ranks
      use t_sph_1d_global_index
      use t_sph_local_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_1d_global_idx_rtp                              &
     &         (m_folding, nphi, mdx_ispack,                            &
     &          s3d_ranks, sph_dbc, stk_lc1d, sph_gl1d)
!
      integer(kind = kint), intent(in) :: m_folding, nphi
      integer(kind = kint), intent(in) :: mdx_ispack(nphi)
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_local_default_BC), intent(in) :: sph_dbc
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
!
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
      integer(kind = kint) :: ist, ied
      integer(kind = kint) :: icou, i, ip, kr
!
!
      do ip  = 1, s3d_ranks%ndomain_rtp(1)
        icou = stk_lc1d%istack_idx_local_rtp_r(ip-1)
!
        do i = 1, sph_dbc%nidx_local_rtp_IC(ip)
          icou = icou + 1
          kr = i + sph_dbc%ist_idx_local_rtp_IC(ip)
          sph_gl1d%idx_global_rtp_r(icou) = kr
        end do
!
        do i = 1, sph_dbc%nidx_local_rtp_OC(ip)
          icou = icou + 1
          kr = i + sph_dbc%ist_idx_local_rtp_OC(ip)
          sph_gl1d%idx_global_rtp_r(icou) = kr
        end do
!
        do i = 1, sph_dbc%nidx_local_rtp_MT(ip)
          icou = icou + 1
          kr = i + sph_dbc%ist_idx_local_rtp_MT(ip)
          sph_gl1d%idx_global_rtp_r(icou) = kr
        end do
      end do
!
!
      do ip = 1, s3d_ranks%ndomain_rtp(2)
        ist = stk_lc1d%istack_idx_local_rtp_t(ip-1) + 1
        ied = stk_lc1d%istack_idx_local_rtp_t(ip)
        do i = ist, ied
          sph_gl1d%idx_global_rtp_t(i) = i
        end do
      end do
!
      do ip = 1, s3d_ranks%ndomain_rtp(3)
        ist = stk_lc1d%istack_idx_local_rtp_p(ip-1) + 1
        ied = stk_lc1d%istack_idx_local_rtp_p(ip)
        do i = ist, ied
          sph_gl1d%idx_global_rtp_p(i,1) = i
          sph_gl1d%idx_global_rtp_p(i,2) = mdx_ispack(i) * m_folding
        end do
      end do
!
      end subroutine set_sph_1d_global_idx_rtp
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_1d_global_idx_rtm                              &
     &         (m_folding, nphi, mtbl_fft_2_lgd, mdx_4_lgd,             &
     &          s3d_ranks, sph_dbc, stk_lc1d, sph_gl1d)
!
      integer(kind = kint), intent(in) :: m_folding, nphi
      integer(kind = kint), intent(in) :: mtbl_fft_2_lgd(0:nphi)
      integer(kind = kint), intent(in) :: mdx_4_lgd(0:nphi)
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_local_default_BC), intent(in) :: sph_dbc
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
!
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
      integer(kind = kint) :: n1, n2, n3
      integer(kind = kint) :: icou, i, ip
!
!
      n1 = s3d_ranks%ndomain_rtm(1)
      n2 = s3d_ranks%ndomain_rtm(2)
      n3 = s3d_ranks%ndomain_rtm(3)
!
      do ip  = 1, n1
        icou = stk_lc1d%istack_idx_local_rtm_r(ip-1)
!
        do i = 1, sph_dbc%nidx_local_rtm_IC(ip)
          icou = icou + 1
          sph_gl1d%idx_global_rtm_r(icou)                               &
     &         = i + sph_dbc%ist_idx_local_rtm_IC(ip)
        end do
!
        do i = 1, sph_dbc%nidx_local_rtm_OC(ip)
          icou = icou + 1
          sph_gl1d%idx_global_rtm_r(icou)                               &
     &         = i + sph_dbc%ist_idx_local_rtm_OC(ip)
        end do
!
        do i = 1, sph_dbc%nidx_local_rtm_MT(ip)
          icou = icou + 1
          sph_gl1d%idx_global_rtm_r(icou)                               &
     &         = i + sph_dbc%ist_idx_local_rtm_MT(ip)
        end do
!
      end do
!
!
      do i = 1, sph_gl1d%num_gl_rtm_t
        sph_gl1d%idx_global_rtm_t(i) = i
      end do
!
      do i = 0, sph_gl1d%num_gl_rtm_m
        sph_gl1d%idx_global_rtm_m(i,1) = mtbl_fft_2_lgd(i)
        sph_gl1d%idx_global_rtm_m(i,2) = mdx_4_lgd(i) * m_folding
      end do
!
!      write(*,*) 'i,j, idx_global_rtm_m(j,1:2)'
!      do i = 0, sph_gl1d%num_gl_rtm_m
!        write(*,*) i, sph_gl1d%idx_global_rtm_m(i,1:2)
!      end do
!
      end subroutine set_sph_1d_global_idx_rtm
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_1d_global_idx_rlm(jmax, jtbl_fsph, sph_gl1d)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: jtbl_fsph(0:jmax,3)
!
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
      integer(kind = kint) :: i
!
!
      do i = 1, sph_gl1d%num_gl_rlm_r
        sph_gl1d%idx_global_rlm_r(i) = sph_gl1d%idx_global_rtm_r(i)
      end do
!
      do i = 0, sph_gl1d%num_gl_rlm_j
        sph_gl1d%idx_global_rlm_j(i,1) = jtbl_fsph(i,1)
        sph_gl1d%idx_global_rlm_j(i,2) = jtbl_fsph(i,2)
        sph_gl1d%idx_global_rlm_j(i,3) = jtbl_fsph(i,3)
      end do
!
!      write(*,*) 'i, jtbl_fsph(i,1:3)'
!      do i = 0, sph_gl1d%num_gl_rlm_j
!        write(*,*) i, jtbl_fsph(i,1:3)
!      end do
!
      end subroutine set_sph_1d_global_idx_rlm
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_1d_global_idx_rj(jmax, jtbl_rj, sph_gl1d)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: jtbl_rj(0:jmax,3)
!
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
      integer(kind = kint) :: k, j
!
!
      do k = 1, sph_gl1d%nun_gl_rj_r
        sph_gl1d%idx_global_rj_r(k) = k
      end do
!
      do j = 0, sph_gl1d%num_gl_rj_j
        sph_gl1d%idx_global_rj_j(j,1) = jtbl_rj(j,1)
        sph_gl1d%idx_global_rj_j(j,2) = jtbl_rj(j,2)
        sph_gl1d%idx_global_rj_j(j,3) = jtbl_rj(j,3)
      end do
!
!      write(8,*) 'j, sph_gl1d%idx_global_rj_j(j,1:3)'
!      do j = 0, sph_gl1d%num_gl_rj_j
!        write(8,*) j, sph_gl1d%idx_global_rj_j(j,1:3)
!      end do
!
      end subroutine set_sph_1d_global_idx_rj
!
! ----------------------------------------------------------------------
!
      end module set_sph_1d_global_index
