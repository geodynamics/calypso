!
!      module set_sph_1d_global_index
!
!     Written by H. Matsui on July, 2007
!
!      subroutine set_sph_1d_global_idx_rtp
!      subroutine set_sph_1d_global_idx_rtm
!      subroutine set_sph_1d_global_idx_rlm
!      subroutine set_sph_1d_global_idx_rj
!
      module set_sph_1d_global_index
!
      use m_precision
!
      use m_spheric_parameter
      use m_sph_1d_global_index
      use m_2d_sph_trans_table
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_1d_global_idx_rtp
!
      use m_sph_global_parameter
!
      integer(kind = kint) :: ist, ied
      integer(kind = kint) :: icou, i, ip, kr
!
!
      do ip  = 1, ndomain_rtp(1)
        icou = istack_idx_local_rtp_r(ip-1)
!
        do i = 1, nidx_local_rtp_IC(ip)
          icou = icou + 1
          kr = i + ist_idx_local_rtp_IC(ip)
          idx_global_rtp_r(icou) = kr
        end do
!
        do i = 1, nidx_local_rtp_OC(ip)
          icou = icou + 1
          kr = i + ist_idx_local_rtp_OC(ip)
          idx_global_rtp_r(icou) = kr
        end do
!
        do i = 1, nidx_local_rtp_MT(ip)
          icou = icou + 1
          kr = i + ist_idx_local_rtp_MT(ip)
          idx_global_rtp_r(icou) = kr
        end do
      end do
!
!
      do ip = 1, ndomain_rtp(2)
        ist = istack_idx_local_rtp_t(ip-1) + 1
        ied = istack_idx_local_rtp_t(ip)
        do i = ist, ied
          idx_global_rtp_t(i) = i
        end do
      end do
!
      do ip = 1, ndomain_rtp(3)
        ist = istack_idx_local_rtp_p(ip-1) + 1
        ied = istack_idx_local_rtp_p(ip)
        do i = ist, ied
          idx_global_rtp_p(i,1) = i
          idx_global_rtp_p(i,2) = mdx_ispack(i)
        end do
      end do
!
      end subroutine set_sph_1d_global_idx_rtp
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_1d_global_idx_rtm
!
      use m_sph_global_parameter
!
      integer(kind = kint) :: n1, n2, n3
      integer(kind = kint) :: icou, i, ip
!
!
      n1 = ndomain_rtm(1)
      n2 = ndomain_rtm(2)
      n3 = ndomain_rtm(3)
!
      do ip  = 1, n1
        icou = istack_idx_local_rtm_r(ip-1)
!
        do i = 1, nidx_local_rtm_IC(ip)
          icou = icou + 1
          idx_global_rtm_r(icou) = i + ist_idx_local_rtm_IC(ip)
        end do
!
        do i = 1, nidx_local_rtm_OC(ip)
          icou = icou + 1
          idx_global_rtm_r(icou) = i + ist_idx_local_rtm_OC(ip)
        end do
!
        do i = 1, nidx_local_rtm_MT(ip)
          icou = icou + 1
          idx_global_rtm_r(icou) = i + ist_idx_local_rtm_MT(ip)
        end do
!
      end do
!
!
      do i = 1, istack_idx_local_rtm_t(n2)
        idx_global_rtm_t(i) = i
      end do
!
      do i = 0, istack_idx_local_rtm_m(n3)
        idx_global_rtm_m(i,1) = mtbl_fft_2_lgd(i)
        idx_global_rtm_m(i,2) = mdx_4_lgd(i)
      end do
!
!      write(8,*) 'i,j, idx_global_rtm_m(j,1:2)'
!      do i = 0, istack_idx_local_rtm_m(n3)
!        write(8,*) i, idx_global_rtm_m(i,1:2)
!      end do
!
      end subroutine set_sph_1d_global_idx_rtm
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_1d_global_idx_rlm
!
      integer(kind = kint) :: n1, n2, i
!
!
      n1 = ndomain_rlm(1)
      n2 = ndomain_rlm(2)
!
      do i = 1, istack_idx_local_rlm_r(n1)
        idx_global_rlm_r(i) = idx_global_rtm_r(i)
      end do
!
      do i = 0, istack_idx_local_rlm_j(n2)
        idx_global_rlm_j(i,1) = jtbl_fsph(i,1)
        idx_global_rlm_j(i,2) = jtbl_fsph(i,2)
        idx_global_rlm_j(i,3) = jtbl_fsph(i,3)
      end do
!
!      write(8,*) 'i, jtbl_fsph(i,1:3)'
!      do i = 0, istack_idx_local_rlm_j(n2)
!        write(8,*) i, jtbl_fsph(i,1:3)
!      end do
!
      end subroutine set_sph_1d_global_idx_rlm
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_1d_global_idx_rj
!
      integer(kind = kint) :: n1, n2, j
!
!
      n1 = ndomain_rj(1)
      n2 = ndomain_rj(2)
!
      do j = 1, istack_idx_local_rj_r(n1)
        idx_global_rj_r(j) = j
      end do
!
      do j = 0, istack_idx_local_rj_j(n2)
        idx_global_rj_j(j,1) = jtbl_rj(j,1)
        idx_global_rj_j(j,2) = jtbl_rj(j,2)
        idx_global_rj_j(j,3) = jtbl_rj(j,3)
      end do
!
!      write(8,*) 'j, idx_global_rj_j(j,1:3)'
!      do j = 0, istack_idx_local_rj_j(n2)
!        write(8,*) j, idx_global_rj_j(j,1:3)
!      end do
!
      end subroutine set_sph_1d_global_idx_rj
!
! ----------------------------------------------------------------------
!
      end module set_sph_1d_global_index
