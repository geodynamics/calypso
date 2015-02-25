!>@file   legendre_fwd_trans_testloop.f90
!!@brief  module legendre_fwd_trans_testloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_test(ncomp, nvector,         &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_f_trans_scalar_test(ncomp, nvector, nscalar, &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_fwd_trans_testloop
!
      use m_precision
!
      use m_constants
      use m_work_time
      use calypso_mpi
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_legendre_work_testlooop
      use matmul_for_legendre_trans
!
      implicit none
!
      real(kind = kreal), private :: st_elapsed
      real(kind = kreal), private :: elaps(4)
      integer, external :: omp_get_max_threads
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_vector_test(ncomp, nvector, nscalar,  &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, mp_rlm, mn_rlm, nle_rtm, nlo_rtm
      integer(kind = kint) :: kst(np_smp),  nkr(np_smp)
      integer(kind = kint) :: nkrs(np_smp),  nkrt(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      elaps(1:4) = 0
      nle_rtm = (nidx_rtm(2) + 1)/2
      nlo_rtm = nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1)
        nkrs(ip) = ncomp*nkr(ip)
        nkrt(ip) = 2*nvector*nkr(ip)
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          jst(ip) = lstack_rlm(mp_rlm-1)
          jst_h(ip) = lstack_even_rlm(mp_rlm) + 1
          n_jk_e(ip) = lstack_even_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
          n_jk_o(ip) = lstack_rlm(mp_rlm) - lstack_even_rlm(mp_rlm)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vector_sym_matmul                             &
     &       (kst(ip), nkr(ip), mp_rlm, mn_rlm, nle_rtm, nlo_rtm,       &
     &        ncomp, nvector, irev_sr_rtm, n_WR, WR,                    &
     &        symp_r(1,ip), asmp_p(1,ip), asmp_r(1,ip), symp_p(1,ip) )
          call set_vr_rtm_scalar_sym_matmul                             &
     &       (kst(ip), nkr(ip), mp_rlm, nle_rtm, nlo_rtm,               &
     &        ncomp, nvector, nscalar, irev_sr_rtm, n_WR, WR,           &
     &        symp_r(1,ip), asmp_r(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
!  even l-m
          call matmul_fwd_leg_trans(nkrs(ip), n_jk_e(ip), nth_hemi_rtm, &
     &        symp_r(1,ip), Ps_rtm(1,jst(ip)+1), pol_e(1,ip))
          call matmul_fwd_leg_trans(nkrt(ip), n_jk_e(ip), nth_hemi_rtm, &
     &        asmp_p(1,ip), dPsdt_rtm(1,jst(ip)+1), tor_e(1,ip))
!
!  odd l-m
          call matmul_fwd_leg_trans(nkrs(ip), n_jk_o(ip), nth_hemi_rtm, &
     &        asmp_r(1,ip), Ps_rtm(1,jst_h(ip)), pol_o(1,ip))
          call matmul_fwd_leg_trans(nkrt(ip), n_jk_o(ip), nth_hemi_rtm, &
     &        symp_p(1,ip), dPsdt_rtm(1,jst_h(ip)), tor_o(1,ip))
  !          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vector_sym_matmul                             &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        pol_e(1,ip), pol_o(1,ip), tor_e(1,ip), tor_o(1,ip),       &
     &        ncomp, nvector, irev_sr_rlm, n_WS, WS)
          call cal_sp_rlm_scalar_sym_matmul                             &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        pol_e(1,ip), pol_o(1,ip), ncomp, nvector, nscalar,        &
     &        irev_sr_rlm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine legendre_f_trans_vector_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_vector_sym_matmul                           &
     &         (kst, nkr, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,            &
     &          ncomp, nvector, irev_sr_rtm, n_WR, WR,                           &
     &          symp_r, asmp_p, asmp_r, symp_p)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind=kreal), intent(inout) :: symp_r(ncomp*nkr,nle_rtm)
      real(kind=kreal), intent(inout) :: asmp_p(2*nkr*nvector,nle_rtm)
      real(kind=kreal), intent(inout) :: asmp_r(ncomp*nkr,nle_rtm)
      real(kind=kreal), intent(inout) :: symp_p(2*nkr*nvector,nle_rtm)
!
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      nkrv = nkr * nvector
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm =   weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
          inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
          inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
          symp_r(kk,lp_rtm) =      (WR(ipp_recv-2) + WR(ipn_recv-2))    &
     &                             * wp_rtm
          symp_p(kk+nkrv,lp_rtm) = (WR(ipp_recv-1) + WR(ipn_recv-1))    &
     &                             * wp_rtm
          symp_p(kk,lp_rtm) =      (WR(ipp_recv  ) + WR(ipn_recv  ))    &
     &                             * wp_rtm
!
          asmp_r(kk,lp_rtm) = (WR(ipp_recv-2) - WR(ipn_recv-2))*wp_rtm
          asmp_p(kk+nkrv,lp_rtm) = (WR(ipp_recv-1) - WR(ipn_recv-1))    &
     &                             * wp_rtm
          asmp_p(kk,lp_rtm) = (WR(ipp_recv  ) - WR(ipn_recv  ))*wp_rtm
!
          symp_r(kk+nkrv,lp_rtm) =   (WR(inp_recv-1) + WR(inn_recv-1))  &
     &                             * wp_rtm * asin_rtm
          symp_r(kk+2*nkrv,lp_rtm) = (WR(inp_recv  ) + WR(inn_recv  ))  &
     &                             * wp_rtm * asin_rtm
!
          asmp_r(kk+nkrv,lp_rtm) =   (WR(inp_recv-1) - WR(inn_recv-1))  &
     &                             * wp_rtm * asin_rtm
          asmp_r(kk+2*nkrv,lp_rtm) = (WR(inp_recv  ) - WR(inn_recv  ))  &
     &                             * wp_rtm * asin_rtm
        end do
      end do
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
          symp_r(kk,lp_rtm) =      WR(ipp_recv-2) * wp_rtm
          symp_p(kk+nkrv,lp_rtm) = WR(ipp_recv-1) * wp_rtm
          symp_p(kk,lp_rtm) =      WR(ipp_recv  ) * wp_rtm
!
          asmp_r(kk,lp_rtm) =      0.0d0
          asmp_p(kk+nkrv,lp_rtm) = 0.0d0
          asmp_p(kk,lp_rtm) =      0.0d0
!
          symp_r(kk+nkrv,lp_rtm) =   WR(inp_recv-1) * wp_rtm * asin_rtm
          symp_r(kk+2*nkrv,lp_rtm) = WR(inp_recv  ) * wp_rtm * asin_rtm
!
          asmp_r(kk+nkrv,lp_rtm) =   0.0d0
          asmp_r(kk+2*nkrv,lp_rtm) = 0.0d0
        end do
      end do
!
      end subroutine set_vr_rtm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_scalar_sym_matmul(kst, nkr, mp_rlm,         &
     &          nle_rtm, nlo_rtm, ncomp, nvector, nscalar,              &
     &          irev_sr_rtm, n_WR, WR, symp, asmp)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp(ncomp*nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: asmp(ncomp*nkr,nle_rtm)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_recv, ipn_recv
      real(kind = kreal) :: wp_rtm
!
!
      nkrv = nkr * nvector
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm = weight_rtm(lp_rtm)
        do kk = 1, nkr*nscalar
          kr_nd = kk + kst*nscalar
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_recv = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_recv = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
          symp(kk+3*nkrv,lp_rtm) = (WR(ipp_recv) + WR(ipn_recv))        &
     &                            * wp_rtm
          asmp(kk+3*nkrv,lp_rtm) = (WR(ipp_recv) - WR(ipn_recv))        &
     &                            * wp_rtm
        end do
      end do
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        do kk = 1, nkr*nscalar
          kr_nd = kk + kst*nscalar
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                + (k_rlm-1) *  istep_rtm(1)                   &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_recv = nd + 3*nvector                                 &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
          symp(kk+3*nkrv,lp_rtm) = WR(ipp_recv) * wp_rtm
          asmp(kk+3*nkrv,lp_rtm) = 0.0d0
        end do
      end do
!
      end subroutine set_vr_rtm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_vector_sym_matmul                           &
     &         (kst, nkr, jst, n_jk_o, n_jk_e, pol_e, pol_o,            &
     &          tor_e, tor_o, ncomp, nvector, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout) :: pol_e(ncomp*nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: pol_o(ncomp*nkr,n_jk_o)
      real(kind = kreal), intent(inout) :: tor_e(2*nvector*nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: tor_o(2*nvector*nkr,n_jk_o)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nkrv
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj
      real(kind = kreal) :: g7, gm, r1, r2
!
!
      nkrv = nkr * nvector
      do jj = 1, n_jk_e
        g7 = g_sph_rlm(2*jj+jst-1,7)
        gm = dble(idx_gl_1d_rlm_j(2*jj+jst-1,3))
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
!
          pol_e(kk,jj) =        pol_e(kk,jj) *        r2 * g7
          tor_e(kk+nkrv,jj) =   tor_e(kk+nkrv,jj) *   r1 * g7
          pol_e(kk+2*nkrv,jj) = pol_e(kk+2*nkrv,jj) * r1 * g7 * gm
          tor_e(kk,jj) =        tor_e(kk,jj) *        r1 * g7
          pol_e(kk+nkrv,jj) =   pol_e(kk+nkrv,jj) *   r1 * g7 * gm
        end do
      end do
      do jj = 1, n_jk_o
        g7 = g_sph_rlm(2*jj+jst,7)
        gm = dble(idx_gl_1d_rlm_j(2*jj+jst,3))
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
!
          pol_o(kk,jj) =        pol_o(kk,jj) *        r2 * g7
          tor_o(kk+nkrv,jj) =   tor_o(kk+nkrv,jj) *   r1 * g7
          pol_o(kk+2*nkrv,jj) = pol_o(kk+2*nkrv,jj) * r1 * g7 * gm
          tor_o(kk,jj) =        tor_o(kk,jj) *        r1 * g7
          pol_o(kk+nkrv,jj) =   pol_o(kk+nkrv,jj) *   r1 * g7 * gm
        end do
      end do
!
      do jj = 1, n_jk_o
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp
          io_send = 3*nd + (irev_sr_rlm(io_rlm) - 1) * ncomp
!
!  even l-m
          WS(ie_send-2) = WS(ie_send-2) + pol_e(kk,jj)
          WS(ie_send-1) = WS(ie_send-1)                                 &
     &                   - pol_e(kk+2*nkrv,jj) + tor_e(kk+nkrv,jj)
          WS(ie_send  ) = WS(ie_send  )                                 &
     &                   - pol_e(kk+nkrv,jj) -   tor_e(kk,jj)
!  odd l-m
          WS(io_send-2) = WS(io_send-2) + pol_o(kk,jj)
          WS(io_send-1) = WS(io_send-1)                                 &
     &                   - pol_o(kk+2*nkrv,jj) + tor_o(kk+nkrv,jj)
          WS(io_send  ) = WS(io_send  )                                 &
     &                   - pol_o(kk+nkrv,jj) -   tor_o(kk,jj)
        end do
      end do
!
      do jj = n_jk_o+1, n_jk_e
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp
!
          WS(ie_send-2) = WS(ie_send-2) + pol_e(kk,jj)
          WS(ie_send-1) = WS(ie_send-1)                                 &
     &                   - pol_e(kk+2*nkrv,jj) + tor_e(kk+nkrv,jj)
          WS(ie_send  ) = WS(ie_send  )                                 &
     &                   - pol_e(kk+nkrv,jj) -   tor_e(kk,jj)
        end do
      end do
!
      end subroutine cal_sp_rlm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_scalar_sym_matmul                           &
     &         (kst, nkr, jst, n_jk_o, n_jk_e, scl_e, scl_o,            &
     &          ncomp, nvector, nscalar, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout) :: scl_e(ncomp*nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: scl_o(ncomp*nkr,n_jk_o)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nkrv
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj
      real(kind = kreal) :: g6
!
!
      nkrv = nkr * nvector
      do jj = 1, n_jk_e
        g6 = g_sph_rlm(2*jj+jst-1,6)
        do kk = 1, nkr*nscalar
          scl_e(kk+3*nkrv,jj) = scl_e(kk+3*nkrv,jj) * g6
        end do
      end do
      do jj = 1, n_jk_o
        g6 = g_sph_rlm(2*jj+jst,6)
        do kk = 1, nkr*nscalar
          scl_o(kk+3*nkrv,jj) = scl_o(kk+3*nkrv,jj) * g6
        end do
      end do
!
      do kk = 1, nkr*nscalar
        kr_nd = kk + kst*nscalar
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do jj = 1, n_jk_o
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = nd + 3*nvector + (irev_sr_rlm(ie_rlm) - 1) * ncomp
          io_send = nd + 3*nvector + (irev_sr_rlm(io_rlm) - 1) * ncomp
!
          WS(ie_send) = WS(ie_send) + scl_e(kk+3*nkrv,jj)
          WS(io_send) = WS(io_send) + scl_o(kk+3*nkrv,jj)
        end do
!
        do jj = n_jk_o+1, n_jk_e
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = nd + 3*nvector + (irev_sr_rlm(ie_rlm) - 1) * ncomp
          WS(ie_send) = WS(ie_send) + scl_e(kk+3*nkrv,jj)
        end do
      end do
!
      end subroutine cal_sp_rlm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_testloop
