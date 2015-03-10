!>@file   legendre_bwd_trans_testloop.f90
!!@brief  module legendre_bwd_trans_testloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_test(ncomp, nvector,         &
!!     &          irev_sr_rlm, n_WR, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_test(ncomp, nvector, nscalar,&
!!     &          irev_sr_rlm, n_WR, WR, WS)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_testloop
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
      subroutine legendre_b_trans_vector_test(ncomp, nvector, nscalar,  &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: nl_rtm, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp),  nkr(np_smp)
      integer(kind = kint) :: nkrs(np_smp),  nkrt(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      elaps(1:4) = 0
      nl_rtm = (nidx_rtm(2) + 1)/2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
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
          call set_sp_rlm_vector_sym_matmul                             &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, irev_sr_rlm, n_WR, WR,                    &
     &        pol_e(1,ip), tor_e(1,ip), pol_o(1,ip), tor_o(1,ip) )
          call set_sp_rlm_scalar_sym_matmul                             &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, nscalar, irev_sr_rlm, n_WR, WR,           &
     &        pol_e(1,ip), pol_o(1,ip) )
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
!          st_elapsed = MPI_WTIME()
          call matmul_bwd_leg_trans(nl_rtm, nkrs(ip), n_jk_e(ip),       &
     &        Ps_rtm(1,jst(ip)+1), pol_e(1,ip), symp_r(1,ip))
          call matmul_bwd_leg_trans(nl_rtm, nkrt(ip), n_jk_e(ip),       &
     &        dPsdt_rtm(1,jst(ip)+1), tor_e(1,ip), asmp_p(1,ip))
!   odd l-m
          call matmul_bwd_leg_trans(nl_rtm, nkrs(ip), n_jk_o(ip),       &
     &        Ps_rtm(1,jst_h(ip)), pol_o(1,ip), asmp_r(1,ip))
          call matmul_bwd_leg_trans(nl_rtm, nkrt(ip), n_jk_o(ip),       &
     &        dPsdt_rtm(1,jst_h(ip)), tor_o(1,ip), symp_p(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_vector_sym_matmul                             &
     &       (kst(ip), nkr(ip), mp_rlm, mn_rlm, nl_rtm,                 &
     &        symp_r(1,ip), asmp_p(1,ip), asmp_r(1,ip), symp_p(1,ip),   &
     &        ncomp, nvector, irev_sr_rtm, n_WS, WS)
          call cal_vr_rtm_scalar_sym_matmul(kst(ip), nkr(ip),           &
     &        mp_rlm, nl_rtm, symp_r(1,ip), asmp_r(1,ip),               &
     &        ncomp, nvector, nscalar, irev_sr_rtm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine legendre_b_trans_vector_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_vector_sym_matmul(kst, nkr, jst,            &
     &          n_jk_e, n_jk_o, ncomp, nvector, irev_sr_rlm, n_WR, WR,  &
     &          pol_e, tor_e, pol_o, tor_o)
!
      use m_schmidt_poly_on_rtm
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: pol_e(n_jk_e,ncomp*nkr)
      real(kind = kreal), intent(inout) :: tor_e(n_jk_e,2*nvector*nkr)
      real(kind = kreal), intent(inout) :: pol_o(n_jk_o,ncomp*nkr)
      real(kind = kreal), intent(inout) :: tor_o(n_jk_o,2*nvector*nkr)
!
      integer(kind = kint) :: jj, kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: j_rlm, i_rlm, i_recv, nkrv
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
      real(kind = kreal) :: g3, gm
!
!
      nkrv = nkr * nvector
      do kk = 1, nkrv
        kr_nd = kk + kst*nvector
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
        a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
!   even l-m
        do jj = 1, n_jk_e
          j_rlm = 2*jj + jst - 1
          g3 = g_sph_rlm(j_rlm,3)
          gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
          pol_e(jj,kk) =        WR(i_recv-2) * a2r_1d_rlm_r * g3
          tor_e(jj,kk+nkrv) =   WR(i_recv-1) * a1r_1d_rlm_r
          pol_e(jj,kk+2*nkrv) = WR(i_recv-1) * a1r_1d_rlm_r * gm
          tor_e(jj,kk) =        WR(i_recv  ) * a1r_1d_rlm_r
          pol_e(jj,kk+nkrv) =   WR(i_recv  ) * a1r_1d_rlm_r * gm
        end do
!   odd l-m
        do jj = 1, n_jk_o
          j_rlm = 2*jj + jst
          g3 = g_sph_rlm(j_rlm,3)
          gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
          pol_o(jj,kk) =        WR(i_recv-2) * a2r_1d_rlm_r * g3
          tor_o(jj,kk+nkrv) =   WR(i_recv-1) * a1r_1d_rlm_r
          pol_o(jj,kk+2*nkrv) = WR(i_recv-1) * a1r_1d_rlm_r * gm
          tor_o(jj,kk) =        WR(i_recv  ) * a1r_1d_rlm_r
          pol_o(jj,kk+nkrv) =   WR(i_recv  ) * a1r_1d_rlm_r * gm
        end do
      end do
!
      end subroutine set_sp_rlm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scalar_sym_matmul(kst, nkr, jst,            &
     &          n_jk_e, n_jk_o, ncomp, nvector, nscalar, irev_sr_rlm,   &
     &          n_WR, WR, scl_e, scl_o)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: scl_e(n_jk_e,ncomp*nkr)
      real(kind = kreal), intent(inout) :: scl_o(n_jk_o,ncomp*nkr)
!
      integer(kind = kint) :: jj, kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: i_rlm, i_recv, nkrv
!
!
      nkrv = nkr * nvector
      do kk = 1, nkr*nscalar
        kr_nd = kk + kst*nscalar
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!   even l-m
        do jj = 1, n_jk_e
          i_rlm = 1 + (2*jj + jst - 2) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
          scl_e(jj,kk+3*nkrv) = WR(i_recv)
        end do
!   odd l-m
        do jj = 1, n_jk_o
          i_rlm = 1 + (2*jj + jst - 1) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
          scl_o(jj,kk+3*nkrv) = WR(i_recv)
        end do
      end do
!
      end subroutine set_sp_rlm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_vector_sym_matmul                           &
     &       (kst, nkr, mp_rlm, mn_rlm, nl_rtm, symp_r, asmp_p,         &
     &        asmp_r, symp_p, ncomp, nvector, irev_sr_rtm, n_WS, WS)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nl_rtm
!
      real(kind = kreal), intent(inout) :: symp_r(nl_rtm,ncomp*nkr)
      real(kind = kreal), intent(in) ::    asmp_p(nl_rtm,2*nvector*nkr)
      real(kind = kreal), intent(inout) :: asmp_r(nl_rtm,ncomp*nkr)
      real(kind = kreal), intent(in) ::    symp_p(nl_rtm,2*nvector*nkr)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
!
!
      nkrv = nkr * nvector
      do kk = 1, nkrv
        do lp_rtm = 1, nl_rtm
          symp_r(lp_rtm,kk+nkrv)                                        &
     &         = - symp_r(lp_rtm,kk+nkrv) *   asin_theta_1d_rtm(lp_rtm)
          symp_r(lp_rtm,kk+2*nkrv)                                      &
     &         = - symp_r(lp_rtm,kk+2*nkrv) * asin_theta_1d_rtm(lp_rtm)
          asmp_r(lp_rtm,kk+nkrv)                                        &
     &         = - asmp_r(lp_rtm,kk+nkrv) *   asin_theta_1d_rtm(lp_rtm)
          asmp_r(lp_rtm,kk+2*nkrv)                                      &
     &         = - asmp_r(lp_rtm,kk+2*nkrv) * asin_theta_1d_rtm(lp_rtm)
        end do
      end do
!
      do kk = 1, nkrv
        kr_nd = kk + kst*nvector
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do lp_rtm = 1, nidx_rtm(2)/2
          ln_rtm =  nidx_rtm(2) - lp_rtm + 1
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
          ipn_send = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
          inn_send = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
          WS(ipp_send-2) = WS(ipp_send-2)                               &
     &            + symp_r(lp_rtm,kk) +     asmp_r(lp_rtm,kk)
          WS(ipp_send-1) = WS(ipp_send-1)                               &
     &            + asmp_p(lp_rtm,kk+nkrv) + symp_p(lp_rtm,kk+nkrv)
          WS(ipp_send  ) = WS(ipp_send  )                               &
     &            - asmp_p(lp_rtm,kk) -     symp_p(lp_rtm,kk)
!
          WS(inp_send-1) = WS(inp_send-1)                               &
     &            + symp_r(lp_rtm,kk+nkrv) +   asmp_r(lp_rtm,kk+nkrv)
          WS(inp_send  ) = WS(inp_send  )                               &
     &            + symp_r(lp_rtm,kk+2*nkrv) + asmp_r(lp_rtm,kk+2*nkrv)
!
!
          WS(ipn_send-2) = WS(ipn_send-2)                               &
     &            + symp_r(lp_rtm,kk) -     asmp_r(lp_rtm,kk)
          WS(ipn_send-1) = WS(ipn_send-1)                               &
     &            - asmp_p(lp_rtm,kk+nkrv) + symp_p(lp_rtm,kk+nkrv)
          WS(ipn_send  ) = WS(ipn_send  )                               &
     &            + asmp_p(lp_rtm,kk) -     symp_p(lp_rtm,kk)
!
          WS(inn_send-1) = WS(inn_send-1)                               &
     &            + symp_r(lp_rtm,kk+nkrv) -   asmp_r(lp_rtm,kk+nkrv)
          WS(inn_send  ) = WS(inn_send  )                               &
     &            + symp_r(lp_rtm,kk+2*nkrv) - asmp_r(lp_rtm,kk+2*nkrv)
        end do
!
        do lp_rtm = nidx_rtm(2)/2+1, nl_rtm
          ln_rtm =  nidx_rtm(2) - nidx_rtm(2)/2-1 + 1
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
          WS(ipp_send-2) = WS(ipp_send-2) + symp_r(lp_rtm,kk)
          WS(ipp_send-1) = WS(ipp_send-1) + symp_p(lp_rtm,kk+nkrv)
          WS(ipp_send  ) = WS(ipp_send  ) - symp_p(lp_rtm,kk)
!
          WS(inp_send-1) = WS(inp_send-1) + symp_r(lp_rtm,kk+nkrv)
          WS(inp_send  ) = WS(inp_send  ) + symp_r(lp_rtm,kk+2*nkrv)
        end do
      end do
!
      end subroutine cal_vr_rtm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scalar_sym_matmul(kst, nkr, mp_rlm,         &
     &          nl_rtm, symp, asmp, ncomp, nvector, nscalar,            &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nl_rtm
      real(kind = kreal), intent(in) :: symp(nl_rtm,ncomp*nkr)
      real(kind = kreal), intent(in) :: asmp(nl_rtm,ncomp*nkr)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_send, ipn_send
!
!
      nkrv = nkr * nvector
      do kk = 1, nkr*nscalar
        kr_nd = kk + kst*nscalar
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do lp_rtm = 1, nidx_rtm(2)/2
          ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
!
          ipp_send = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_send = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
          WS(ipp_send) = WS(ipp_send)                                   &
     &                + symp(lp_rtm,kk+3*nkrv) + asmp(lp_rtm,kk+3*nkrv)
          WS(ipn_send) = WS(ipn_send)                                   &
     &                + symp(lp_rtm,kk+3*nkrv) - asmp(lp_rtm,kk+3*nkrv)
        end do
!
        do lp_rtm = nidx_rtm(2)/2+1, nl_rtm
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_send = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
          WS(ipp_send) = WS(ipp_send) + symp(lp_rtm,kk+3*nkrv)
        end do
      end do
!
      end subroutine cal_vr_rtm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_testloop
