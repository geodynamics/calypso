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
!!      subroutine legendre_b_trans_vector_test(ncomp, nvector, nscalar,&
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm,                         &
!!     &          n_WR, n_WS, WR, WS, WK_l_tst)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
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
      use matmul_for_legendre_trans
!
      use t_legendre_work_testlooop
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
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
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm,                           &
     &          n_WR, n_WS, WR, WS, WK_l_tst)
!
!      use set_from_recv_leg_inner_omp
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in)                                    &
     &           :: asin_theta_1d_rtm(sph_rtm%nidx_rtm(2))
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: mp_rlm, mn_rlm
      integer(kind = kint) :: nkrs,  nkrt
      integer(kind = kint) :: jst, jst_h
      integer(kind = kint) :: n_jk_e, n_jk_o
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      elaps(1:4) = 0
      nkrs = ncomp * sph_rlm%nidx_rlm(1)
      nkrt = 2*nvector * sph_rlm%nidx_rlm(1)
!
      do mp_rlm = 1, sph_rtm%nidx_rtm(3)
        mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
        jst = idx_trns%lstack_rlm(mp_rlm-1)
        jst_h = idx_trns%lstack_even_rlm(mp_rlm) + 1
        n_jk_e = idx_trns%lstack_even_rlm(mp_rlm)                       &
     &          - idx_trns%lstack_rlm(mp_rlm-1)
        n_jk_o = idx_trns%lstack_rlm(mp_rlm)                            &
     &          - idx_trns%lstack_even_rlm(mp_rlm)
!
!        st_elapsed = MPI_WTIME()
        call set_sp_rlm_vec_sym_inner_omp                               &
     &     (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,      &
     &      sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r, g_sph_rlm,   &
     &      jst, n_jk_e, n_jk_o, ncomp, nvector,                        &
     &      comm_rlm%irev_sr, n_WR, WR,                                 &
     &      WK_l_tst%pol_e, WK_l_tst%tor_e,                             &
     &      WK_l_tst%pol_o, WK_l_tst%tor_o)
        call set_sp_rlm_scl_sym_inner_omp                               &
     &     (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,      &
     &      jst, n_jk_e, n_jk_o, ncomp, nvector, nscalar,               &
     &      comm_rlm%irev_sr, n_WR, WR,                                 &
     &      WK_l_tst%pol_e, WK_l_tst%pol_o )
!        elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
!          st_elapsed = MPI_WTIME()
!$omp parallel do private(ip)
        do ip = 1, np_smp
          call matmul_bwd_leg_trans(WK_l_tst%narray_nl, nkrs, n_jk_e,   &
     &        WK_l_tst%Ps_tj(1,jst+1,ip), WK_l_tst%pol_e,               &
     &        WK_l_tst%symp_r(1,ip))
          call matmul_bwd_leg_trans(WK_l_tst%narray_nl, nkrt, n_jk_e,   &
     &        WK_l_tst%dPsdt_tj(1,jst+1,ip), WK_l_tst%tor_e,            &
     &        WK_l_tst%asmp_p(1,ip))
!   odd l-m
          call matmul_bwd_leg_trans(WK_l_tst%narray_nl, nkrs, n_jk_o,   &
     &        WK_l_tst%Ps_tj(1,jst_h,ip), WK_l_tst%pol_o,               &
     &        WK_l_tst%asmp_r(1,ip))
          call matmul_bwd_leg_trans(WK_l_tst%narray_nl, nkrt, n_jk_o,   &
     &        WK_l_tst%dPsdt_tj(1,jst_h,ip), WK_l_tst%tor_o,            &
     &        WK_l_tst%symp_p(1,ip))
        end do
!$omp end parallel do
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
        call cal_vr_rtm_vec_sym_inner_omp                               &
     &     (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,      &
     &      sph_rlm%nidx_rlm, asin_theta_1d_rtm,                        &
     &      np_smp, WK_l_tst%istack_nlo_rtm, WK_l_tst%narray_nl,        &
     &      WK_l_tst%nl_e_rtm, mp_rlm, mn_rlm,                          &
     &      WK_l_tst%symp_r, WK_l_tst%asmp_p,                           &
     &      WK_l_tst%asmp_r, WK_l_tst%symp_p,                           &
     &      ncomp, nvector, comm_rtm%irev_sr, n_WS, WS)
        call cal_vr_rtm_scl_sym_inner_omp                               &
     &     (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,      &
     &      sph_rlm%nidx_rlm, np_smp, WK_l_tst%istack_nlo_rtm,          &
     &      WK_l_tst%narray_nl, WK_l_tst%nl_e_rtm,                      &
     &      mp_rlm, WK_l_tst%symp_r, WK_l_tst%asmp_r,                   &
     &      ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
      end do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine legendre_b_trans_vector_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_vec_sym_inner_omp(nnod_rlm, nidx_rlm,       &
     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,    &
     &          jst, n_jk_e, n_jk_o, ncomp, nvector,          &
     &          irev_sr_rlm, n_WR, WR,  pol_e, tor_e, pol_o, tor_o)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: pol_e(n_jk_e,ncomp*nidx_rlm(1))
      real(kind = kreal), intent(inout)                                 &
     &                   :: tor_e(n_jk_e,2*nvector*nidx_rlm(1))
      real(kind = kreal), intent(inout)                                 &
     &                   :: pol_o(n_jk_o,ncomp*nidx_rlm(1))
      real(kind = kreal), intent(inout)                                 &
     &                   :: tor_o(n_jk_o,2*nvector*nidx_rlm(1))
!
      integer(kind = kint) :: jj, kk, k_rlm, nd
      integer(kind = kint) :: j_rlm, i_rlm, i_recv, nkrv
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
      real(kind = kreal) :: g3, gm
!
!
      nkrv = nidx_rlm(1) * nvector
!$omp parallel do private(kk,k_rlm,nd,a1r_1d_rlm_r,a2r_1d_rlm_r,        &
!$omp&                    jj,j_rlm,g3,gm,i_rlm,i_recv)
      do kk = 1, nkrv
        k_rlm = 1 + mod((kk-1),nidx_rlm(1))
        nd = 1 + (kk - k_rlm) / nidx_rlm(1)
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
!$omp end parallel do
!
      end subroutine set_sp_rlm_vec_sym_inner_omp
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scl_sym_inner_omp                           &
     &         (nnod_rlm, nidx_rlm, istep_rlm, jst,           &
     &          n_jk_e, n_jk_o, ncomp, nvector, nscalar, irev_sr_rlm,   &
     &          n_WR, WR, scl_e, scl_o)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
!
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: scl_e(n_jk_e,ncomp*nidx_rlm(1))
      real(kind = kreal), intent(inout)                                 &
     &                   :: scl_o(n_jk_o,ncomp*nidx_rlm(1))
!
      integer(kind = kint) :: jj, kk, k_rlm, nd
      integer(kind = kint) :: i_rlm, i_recv, nkrv
!
!
      nkrv = nidx_rlm(1) * nvector
!$omp parallel do private(kk,k_rlm,nd,jj,i_rlm,i_recv)
      do kk = 1, nidx_rlm(1)*nscalar
        k_rlm = 1 + mod((kk-1),nidx_rlm(1))
        nd = 1 + (kk - k_rlm) / nidx_rlm(1)
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
!$omp end parallel do
!
      end subroutine set_sp_rlm_scl_sym_inner_omp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_vec_sym_inner_omp(nnod_rtm, nidx_rtm,       &
     &          istep_rtm, nidx_rlm, asin_theta_1d_rtm,                 &
     &          np_smp, istack_nlo_rtm, narray_nl, nl_e_rtm,            &
     &          mp_rlm, mn_rlm, symp_r, asmp_p,                         &
     &          asmp_r, symp_p, ncomp, nvector, irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: istack_nlo_rtm(0:np_smp)
      integer(kind = kint), intent(in) :: narray_nl, nl_e_rtm
!
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout)                                 &
     &               :: symp_r(narray_nl,ncomp*nidx_rlm(1),    np_smp)
      real(kind = kreal), intent(in)                                    &
     &               :: asmp_p(narray_nl,2*nvector*nidx_rlm(1),np_smp)
      real(kind = kreal), intent(inout)                                 &
     &               :: asmp_r(narray_nl,ncomp*nidx_rlm(1),    np_smp)
      real(kind = kreal), intent(in)                                    &
     &               :: symp_p(narray_nl,2*nvector*nidx_rlm(1),np_smp)
!
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, lst, num, lnum
      integer(kind = kint) :: kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
!
!
      nkrv = nidx_rlm(1) * nvector
!$omp parallel do private(ip,lst,num,kk,lnum,lp_rtm)
      do ip = 1, np_smp
        lst = istack_nlo_rtm(ip-1)
        num = istack_nlo_rtm(ip) - istack_nlo_rtm(ip-1)
        do kk = 1, nkrv
          do lnum = 1, num
            lp_rtm = lnum + lst
            symp_r(lnum,kk+nkrv,  ip)                                   &
     &        = - symp_r(lnum,kk+nkrv,  ip) * asin_theta_1d_rtm(lp_rtm)
            symp_r(lnum,kk+2*nkrv,ip)                                   &
     &        = - symp_r(lnum,kk+2*nkrv,ip) * asin_theta_1d_rtm(lp_rtm)
            asmp_r(lnum,kk+nkrv,  ip)                                   &
     &        = - asmp_r(lnum,kk+nkrv,  ip) * asin_theta_1d_rtm(lp_rtm)
            asmp_r(lnum,kk+2*nkrv,ip)                                   &
     &        = - asmp_r(lnum,kk+2*nkrv,ip) * asin_theta_1d_rtm(lp_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(ip,lst,num,kk,k_rlm,nd,lnum,lp_rtm,ln_rtm,    &
!$omp&                    ip_rtpm,in_rtpm,ip_rtnm,in_rtnm,              &
!$omp&                    ipp_send,inp_send,ipn_send,inn_send)
      do ip = 1, np_smp
        lst = istack_nlo_rtm(ip-1)
        num = istack_nlo_rtm(ip) - istack_nlo_rtm(ip-1)
        do kk = 1, nkrv
          k_rlm = 1 + mod((kk-1),nidx_rlm(1))
          nd = 1 + (kk - k_rlm) / nidx_rlm(1)
          do lnum = 1, num
            lp_rtm = lnum + lst
            ln_rtm =  nidx_rtm(2) - lp_rtm + 1
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rlm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rlm-1) *  istep_rtm(1)                     &
     &                  + (mn_rlm-1) * istep_rtm(3)
            ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rlm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rlm-1) *  istep_rtm(1)                     &
     &                  + (mn_rlm-1) * istep_rtm(3)
!
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
            ipn_send = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
            inn_send = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
            WS(ipp_send-2) = WS(ipp_send-2)                             &
     &          + symp_r(lnum,kk,       ip) + asmp_r(lnum,kk,       ip)
            WS(ipp_send-1) = WS(ipp_send-1)                             &
     &          + asmp_p(lnum,kk+nkrv,  ip) + symp_p(lnum,kk+nkrv,  ip)
            WS(ipp_send  ) = WS(ipp_send  )                             &
     &          - asmp_p(lnum,kk,       ip) - symp_p(lnum,kk,       ip)
!
            WS(inp_send-1) = WS(inp_send-1)                             &
     &          + symp_r(lnum,kk+nkrv,  ip) + asmp_r(lnum,kk+nkrv,  ip)
            WS(inp_send  ) = WS(inp_send  )                             &
     &          + symp_r(lnum,kk+2*nkrv,ip) + asmp_r(lnum,kk+2*nkrv,ip)
!
!
            WS(ipn_send-2) = WS(ipn_send-2)                             &
     &          + symp_r(lnum,kk,       ip) - asmp_r(lnum,kk,       ip)
            WS(ipn_send-1) = WS(ipn_send-1)                             &
     &          - asmp_p(lnum,kk+nkrv,  ip) + symp_p(lnum,kk+nkrv,  ip)
            WS(ipn_send  ) = WS(ipn_send  )                             &
     &          + asmp_p(lnum,kk,       ip) - symp_p(lnum,kk,       ip)
!
            WS(inn_send-1) = WS(inn_send-1)                             &
     &          + symp_r(lnum,kk+nkrv,  ip) - asmp_r(lnum,kk+nkrv,  ip)
            WS(inn_send  ) = WS(inn_send  )                             &
     &          + symp_r(lnum,kk+2*nkrv,ip) - asmp_r(lnum,kk+2*nkrv,ip)
          end do
        end do
      end do
!$omp end parallel do
!
!
      if(nl_e_rtm .gt. istack_nlo_rtm(np_smp)) then
        lnum = nl_e_rtm - istack_nlo_rtm(np_smp-1)
!$omp parallel do private(kk)
        do kk = 1, nkrv
          symp_r(lnum,kk+nkrv,  np_smp)                                 &
     &      = - symp_r(lnum,kk+nkrv,  np_smp)                           &
     &         * asin_theta_1d_rtm(nl_e_rtm)
          symp_r(lnum,kk+2*nkrv,np_smp)                                 &
     &      = - symp_r(lnum,kk+2*nkrv,np_smp)                           &
     &         * asin_theta_1d_rtm(nl_e_rtm)
          asmp_r(lnum,kk+nkrv,  np_smp)                                 &
     &      = - asmp_r(lnum,kk+nkrv,  np_smp)                           &
     &         * asin_theta_1d_rtm(nl_e_rtm)
          asmp_r(lnum,kk+2*nkrv,np_smp)                                 &
     &      = - asmp_r(lnum,kk+2*nkrv,np_smp)                           &
     &         * asin_theta_1d_rtm(nl_e_rtm)
        end do
!$omp end parallel do
!
!$omp parallel do                                                       &
!$omp&  private(kk,k_rlm,nd,ln_rtm,ip_rtpm,in_rtpm,ipp_send,inp_send)
        do kk = 1, nkrv
          k_rlm = 1 + mod((kk-1),nidx_rlm(1))
          nd = 1 + (kk - k_rlm) / nidx_rlm(1)
!
          ln_rtm =  nidx_rtm(2) - nidx_rtm(2)/2-1 + 1
          ip_rtpm = 1 + (nl_e_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (nl_e_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
          WS(ipp_send-2) = WS(ipp_send-2)                               &
     &          + symp_r(lnum,kk,       np_smp)
          WS(ipp_send-1) = WS(ipp_send-1)                               &
     &          + symp_p(lnum,kk+nkrv,  np_smp)
          WS(ipp_send  ) = WS(ipp_send  )                               &
     &          - symp_p(lnum,kk,       np_smp)
!
          WS(inp_send-1) = WS(inp_send-1)                               &
     &          + symp_r(lnum,kk+nkrv,  np_smp)
          WS(inp_send  ) = WS(inp_send  )                               &
     &          + symp_r(lnum,kk+2*nkrv,np_smp)
        end do
!$omp end parallel do
      end if
!
      end subroutine cal_vr_rtm_vec_sym_inner_omp
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scl_sym_inner_omp                          &
     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm,               &
     &          np_smp, istack_nlo_rtm, narray_nl, nl_e_rtm,           &
     &          mp_rlm, symp, asmp, ncomp, nvector, nscalar,           &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: istack_nlo_rtm(0:np_smp)
      integer(kind = kint), intent(in) :: narray_nl, nl_e_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: mp_rlm
      real(kind = kreal), intent(in)                                    &
     &                   :: symp(narray_nl,ncomp*nidx_rlm(1),np_smp)
      real(kind = kreal), intent(in)                                    &
     &                   :: asmp(narray_nl,ncomp*nidx_rlm(1),np_smp)
!
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, lst, num, lnum
      integer(kind = kint) :: kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_send, ipn_send
!
!
      nkrv = nidx_rlm(1) * nvector
!$omp parallel do private(ip,lst,num,kk,k_rlm,nd,lnum,lp_rtm,ln_rtm,    &
!$omp&                    ip_rtpm,ip_rtnm,ipp_send,ipn_send)
      do ip = 1, np_smp
        lst = istack_nlo_rtm(ip-1)
        num = istack_nlo_rtm(ip) - istack_nlo_rtm(ip-1)
        do kk = 1, nidx_rlm(1)*nscalar
          k_rlm = 1 + mod((kk-1),nidx_rlm(1))
          nd = 1 + (kk - k_rlm) / nidx_rlm(1)
          do lnum = 1, num
            lp_rtm = lnum + lst
            ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rlm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rlm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
!
            ipp_send = nd + 3*nvector                                   &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
            ipn_send = nd + 3*nvector                                   &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &          + symp(lnum,kk+3*nkrv,ip) + asmp(lnum,kk+3*nkrv,ip)
            WS(ipn_send) = WS(ipn_send)                                 &
     &          + symp(lnum,kk+3*nkrv,ip) - asmp(lnum,kk+3*nkrv,ip)
          end do
        end do
      end do
!$omp end parallel do
!
      if(nl_e_rtm .gt. istack_nlo_rtm(np_smp)) then
        lnum = nl_e_rtm - istack_nlo_rtm(np_smp-1)
!$omp parallel do private(kk,k_rlm,nd,ip_rtpm,ipp_send)
        do kk = 1, nidx_rlm(1)*nscalar
          k_rlm = 1 + mod((kk-1),nidx_rlm(1))
          nd = 1 + (kk - k_rlm) / nidx_rlm(1)
!
          ip_rtpm = 1 + (nl_e_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_send = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
          WS(ipp_send) = WS(ipp_send) + symp(lnum,kk+3*nkrv,np_smp)
        end do
!$omp end parallel do
      end if
!
      end subroutine cal_vr_rtm_scl_sym_inner_omp
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_testloop
