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
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use t_legendre_work_testlooop
      use m_elapsed_labels_SPH_TRNS
!
      use matmul_for_legendre_trans
!
      implicit none
!
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
      integer(kind = kint) :: mp_rlm, mn_rlm
      integer(kind = kint) :: nkrs, nkrt, lst_rtm
      integer(kind = kint) :: ip, jst, lt, lp_rtm
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nkrs = (3*nvector + nscalar) * sph_rlm%nidx_rlm(1)
      nkrt = 2*nvector * sph_rlm%nidx_rlm(1)
!
      do mp_rlm = 1, sph_rtm%nidx_rtm(3)
        mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
        jst = idx_trns%lstack_rlm(mp_rlm-1)
!
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+12)
          call set_sp_rlm_vec_testloop                                  &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r, g_sph_rlm, &
     &        jst, WK_l_tst%n_jk_e(mp_rlm),  WK_l_tst%n_jk_o(mp_rlm),   &
     &        ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WR, WR,      &
     &        WK_l_tst%Smat(1)%pol_e(1), WK_l_tst%Smat(1)%tor_e(1),     &
     &        WK_l_tst%Smat(1)%pol_o(1), WK_l_tst%Smat(1)%tor_o(1) )
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+12)
!
!$omp parallel do private(ip,lst_rtm,lt,lp_rtm)
        do ip = 1, np_smp
          lst_rtm = WK_l_tst%lst_rtm(ip)
!
          do lt = 1, WK_l_tst%nlo_rtm(ip)
            lp_rtm = WK_l_tst%lst_rtm(ip) + lt
!   even l-m
            call matmul_bwd_leg_trans_tstlop                            &
     &       (nkrs, WK_l_tst%n_jk_e(mp_rlm),  &
     &        WK_l_tst%Pmat(mp_rlm,ip)%Pse_jt(1,lt),                    &
     &        WK_l_tst%Smat(1)%pol_e(1), WK_l_tst%Fmat(ip)%symp_r(1))
            call matmul_bwd_leg_trans_tstlop                            &
     &       (nkrt, WK_l_tst%n_jk_e(mp_rlm),  &
     &        WK_l_tst%Pmat(mp_rlm,ip)%dPsedt_jt(1,lt),                 &
     &        WK_l_tst%Smat(1)%tor_e(1), WK_l_tst%Fmat(ip)%asmp_p(1))
!   odd l-m
            call matmul_bwd_leg_trans_tstlop                            &
     &       (nkrs, WK_l_tst%n_jk_o(mp_rlm),  &
     &        WK_l_tst%Pmat(mp_rlm,ip)%Pso_jt(1,lt),                    &
     &        WK_l_tst%Smat(1)%pol_o(1), WK_l_tst%Fmat(ip)%asmp_r(1))
            call matmul_bwd_leg_trans_tstlop                            &
     &       (nkrt, WK_l_tst%n_jk_o(mp_rlm),  &
     &        WK_l_tst%Pmat(mp_rlm,ip)%dPsodt_jt(1,lt),                 &
     &        WK_l_tst%Smat(1)%tor_o(1), WK_l_tst%Fmat(ip)%symp_p(1))
!
            call mul_asin_to_vr_rtm                                     &
     &       (sph_rlm%nidx_rlm, asin_theta_1d_rtm(lp_rtm),              &
     &        WK_l_tst%Fmat(ip)%symp_r(1), WK_l_tst%Fmat(ip)%asmp_r(1), &
     &        nvector, nscalar)
!
            call cal_vr_rtm_vec_testloop                                &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, mp_rlm, mn_rlm, lp_rtm,                 &
     &        WK_l_tst%Fmat(ip)%symp_r(1), WK_l_tst%Fmat(ip)%asmp_p(1), &
     &        WK_l_tst%Fmat(ip)%asmp_r(1), WK_l_tst%Fmat(ip)%symp_p(1), &
     &        ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WS, WS)
          end do
!
          do lt = WK_l_tst%nlo_rtm(ip)+1, WK_l_tst%nle_rtm(ip)
            lp_rtm = WK_l_tst%lst_rtm(ip) + lt
!   even l-m
            call matmul_bwd_leg_trans_tstlop                            &
     &       (nkrs, WK_l_tst%n_jk_e(mp_rlm),  &
     &        WK_l_tst%Pmat(mp_rlm,ip)%Pse_jt(1,lt),                    &
     &        WK_l_tst%Smat(1)%pol_e(1), WK_l_tst%Fmat(ip)%symp_r(1))
            call matmul_bwd_leg_trans_tstlop                            &
     &       (nkrt, WK_l_tst%n_jk_e(mp_rlm),  &
     &        WK_l_tst%Pmat(mp_rlm,ip)%dPsedt_jt(1,lt),                 &
     &        WK_l_tst%Smat(1)%tor_e(1), WK_l_tst%Fmat(ip)%asmp_p(1))
!   odd l-m
            call matmul_bwd_leg_trans_tstlop                            &
     &       (nkrs, WK_l_tst%n_jk_o(mp_rlm),  &
     &        WK_l_tst%Pmat(mp_rlm,ip)%Pso_jt(1,lt),                    &
     &        WK_l_tst%Smat(1)%pol_o(1), WK_l_tst%Fmat(ip)%asmp_r(1))
            call matmul_bwd_leg_trans_tstlop                            &
     &       (nkrt, WK_l_tst%n_jk_o(mp_rlm),  &
     &        WK_l_tst%Pmat(mp_rlm,ip)%dPsodt_jt(1,lt),                 &
     &        WK_l_tst%Smat(1)%tor_o(1), WK_l_tst%Fmat(ip)%symp_p(1))
!
            call mul_asin_to_vr_rtm                                     &
     &       (sph_rlm%nidx_rlm, asin_theta_1d_rtm(lp_rtm),              &
     &        WK_l_tst%Fmat(ip)%symp_r(1), WK_l_tst%Fmat(ip)%asmp_r(1), &
     &        nvector, nscalar)
!
            call cal_vr_rtm_vec_equator                                 &
     &       (sph_rtm%nnod_rtm, sph_rtm%istep_rtm,                      &
     &        sph_rlm%nidx_rlm, mp_rlm, mn_rlm, lp_rtm,                 &
     &        WK_l_tst%Fmat(ip)%symp_r(1), WK_l_tst%Fmat(ip)%symp_p(1), &
     &        ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WS, WS)
          end do
        end do
!$omp end parallel do
!
      end do
!
      end subroutine legendre_b_trans_vector_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_vec_testloop(nnod_rlm, nidx_rlm,      &
     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,    &
     &          jst, n_jk_e, n_jk_o, ncomp_recv, nvector, nscalar,    &
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
      integer(kind = kint), intent(in) :: ncomp_recv
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      real(kind = kreal), intent(inout)                                 &
     &           :: pol_e(3*nvector+nscalar,nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &           :: tor_e(2*nvector,nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &           :: pol_o(3*nvector+nscalar,nidx_rlm(1),n_jk_o)
      real(kind = kreal), intent(inout)                                 &
     &           :: tor_o(2*nvector,nidx_rlm(1),n_jk_o)
!
      integer(kind = kint) :: jj, k_rlm, nd
      integer(kind = kint) :: j_rlm, i_rlm, i_recv
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
      real(kind = kreal) :: g3, gm
!
!
!$omp parallel do private(k_rlm,nd,a1r_1d_rlm_r,a2r_1d_rlm_r,        &
!$omp&                    jj,j_rlm,i_rlm,i_recv,g3,gm)
      do jj = 1, n_jk_e
        j_rlm = 2*jj + jst - 1
        g3 = g_sph_rlm(j_rlm,3)
        gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
        do k_rlm = 1, nidx_rlm(1)
          a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
          a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
          do nd = 1, nvector
            i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                        &
     &                + (k_rlm-1) * istep_rlm(1)
            i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
!
            pol_e(3*nd-2,k_rlm,jj) = WR(i_recv-2) * a2r_1d_rlm_r * g3
            tor_e(2*nd,  k_rlm,jj) = WR(i_recv-1) * a1r_1d_rlm_r
            pol_e(3*nd,  k_rlm,jj) = WR(i_recv-1) * a1r_1d_rlm_r * gm
            tor_e(2*nd-1,k_rlm,jj) = WR(i_recv  ) * a1r_1d_rlm_r
            pol_e(3*nd-1,k_rlm,jj) = WR(i_recv  ) * a1r_1d_rlm_r * gm
          end do
        end do
      end do
!$omp end parallel do
!
!   odd l-m
!$omp parallel do private(k_rlm,nd,a1r_1d_rlm_r,a2r_1d_rlm_r,        &
!$omp&                    jj,j_rlm,i_rlm,i_recv,g3,gm)
      do jj = 1, n_jk_o
        j_rlm = 2*jj + jst
        g3 = g_sph_rlm(j_rlm,3)
        gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
        do k_rlm = 1, nidx_rlm(1)
          a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
          a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
          do nd = 1, nvector
            i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                        &
     &                + (k_rlm-1) * istep_rlm(1)
            i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
!
            pol_o(3*nd-2,k_rlm,jj) = WR(i_recv-2) * a2r_1d_rlm_r * g3
            tor_o(2*nd,  k_rlm,jj) = WR(i_recv-1) * a1r_1d_rlm_r
            pol_o(3*nd,  k_rlm,jj) = WR(i_recv-1) * a1r_1d_rlm_r * gm
            tor_o(2*nd-1,k_rlm,jj) = WR(i_recv  ) * a1r_1d_rlm_r
            pol_o(3*nd-1,k_rlm,jj) = WR(i_recv  ) * a1r_1d_rlm_r * gm
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(k_rlm,nd,jj,i_rlm,i_recv)
      do jj = 1, n_jk_e
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nscalar
!   even l-m
            i_rlm = 1 + (2*jj + jst - 2) * istep_rlm(2)                 &
     &                + (k_rlm-1) *        istep_rlm(1)
            i_recv = nd + 3*nvector                                     &
     &              + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
            pol_e(nd+3*nvector,k_rlm,jj) = WR(i_recv)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(k_rlm,nd,jj,i_rlm,i_recv)
      do jj = 1, n_jk_o
        do k_rlm = 1, nidx_rlm(1)
          do nd = 1, nscalar
!   odd l-m
            i_rlm = 1 + (2*jj + jst - 1) * istep_rlm(2)                 &
     &                + (k_rlm-1) *        istep_rlm(1)
            i_recv = nd + 3*nvector                                     &
     &              + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
            pol_o(nd+3*nvector,k_rlm,jj) = WR(i_recv)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_sp_rlm_vec_testloop
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mul_asin_to_vr_rtm                                     &
     &         (nidx_rlm, asin_theta_1d_rtm,              &
     &          symp_r, asmp_r, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm
!
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      real(kind = kreal), intent(inout)                             &
     &           :: symp_r(3*nvector+nscalar,nidx_rlm(1))
      real(kind = kreal), intent(inout)                             &
     &           :: asmp_r(3*nvector+nscalar,nidx_rlm(1))
!
      integer(kind = kint) :: k_rlm, nd
!
!
        do nd = 1, nvector
          do k_rlm = 1, nidx_rlm(1)
            symp_r(3*nd-1,k_rlm) = - symp_r(3*nd-1,k_rlm) &
     &                                    * asin_theta_1d_rtm
            symp_r(3*nd,  k_rlm) = - symp_r(3*nd,  k_rlm) &
     &                                    * asin_theta_1d_rtm
            asmp_r(3*nd-1,k_rlm) = - asmp_r(3*nd-1,k_rlm) &
     &                                    * asin_theta_1d_rtm
            asmp_r(3*nd,  k_rlm) = - asmp_r(3*nd,  k_rlm) &
     &                                    * asin_theta_1d_rtm
          end do
        end do
!
      end subroutine mul_asin_to_vr_rtm
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_vec_testloop                                &
     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm,           &
     &          mp_rlm, mn_rlm, lp_rtm, symp_r, asmp_p,      &
     &          asmp_r, symp_p, ncomp_send, nvector, nscalar,      &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
!
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: lp_rtm
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      real(kind = kreal), intent(inout)                             &
     &           :: symp_r(3*nvector+nscalar,nidx_rlm(1))
      real(kind = kreal), intent(in)                                &
     &           :: asmp_p(2*nvector,nidx_rlm(1))
      real(kind = kreal), intent(inout)                             &
     &           :: asmp_r(3*nvector+nscalar,nidx_rlm(1))
      real(kind = kreal), intent(in)                                &
     &           :: symp_p(2*nvector,nidx_rlm(1))
!
      integer(kind = kint), intent(in) :: ncomp_send
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: k_rlm, nd, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
!
!
        do k_rlm = 1, nidx_rlm(1)
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
          do nd = 1, nvector
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_send
            ipn_send = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_send
            inn_send = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp_send
!
            WS(ipp_send-2) = WS(ipp_send-2)                             &
     &                      + symp_r(3*nd-2,k_rlm)               &
     &                      + asmp_r(3*nd-2,k_rlm)
            WS(ipp_send-1) = WS(ipp_send-1)                             &
     &                      + asmp_p(2*nd,  k_rlm)               &
     &                      + symp_p(2*nd,  k_rlm)
            WS(ipp_send  ) = WS(ipp_send  )                             &
     &                      - asmp_p(2*nd-1,k_rlm)               &
     &                      - symp_p(2*nd-1,k_rlm)
!
            WS(inp_send-1) = WS(inp_send-1)                             &
     &                      + symp_r(3*nd-1,k_rlm)               &
     &                      + asmp_r(3*nd-1,k_rlm)
            WS(inp_send  ) = WS(inp_send  )                             &
     &                      + symp_r(3*nd,  k_rlm)               &
     &                      + asmp_r(3*nd,  k_rlm)
!
!
            WS(ipn_send-2) = WS(ipn_send-2)                             &
     &                      + symp_r(3*nd-2,k_rlm)               &
     &                      - asmp_r(3*nd-2,k_rlm)
            WS(ipn_send-1) = WS(ipn_send-1)                             &
     &                      - asmp_p(2*nd,  k_rlm)               &
     &                      + symp_p(2*nd,  k_rlm)
            WS(ipn_send  ) = WS(ipn_send  )                             &
     &                      + asmp_p(2*nd-1,k_rlm)               &
     &                      - symp_p(2*nd-1,k_rlm)
!
            WS(inn_send-1) = WS(inn_send-1)                             &
     &                      + symp_r(3*nd-1,k_rlm)               &
     &                      - asmp_r(3*nd-1,k_rlm)
            WS(inn_send  ) = WS(inn_send  )                             &
     &                      + symp_r(3*nd,  k_rlm)               &
     &                      - asmp_r(3*nd,  k_rlm)
          end do
!
          do nd = 1, nscalar
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            ipn_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_send
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                  + symp_r(nd+3*nvector,k_rlm)             &
     &                  + asmp_r(nd+3*nvector,k_rlm)
            WS(ipn_send) = WS(ipn_send)                                 &
     &                  + symp_r(nd+3*nvector,k_rlm)             &
     &                  - asmp_r(nd+3*nvector,k_rlm)
          end do
        end do
!
      end subroutine cal_vr_rtm_vec_testloop
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_vec_equator(nnod_rtm,                       &
     &          istep_rtm, nidx_rlm, mp_rlm, mn_rlm, lp_rtm, symp_r,    &
     &          symp_p, ncomp_send, nvector, nscalar,                   &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: lp_rtm
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
!
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      real(kind = kreal), intent(inout)                             &
     &           :: symp_r(3*nvector+nscalar,nidx_rlm(1))
      real(kind = kreal), intent(in)                                &
     &           :: symp_p(2*nvector,nidx_rlm(1))
!
      integer(kind = kint), intent(in) :: ncomp_send
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: k_rlm, nd
      integer(kind = kint) :: ip_rtpm, in_rtpm
      integer(kind = kint) :: ipp_send, inp_send
!
!
        do k_rlm = 1, nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          do nd = 1, nvector
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_send
!
            WS(ipp_send-2) = WS(ipp_send-2) + symp_r(3*nd-2,k_rlm)
            WS(ipp_send-1) = WS(ipp_send-1) + symp_p(2*nd,  k_rlm)
            WS(ipp_send  ) = WS(ipp_send  ) - symp_p(2*nd-1,k_rlm)
!
            WS(inp_send-1) = WS(inp_send-1) + symp_r(3*nd-1,k_rlm)
            WS(inp_send  ) = WS(inp_send  ) + symp_r(3*nd,  k_rlm)
          end do
!
          do nd = 1, nscalar
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                    + symp_r(nd+3*nvector,k_rlm)
          end do
        end do
!
      end subroutine cal_vr_rtm_vec_equator
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine matmul_bwd_leg_trans_tstlop(nkr, n_jk,     &
     &          P_jl, S_kj, V_kl)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: S_kj(nkr,n_jk)
      real(kind = kreal), intent(in) :: P_jl(n_jk)
!
      real(kind = kreal), intent(inout) :: V_kl(nkr)
!
      integer(kind = kint) :: jj, kk
!
!
!
      V_kl(1:nkr) = 0.0d0
      do jj = 1, n_jk
        do kk = 1, nkr
          V_kl(kk) = V_kl(kk) + S_kj(kk,jj) * P_jl(jj)
        end do
      end do
!
      end subroutine matmul_bwd_leg_trans_tstlop
!
! ----------------------------------------------------------------------
!
      end module legendre_bwd_trans_testloop
