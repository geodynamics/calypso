!>@file   legendre_bwd_trans_symmetry.f90
!!@brief  module legendre_bwd_trans_symmetry
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  backward Legendre transform considering symmetry
!!
!!@verbatim
!!      subroutine leg_bwd_trans_vector_sym_org(ncomp, nvector,         &
!!     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar,&
!!     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_symmetry
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_legendre_work_sym_matmul
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_vector_sym_org(ncomp, nvector,           &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!
      use cal_vr_rtm_by_vecprod
      use set_sp_rlm_for_leg_vecprod
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, je, jo
      integer(kind = kint) :: ip_rtpm,  in_rtpm,  ip_rtnm,  in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
      integer(kind = kint) :: lp, lst, nl_rtm, ll, lp_rtm, ln_rtm
      integer(kind = kint) :: mp_rlm, mn_rlm, jst, nj_rlm
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,nl_rtm,jst,nd,k_rlm,ll,     &
!$omp&                    lp_rtm,ln_rtm,nj_rlm,je,jo,                   &
!$omp&                    ip_rtpm,in_rtpm,ip_rtnm,in_rtnm,              &
!$omp&                    ipp_send,inp_send,ipn_send,inn_send,          &
!$omp&                    mp_rlm,mn_rlm,a1r_1d_rlm_r,a2r_1d_rlm_r)
      do ip = 1, np_smp
        kst = idx_rtm_smp_stack(ip-1,1) + 1
        ked = idx_rtm_smp_stack(ip,  1)
        do lp = 1, nblock_l_rtm
          lst = lstack_block_rtm(lp-1)/2
          nl_rtm = lstack_block_rtm(lp  )/2 - lstack_block_rtm(lp-1)/2
!
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = nidx_rtm(3) - mp_rlm + 1
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            je = 1 + jst
            jo = 1 + jst + (nj_rlm+1) / 2
            do k_rlm = kst, ked
              a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
              a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
              do ll = 1, nl_rtm
                lp_rtm =  ll + lst
                ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!
                do nd = 1, nvector
                  ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mp_rlm-1) * istep_rtm(3)
                  in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mn_rlm-1) * istep_rtm(3)
                  ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mp_rlm-1) * istep_rtm(3)
                  in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mn_rlm-1) * istep_rtm(3)
                  ipp_send = 3*nd-2 + (irev_sr_rtm(ip_rtpm)-1) * ncomp
                  inp_send = 3*nd-2 + (irev_sr_rtm(in_rtpm)-1) * ncomp
                  ipn_send = 3*nd-2 + (irev_sr_rtm(ip_rtnm)-1) * ncomp
                  inn_send = 3*nd-2 + (irev_sr_rtm(in_rtnm)-1) * ncomp
!
                  call set_sp_rlm_vector_symmetry                       &
     &               (jst, nd, k_rlm, a1r_1d_rlm_r, a2r_1d_rlm_r,       &
     &                ncomp, n_WR, irev_sr_rlm, WR, nj_rlm,             &
     &                pol_e(1,ip), dpoldt_e(1,ip), dpoldp_e(1,ip),      &
     &                dtordt_e(1,ip), dtordp_e(1,ip),                   &
     &                pol_o(1,ip), dpoldt_o(1,ip), dpoldp_o(1,ip),      &
     &                dtordt_o(1,ip), dtordp_o(1,ip))
!
                  call cal_vr_rtm_dydtheta_symmetry(nj_rlm,             &
     &                Ps_jl(je,lp_rtm), dPsdt_jl(je,lp_rtm),            &
     &                Ps_jl(jo,lp_rtm), dPsdt_jl(jo,lp_rtm),            &
     &                pol_e(1,ip), dpoldt_e(1,ip), dtordt_e(1,ip),      &
     &                pol_o(1,ip), dpoldt_o(1,ip), dtordt_o(1,ip),      &
     &                WS(ipp_send), WS(ipn_send))
                  call cal_vr_rtm_dydphi_symmetry(nj_rlm,               &
     &                Ps_jl(je,lp_rtm), Ps_jl(jo,lp_rtm),               &
     &                asin_theta_1d_rtm(lp_rtm),                        &
     &                dpoldp_e(1,ip), dtordp_e(1,ip),                   &
     &                dpoldp_o(1,ip), dtordp_o(1,ip),                   &
     &                WS(inp_send), WS(inn_send))
                end do
              end do
            end do
          end do
        end do
!
!   Equator (if necessary)
        do lp_rtm = nidx_rtm(2)/2+1, (nidx_rtm(2)+1)/2
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = nidx_rtm(3) - mp_rlm + 1
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            je = 1 + jst
            jo = 1 + jst + (nj_rlm+1) / 2
            do k_rlm = kst, ked
              a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
              a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
!
              do nd = 1, nvector
                ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                 &
     &                      + (k_rlm-1) *  istep_rtm(1)                 &
     &                      + (mp_rlm-1) * istep_rtm(3)
                in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                 &
     &                      + (k_rlm-1) *  istep_rtm(1)                 &
     &                      + (mn_rlm-1) * istep_rtm(3)
                ipp_send = 3*nd-2 + (irev_sr_rtm(ip_rtpm)-1) * ncomp
                inp_send = 3*nd-2 + (irev_sr_rtm(in_rtpm)-1) * ncomp
!
                call set_sp_rlm_vector_equator                          &
     &               (jst, nd, k_rlm, a1r_1d_rlm_r, a2r_1d_rlm_r,       &
     &                ncomp, n_WR, irev_sr_rlm, WR, nj_rlm,             &
     &                pol_e(1,ip), dpoldp_e(1,ip), dtordp_e(1,ip),      &
     &                dpoldt_o(1,ip), dtordt_o(1,ip))
!
                call cal_vr_rtm_dydtheta_equator(nj_rlm,                &
     &              Ps_jl(je,lp_rtm), dPsdt_jl(jo,lp_rtm),              &
     &              pol_e(1,ip), dpoldt_o(1,ip), dtordt_o(1,ip),        &
     &              WS(ipp_send))
                call cal_vr_rtm_dydphi_equator(nj_rlm,                  &
     &              Ps_jl(je,lp_rtm), dpoldp_e(1,ip), dtordp_e(1,ip),   &
     &              WS(inp_send))
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine leg_bwd_trans_vector_sym_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar,  &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!
      use cal_vr_rtm_by_vecprod
      use set_sp_rlm_for_leg_vecprod
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, je, jo
      integer(kind = kint) :: ip_rtm, in_rtm, ip_send, in_send
      integer(kind = kint) :: lp, lst, nl_rtm, ll, lp_rtm, ln_rtm
      integer(kind = kint) :: mp_rlm, jst, nj_rlm
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,nl_rtm,nd,mp_rlm,je,jo,     &
!$omp&                    ll,lp_rtm,ln_rtm,jst,nj_rlm,                  &
!$omp&                    ip_send,in_send,k_rlm,ip_rtm,in_rtm)
      do ip = 1, np_smp
        kst = idx_rtm_smp_stack(ip-1,1) + 1
        ked = idx_rtm_smp_stack(ip,  1)
        do lp = 1, nblock_l_rtm
          lst = lstack_block_rtm(lp-1)/2
          nl_rtm = lstack_block_rtm(lp  )/2 - lstack_block_rtm(lp-1)/2
!
          do mp_rlm = 1, nidx_rtm(3)
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            je = 1 + jst
            jo = 1 + jst + (nj_rlm+1) / 2
            do k_rlm = kst, ked
!
              do ll = 1, nl_rtm
                lp_rtm =  ll + lst
                ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!
                do nd = 1, nscalar
                  ip_rtm = 1 + (lp_rtm-1) * istep_rtm(2)                &
     &                       + (k_rlm-1) *  istep_rtm(1)                &
     &                       + (mp_rlm-1) * istep_rtm(3)
                  in_rtm = 1 + (ln_rtm-1) * istep_rtm(2)                &
     &                       + (k_rlm-1) *  istep_rtm(1)                &
     &                       + (mp_rlm-1) * istep_rtm(3)
                  ip_send = nd + 3*nvector                              &
     &                         + (irev_sr_rtm(ip_rtm)-1) * ncomp
                  in_send = nd + 3*nvector                              &
     &                         + (irev_sr_rtm(in_rtm)-1) * ncomp
!
                  call set_sp_rlm_scalar_symmetry(jst, nd, k_rlm,       &
     &               ncomp, nvector, n_WR, irev_sr_rlm, WR,             &
     &               nj_rlm, scl_e(1,ip), scl_o(1,ip))
!
                  call cal_vr_rtm_scalar_symmetry(nj_rlm,               &
     &                Ps_jl(je,lp_rtm), Ps_jl(jo,lp_rtm),               &
     &                scl_e(1,ip), scl_o(1,ip),                         &
     &                WS(ip_send), WS(in_send))
                end do
              end do
!
            end do
          end do
        end do
!
!   Equator (if necessary)
        do lp_rtm = nidx_rtm(2)/2+1, (nidx_rtm(2)+1)/2
          do mp_rlm = 1, nidx_rtm(3)
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            je = 1 + jst
            do k_rlm = kst, ked
              do nd = 1, nscalar
                ip_rtm = 1 + (lp_rtm-1) * istep_rtm(2)                  &
     &                     + (k_rlm-1) *  istep_rtm(1)                  &
     &                     + (mp_rlm-1) * istep_rtm(3)
                ip_send = nd + 3*nvector                                &
     &                       + (irev_sr_rtm(ip_rtm)-1) * ncomp
!
                call set_sp_rlm_scalar_equator(jst, nd, k_rlm,          &
     &               ncomp, nvector, n_WR, irev_sr_rlm, WR,             &
     &               nj_rlm, scl_e(1,ip))
!
                call cal_vr_rtm_scalar_equator(nj_rlm,                  &
     &              Ps_jl(je,lp_rtm), scl_e(1,ip), WS(ip_send))
              end do
!
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine leg_bwd_trans_scalar_sym_org
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_symmetry
