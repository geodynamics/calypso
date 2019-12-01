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
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm,                         &
!!     &          n_WR, n_WS, WR, WS, WK_l_sml)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar,&
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          n_WR, n_WS, WR, WS, WK_l_sml)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
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
      use m_machine_parameter
!
      use t_legendre_work_sym_matmul
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
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
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm,                           &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use cal_vr_rtm_by_vecprod
      use set_sp_rlm_for_leg_vecprod
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
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, je, jo
      integer(kind = kint) :: ip_rtpm,  in_rtpm,  ip_rtnm,  in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
      integer(kind = kint) :: lp, lst, nl_rtm, ll, lp_rtm, ln_rtm
      integer(kind = kint) :: mp_rlm, mn_rlm, jst, nj_rlm
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .le. 0) return
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,nl_rtm,jst,nd,k_rlm,ll,     &
!$omp&                    lp_rtm,ln_rtm,nj_rlm,je,jo,                   &
!$omp&                    ip_rtpm,in_rtpm,ip_rtnm,in_rtnm,              &
!$omp&                    ipp_send,inp_send,ipn_send,inn_send,          &
!$omp&                    mp_rlm,mn_rlm,a1r_1d_rlm_r,a2r_1d_rlm_r)
      do ip = 1, np_smp
        kst = sph_rtm%istack_rtm_kr_smp(ip-1) + 1
        ked = sph_rtm%istack_rtm_kr_smp(ip  )
        do lp = 1, idx_trns%nblock_l_rtm
          lst = idx_trns%lstack_block_rtm(lp-1)/2
          nl_rtm = idx_trns%lstack_block_rtm(lp  )/2                    &
     &            - idx_trns%lstack_block_rtm(lp-1)/2
!
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
            jst = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm = idx_trns%lstack_rlm(mp_rlm)                        &
     &              - idx_trns%lstack_rlm(mp_rlm-1)
            je = 1 + jst
            jo = 1 + jst + (nj_rlm+1) / 2
            do k_rlm = kst, ked
              a1r_1d_rlm_r = sph_rlm%a_r_1d_rlm_r(k_rlm)
              a2r_1d_rlm_r = a1r_1d_rlm_r**2
              do ll = 1, nl_rtm
                lp_rtm =  ll + lst
                ln_rtm =  sph_rtm%nidx_rtm(2) - lp_rtm + 1
!
                ip_rtpm = 1 + (lp_rtm-1) * sph_rtm%istep_rtm(2)         &
     &                      + (k_rlm-1) *  sph_rtm%istep_rtm(1)         &
     &                      + (mp_rlm-1) * sph_rtm%istep_rtm(3)
                in_rtpm = 1 + (lp_rtm-1) * sph_rtm%istep_rtm(2)         &
     &                      + (k_rlm-1) *  sph_rtm%istep_rtm(1)         &
     &                      + (mn_rlm-1) * sph_rtm%istep_rtm(3)
                ip_rtnm = 1 + (ln_rtm-1) * sph_rtm%istep_rtm(2)         &
     &                      + (k_rlm-1) *  sph_rtm%istep_rtm(1)         &
     &                      + (mp_rlm-1) * sph_rtm%istep_rtm(3)
                in_rtnm = 1 + (ln_rtm-1) * sph_rtm%istep_rtm(2)         &
     &                      + (k_rlm-1) *  sph_rtm%istep_rtm(1)         &
     &                      + (mn_rlm-1) * sph_rtm%istep_rtm(3)
!
                do nd = 1, nvector
                  ipp_send = 3*nd-2                                     &
     &                      + (comm_rtm%irev_sr(ip_rtpm)-1) * ncomp
                  inp_send = 3*nd-2                                     &
     &                      + (comm_rtm%irev_sr(in_rtpm)-1) * ncomp
                  ipn_send = 3*nd-2                                     &
     &                      + (comm_rtm%irev_sr(ip_rtnm)-1) * ncomp
                  inn_send = 3*nd-2                                     &
     &                      + (comm_rtm%irev_sr(in_rtnm)-1) * ncomp
!
                  call set_sp_rlm_vector_symmetry                       &
     &               (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,               &
     &                sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,       &
     &                g_sph_rlm, jst, nd, k_rlm,                        &
     &                a1r_1d_rlm_r, a2r_1d_rlm_r,                       &
     &                ncomp, n_WR, comm_rlm%irev_sr, WR, nj_rlm,        &
     &                WK_l_sml%pol_e(1,ip), WK_l_sml%dpoldt_e(1,ip),    &
     &                WK_l_sml%dpoldp_e(1,ip), WK_l_sml%dtordt_e(1,ip), &
     &                WK_l_sml%dtordp_e(1,ip), WK_l_sml%pol_o(1,ip),    &
     &                WK_l_sml%dpoldt_o(1,ip), WK_l_sml%dpoldp_o(1,ip), &
     &                WK_l_sml%dtordt_o(1,ip), WK_l_sml%dtordp_o(1,ip))
!
                  call cal_vr_rtm_dydtheta_symmetry(nj_rlm,             &
     &                WK_l_sml%Ps_jl(je,lp_rtm),                        &
     &                WK_l_sml%dPsdt_jl(je,lp_rtm),                     &
     &                WK_l_sml%Ps_jl(jo,lp_rtm),                        &
     &                WK_l_sml%dPsdt_jl(jo,lp_rtm),                     &
     &                WK_l_sml%pol_e(1,ip), WK_l_sml%dpoldt_e(1,ip),    &
     &                WK_l_sml%dtordt_e(1,ip), WK_l_sml%pol_o(1,ip),    &
     &                WK_l_sml%dpoldt_o(1,ip), WK_l_sml%dtordt_o(1,ip), &
     &                WS(ipp_send), WS(ipn_send))
                  call cal_vr_rtm_dydphi_symmetry(nj_rlm,               &
     &                WK_l_sml%Ps_jl(je,lp_rtm),                        &
     &                WK_l_sml%Ps_jl(jo,lp_rtm),                        &
     &                asin_theta_1d_rtm(lp_rtm),                        &
     &                WK_l_sml%dpoldp_e(1,ip), WK_l_sml%dtordp_e(1,ip), &
     &                WK_l_sml%dpoldp_o(1,ip), WK_l_sml%dtordp_o(1,ip), &
     &                WS(inp_send), WS(inn_send))
                end do
              end do
            end do
          end do
        end do
!
!   Equator (if necessary)
        do lp_rtm = sph_rtm%nidx_rtm(2)/2+1, (sph_rtm%nidx_rtm(2)+1)/2
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
            jst = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm = idx_trns%lstack_rlm(mp_rlm)                        &
     &              - idx_trns%lstack_rlm(mp_rlm-1)
            je = 1 + jst
            jo = 1 + jst + (nj_rlm+1) / 2
            do k_rlm = kst, ked
              a1r_1d_rlm_r = sph_rlm%a_r_1d_rlm_r(k_rlm)
              a2r_1d_rlm_r = a1r_1d_rlm_r**2
!
              ip_rtpm = 1 + (lp_rtm-1) * sph_rtm%istep_rtm(2)           &
     &                    + (k_rlm-1) *  sph_rtm%istep_rtm(1)           &
     &                    + (mp_rlm-1) * sph_rtm%istep_rtm(3)
              in_rtpm = 1 + (lp_rtm-1) * sph_rtm%istep_rtm(2)           &
     &                    + (k_rlm-1) *  sph_rtm%istep_rtm(1)           &
     &                    + (mn_rlm-1) * sph_rtm%istep_rtm(3)
!
              do nd = 1, nvector
                ipp_send = 3*nd-2                                       &
     &                    + (comm_rtm%irev_sr(ip_rtpm)-1) * ncomp
                inp_send = 3*nd-2                                       &
     &                    + (comm_rtm%irev_sr(in_rtpm)-1) * ncomp
!
                call set_sp_rlm_vector_equator                          &
     &             (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,                 &
     &              sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,         &
     &              g_sph_rlm, jst, nd, k_rlm,                          &
     &              a1r_1d_rlm_r, a2r_1d_rlm_r,                         &
     &              ncomp, n_WR, comm_rlm%irev_sr, WR, nj_rlm,          &
     &              WK_l_sml%pol_e(1,ip), WK_l_sml%dpoldp_e(1,ip),      &
     &              WK_l_sml%dtordp_e(1,ip), WK_l_sml%dpoldt_o(1,ip),   &
     &              WK_l_sml%dtordt_o(1,ip))
!
                call cal_vr_rtm_dydtheta_equator(nj_rlm,                &
     &              WK_l_sml%Ps_jl(je,lp_rtm),                          &
     &              WK_l_sml%dPsdt_jl(jo,lp_rtm),                       &
     &              WK_l_sml%pol_e(1,ip), WK_l_sml%dpoldt_o(1,ip),      &
     &              WK_l_sml%dtordt_o(1,ip), WS(ipp_send))
                call cal_vr_rtm_dydphi_equator(nj_rlm,                  &
     &              WK_l_sml%Ps_jl(je,lp_rtm),                          &
     &              WK_l_sml%dpoldp_e(1,ip), WK_l_sml%dtordp_e(1,ip),   &
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
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use cal_vr_rtm_by_vecprod
      use set_sp_rlm_for_leg_vecprod
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, je, jo
      integer(kind = kint) :: ip_rtm, in_rtm, ip_send, in_send
      integer(kind = kint) :: lp, lst, nl_rtm, ll, lp_rtm, ln_rtm
      integer(kind = kint) :: mp_rlm, jst, nj_rlm
!
!
      if(nscalar .le. 0) return
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,nl_rtm,nd,mp_rlm,je,jo,     &
!$omp&                    ll,lp_rtm,ln_rtm,jst,nj_rlm,                  &
!$omp&                    ip_send,in_send,k_rlm,ip_rtm,in_rtm)
      do ip = 1, np_smp
        kst = sph_rtm%istack_rtm_kr_smp(ip-1) + 1
        ked = sph_rtm%istack_rtm_kr_smp(ip  )
        do lp = 1, idx_trns%nblock_l_rtm
          lst = idx_trns%lstack_block_rtm(lp-1)/2
          nl_rtm = idx_trns%lstack_block_rtm(lp  )/2                    &
     &            - idx_trns%lstack_block_rtm(lp-1)/2
!
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            jst = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm = idx_trns%lstack_rlm(mp_rlm)                        &
     &              - idx_trns%lstack_rlm(mp_rlm-1)
            je = 1 + jst
            jo = 1 + jst + (nj_rlm+1) / 2
            do k_rlm = kst, ked
!
              do ll = 1, nl_rtm
                lp_rtm =  ll + lst
                ln_rtm =  sph_rtm%nidx_rtm(2) - lp_rtm + 1
!
                do nd = 1, nscalar
                  ip_rtm = 1 + (lp_rtm-1) * sph_rtm%istep_rtm(2)        &
     &                       + (k_rlm-1) *  sph_rtm%istep_rtm(1)        &
     &                       + (mp_rlm-1) * sph_rtm%istep_rtm(3)
                  in_rtm = 1 + (ln_rtm-1) * sph_rtm%istep_rtm(2)        &
     &                       + (k_rlm-1) *  sph_rtm%istep_rtm(1)        &
     &                       + (mp_rlm-1) * sph_rtm%istep_rtm(3)
                  ip_send = nd + 3*nvector                              &
     &                         + (comm_rtm%irev_sr(ip_rtm)-1) * ncomp
                  in_send = nd + 3*nvector                              &
     &                         + (comm_rtm%irev_sr(in_rtm)-1) * ncomp
!
                  call set_sp_rlm_scalar_symmetry                       &
     &               (sph_rlm%nnod_rlm, sph_rlm%istep_rlm,              &
     &                jst, nd, k_rlm,  ncomp, nvector,                  &
     &                n_WR, comm_rlm%irev_sr, WR, nj_rlm,               &
     &                WK_l_sml%scl_e(1,ip), WK_l_sml%scl_o(1,ip))
!
                  call cal_vr_rtm_scalar_symmetry(nj_rlm,               &
     &                WK_l_sml%Ps_jl(je,lp_rtm),                        &
     &                WK_l_sml%Ps_jl(jo,lp_rtm),                        &
     &                WK_l_sml%scl_e(1,ip), WK_l_sml%scl_o(1,ip),       &
     &                WS(ip_send), WS(in_send))
                end do
              end do
!
            end do
          end do
        end do
!
!   Equator (if necessary)
        do lp_rtm = sph_rtm%nidx_rtm(2)/2+1, (sph_rtm%nidx_rtm(2)+1)/2
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            jst = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm = idx_trns%lstack_rlm(mp_rlm)                        &
     &              - idx_trns%lstack_rlm(mp_rlm-1)
            je = 1 + jst
            do k_rlm = kst, ked
              do nd = 1, nscalar
                ip_rtm = 1 + (lp_rtm-1) * sph_rtm%istep_rtm(2)          &
     &                     + (k_rlm-1) *  sph_rtm%istep_rtm(1)          &
     &                     + (mp_rlm-1) * sph_rtm%istep_rtm(3)
                ip_send = nd + 3*nvector                                &
     &                       + (comm_rtm%irev_sr(ip_rtm)-1) * ncomp
!
                call set_sp_rlm_scalar_equator                          &
     &             (sph_rlm%nnod_rlm, sph_rlm%istep_rlm,                &
     &              jst, nd, k_rlm, ncomp, nvector,                     &
     &              n_WR, comm_rlm%irev_sr, WR, nj_rlm,                 &
     &              WK_l_sml%scl_e(1,ip))
!
                call cal_vr_rtm_scalar_equator(nj_rlm,                  &
     &              WK_l_sml%Ps_jl(je,lp_rtm), WK_l_sml%scl_e(1,ip),    &
     &              WS(ip_send))
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
