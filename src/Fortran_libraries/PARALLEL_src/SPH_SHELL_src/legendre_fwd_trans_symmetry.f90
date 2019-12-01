!>@file   legendre_fwd_trans_symmetry.f90
!!@brief  module legendre_fwd_trans_symmetry
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform considering symmetry
!!
!!@verbatim
!!      subroutine leg_fwd_trans_vector_sym_org(ncomp, nvector,         &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,             &
!!     &          n_WR, n_WS, WR, WS, WK_l_sml)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar,&
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
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
      module legendre_fwd_trans_symmetry
!
      use m_precision
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
      subroutine leg_fwd_trans_vector_sym_org(ncomp, nvector,           &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,               &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use set_vr_rtm_for_leg_vecprod
      use cal_sp_rlm_by_vecprod
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in)                                    &
     &           :: asin_theta_1d_rtm(sph_rtm%nidx_rtm(2))
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
      real(kind = kreal), intent(in) :: weight_rtm(sph_rtm%nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, ie_rlm, io_rlm
      integer(kind = kint) :: mp_rlm, mn_rlm, ie_send, io_send
      integer(kind = kint) :: jst, nj_rlm, jj, je_rlm, jo_rlm
      integer(kind = kint) :: nle_rtm, nlo_rtm, n_jk_e, n_jk_o
      real(kind = kreal) :: r1_1d_rlm_r, r2_1d_rlm_r, gme, gmo
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rlm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .le. 0) return
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,kst,ked,jj,k_rlm,nd,je_rlm,jo_rlm,         &
!$omp&                    mp_rlm,mn_rlm,jst,nj_rlm,n_jk_e,n_jk_o,       &
!$omp&                    r1_1d_rlm_r,r2_1d_rlm_r,                      &
!$omp&                    ie_rlm,io_rlm,ie_send,io_send,gme,gmo)
      do ip = 1, np_smp
        kst = sph_rlm%istack_rlm_kr_smp(ip-1) + 1
        ked = sph_rlm%istack_rlm_kr_smp(ip  )
        do k_rlm = kst, ked
          r1_1d_rlm_r = sph_rlm%radius_1d_rlm_r(k_rlm)
          r2_1d_rlm_r = r1_1d_rlm_r*r1_1d_rlm_r
!
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
            jst = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm = idx_trns%lstack_rlm(mp_rlm)                        &
     &              - idx_trns%lstack_rlm(mp_rlm-1)
            n_jk_e = (nj_rlm+1) / 2
            n_jk_o =  nj_rlm - n_jk_e
!    even l-m
!    odd  l-m
            do jj = 1, nj_rlm/2
              je_rlm = 2*jj + jst - 1
              jo_rlm = 2*jj + jst
              ie_rlm = 1 + (je_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              io_rlm = 1 + (jo_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1)  * sph_rlm%istep_rlm(1)
!
              gme = dble(sph_rlm%idx_gl_1d_rlm_j(je_rlm,3))
              gmo = dble(sph_rlm%idx_gl_1d_rlm_j(jo_rlm,3))
!
              do nd = 1, nvector
                ie_send = 3*nd-2                                        &
     &                   + (comm_rlm%irev_sr(ie_rlm) - 1) * ncomp
                io_send = 3*nd-2                                        &
     &                   + (comm_rlm%irev_sr(io_rlm) - 1) * ncomp
!
                call set_vr_rtm_vector_symmetry                         &
     &             (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm,                 &
     &              sph_rtm%istep_rtm, weight_rtm, asin_theta_1d_rtm,   &
     &              nd, k_rlm, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,        &
     &              ncomp, comm_rtm%irev_sr, n_WR, WR,                  &
     &              WK_l_sml%symp_r(1,ip), WK_l_sml%asmp_t(1,ip),       &
     &              WK_l_sml%asmp_p(1,ip), WK_l_sml%symn_t(1,ip),       &
     &              WK_l_sml%symn_p(1,ip), WK_l_sml%asmp_r(1,ip),       &
     &              WK_l_sml%symp_t(1,ip), WK_l_sml%symp_p(1,ip),       &
     &              WK_l_sml%asmn_t(1,ip), WK_l_sml%asmn_p(1,ip))
!
                call cal_vector_sp_rlm_dotprod(WK_l_sml%nth_hemi_rtm,   &
     &              g_sph_rlm(je_rlm,7), gme, r1_1d_rlm_r, r2_1d_rlm_r, &
     &              WK_l_sml%Ps_rtm(1,jj+jst),                          &
     &              WK_l_sml%dPsdt_rtm(1,jj+jst),                       &
     &              WK_l_sml%symp_r(1,ip), WK_l_sml%asmp_t(1,ip),       &
     &              WK_l_sml%asmp_p(1,ip), WK_l_sml%symn_t(1,ip),       &
     &              WK_l_sml%symn_p(1,ip), WS(ie_send))
                call cal_vector_sp_rlm_dotprod(WK_l_sml%nth_hemi_rtm,   &
     &              g_sph_rlm(jo_rlm,7), gmo, r1_1d_rlm_r, r2_1d_rlm_r, &
     &              WK_l_sml%Ps_rtm(1,jj+jst+n_jk_e),                   &
     &              WK_l_sml%dPsdt_rtm(1,jj+jst+n_jk_e),                &
     &              WK_l_sml%asmp_r(1,ip), WK_l_sml%symp_t(1,ip),       &
     &              WK_l_sml%symp_p(1,ip), WK_l_sml%asmn_t(1,ip),       &
     &              WK_l_sml%asmn_p(1,ip), WS(io_send))
              end do
            end do
!
!   the last even l-m
            do jj = nj_rlm/2+1, (nj_rlm+1)/2
              je_rlm = 2*jj + jst - 1
              ie_rlm = 1 + (je_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              gme = dble(sph_rlm%idx_gl_1d_rlm_j(je_rlm,3))
!
              do nd = 1, nvector
                ie_send = 3*nd-2                                        &
     &                   + (comm_rlm%irev_sr(ie_rlm) - 1) * ncomp
!
                call set_vr_rtm_vector_symmetry                         &
     &             (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm,                 &
     &              sph_rtm%istep_rtm, weight_rtm, asin_theta_1d_rtm,   &
     &              nd, k_rlm, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,        &
     &              ncomp, comm_rtm%irev_sr, n_WR, WR,                  &
     &              WK_l_sml%symp_r(1,ip), WK_l_sml%asmp_t(1,ip),       &
     &              WK_l_sml%asmp_p(1,ip), WK_l_sml%symn_t(1,ip),       &
     &              WK_l_sml%symn_p(1,ip), WK_l_sml%asmp_r(1,ip),       &
     &              WK_l_sml%symp_t(1,ip), WK_l_sml%symp_p(1,ip),       &
     &              WK_l_sml%asmn_t(1,ip), WK_l_sml%asmn_p(1,ip))
!
                call cal_vector_sp_rlm_dotprod(WK_l_sml%nth_hemi_rtm,   &
     &              g_sph_rlm(je_rlm,7), gme, r1_1d_rlm_r, r2_1d_rlm_r, &
     &              WK_l_sml%Ps_rtm(1,jj+jst),                          &
     &              WK_l_sml%dPsdt_rtm(1,jj+jst),                       &
     &              WK_l_sml%symp_r(1,ip), WK_l_sml%asmp_t(1,ip),       &
     &              WK_l_sml%asmp_p(1,ip), WK_l_sml%symn_t(1,ip),       &
     &              WK_l_sml%symn_p(1,ip), WS(ie_send))
              end do
            end do
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine leg_fwd_trans_vector_sym_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar,  &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!
      use set_vr_rtm_for_leg_vecprod
      use cal_sp_rlm_by_vecprod
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
      real(kind = kreal), intent(in) :: weight_rtm(sph_rtm%nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd
      integer(kind = kint) :: nle_rtm, nlo_rtm, je_rlm, jo_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: mp_rlm, jst, nj_rlm, jj, n_jk_e, n_jk_o
!
!
      if(nscalar .le. 0) return
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,jj,k_rlm,mp_rlm,n_jk_e,n_jk_o,     &
!$omp&                    nd,jst,nj_rlm,ie_rlm,io_rlm,je_rlm,jo_rlm,    &
!$omp&                    ie_send,io_send)
      do ip = 1, np_smp
        kst = sph_rlm%istack_rlm_kr_smp(ip-1) + 1
        ked = sph_rlm%istack_rlm_kr_smp(ip  )
        do k_rlm = kst, ked
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            jst = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm = idx_trns%lstack_rlm(mp_rlm)                        &
     &              - idx_trns%lstack_rlm(mp_rlm-1)
            n_jk_e = (nj_rlm+1) / 2
            n_jk_o =  nj_rlm - n_jk_e
!    even l-m
!    odd  l-m
            do jj = 1, nj_rlm/2
              je_rlm = 2*jj + jst - 1
              jo_rlm = 2*jj + jst
!
              ie_rlm = 1 + (je_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              io_rlm = 1 + (jo_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              do nd = 1, nscalar
                ie_send = nd + 3*nvector                                &
     &                       + (comm_rlm%irev_sr(ie_rlm) - 1) * ncomp
                io_send = nd + 3*nvector                                &
     &                       + (comm_rlm%irev_sr(io_rlm) - 1) * ncomp
                call set_vr_rtm_scalar_symmetry                         &
     &             (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm,                 &
     &              sph_rtm%istep_rtm, weight_rtm, nd, k_rlm, mp_rlm,   &
     &              izero, nle_rtm, nlo_rtm, ncomp, nvector,            &
     &              comm_rtm%irev_sr, n_WR, WR,                         &
     &              WK_l_sml%symp(1,ip), WK_l_sml%asmp(1,ip))
!
                call cal_scalar_sp_rlm_dotprod                          &
     &             (nle_rtm, g_sph_rlm(je_rlm,6),                       &
     &              WK_l_sml%Ps_rtm(1,jj+jst),                          &
     &              WK_l_sml%symp(1,ip), WS(ie_send))
!
                call cal_scalar_sp_rlm_dotprod                          &
     &             (nlo_rtm, g_sph_rlm(jo_rlm,6),                       &
     &              WK_l_sml%Ps_rtm(1,jj+jst+n_jk_e),                   &
     &              WK_l_sml%asmp(1,ip), WS(io_send))
              end do
            end do
!
!   the last even l-m
            do jj = nj_rlm/2+1, (nj_rlm+1)/2
              je_rlm = 2*jj + jst - 1
              ie_rlm = 1 + (je_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              do nd = 1, nscalar
                call set_vr_rtm_scalar_symmetry                         &
     &             (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm,                 &
     &              sph_rtm%istep_rtm, weight_rtm, nd, k_rlm, mp_rlm,   &
     &              izero, nle_rtm, nlo_rtm, ncomp, nvector,            &
     &              comm_rtm%irev_sr, n_WR, WR,                         &
     &              WK_l_sml%symp(1,ip), WK_l_sml%asmp(1,ip))
!
                ie_send = nd + 3*nvector                                &
     &                       + (comm_rlm%irev_sr(ie_rlm) - 1) * ncomp
                call cal_scalar_sp_rlm_dotprod                          &
     &             (nle_rtm, g_sph_rlm(je_rlm,6),                       &
     &              WK_l_sml%Ps_rtm(1,jj+jst), WK_l_sml%symp(1,ip),     &
     &              WS(ie_send))
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine leg_fwd_trans_scalar_sym_org
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_symmetry
