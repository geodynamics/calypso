!>@file   legendre_bwd_trans_spin.f90
!!@brief  module legendre_bwd_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_spin(ncomp, nvector,         &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, P_jl, dPdt_jl,          &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_spin(ncomp, nvector, nscalar,&
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns, P_jl, &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!
!!         type(sph_rlm_grid), intent(in) :: sph_rlm
!!         type(sph_rtm_grid), intent(in) :: sph_rtm
!!         type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!         type(index_4_sph_trans), intent(in) :: idx_trns
!!         type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_spin
!
      use m_precision
      use m_machine_parameter
!
      use t_legendre_work_matmul
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
      subroutine legendre_b_trans_vector_spin(ncomp, nvector,           &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, P_jl, dPdt_jl,            &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
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
      real(kind= kreal), intent(in)                                     &
     &           :: P_jl(sph_rlm%nidx_rlm(2),sph_rtm%nidx_rtm(2))
      real(kind= kreal), intent(in)                                     &
     &           :: dPdt_jl(sph_rlm%nidx_rlm(2),sph_rtm%nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: ip_send, in_send
      integer(kind = kint) :: kr_nd, k_rlm, l_rtm, nd
      integer(kind = kint) :: ip, kst, ked, lp, lst, led
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
!$omp&            private(ip,kst,ked,kr_nd,lp,lst,led,jst,nj_rlm,k_rlm, &
!$omp&                    l_rtm,nd,ip_rtm,in_rtm,ip_send,in_send,       &
!$omp&                    mp_rlm,mn_rlm,a1r_1d_rlm_r,a2r_1d_rlm_r)
      do ip = 1, np_smp
        kst = nvector*sph_rtm%istack_rtm_kr_smp(ip-1) + 1
        ked = nvector*sph_rtm%istack_rtm_kr_smp(ip  )
        do kr_nd = kst, ked
          nd = 1 + mod((kr_nd-1),nvector)
          k_rlm = 1 + (kr_nd - nd) / nvector
          a1r_1d_rlm_r = sph_rlm%a_r_1d_rlm_r(k_rlm)
          a2r_1d_rlm_r = a1r_1d_rlm_r**2
          do lp = 1, idx_trns%nblock_l_rtm
            lst = idx_trns%lstack_block_rtm(lp-1) + 1
            led = idx_trns%lstack_block_rtm(lp  )
            do mp_rlm = 1, sph_rtm%nidx_rtm(3)
              mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
              jst = idx_trns%lstack_rlm(mp_rlm-1)
              nj_rlm = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
              do l_rtm = lst, led
!
                ip_rtm = 1 + (l_rtm-1) *  sph_rtm%istep_rtm(2)          &
     &                     + (k_rlm-1) *  sph_rtm%istep_rtm(1)          &
     &                     + (mp_rlm-1) * sph_rtm%istep_rtm(3)
                in_rtm = 1 + (l_rtm-1) *  sph_rtm%istep_rtm(2)          &
     &                     + (k_rlm-1) *  sph_rtm%istep_rtm(1)          &
     &                     + (mn_rlm-1) * sph_rtm%istep_rtm(3)
                ip_send = 3*nd-2 + (comm_rtm%irev_sr(ip_rtm)-1) * ncomp
                in_send = 3*nd-2 + (comm_rtm%irev_sr(in_rtm)-1) * ncomp
!
                call set_sp_rlm_vector_blocked                          &
     &             (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,                 &
     &              sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,         &
     &              g_sph_rlm, jst, nd, k_rlm,                          &
     &              a1r_1d_rlm_r, a2r_1d_rlm_r,                         &
     &              ncomp, n_WR, comm_rlm%irev_sr, WR, nj_rlm,          &
     &              WK_l_mtl%pol_e(1,ip), WK_l_mtl%dpoldt_e(1,ip),      &
     &              WK_l_mtl%dpoldp_e(1,ip), WK_l_mtl%dtordt_e(1,ip),   &
     &              WK_l_mtl%dtordp_e(1,ip))
!
                call cal_vr_rtm_dydtheta_vector                         &
     &             (nj_rlm, P_jl(jst+1,l_rtm), dPdt_jl(jst+1,l_rtm),    &
     &              WK_l_mtl%pol_e(1,ip), WK_l_mtl%dpoldt_e(1,ip),      &
     &              WK_l_mtl%dtordt_e(1,ip), WS(ip_send))
                call cal_vr_rtm_dydphi_vector(nj_rlm,                   &
     &              P_jl(jst+1,l_rtm), asin_theta_1d_rtm(l_rtm),        &
     &              WK_l_mtl%dpoldp_e(1,ip), WK_l_mtl%dtordp_e(1,ip),   &
     &              WS(in_send))
              end do
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_spin(ncomp, nvector, nscalar,  &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns, P_jl,   &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use cal_vr_rtm_by_vecprod
      use set_sp_rlm_for_leg_vecprod
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind= kreal), intent(in)                                     &
     &           :: P_jl(sph_rlm%nidx_rlm(2),sph_rtm%nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
      integer(kind = kint) :: kr_nd, k_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, nd, ip, kst, ked, lp, lst, led
      integer(kind = kint) :: ip_send, i_recv
      integer(kind = kint) :: mp_rlm, jst, nj_rlm
!
!
      if(nscalar .le. 0) return
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,kr_nd,lp,lst,led,l_rtm,nd,i_recv,  &
!$omp&                    k_rlm,ip_rtm,ip_send,mp_rlm,jst,nj_rlm)
      do ip = 1, np_smp
        kst = nscalar*sph_rtm%istack_rtm_kr_smp(ip-1) + 1
        ked = nscalar*sph_rtm%istack_rtm_kr_smp(ip  )
        do kr_nd = kst, ked
          nd = 1 + mod((kr_nd-1),nscalar)
          k_rlm = 1 + (kr_nd - nd) / nscalar
          do lp = 1, idx_trns%nblock_l_rtm
            lst = idx_trns%lstack_block_rtm(lp-1) + 1
            led = idx_trns%lstack_block_rtm(lp  )
!
            do mp_rlm = 1, sph_rtm%nidx_rtm(3)
              jst = idx_trns%lstack_rlm(mp_rlm-1)
              nj_rlm = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
!
              do l_rtm = lst, led
                ip_rtm = 1 + (l_rtm-1) *  sph_rtm%istep_rtm(2)          &
     &                     + (k_rlm-1) *  sph_rtm%istep_rtm(1)          &
     &                     + (mp_rlm-1) * sph_rtm%istep_rtm(3)
                ip_send = nd + 3*nvector                                &
     &                       + (comm_rtm%irev_sr(ip_rtm)-1) * ncomp
!
                call set_sp_rlm_scalar_blocked                          &
     &             (sph_rlm%nnod_rlm, sph_rlm%istep_rlm,                &
     &              jst, nd, k_rlm, ncomp, nvector,                     &
     &              n_WR, comm_rlm%irev_sr, WR, nj_rlm,                 &
     &              WK_l_mtl%scl_e(1,ip))
                call cal_vr_rtm_scalar_blocked(nj_rlm,                  &
     &              P_jl(jst+1,l_rtm), WK_l_mtl%scl_e(1,ip),            &
     &              WS(ip_send))
              end do
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_spin
