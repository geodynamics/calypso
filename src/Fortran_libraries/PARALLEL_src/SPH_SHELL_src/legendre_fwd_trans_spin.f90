!>@file   legendre_fwd_trans_spin.f90
!!@brief  module legendre_fwd_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_spin(ncomp, nvector,         &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_f_trans_scalar_spin                         &
!!     &         (ncomp, nvector, nscalar, vr_rtm, sp_rlm)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_fwd_trans_spin
!
      use m_precision
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
      subroutine legendre_f_trans_vector_spin(ncomp, nvector,           &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      use set_vr_rtm_for_leg_vecprod
      use cal_sp_rlm_by_vecprod
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm, i_send
      integer(kind = kint) :: kr_nd, nd, ip, kst, ked, lp, lst, nth
      real(kind = kreal) :: r1_1d_rlm_r, r2_1d_rlm_r, g7, gm
!
!
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,kst,ked,lp,lst,nth,j_rlm,k_rlm,nd,kr_nd,   &
!$omp&                    i_rlm,r1_1d_rlm_r,r2_1d_rlm_r,i_send,g7,gm)
      do ip = 1, np_smp
        kst = nvector*idx_rlm_smp_stack(ip-1,1) + 1
        ked = nvector*idx_rlm_smp_stack(ip,  1)
        do kr_nd = kst, ked
          nd = 1 + mod((kr_nd-1),nvector)
          k_rlm = 1 + (kr_nd - nd) / nvector
          r1_1d_rlm_r = radius_1d_rlm_r(k_rlm)
          r2_1d_rlm_r = radius_1d_rlm_r(k_rlm)*radius_1d_rlm_r(k_rlm)
!
          do lp = 1, nblock_l_rtm
            lst = lstack_block_rtm(lp-1) 
            nth = lstack_block_rtm(lp  ) - lstack_block_rtm(lp-1)
!
            do j_rlm = 1, nidx_rlm(2)
              g7 = g_sph_rlm(j_rlm,7)
              gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
!
              i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                      &
     &                  + (k_rlm-1) * istep_rlm(1)
              i_send = 3*nd-2 + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
              call set_vr_rtm_vector_blocked(nd, k_rlm,                 &
     &              mdx_p_rlm_rtm(j_rlm), mdx_n_rlm_rtm(j_rlm),         &
     &              asin_theta_1d_rtm(1+lst), lst, nth,                 &
     &              ncomp, irev_sr_rtm, n_WR, WR,                       &
     &              symp_r(1,ip), asmp_t(1,ip), asmp_p(1,ip),           &
     &              symn_t(1,ip), symn_p(1,ip))
              call cal_vector_sp_rlm_dotprod                            &
     &             (nth, g7, gm, r1_1d_rlm_r, r2_1d_rlm_r,              &
     &              P_rtm(1+lst,j_rlm), dPdt_rtm(1+lst,j_rlm),          &
     &              symp_r(1,ip), asmp_t(1,ip), asmp_p(1,ip),           &
     &              symn_t(1,ip), symn_p(1,ip), WS(i_send))
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_spin(ncomp, nvector, nscalar,  &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      use set_vr_rtm_for_leg_vecprod
      use cal_sp_rlm_by_vecprod
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm, i_send
      integer(kind = kint) :: kr_nd, nd, ip, kst, ked, lp, lst, nth
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,nth,j_rlm,kr_nd,k_rlm,nd,   &
!$omp&                    i_rlm,i_send)
      do ip = 1, np_smp
        kst = nscalar*idx_rlm_smp_stack(ip-1,1) + 1
        ked = nscalar*idx_rlm_smp_stack(ip,  1)
!
        do kr_nd = kst, ked
          nd = 1 + mod((kr_nd-1),nscalar)
          k_rlm = 1 + (kr_nd - nd) / nscalar
          do lp = 1, nblock_l_rtm
            lst = lstack_block_rtm(lp-1)
            nth = lstack_block_rtm(lp  ) - lstack_block_rtm(lp-1)
!
            do j_rlm = 1, nidx_rlm(2)
              i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                      &
     &                    + (k_rlm-1) * istep_rlm(1)
              i_send = nd + 3*nvector                                   &
     &                      + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
              call set_vr_rtm_scalar_blocked                            &
     &             (nd, k_rlm, mdx_p_rlm_rtm(j_rlm), lst, nth,          &
     &              ncomp, nvector, irev_sr_rtm, n_WR, WR, symp(1,ip))
              call cal_scalar_sp_rlm_dotprod(nth, g_sph_rlm(j_rlm,6),   &
     &             P_rtm(lst+1,j_rlm), symp(1,ip), WS(i_send))
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_spin
