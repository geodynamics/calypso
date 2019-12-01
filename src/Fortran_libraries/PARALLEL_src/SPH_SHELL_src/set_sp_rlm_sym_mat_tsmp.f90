!>@file   set_sp_rlm_sym_mat_tsmp.f90
!!@brief  module set_sp_rlm_sym_mat_tsmp
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine set_sp_rlm_sym_mat_rin(nnod_rlm, nidx_rlm,           &
!!     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,  &
!!     &          jst, n_jk_e, n_jk_o, ncomp_recv, nvector, nscalar,    &
!!     &          irev_sr_rlm, n_WR, WR,  pol_e, tor_e, pol_o, tor_o)
!!      subroutine set_sp_rlm_sym_mat_rout(nnod_rlm, nidx_rlm,          &
!!     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,  &
!!     &          jst, n_jk_e, n_jk_o, ncomp_recv, nvector, nscalar,    &
!!     &          irev_sr_rlm, n_WR, WR,  pol_e, tor_e, pol_o, tor_o)
!!@endverbatim
!!
      module set_sp_rlm_sym_mat_tsmp
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
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
      subroutine set_sp_rlm_sym_mat_rin(nnod_rlm, nidx_rlm,             &
     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,    &
     &          jst, n_jk_e, n_jk_o, ncomp_recv, nvector, nscalar,      &
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
      end subroutine set_sp_rlm_sym_mat_rin
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_sym_mat_rout(nnod_rlm, nidx_rlm,            &
     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,    &
     &          jst, n_jk_e, n_jk_o, ncomp_recv, nvector, nscalar,      &
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
     &           :: pol_e(n_jk_e,nidx_rlm(1),3*nvector+nscalar)
      real(kind = kreal), intent(inout)                                 &
     &           :: tor_e(n_jk_e,nidx_rlm(1),2*nvector)
      real(kind = kreal), intent(inout)                                 &
     &           :: pol_o(n_jk_o,nidx_rlm(1),3*nvector+nscalar)
      real(kind = kreal), intent(inout)                                 &
     &           :: tor_o(n_jk_o,nidx_rlm(1),2*nvector)
!
      integer(kind = kint) :: jj, kr_nd, k_rlm, nd
      integer(kind = kint) :: j_rlm, i_rlm, i_recv
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
      real(kind = kreal) :: g3, gm
!
!
!$omp parallel do private(kr_nd,k_rlm,nd,a1r_1d_rlm_r,a2r_1d_rlm_r,     &
!$omp&                    jj,j_rlm,i_rlm,i_recv,g3,gm)
      do kr_nd = 1, nvector*nidx_rlm(1)
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
        a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
        a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        do jj = 1, n_jk_e
          j_rlm = 2*jj + jst - 1
          g3 = g_sph_rlm(j_rlm,3)
          gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
!
          pol_e(jj,k_rlm,3*nd-2) = WR(i_recv-2) * a2r_1d_rlm_r * g3
          tor_e(jj,k_rlm,2*nd  ) = WR(i_recv-1) * a1r_1d_rlm_r
          pol_e(jj,k_rlm,3*nd  ) = WR(i_recv-1) * a1r_1d_rlm_r * gm
          tor_e(jj,k_rlm,2*nd-1) = WR(i_recv  ) * a1r_1d_rlm_r
          pol_e(jj,k_rlm,3*nd-1) = WR(i_recv  ) * a1r_1d_rlm_r * gm
        end do
!
!   odd l-m
        do jj = 1, n_jk_o
          j_rlm = 2*jj + jst
          g3 = g_sph_rlm(j_rlm,3)
          gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
!
          pol_o(jj,k_rlm,3*nd-2) = WR(i_recv-2) * a2r_1d_rlm_r * g3
          tor_o(jj,k_rlm,2*nd  ) = WR(i_recv-1) * a1r_1d_rlm_r
          pol_o(jj,k_rlm,3*nd  ) = WR(i_recv-1) * a1r_1d_rlm_r * gm
          tor_o(jj,k_rlm,2*nd-1) = WR(i_recv  ) * a1r_1d_rlm_r
          pol_o(jj,k_rlm,3*nd-1) = WR(i_recv  ) * a1r_1d_rlm_r * gm
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(kr_nd,k_rlm,nd,jj,i_rlm,i_recv)
      do kr_nd = 1, nscalar*nidx_rlm(1)
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
!   even l-m
        do jj = 1, n_jk_e
          i_rlm = 1 + (2*jj + jst - 2) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = nd + 3*nvector                                       &
     &            + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
          pol_e(jj,k_rlm,nd+3*nvector) = WR(i_recv)
        end do
!
!   odd l-m
        do jj = 1, n_jk_o
          i_rlm = 1 + (2*jj + jst - 1) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = nd + 3*nvector                                       &
     &            + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
          pol_o(jj,k_rlm,nd+3*nvector) = WR(i_recv)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_sp_rlm_sym_mat_rout
!
! -----------------------------------------------------------------------
!
      end module set_sp_rlm_sym_mat_tsmp
