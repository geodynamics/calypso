!>@file   set_sp_rlm_for_leg_vecprod.f90
!!@brief  module set_sp_rlm_for_leg_vecprod
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Set spectrum data for backward Legendre transform
!!
!!@verbatim
!!      subroutine set_sp_rlm_vector_matmul                             &
!!     &         (kst, nkr, jst, nj_rlm, ncomp, irev_sr_rlm, n_WR, WR,  &
!!     &          nvec_jk, pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e)
!!      subroutine set_sp_rlm_scalar_matmul(kst, nkr, jst, nj_rlm,      &
!!     &          ncomp, nvector, irev_sr_rlm, n_WR, WR, nscl_jk, scl_e)
!!
!!      subroutine set_sp_rlm_vector_sym_matmul                         &
!!     &         (kst, nkr, jst, n_jk_e, n_jk_o,                        &
!!     &          ncomp, irev_sr_rlm, n_WR, WR,                         &
!!     &          nvec_jk, pol_e, dpl_e, tor_e, pol_o, dpl_o, tor_o)
!!      subroutine set_sp_rlm_scalar_sym_matmul                         &
!!     &         (kst, nkr, jst, n_jk_e, n_jk_o,                        &
!!     &          ncomp, nvector, irev_sr_rlm, n_WR, WR,                &
!!     &          nscl_jk, scl_e, scl_o)
!!
!!      subroutine set_sp_rlm_vector_equator                            &
!!     &       (jst, nd, k_rlm, a1r_1d_rlm_r, a2r_1d_rlm_r,             &
!!     &        ncomp, n_WR, irev_sr_rlm, WR, nj_rlm,                   &
!!     &        pol_e, dpoldp_e, dtordp_e, dpoldt_o, dtordt_o)
!!      subroutine set_sp_rlm_scalar_equator                            &
!!     &       (jst, nd, k_rlm, ncomp, nvector, n_WR, irev_sr_rlm, WR,  &
!!     &        nj_rlm, scl_e)
!!@endverbatim
!!
      module set_sp_rlm_for_leg_vecprod
!
      use m_precision
      use m_spheric_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_vector_blocked                              &
     &       (jst, nd, k_rlm, a1r_1d_rlm_r, a2r_1d_rlm_r,               &
     &        ncomp, n_WR, irev_sr_rlm, WR, nj_rlm,                     &
     &        pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e)
!
      use m_schmidt_poly_on_rtm
!
      integer(kind = kint), intent(in) :: jst, nd, k_rlm
      real(kind = kreal), intent(in)  :: a1r_1d_rlm_r, a2r_1d_rlm_r
      integer(kind = kint), intent(in) :: ncomp, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(inout) :: pol_e(nj_rlm)
      real(kind = kreal), intent(inout) :: dpoldt_e(nj_rlm)
      real(kind = kreal), intent(inout) :: dpoldp_e(nj_rlm)
      real(kind = kreal), intent(inout) :: dtordt_e(nj_rlm)
      real(kind = kreal), intent(inout) :: dtordp_e(nj_rlm)
!
      integer(kind = kint) :: jj, j_rlm, i_rlm, i_recv
      real(kind = kreal) :: g3, gm
!
!
!   even l-m
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        g3 = g_sph_rlm(j_rlm,3)
        gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        pol_e(jj) = WR(i_recv-2) *    a2r_1d_rlm_r * g3
        dpoldt_e(jj) = WR(i_recv-1) * a1r_1d_rlm_r
        dpoldp_e(jj) = WR(i_recv-1) * a1r_1d_rlm_r * gm
        dtordt_e(jj) = WR(i_recv  ) * a1r_1d_rlm_r
        dtordp_e(jj) = WR(i_recv  ) * a1r_1d_rlm_r * gm
      end do
!
      end subroutine set_sp_rlm_vector_blocked
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scalar_blocked                              &
     &       (jst, nd, k_rlm, ncomp, nvector, n_WR, irev_sr_rlm, WR,    &
     &        nj_rlm, scl_e)
!
      integer(kind = kint), intent(in) :: jst, nd, k_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(inout) :: scl_e(nj_rlm)
!
      integer(kind = kint) :: jj, j_rlm, i_rlm, i_recv
!
!
!   even l-m
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        scl_e(jj) = WR(i_recv)
      end do
!
      end subroutine set_sp_rlm_scalar_blocked
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_vector_symmetry                             &
     &       (jst, nd, k_rlm, a1r_1d_rlm_r, a2r_1d_rlm_r,               &
     &        ncomp, n_WR, irev_sr_rlm, WR, nj_rlm,                     &
     &        pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,            &
     &        pol_o, dpoldt_o, dpoldp_o, dtordt_o, dtordp_o)
!
      use m_schmidt_poly_on_rtm
!
      integer(kind = kint), intent(in) :: jst, nd, k_rlm
      real(kind = kreal), intent(in)  :: a1r_1d_rlm_r, a2r_1d_rlm_r
      integer(kind = kint), intent(in) :: ncomp, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(inout) :: pol_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dpoldt_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dpoldp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dtordt_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dtordp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: pol_o(nj_rlm/2)
      real(kind = kreal), intent(inout) :: dpoldt_o(nj_rlm/2)
      real(kind = kreal), intent(inout) :: dpoldp_o(nj_rlm/2)
      real(kind = kreal), intent(inout) :: dtordt_o(nj_rlm/2)
      real(kind = kreal), intent(inout) :: dtordp_o(nj_rlm/2)
!
      integer(kind = kint) :: jj, j_rlm, i_rlm, i_recv
      real(kind = kreal) :: g3, gm
!
!
!   even l-m
      do jj = 1, (nj_rlm+1)/2
        j_rlm = 2*jj + jst - 1
        g3 = g_sph_rlm(j_rlm,3)
        gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        pol_e(jj) = WR(i_recv-2) *    a2r_1d_rlm_r * g3
        dpoldt_e(jj) = WR(i_recv-1) * a1r_1d_rlm_r
        dpoldp_e(jj) = WR(i_recv-1) * a1r_1d_rlm_r * gm
        dtordt_e(jj) = WR(i_recv  ) * a1r_1d_rlm_r
        dtordp_e(jj) = WR(i_recv  ) * a1r_1d_rlm_r * gm
      end do
!   odd l-m
      do jj = 1, nj_rlm/2
        j_rlm = 2*jj + jst
        g3 = g_sph_rlm(j_rlm,3)
        gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        pol_o(jj) = WR(i_recv-2) *    a2r_1d_rlm_r * g3
        dpoldt_o(jj) = WR(i_recv-1) * a1r_1d_rlm_r
        dpoldp_o(jj) = WR(i_recv-1) * a1r_1d_rlm_r * gm
        dtordt_o(jj) = WR(i_recv  ) * a1r_1d_rlm_r
        dtordp_o(jj) = WR(i_recv  ) * a1r_1d_rlm_r * gm
      end do
!
      end subroutine set_sp_rlm_vector_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scalar_symmetry                             &
     &       (jst, nd, k_rlm, ncomp, nvector, n_WR, irev_sr_rlm, WR,    &
     &        nj_rlm, scl_e, scl_o)
!
      integer(kind = kint), intent(in) :: jst, nd, k_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(inout) :: scl_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: scl_o(nj_rlm/2)
!
      integer(kind = kint) :: jj, j_rlm, i_rlm, i_recv
!
!
!   even l-m
      do jj = 1, (nj_rlm+1)/2
        j_rlm = 2*jj + jst - 1
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        scl_e(jj) = WR(i_recv)
      end do
!   odd l-m
      do jj = 1, nj_rlm/2
        j_rlm = 2*jj + jst
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        scl_o(jj) = WR(i_recv)
      end do
!
      end subroutine set_sp_rlm_scalar_symmetry
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_vector_equator                              &
     &       (jst, nd, k_rlm, a1r_1d_rlm_r, a2r_1d_rlm_r,               &
     &        ncomp, n_WR, irev_sr_rlm, WR, nj_rlm,                     &
     &        pol_e, dpoldp_e, dtordp_e, dpoldt_o, dtordt_o)
!
      use m_schmidt_poly_on_rtm
!
      integer(kind = kint), intent(in) :: jst, nd, k_rlm
      real(kind = kreal), intent(in)  :: a1r_1d_rlm_r, a2r_1d_rlm_r
      integer(kind = kint), intent(in) :: ncomp, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(inout) :: pol_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dpoldp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dtordp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dpoldt_o(nj_rlm/2)
      real(kind = kreal), intent(inout) :: dtordt_o(nj_rlm/2)
!
      integer(kind = kint) :: jj, j_rlm, i_rlm, i_recv
      real(kind = kreal) :: g3, gm
!
!
!   even l-m
      do jj = 1, (nj_rlm+1)/2
        j_rlm = 2*jj + jst - 1
        g3 = g_sph_rlm(j_rlm,3)
        gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        pol_e(jj) = WR(i_recv-2) *    a2r_1d_rlm_r * g3
        dpoldp_e(jj) = WR(i_recv-1) * a1r_1d_rlm_r * gm
        dtordp_e(jj) = WR(i_recv  ) * a1r_1d_rlm_r * gm
      end do
!   odd l-m
      do jj = 1, nj_rlm/2
        j_rlm = 2*jj + jst
        g3 = g_sph_rlm(j_rlm,3)
        gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        dpoldt_o(jj) = WR(i_recv-1) * a1r_1d_rlm_r
        dtordt_o(jj) = WR(i_recv  ) * a1r_1d_rlm_r
      end do
!
      end subroutine set_sp_rlm_vector_equator
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scalar_equator                              &
     &       (jst, nd, k_rlm, ncomp, nvector, n_WR, irev_sr_rlm, WR,    &
     &        nj_rlm, scl_e)
!
      integer(kind = kint), intent(in) :: jst, nd, k_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(inout) :: scl_e((nj_rlm+1)/2)
!
      integer(kind = kint) :: jj, j_rlm, i_rlm, i_recv
!
!
!   even l-m
      do jj = 1, (nj_rlm+1)/2
        j_rlm = 2*jj + jst - 1
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        scl_e(jj) = WR(i_recv)
      end do
!
      end subroutine set_sp_rlm_scalar_equator
!
! -----------------------------------------------------------------------
!
      end module set_sp_rlm_for_leg_vecprod
