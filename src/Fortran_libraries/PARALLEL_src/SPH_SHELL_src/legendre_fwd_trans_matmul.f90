!>@file   legendre_fwd_trans_matmul.f90
!!@brief  module legendre_fwd_trans_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform for testing
!!
!!@verbatim
!!      subroutine alloc_vec_fleg_mat_test(nvector)
!!
!!      subroutine leg_f_trans_vector_matmul(ncomp, nvector,            &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!      subroutine leg_f_trans_scalar_matmul(ncomp, nvector, nscalar,   &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!
!!      subroutine leg_f_trans_vector_dgemm(ncomp, nvector,             &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!      subroutine leg_f_trans_scalar_dgemm(ncomp, nvector, nscalar,    &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!
!!      subroutine leg_f_trans_vector_matprod(ncomp, nvector,           &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!      subroutine leg_f_trans_vector_matprod(ncomp, nvector, nscalar,  &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm(l_rtm,m_rtm,k_rtm,icomp)
!!       size: vr_rtm(nidx_rtm(2),nidx_rtm(1)*ncomp,nidx_rtm(3))
!!      real(kind = kreal), allocatable :: vr_rtm(:,:,:)
!!
!!     spectr data for Legendre transform
!!       original layout: sp_rlm(j_rlm,k_rtm,icomp)
!!        size: sp_rlm(nidx_rlm(2),nidx_rtm(1)*ncomp)
!!      real(kind = kreal), allocatable :: sp_rlm(:,:)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_fwd_trans_matmul
!
      use m_precision
      use m_constants
      use m_work_time
      use calypso_mpi
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_legendre_work_sym_matmul
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
      subroutine leg_f_trans_vector_matmul(ncomp, nvector,              &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
!
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector*idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = nvector                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vector_matmul(kst(ip), nkr(ip),               &
     &        mp_rlm, mn_rlm, ncomp, irev_sr_rtm, n_WR, WR,             &
     &        nvec_lk, symp_r(1,ip), asmp_t(1,ip), asmp_p(1,ip),        &
     &        symn_t(1,ip), symn_p(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2) 
!
!          st_elapsed = MPI_WTIME()
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symp_r(1,ip), P_rtm(1,jst(ip)+1),    pol_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        asmp_t(1,ip), dPdt_rtm(1,jst(ip)+1), dpoldt_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symn_p(1,ip), P_rtm(1,jst(ip)+1),    dpoldp_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symn_t(1,ip), P_rtm(1,jst(ip)+1),    dtordp_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        asmp_p(1,ip), dPdt_rtm(1,jst(ip)+1), dtordt_e(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vector_matmul                                 &
     &       (kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                    &
     &        nvec_jk, pol_e(1,ip), dpoldt_e(1,ip), dpoldp_e(1,ip),     &
     &        dtordt_e(1,ip), dtordp_e(1,ip), ncomp,                    &
     &        irev_sr_rlm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_scalar_matmul(ncomp, nvector, nscalar,     &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar*idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = nscalar                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
        do mp_rlm = 1, nidx_rtm(3)
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_scalar_matmul(kst(ip), nkr(ip), mp_rlm,       &
     &        ncomp, nvector, irev_sr_rtm, n_WR, WR,                    &
     &        nscl_lk, symp(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symp(1,ip), P_rtm(1,jst(ip)+1), scl_e(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_scalar_matmul                                 &
     &       (kst(ip), nkr(ip), jst(ip), nj_rlm(ip),  nscl_jk,          &
     &       scl_e(1,ip), ncomp, nvector, irev_sr_rlm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_scalar_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_vector_dgemm(ncomp, nvector,               &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
!
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector*idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = nvector                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vector_matmul(kst(ip), nkr(ip),               &
     &        mp_rlm, mn_rlm, ncomp, irev_sr_rtm, n_WR, WR,             &
     &        nvec_lk, symp_r(1,ip), asmp_t(1,ip), asmp_p(1,ip),        &
     &        symn_t(1,ip), symn_p(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2) 
!
!          st_elapsed = MPI_WTIME()
          call dgemm_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),    &
     &      symp_r(1,ip), P_rtm(1,jst(ip)+1),    zero, pol_e(1,ip))
          call dgemm_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),    &
     &      asmp_t(1,ip), dPdt_rtm(1,jst(ip)+1), zero, dpoldt_e(1,ip))
          call dgemm_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),    &
     &      symn_p(1,ip), P_rtm(1,jst(ip)+1),    zero, dpoldp_e(1,ip))
          call dgemm_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),    &
     &      symn_t(1,ip), P_rtm(1,jst(ip)+1),    zero, dtordp_e(1,ip))
          call dgemm_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),    &
     &      asmp_p(1,ip), dPdt_rtm(1,jst(ip)+1), zero, dtordt_e(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vector_matmul                                 &
     &       (kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                    &
     &        nvec_jk, pol_e(1,ip), dpoldt_e(1,ip), dpoldp_e(1,ip),     &
     &        dtordt_e(1,ip), dtordp_e(1,ip), ncomp,                    &
     &        irev_sr_rlm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_vector_dgemm
!
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_scalar_dgemm(ncomp, nvector, nscalar,      &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar*idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = nscalar                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
        do mp_rlm = 1, nidx_rtm(3)
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_scalar_matmul(kst(ip), nkr(ip), mp_rlm,       &
     &        ncomp, nvector, irev_sr_rtm, n_WR, WR,                    &
     &        nscl_lk, symp(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
          call dgemm_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),    &
     &        symp(1,ip), P_rtm(1,jst(ip)+1), zero, scl_e(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_scalar_matmul                                 &
     &       (kst(ip), nkr(ip), jst(ip), nj_rlm(ip),  nscl_jk,          &
     &       scl_e(1,ip), ncomp, nvector, irev_sr_rlm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_scalar_dgemm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_vector_matprod(ncomp, nvector,             &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
!
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector*idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = nvector                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vector_matmul(kst(ip), nkr(ip),               &
     &        mp_rlm, mn_rlm, ncomp, irev_sr_rtm, n_WR, WR,             &
     &        nvec_lk, symp_r(1,ip), asmp_t(1,ip), asmp_p(1,ip),        &
     &        symn_t(1,ip), symn_p(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2) 
!
!          st_elapsed = MPI_WTIME()
          call matmat_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &      symp_r(1,ip), P_rtm(1,jst(ip)+1),    pol_e(1,ip))
          call matmat_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &      asmp_t(1,ip), dPdt_rtm(1,jst(ip)+1), dpoldt_e(1,ip))
          call matmat_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &      symn_p(1,ip), P_rtm(1,jst(ip)+1),    dpoldp_e(1,ip))
          call matmat_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &      symn_t(1,ip), P_rtm(1,jst(ip)+1),    dtordp_e(1,ip))
          call matmat_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &      asmp_p(1,ip), dPdt_rtm(1,jst(ip)+1), dtordt_e(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vector_matmul                                 &
     &       (kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                    &
     &        nvec_jk, pol_e(1,ip), dpoldt_e(1,ip), dpoldp_e(1,ip),     &
     &        dtordt_e(1,ip), dtordp_e(1,ip), ncomp,                    &
     &        irev_sr_rlm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_vector_matprod
!
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_scalar_matprod(ncomp, nvector, nscalar,    &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar*idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = nscalar                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
        do mp_rlm = 1, nidx_rtm(3)
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_scalar_matmul(kst(ip), nkr(ip), mp_rlm,       &
     &        ncomp, nvector, irev_sr_rtm, n_WR, WR,                    &
     &        nscl_lk, symp(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
          call matmat_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symp(1,ip), P_rtm(1,jst(ip)+1), scl_e(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_scalar_matmul                                 &
     &       (kst(ip), nkr(ip), jst(ip), nj_rlm(ip),  nscl_jk,          &
     &       scl_e(1,ip), ncomp, nvector, irev_sr_rlm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_scalar_matprod
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_matmul
