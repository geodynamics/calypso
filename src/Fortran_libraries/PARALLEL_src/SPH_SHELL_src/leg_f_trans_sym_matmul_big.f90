!>@file   leg_f_trans_sym_matmul_big.f90
!!@brief  module leg_f_trans_sym_matmul_big
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (using matrix products version with big array)
!!
!!@verbatim
!!      subroutine leg_fwd_trans_sym_matmul_big(ncomp, nvector,         &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!      subroutine leg_fwd_trans_sym_dgemm_big(ncomp, nvector, nscalar, &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!      subroutine leg_fwd_trans_sym_matprod_big                        &
!!     &         (ncomp, nvector, nscalar,  irev_sr_rtm, irev_sr_rlm,   &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module leg_f_trans_sym_matmul_big
!
      use m_precision
!
      use m_constants
      use m_work_time
      use calypso_mpi
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_leg_trans_sym_matmul_big
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
      subroutine leg_fwd_trans_sym_matmul_big(ncomp, nvector, nscalar,  &
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
      integer(kind = kint) :: ip, mp_rlm, mn_rlm, nle_rtm, nlo_rtm
      integer(kind = kint) :: kst(np_smp),  nkr(np_smp)
      integer(kind = kint) :: nkrs(np_smp),  nkrt(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      elaps(1:4) = 0
      nle_rtm = (nidx_rtm(2) + 1)/2
      nlo_rtm = nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1)
        nkrs(ip) = ncomp*nkr(ip)
        nkrt(ip) = 2*nvector*nkr(ip)
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          jst(ip) = lstack_rlm(mp_rlm-1)
          jst_h(ip) = lstack_even_rlm(mp_rlm) + 1
          n_jk_e(ip) = lstack_even_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
          n_jk_o(ip) = lstack_rlm(mp_rlm) - lstack_even_rlm(mp_rlm)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), mp_rlm, mn_rlm, nle_rtm, nlo_rtm,       &
     &        ncomp, nvector, irev_sr_rtm, n_WR, WR,                    &
     &        symp_r(1,ip), asmp_p(1,ip), asmp_r(1,ip), symp_p(1,ip) )
          call set_vr_rtm_scl_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), mp_rlm, nle_rtm, nlo_rtm,               &
     &        ncomp, nvector, nscalar, irev_sr_rtm, n_WR, WR,           &
     &        symp_r(1,ip), asmp_r(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
!  even l-m
          call matmul_fwd_leg_trans(nkrs(ip), n_jk_e(ip), nth_hemi_rtm, &
     &        symp_r(1,ip), Ps_rtm(1,jst(ip)+1), pol_e(1,ip))
          call matmul_fwd_leg_trans(nkrt(ip), n_jk_e(ip), nth_hemi_rtm, &
     &        asmp_p(1,ip), dPsdt_rtm(1,jst(ip)+1), tor_e(1,ip))
!
!  odd l-m
          call matmul_fwd_leg_trans(nkrs(ip), n_jk_o(ip), nth_hemi_rtm, &
     &        asmp_r(1,ip), Ps_rtm(1,jst_h(ip)), pol_o(1,ip))
          call matmul_fwd_leg_trans(nkrt(ip), n_jk_o(ip), nth_hemi_rtm, &
     &        symp_p(1,ip), dPsdt_rtm(1,jst_h(ip)), tor_o(1,ip))
  !          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        pol_e(1,ip), pol_o(1,ip), tor_e(1,ip), tor_o(1,ip),       &
     &        ncomp, nvector, irev_sr_rlm, n_WS, WS)
          call cal_sp_rlm_scl_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        pol_e(1,ip), pol_o(1,ip), ncomp, nvector, nscalar,        &
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
      end subroutine leg_fwd_trans_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_sym_dgemm_big(ncomp, nvector, nscalar,   &
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
      integer(kind = kint) :: ip, mp_rlm, mn_rlm, nle_rtm, nlo_rtm
      integer(kind = kint) :: kst(np_smp),  nkr(np_smp)
      integer(kind = kint) :: nkrs(np_smp),  nkrt(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      elaps(1:4) = 0
      nle_rtm = (nidx_rtm(2) + 1)/2
      nlo_rtm = nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1)
        nkrs(ip) = ncomp*nkr(ip)
        nkrt(ip) = 2*nvector*nkr(ip)
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          jst(ip) = lstack_rlm(mp_rlm-1)
          jst_h(ip) = lstack_even_rlm(mp_rlm) + 1
          n_jk_e(ip) = lstack_even_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
          n_jk_o(ip) = lstack_rlm(mp_rlm) - lstack_even_rlm(mp_rlm)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), mp_rlm, mn_rlm, nle_rtm, nlo_rtm,       &
     &        ncomp, nvector, irev_sr_rtm, n_WR, WR,                    &
     &        symp_r(1,ip), asmp_p(1,ip), asmp_r(1,ip), symp_p(1,ip) )
          call set_vr_rtm_scl_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), mp_rlm, nle_rtm, nlo_rtm,               &
     &        ncomp, nvector, nscalar, irev_sr_rtm, n_WR, WR,           &
     &        symp_r(1,ip), asmp_r(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
!  even l-m
          call dgemm_fwd_leg_trans(nkrs(ip), n_jk_e(ip), nth_hemi_rtm,  &
     &        symp_r(1,ip), Ps_rtm(1,jst(ip)+1), zero, pol_e(1,ip))
          call dgemm_fwd_leg_trans(nkrt(ip), n_jk_e(ip), nth_hemi_rtm,  &
     &        asmp_p(1,ip), dPsdt_rtm(1,jst(ip)+1), zero, tor_e(1,ip))
!
!  odd l-m
          call dgemm_fwd_leg_trans(nkrs(ip), n_jk_o(ip), nth_hemi_rtm,  &
     &        asmp_r(1,ip), Ps_rtm(1,jst_h(ip)), zero, pol_o(1,ip))
          call dgemm_fwd_leg_trans(nkrt(ip), n_jk_o(ip), nth_hemi_rtm,  &
     &        symp_p(1,ip), dPsdt_rtm(1,jst_h(ip)), zero, tor_o(1,ip))
  !          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        pol_e(1,ip), pol_o(1,ip), tor_e(1,ip), tor_o(1,ip),       &
     &        ncomp, nvector, irev_sr_rlm, n_WS, WS)
          call cal_sp_rlm_scl_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        pol_e(1,ip), pol_o(1,ip), ncomp, nvector, nscalar,        &
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
      end subroutine leg_fwd_trans_sym_dgemm_big
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_sym_matprod_big                          &
     &         (ncomp, nvector, nscalar,  irev_sr_rtm, irev_sr_rlm,     &
     &          n_WR, n_WS, WR, WS)
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
      integer(kind = kint) :: ip, mp_rlm, mn_rlm, nle_rtm, nlo_rtm
      integer(kind = kint) :: kst(np_smp),  nkr(np_smp)
      integer(kind = kint) :: nkrs(np_smp),  nkrt(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      elaps(1:4) = 0
      nle_rtm = (nidx_rtm(2) + 1)/2
      nlo_rtm = nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1)
        nkrs(ip) = ncomp*nkr(ip)
        nkrt(ip) = 2*nvector*nkr(ip)
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          jst(ip) = lstack_rlm(mp_rlm-1)
          jst_h(ip) = lstack_even_rlm(mp_rlm) + 1
          n_jk_e(ip) = lstack_even_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
          n_jk_o(ip) = lstack_rlm(mp_rlm) - lstack_even_rlm(mp_rlm)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), mp_rlm, mn_rlm, nle_rtm, nlo_rtm,       &
     &        ncomp, nvector, irev_sr_rtm, n_WR, WR,                    &
     &        symp_r(1,ip), asmp_p(1,ip), asmp_r(1,ip), symp_p(1,ip) )
          call set_vr_rtm_scl_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), mp_rlm, nle_rtm, nlo_rtm,               &
     &        ncomp, nvector, nscalar, irev_sr_rtm, n_WR, WR,           &
     &        symp_r(1,ip), asmp_r(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
!  even l-m
          call matmat_fwd_leg_trans(nkrs(ip), n_jk_e(ip), nth_hemi_rtm, &
     &        symp_r(1,ip), Ps_rtm(1,jst(ip)+1), pol_e(1,ip))
          call matmat_fwd_leg_trans(nkrt(ip), n_jk_e(ip), nth_hemi_rtm, &
     &        asmp_p(1,ip), dPsdt_rtm(1,jst(ip)+1), tor_e(1,ip))
!
!  odd l-m
          call matmat_fwd_leg_trans(nkrs(ip), n_jk_o(ip), nth_hemi_rtm, &
     &        asmp_r(1,ip), Ps_rtm(1,jst_h(ip)), pol_o(1,ip))
          call matmat_fwd_leg_trans(nkrt(ip), n_jk_o(ip), nth_hemi_rtm, &
     &        symp_p(1,ip), dPsdt_rtm(1,jst_h(ip)), tor_o(1,ip))
  !          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        pol_e(1,ip), pol_o(1,ip), tor_e(1,ip), tor_o(1,ip),       &
     &        ncomp, nvector, irev_sr_rlm, n_WS, WS)
          call cal_sp_rlm_scl_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        pol_e(1,ip), pol_o(1,ip), ncomp, nvector, nscalar,        &
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
      end subroutine leg_fwd_trans_sym_matprod_big
!
! -----------------------------------------------------------------------
!
      end module leg_f_trans_sym_matmul_big
