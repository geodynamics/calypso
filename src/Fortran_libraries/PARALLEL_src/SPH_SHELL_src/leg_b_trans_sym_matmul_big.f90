!>@file   leg_b_trans_sym_matmul_big.f90
!!@brief  module leg_b_trans_sym_matmul_big
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine leg_bwd_trans_sym_matmul_big(ncomp, nvector,         &
!!     &          irev_sr_rlm, n_WR, WR, WS)
!!      subroutine leg_bwd_trans_sym_dgemm_big(ncomp, nvector, nscalar, &
!!     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!!      subroutine leg_bwd_trans_sym_matprod_big                        &
!!     &         (ncomp, nvector, nscalar, irev_sr_rlm, irev_sr_rtm,    &
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
      module leg_b_trans_sym_matmul_big
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
      subroutine leg_bwd_trans_sym_matmul_big(ncomp, nvector, nscalar,  &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: nl_rtm, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp),  nkr(np_smp)
      integer(kind = kint) :: nkrs(np_smp),  nkrt(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      elaps(1:4) = 0
      nl_rtm = (nidx_rtm(2) + 1)/2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
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
          call set_sp_rlm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, irev_sr_rlm, n_WR, WR,                    &
     &        pol_e(1,ip), tor_e(1,ip), pol_o(1,ip), tor_o(1,ip) )
          call set_sp_rlm_scl_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, nscalar, irev_sr_rlm, n_WR, WR,           &
     &        pol_e(1,ip), pol_o(1,ip) )
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
!          st_elapsed = MPI_WTIME()
          call matmul_bwd_leg_trans(nl_rtm, nkrs(ip), n_jk_e(ip),       &
     &        Ps_rtm(1,jst(ip)+1), pol_e(1,ip), symp_r(1,ip))
          call matmul_bwd_leg_trans(nl_rtm, nkrt(ip), n_jk_e(ip),       &
     &        dPsdt_rtm(1,jst(ip)+1), tor_e(1,ip), asmp_p(1,ip))
!   odd l-m
          call matmul_bwd_leg_trans(nl_rtm, nkrs(ip), n_jk_o(ip),       &
     &        Ps_rtm(1,jst_h(ip)), pol_o(1,ip), asmp_r(1,ip))
          call matmul_bwd_leg_trans(nl_rtm, nkrt(ip), n_jk_o(ip),       &
     &        dPsdt_rtm(1,jst_h(ip)), tor_o(1,ip), symp_p(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), mp_rlm, mn_rlm, nl_rtm,                 &
     &        symp_r(1,ip), asmp_p(1,ip), asmp_r(1,ip), symp_p(1,ip),   &
     &        ncomp, nvector, irev_sr_rtm, n_WS, WS)
          call cal_vr_rtm_scl_sym_matmul_big(kst(ip), nkr(ip),          &
     &        mp_rlm, nl_rtm, symp_r(1,ip), asmp_r(1,ip),               &
     &        ncomp, nvector, nscalar, irev_sr_rtm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_bwd_trans_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_sym_dgemm_big(ncomp, nvector, nscalar,   &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: nl_rtm, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp),  nkr(np_smp)
      integer(kind = kint) :: nkrs(np_smp),  nkrt(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      elaps(1:4) = 0
      nl_rtm = (nidx_rtm(2) + 1)/2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
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
          call set_sp_rlm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, irev_sr_rlm, n_WR, WR,                    &
     &        pol_e(1,ip), tor_e(1,ip), pol_o(1,ip), tor_o(1,ip) )
          call set_sp_rlm_scl_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, nscalar, irev_sr_rlm, n_WR, WR,           &
     &        pol_e(1,ip), pol_o(1,ip) )
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
!          st_elapsed = MPI_WTIME()
          call dgemm_bwd_leg_trans(nl_rtm, nkrs(ip), n_jk_e(ip),        &
     &        Ps_rtm(1,jst(ip)+1), pol_e(1,ip), zero, symp_r(1,ip))
          call dgemm_bwd_leg_trans(nl_rtm, nkrt(ip), n_jk_e(ip),        &
     &        dPsdt_rtm(1,jst(ip)+1), tor_e(1,ip), zero, asmp_p(1,ip))
!   odd l-m
          call dgemm_bwd_leg_trans(nl_rtm, nkrs(ip), n_jk_o(ip),        &
     &        Ps_rtm(1,jst_h(ip)), pol_o(1,ip), zero, asmp_r(1,ip))
          call dgemm_bwd_leg_trans(nl_rtm, nkrt(ip), n_jk_o(ip),        &
     &        dPsdt_rtm(1,jst_h(ip)), tor_o(1,ip), zero, symp_p(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), mp_rlm, mn_rlm, nl_rtm,                 &
     &        symp_r(1,ip), asmp_p(1,ip), asmp_r(1,ip), symp_p(1,ip),   &
     &        ncomp, nvector, irev_sr_rtm, n_WS, WS)
          call cal_vr_rtm_scl_sym_matmul_big(kst(ip), nkr(ip),          &
     &        mp_rlm, nl_rtm, symp_r(1,ip), asmp_r(1,ip),               &
     &        ncomp, nvector, nscalar, irev_sr_rtm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_bwd_trans_sym_dgemm_big
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_sym_matprod_big                          &
     &         (ncomp, nvector, nscalar, irev_sr_rlm, irev_sr_rtm,      &
     &          n_WR, n_WS, WR, WS)
!
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: nl_rtm, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp),  nkr(np_smp)
      integer(kind = kint) :: nkrs(np_smp),  nkrt(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      elaps(1:4) = 0
      nl_rtm = (nidx_rtm(2) + 1)/2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
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
          call set_sp_rlm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, irev_sr_rlm, n_WR, WR,                    &
     &        pol_e(1,ip), tor_e(1,ip), pol_o(1,ip), tor_o(1,ip) )
          call set_sp_rlm_scl_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, nscalar, irev_sr_rlm, n_WR, WR,           &
     &        pol_e(1,ip), pol_o(1,ip) )
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
!          st_elapsed = MPI_WTIME()
          call matmat_bwd_leg_trans(nl_rtm, nkrs(ip), n_jk_e(ip),       &
     &        Ps_rtm(1,jst(ip)+1), pol_e(1,ip), symp_r(1,ip))
          call matmat_bwd_leg_trans(nl_rtm, nkrt(ip), n_jk_e(ip),       &
     &        dPsdt_rtm(1,jst(ip)+1), tor_e(1,ip), asmp_p(1,ip))
!   odd l-m
          call matmat_bwd_leg_trans(nl_rtm, nkrs(ip), n_jk_o(ip),       &
     &        Ps_rtm(1,jst_h(ip)), pol_o(1,ip), asmp_r(1,ip))
          call matmat_bwd_leg_trans(nl_rtm, nkrt(ip), n_jk_o(ip),       &
     &        dPsdt_rtm(1,jst_h(ip)), tor_o(1,ip), symp_p(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_vec_sym_matmul_big                            &
     &       (kst(ip), nkr(ip), mp_rlm, mn_rlm, nl_rtm,                 &
     &        symp_r(1,ip), asmp_p(1,ip), asmp_r(1,ip), symp_p(1,ip),   &
     &        ncomp, nvector, irev_sr_rtm, n_WS, WS)
          call cal_vr_rtm_scl_sym_matmul_big(kst(ip), nkr(ip),          &
     &        mp_rlm, nl_rtm, symp_r(1,ip), asmp_r(1,ip),               &
     &        ncomp, nvector, nscalar, irev_sr_rtm, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_bwd_trans_sym_matprod_big
!
! -----------------------------------------------------------------------
!
      end module leg_b_trans_sym_matmul_big
