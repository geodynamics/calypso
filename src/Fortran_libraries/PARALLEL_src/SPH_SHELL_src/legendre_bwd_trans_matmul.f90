!>@file   legendre_bwd_trans_matmul.f90
!!@brief  module legendre_bwd_trans_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Developping version)
!!
!!@verbatim
!!      subroutine leg_b_trans_vector_matmul(ncomp, nvector,            &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, P_rtm, dPdt_rtm,        &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!      subroutine leg_b_trans_scalar_matmul(ncomp, nvector, nscalar,   &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          P_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
!!
!!      subroutine leg_b_trans_vector_dgemm(ncomp, nvector,             &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, P_rtm, dPdt_rtm,        &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!      subroutine leg_b_trans_scalar_dgemm(ncomp, nvector, nscalar,    &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          P_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
!!
!!      subroutine leg_b_trans_vector_matprod(ncomp, nvector,           &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, P_rtm, dPdt_rtm,        &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!      subroutine leg_b_trans_scalar_matprod(ncomp, nvector, nscalar,  &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          P_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!!
!!        Input:  sp_rlm
!!        Output: vr_rtm
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm(l_rtm,m_rtm,k_rlm,icomp)
!!       size: vr_rtm(nidx_rtm(2),nidx_rtm(1)*ncomp,nidx_rtm(3))
!!      real(kind = kreal), allocatable :: vr_rtm(:,:,:)
!!
!!     spectr data for Legendre transform
!!       original layout: sp_rlm(j_rlm,k_rlm,icomp)
!!        size: sp_rlm(nidx_rlm(2),nidx_rtm(1)*ncomp)
!!      real(kind = kreal), allocatable :: sp_rlm(:,:)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_matmul
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_legendre_work_matmul
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
!
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
      subroutine leg_b_trans_vector_matmul(ncomp, nvector,              &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, P_rtm, dPdt_rtm,          &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
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
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
      real(kind= kreal), intent(in)                                     &
     &           :: dPdt_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
      integer(kind = kint) :: nb_nri, ip, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .le. 0) return
      elaps(1:4) = 0
!
      nb_nri = nvector * sph_rtm%nidx_rtm(1)
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nvector * (sph_rlm%istack_rlm_kr_smp(ip)              &
     &                     - sph_rlm%istack_rlm_kr_smp(ip-1))
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
            jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm(ip) = idx_trns%lstack_rlm(mp_rlm)                    &
     &                  - idx_trns%lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_sp_rlm_vector_matmul                                 &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r, g_sph_rlm, &
     &        kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                    &
     &        ncomp, comm_rlm%irev_sr, n_WR, WR,                        &
     &        WK_l_mtl%nvec_jk, WK_l_mtl%pol_e(1,ip),                   &
     &        WK_l_mtl%dpoldt_e(1,ip), WK_l_mtl%dpoldp_e(1,ip),         &
     &        WK_l_mtl%dtordt_e(1,ip), WK_l_mtl%dtordp_e(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
!          st_elapsed = MPI_WTIME()
          call matmul_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1),    WK_l_mtl%pol_e(1,ip),              &
     &        WK_l_mtl%symp_r(1,ip))
          call matmul_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        dPdt_rtm(1,jst(ip)+1), WK_l_mtl%dpoldt_e(1,ip),           &
     &        WK_l_mtl%asmp_t(1,ip))
          call matmul_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        dPdt_rtm(1,jst(ip)+1), WK_l_mtl%dtordt_e(1,ip),           &
     &        WK_l_mtl%asmp_p(1,ip))
!
          call matmul_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1), WK_l_mtl%dtordp_e(1,ip),              &
     &        WK_l_mtl%symn_t(1,ip))
          call matmul_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1), WK_l_mtl%dpoldp_e(1,ip),              &
     &        WK_l_mtl%symn_p(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_vector_matmul                                 &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm,                      &
     &        kst(ip), nkr(ip), mp_rlm, mn_rlm, WK_l_mtl%nvec_lk,       &
     &        WK_l_mtl%symp_r(1,ip), WK_l_mtl%asmp_t(1,ip),             &
     &        WK_l_mtl%asmp_p(1,ip), WK_l_mtl%symn_t(1,ip),             &
     &        WK_l_mtl%symn_p(1,ip), ncomp, comm_rtm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_b_trans_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_scalar_matmul(ncomp, nvector, nscalar,     &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          P_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind= kreal), intent(in)                                     &
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      if(nscalar .le. 0) return
      elaps(1:4) = 0
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nscalar * (sph_rlm%istack_rlm_kr_smp(ip)              &
     &                     - sph_rlm%istack_rlm_kr_smp(ip-1))
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_sp_rlm_scalar_matmul                                 &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                    &
     &        ncomp, nvector, comm_rlm%irev_sr, n_WR, WR,               &
     &        WK_l_mtl%nscl_jk, WK_l_mtl%scl_e(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
          call matmul_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1), WK_l_mtl%scl_e(1,ip),                 &
     &        WK_l_mtl%symp(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_scalar_matmul                                 &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, kst(ip), nkr(ip), mp_rlm,               &
     &        WK_l_mtl%nscl_lk, WK_l_mtl%symp(1,ip),                    &
     &        ncomp, nvector, comm_rtm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_b_trans_scalar_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_vector_dgemm(ncomp, nvector,               &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, P_rtm, dPdt_rtm,          &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
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
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
      real(kind= kreal), intent(in)                                     &
     &           :: dPdt_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
      integer(kind = kint) :: nb_nri, ip, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .le. 0) return
      elaps(1:4) = 0
!
      nb_nri = nvector * sph_rtm%nidx_rtm(1)
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nvector * (sph_rlm%istack_rlm_kr_smp(ip)              &
     &                     - sph_rlm%istack_rlm_kr_smp(ip-1))
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
            jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm(ip) = idx_trns%lstack_rlm(mp_rlm)                    &
     &                  - idx_trns%lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_sp_rlm_vector_matmul                                 &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r, g_sph_rlm, &
     &        kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                    &
     &        ncomp, comm_rlm%irev_sr, n_WR, WR,                        &
     &        WK_l_mtl%nvec_jk, WK_l_mtl%pol_e(1,ip),                   &
     &        WK_l_mtl%dpoldt_e(1,ip), WK_l_mtl%dpoldp_e(1,ip),         &
     &        WK_l_mtl%dtordt_e(1,ip), WK_l_mtl%dtordp_e(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
!          st_elapsed = MPI_WTIME()
          call dgemm_bwd_leg_trans                                      &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1),    WK_l_mtl%pol_e(1,ip),              &
     &        zero, WK_l_mtl%symp_r(1,ip))
          call dgemm_bwd_leg_trans                                      &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        dPdt_rtm(1,jst(ip)+1), WK_l_mtl%dpoldt_e(1,ip),           &
     &        zero, WK_l_mtl%asmp_t(1,ip))
          call dgemm_bwd_leg_trans                                      &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        dPdt_rtm(1,jst(ip)+1), WK_l_mtl%dtordt_e(1,ip),           &
     &        zero, WK_l_mtl%asmp_p(1,ip))
!
          call dgemm_bwd_leg_trans                                      &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1), WK_l_mtl%dtordp_e(1,ip),              &
     &        zero, WK_l_mtl%symn_t(1,ip))
          call dgemm_bwd_leg_trans                                      &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1), WK_l_mtl%dpoldp_e(1,ip),              &
     &        zero, WK_l_mtl%symn_p(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_vector_matmul                                 &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm,                      &
     &        kst(ip), nkr(ip), mp_rlm, mn_rlm, WK_l_mtl%nvec_lk,       &
     &        WK_l_mtl%symp_r(1,ip), WK_l_mtl%asmp_t(1,ip),             &
     &        WK_l_mtl%asmp_p(1,ip), WK_l_mtl%symn_t(1,ip),             &
     &        WK_l_mtl%symn_p(1,ip), ncomp, comm_rtm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_b_trans_vector_dgemm
!
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_scalar_dgemm(ncomp, nvector, nscalar,      &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          P_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind= kreal), intent(in)                                     &
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      if(nscalar .le. 0) return
      elaps(1:4) = 0
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nscalar * (sph_rlm%istack_rlm_kr_smp(ip)              &
     &                     - sph_rlm%istack_rlm_kr_smp(ip-1))
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_sp_rlm_scalar_matmul                                 &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                    &
     &        ncomp, nvector, comm_rlm%irev_sr, n_WR, WR,               &
     &        WK_l_mtl%nscl_jk, WK_l_mtl%scl_e(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
          call dgemm_bwd_leg_trans                                      &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1), WK_l_mtl%scl_e(1,ip),                 &
     &        zero, WK_l_mtl%symp(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_scalar_matmul                                 &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, kst(ip), nkr(ip), mp_rlm,               &
     &        WK_l_mtl%nscl_lk, WK_l_mtl%symp(1,ip),                    &
     &        ncomp, nvector, comm_rtm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_b_trans_scalar_dgemm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_vector_matprod(ncomp, nvector,             &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, P_rtm, dPdt_rtm,          &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
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
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
      real(kind= kreal), intent(in)                                     &
     &           :: dPdt_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
      integer(kind = kint) :: nb_nri, ip, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .le. 0) return
      elaps(1:4) = 0
!
      nb_nri = nvector * sph_rtm%nidx_rtm(1)
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nvector * (sph_rlm%istack_rlm_kr_smp(ip)              &
     &                     - sph_rlm%istack_rlm_kr_smp(ip-1))
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
            jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm(ip) = idx_trns%lstack_rlm(mp_rlm)                    &
     &                  - idx_trns%lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_sp_rlm_vector_matmul                                 &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r, g_sph_rlm, &
     &        kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                    &
     &        ncomp, comm_rlm%irev_sr, n_WR, WR,                        &
     &        WK_l_mtl%nvec_jk, WK_l_mtl%pol_e(1,ip),                   &
     &        WK_l_mtl%dpoldt_e(1,ip), WK_l_mtl%dpoldp_e(1,ip),         &
     &        WK_l_mtl%dtordt_e(1,ip), WK_l_mtl%dtordp_e(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
!          st_elapsed = MPI_WTIME()
          call matmat_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1),    WK_l_mtl%pol_e(1,ip),              &
     &        WK_l_mtl%symp_r(1,ip))
          call matmat_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        dPdt_rtm(1,jst(ip)+1), WK_l_mtl%dpoldt_e(1,ip),           &
     &        WK_l_mtl%asmp_t(1,ip))
          call matmat_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        dPdt_rtm(1,jst(ip)+1), WK_l_mtl%dtordt_e(1,ip),           &
     &        WK_l_mtl%asmp_p(1,ip))
!
          call matmat_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1), WK_l_mtl%dtordp_e(1,ip),              &
     &        WK_l_mtl%symn_t(1,ip))
          call matmat_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1), WK_l_mtl%dpoldp_e(1,ip),              &
     &        WK_l_mtl%symn_p(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_vector_matmul                                 &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm,                      &
     &        kst(ip), nkr(ip), mp_rlm, mn_rlm, WK_l_mtl%nvec_lk,       &
     &        WK_l_mtl%symp_r(1,ip), WK_l_mtl%asmp_t(1,ip),             &
     &        WK_l_mtl%asmp_p(1,ip), WK_l_mtl%symn_t(1,ip),             &
     &        WK_l_mtl%symn_p(1,ip), ncomp, comm_rtm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_b_trans_vector_matprod
!
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_scalar_matprod(ncomp, nvector, nscalar,    &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          P_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind= kreal), intent(in)                                     &
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      if(nscalar .le. 0) return
      elaps(1:4) = 0
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nscalar * (sph_rlm%istack_rlm_kr_smp(ip)              &
     &                     - sph_rlm%istack_rlm_kr_smp(ip-1))
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
!
!          st_elapsed = MPI_WTIME()
          call set_sp_rlm_scalar_matmul                                 &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                    &
     &        ncomp, nvector, comm_rlm%irev_sr, n_WR, WR,               &
     &        WK_l_mtl%nscl_jk, WK_l_mtl%scl_e(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
          call matmat_bwd_leg_trans                                     &
     &       (sph_rtm%nidx_rtm(2), nkr(ip), nj_rlm(ip),                 &
     &        P_rtm(1,jst(ip)+1), WK_l_mtl%scl_e(1,ip),                 &
     &        WK_l_mtl%symp(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_scalar_matmul                                 &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, kst(ip), nkr(ip), mp_rlm,               &
     &        WK_l_mtl%nscl_lk, WK_l_mtl%symp(1,ip),                    &
     &        ncomp, nvector, comm_rtm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(41:44)                                                   &
!     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_b_trans_scalar_matprod
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_matmul
