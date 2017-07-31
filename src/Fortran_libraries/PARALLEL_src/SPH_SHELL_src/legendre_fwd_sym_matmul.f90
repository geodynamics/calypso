!>@file   legendre_fwd_sym_matmul.f90
!!@brief  module legendre_fwd_sym_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine leg_f_trans_vec_sym_matmul(ncomp, nvector,           &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,             &
!!     &          n_WR, n_WS, WR, WS, WK_l_sml)
!!      subroutine leg_f_trans_scl_sym_matmul(ncomp, nvector, nscalar,  &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!!
!!      subroutine leg_f_trans_vec_sym_dgemm(ncomp, nvector,            &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,             &
!!     &          n_WR, n_WS, WR, WS, WK_l_sml)
!!      subroutine leg_f_trans_scl_sym_dgemm(ncomp, nvector, nscalar,   &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!!
!!      subroutine leg_f_trans_vec_sym_matprod(ncomp, nvector,          &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,             &
!!     &          n_WR, n_WS, WR, WS, WK_l_sml)
!!      subroutine leg_f_trans_scl_sym_matprod(ncomp, nvector, nscalar, &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!!
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
      module legendre_fwd_sym_matmul
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_legendre_work_sym_matmul
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
      subroutine leg_f_trans_vec_sym_matmul(ncomp, nvector,             &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,               &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
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
      integer(kind = kint) :: ip, mp_rlm, mn_rlm, nle_rtm, nlo_rtm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rlm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .le. 0) return
      elaps(1:4) = 0
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nvector * (sph_rtm%istack_rtm_kr_smp(ip)              &
     &                     - sph_rtm%istack_rtm_kr_smp(ip-1))
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
          jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
          jst_h(ip) = idx_trns%lstack_even_rlm(mp_rlm) + 1
          n_jk_e(ip) = idx_trns%lstack_even_rlm(mp_rlm)                 &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
          n_jk_o(ip) = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_even_rlm(mp_rlm)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vector_sym_matmul                             &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm, weight_rtm,          &
     &        kst(ip), nkr(ip), mp_rlm, mn_rlm, nle_rtm, nlo_rtm,       &
     &        ncomp, comm_rtm%irev_sr, n_WR, WR,                        &
     &        WK_l_sml%symp_r(1,ip), WK_l_sml%asmp_t(1,ip),             &
     &        WK_l_sml%asmp_p(1,ip), WK_l_sml%symn_t(1,ip),             &
     &        WK_l_sml%symn_p(1,ip), WK_l_sml%asmp_r(1,ip),             &
     &        WK_l_sml%symp_t(1,ip), WK_l_sml%symp_p(1,ip),             &
     &        WK_l_sml%asmn_t(1,ip), WK_l_sml%asmn_p(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
!  even l-m
          call matmul_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symp_r(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),      &
     &        WK_l_sml%pol_e(1,ip))
          call matmul_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmp_t(1,ip), WK_l_sml%dPsdt_rtm(1,jst(ip)+1),   &
     &        WK_l_sml%dpoldt_e(1,ip))
          call matmul_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symn_p(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),      &
     &        WK_l_sml%dpoldp_e(1,ip))
          call matmul_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symn_t(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),      &
     &        WK_l_sml%dtordp_e(1,ip))
          call matmul_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmp_p(1,ip), WK_l_sml%dPsdt_rtm(1,jst(ip)+1),   &
     &        WK_l_sml%dtordt_e(1,ip))
!
!  odd l-m
          call matmul_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmp_r(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),      &
     &        WK_l_sml%pol_o(1,ip))
          call matmul_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symp_t(1,ip), WK_l_sml%dPsdt_rtm(1,jst_h(ip)),   &
     &        WK_l_sml%dpoldt_o(1,ip))
          call matmul_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmn_p(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),      &
     &        WK_l_sml%dpoldp_o(1,ip))
          call matmul_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmn_t(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),      &
     &        WK_l_sml%dtordp_o(1,ip))
          call matmul_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symp_p(1,ip), WK_l_sml%dPsdt_rtm(1,jst_h(ip)),   &
     &        WK_l_sml%dtordt_o(1,ip))
  !          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vector_sym_matmul                             &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,                       &
     &        sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,               &
     &        sph_rlm%radius_1d_rlm_r, g_sph_rlm,                       &
     &        kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        WK_l_sml%pol_e(1,ip), WK_l_sml%pol_o(1,ip),               &
     &        WK_l_sml%dpoldt_e(1,ip), WK_l_sml%dpoldp_e(1,ip),         &
     &        WK_l_sml%dpoldt_o(1,ip), WK_l_sml%dpoldp_o(1,ip),         &
     &        WK_l_sml%dtordt_e(1,ip), WK_l_sml%dtordp_e(1,ip),         &
     &        WK_l_sml%dtordt_o(1,ip),  WK_l_sml%dtordp_o(1,ip),        &
     &       ncomp, comm_rlm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_vec_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_scl_sym_matmul(ncomp, nvector, nscalar,    &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
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
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: nle_rtm, nlo_rtm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      if(nscalar .le. 0) return
      elaps(1:4) = 0
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nscalar * (sph_rtm%istack_rtm_kr_smp(ip)              &
     &                     - sph_rtm%istack_rtm_kr_smp(ip-1))
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
          jst_h(ip) = idx_trns%lstack_even_rlm(mp_rlm) + 1
          n_jk_e(ip) = idx_trns%lstack_even_rlm(mp_rlm)                 &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
          n_jk_o(ip) = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_even_rlm(mp_rlm)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_scalar_sym_matmul                             &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, weight_rtm, kst(ip), nkr(ip), mp_rlm,   &
     &        nle_rtm, nlo_rtm, ncomp, nvector, comm_rtm%irev_sr,       &
     &        n_WR, WR, WK_l_sml%symp(1,ip), WK_l_sml%asmp(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
          call matmul_fwd_leg_trans(nkr(ip), n_jk_e(ip), nle_rtm,       &
     &        WK_l_sml%symp(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),        &
     &        WK_l_sml%scl_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), n_jk_o(ip), nle_rtm,       &
     &        WK_l_sml%asmp(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),        &
     &        WK_l_sml%scl_o(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_scalar_sym_matmul(sph_rlm%nnod_rlm,           &
     &        sph_rlm%nidx_rlm, sph_rlm%istep_rlm, g_sph_rlm,           &
     &        kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        WK_l_sml%scl_e(1,ip), WK_l_sml%scl_o(1,ip),               &
     &        ncomp, nvector, comm_rlm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_scl_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_vec_sym_dgemm(ncomp, nvector,              &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,               &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
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
      integer(kind = kint) :: ip, mp_rlm, mn_rlm, nle_rtm, nlo_rtm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rlm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .le. 0) return
      elaps(1:4) = 0
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nvector * (sph_rtm%istack_rtm_kr_smp(ip)              &
     &                     - sph_rtm%istack_rtm_kr_smp(ip-1))
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
          jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
          jst_h(ip) = idx_trns%lstack_even_rlm(mp_rlm) + 1
          n_jk_e(ip) = idx_trns%lstack_even_rlm(mp_rlm)                 &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
          n_jk_o(ip) = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_even_rlm(mp_rlm)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vector_sym_matmul                             &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm, weight_rtm,          &
     &        kst(ip), nkr(ip), mp_rlm, mn_rlm, nle_rtm, nlo_rtm,       &
     &        ncomp, comm_rtm%irev_sr, n_WR, WR,                        &
     &        WK_l_sml%symp_r(1,ip), WK_l_sml%asmp_t(1,ip),             &
     &        WK_l_sml%asmp_p(1,ip), WK_l_sml%symn_t(1,ip),             &
     &        WK_l_sml%symn_p(1,ip), WK_l_sml%asmp_r(1,ip),             &
     &        WK_l_sml%symp_t(1,ip), WK_l_sml%symp_p(1,ip),             &
     &        WK_l_sml%asmn_t(1,ip), WK_l_sml%asmn_p(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
!  even l-m
          call dgemm_fwd_leg_trans                                      &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symp_r(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),      &
     &        zero, WK_l_sml%pol_e(1,ip))
          call dgemm_fwd_leg_trans                                      &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmp_t(1,ip), WK_l_sml%dPsdt_rtm(1,jst(ip)+1),   &
     &        zero, WK_l_sml%dpoldt_e(1,ip))
          call dgemm_fwd_leg_trans                                      &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symn_p(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),      &
     &        zero, WK_l_sml%dpoldp_e(1,ip))
          call dgemm_fwd_leg_trans                                      &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symn_t(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),      &
     &        zero, WK_l_sml%dtordp_e(1,ip))
          call dgemm_fwd_leg_trans                                      &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmp_p(1,ip), WK_l_sml%dPsdt_rtm(1,jst(ip)+1),   &
     &        zero, WK_l_sml%dtordt_e(1,ip))
!
!  odd l-m
          call dgemm_fwd_leg_trans                                      &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmp_r(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),      &
     &        zero, WK_l_sml%pol_o(1,ip))
          call dgemm_fwd_leg_trans                                      &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symp_t(1,ip), WK_l_sml%dPsdt_rtm(1,jst_h(ip)),   &
     &        zero, WK_l_sml%dpoldt_o(1,ip))
          call dgemm_fwd_leg_trans                                      &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmn_p(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),      &
     &        zero, WK_l_sml%dpoldp_o(1,ip))
          call dgemm_fwd_leg_trans                                      &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmn_t(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),      &
     &        zero, WK_l_sml%dtordp_o(1,ip))
          call dgemm_fwd_leg_trans                                      &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symp_p(1,ip), WK_l_sml%dPsdt_rtm(1,jst_h(ip)),   &
     &        zero, WK_l_sml%dtordt_o(1,ip))
  !          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vector_sym_matmul                             &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,                       &
     &        sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,               &
     &        sph_rlm%radius_1d_rlm_r, g_sph_rlm,                       &
     &        kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        WK_l_sml%pol_e(1,ip), WK_l_sml%pol_o(1,ip),               &
     &        WK_l_sml%dpoldt_e(1,ip), WK_l_sml%dpoldp_e(1,ip),         &
     &        WK_l_sml%dpoldt_o(1,ip), WK_l_sml%dpoldp_o(1,ip),         &
     &        WK_l_sml%dtordt_e(1,ip), WK_l_sml%dtordp_e(1,ip),         &
     &        WK_l_sml%dtordt_o(1,ip),  WK_l_sml%dtordp_o(1,ip),        &
     &       ncomp, comm_rlm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_vec_sym_dgemm
!
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_scl_sym_dgemm(ncomp, nvector, nscalar,     &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
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
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: nle_rtm, nlo_rtm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      if(nscalar .le. 0) return
      elaps(1:4) = 0
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nscalar * (sph_rtm%istack_rtm_kr_smp(ip)              &
     &                     - sph_rtm%istack_rtm_kr_smp(ip-1))
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
          jst_h(ip) = idx_trns%lstack_even_rlm(mp_rlm) + 1
          n_jk_e(ip) = idx_trns%lstack_even_rlm(mp_rlm)                 &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
          n_jk_o(ip) = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_even_rlm(mp_rlm)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_scalar_sym_matmul                             &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, weight_rtm, kst(ip), nkr(ip), mp_rlm,   &
     &        nle_rtm, nlo_rtm, ncomp, nvector, comm_rtm%irev_sr,       &
     &        n_WR, WR, WK_l_sml%symp(1,ip), WK_l_sml%asmp(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
          call dgemm_fwd_leg_trans(nkr(ip), n_jk_e(ip), nle_rtm,        &
     &        WK_l_sml%symp(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),        &
     &        zero, WK_l_sml%scl_e(1,ip))
          call dgemm_fwd_leg_trans(nkr(ip), n_jk_o(ip), nle_rtm,        &
     &        WK_l_sml%asmp(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),        &
     &        zero, WK_l_sml%scl_o(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_scalar_sym_matmul(sph_rlm%nnod_rlm,           &
     &        sph_rlm%nidx_rlm, sph_rlm%istep_rlm, g_sph_rlm,           &
     &        kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        WK_l_sml%scl_e(1,ip), WK_l_sml%scl_o(1,ip),               &
     &        ncomp, nvector, comm_rlm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_scl_sym_dgemm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_vec_sym_matprod(ncomp, nvector,            &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,               &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
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
      integer(kind = kint) :: ip, mp_rlm, mn_rlm, nle_rtm, nlo_rtm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rlm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .le. 0) return
      elaps(1:4) = 0
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nvector * (sph_rtm%istack_rtm_kr_smp(ip)              &
     &                     - sph_rtm%istack_rtm_kr_smp(ip-1))
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
          jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
          jst_h(ip) = idx_trns%lstack_even_rlm(mp_rlm) + 1
          n_jk_e(ip) = idx_trns%lstack_even_rlm(mp_rlm)                 &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
          n_jk_o(ip) = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_even_rlm(mp_rlm)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vector_sym_matmul                             &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm, weight_rtm,          &
     &        kst(ip), nkr(ip), mp_rlm, mn_rlm, nle_rtm, nlo_rtm,       &
     &        ncomp, comm_rtm%irev_sr, n_WR, WR,                        &
     &        WK_l_sml%symp_r(1,ip), WK_l_sml%asmp_t(1,ip),             &
     &        WK_l_sml%asmp_p(1,ip), WK_l_sml%symn_t(1,ip),             &
     &        WK_l_sml%symn_p(1,ip), WK_l_sml%asmp_r(1,ip),             &
     &        WK_l_sml%symp_t(1,ip), WK_l_sml%symp_p(1,ip),             &
     &        WK_l_sml%asmn_t(1,ip), WK_l_sml%asmn_p(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
!  even l-m
          call matmat_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symp_r(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),      &
     &        WK_l_sml%pol_e(1,ip))
          call matmat_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmp_t(1,ip), WK_l_sml%dPsdt_rtm(1,jst(ip)+1),   &
     &        WK_l_sml%dpoldt_e(1,ip))
          call matmat_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symn_p(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),      &
     &        WK_l_sml%dpoldp_e(1,ip))
          call matmat_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symn_t(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),      &
     &        WK_l_sml%dtordp_e(1,ip))
          call matmat_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_e(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmp_p(1,ip), WK_l_sml%dPsdt_rtm(1,jst(ip)+1),   &
     &        WK_l_sml%dtordt_e(1,ip))
!
!  odd l-m
          call matmat_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmp_r(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),      &
     &        WK_l_sml%pol_o(1,ip))
          call matmat_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symp_t(1,ip), WK_l_sml%dPsdt_rtm(1,jst_h(ip)),   &
     &        WK_l_sml%dpoldt_o(1,ip))
          call matmat_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmn_p(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),      &
     &        WK_l_sml%dpoldp_o(1,ip))
          call matmat_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%asmn_t(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),      &
     &        WK_l_sml%dtordp_o(1,ip))
          call matmat_fwd_leg_trans                                     &
     &       (nkr(ip), n_jk_o(ip), WK_l_sml%nth_hemi_rtm,               &
     &        WK_l_sml%symp_p(1,ip), WK_l_sml%dPsdt_rtm(1,jst_h(ip)),   &
     &        WK_l_sml%dtordt_o(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vector_sym_matmul                             &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,                       &
     &        sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,               &
     &        sph_rlm%radius_1d_rlm_r, g_sph_rlm,                       &
     &        kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        WK_l_sml%pol_e(1,ip), WK_l_sml%pol_o(1,ip),               &
     &        WK_l_sml%dpoldt_e(1,ip), WK_l_sml%dpoldp_e(1,ip),         &
     &        WK_l_sml%dpoldt_o(1,ip), WK_l_sml%dpoldp_o(1,ip),         &
     &        WK_l_sml%dtordt_e(1,ip), WK_l_sml%dtordp_e(1,ip),         &
     &        WK_l_sml%dtordt_o(1,ip),  WK_l_sml%dtordp_o(1,ip),        &
     &       ncomp, comm_rlm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_vec_sym_matprod
!
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_scl_sym_matprod(ncomp, nvector, nscalar,   &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
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
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: nle_rtm, nlo_rtm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      if(nscalar .le. 0) return
      elaps(1:4) = 0
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar * sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = nscalar * (sph_rtm%istack_rtm_kr_smp(ip)              &
     &                     - sph_rtm%istack_rtm_kr_smp(ip-1))
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          jst(ip) = idx_trns%lstack_rlm(mp_rlm-1)
          jst_h(ip) = idx_trns%lstack_even_rlm(mp_rlm) + 1
          n_jk_e(ip) = idx_trns%lstack_even_rlm(mp_rlm)                 &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
          n_jk_o(ip) = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_even_rlm(mp_rlm)
!
!          st_elapsed = MPI_WTIME()
          call set_vr_rtm_scalar_sym_matmul                             &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, weight_rtm, kst(ip), nkr(ip), mp_rlm,   &
     &        nle_rtm, nlo_rtm, ncomp, nvector, comm_rtm%irev_sr,       &
     &        n_WR, WR, WK_l_sml%symp(1,ip), WK_l_sml%asmp(1,ip))
!          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!          st_elapsed = MPI_WTIME()
          call matmat_fwd_leg_trans(nkr(ip), n_jk_e(ip), nle_rtm,       &
     &        WK_l_sml%symp(1,ip), WK_l_sml%Ps_rtm(1,jst(ip)+1),        &
     &        WK_l_sml%scl_e(1,ip))
          call matmat_fwd_leg_trans(nkr(ip), n_jk_o(ip), nle_rtm,       &
     &        WK_l_sml%asmp(1,ip), WK_l_sml%Ps_rtm(1,jst_h(ip)),        &
     &        WK_l_sml%scl_o(1,ip))
!          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
!          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_scalar_sym_matmul(sph_rlm%nnod_rlm,           &
     &       sph_rlm%nidx_rlm, sph_rlm%istep_rlm, g_sph_rlm,            &
     &        kst(ip), nkr(ip), jst(ip), n_jk_o(ip), n_jk_e(ip),        &
     &        WK_l_sml%scl_e(1,ip), WK_l_sml%scl_o(1,ip),               &
     &        ncomp, nvector,comm_rlm%irev_sr, n_WS, WS)
!          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!      elapsed(46:49)                                                   &
!     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_scl_sym_matprod
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_sym_matmul
