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
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm,                         &
!!     &          n_WR, n_WS, WR, WS, WK_l_bsm)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!!
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
      use m_constants
      use m_machine_parameter
!
      use m_work_time
      use calypso_mpi
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use t_leg_trans_sym_matmul_big
      use m_elapsed_labels_SPH_TRNS
!
      use matmul_for_legendre_trans
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
      subroutine leg_bwd_trans_sym_matmul_big                           &
     &         (iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm,                           &
     &          n_WR, n_WS, WR, WS, WK_l_bsm)
!
      use set_sp_rlm_leg_matmul_big
      use set_vr_rtm_leg_matmul_big
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in)                                    &
     &           :: asin_theta_1d_rtm(sph_rtm%nidx_rtm(2))
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: nl_rtm, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp),  nkr(np_smp)
      integer(kind = kint) :: nkrs(np_smp),  nkrt(np_smp)
      integer(kind = kint) :: jst(np_smp), jst_h(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
!$omp parallel workshare
        WK_l_bsm%time_omp(1:np_smp,1:3) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nl_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
!$omp parallel do private(ip,mp_rlm,mn_rlm)
      do ip = 1, np_smp
        kst(ip) = sph_rlm%istack_rlm_kr_smp(ip-1)
        nkr(ip) = sph_rlm%istack_rlm_kr_smp(ip)                         &
     &           - sph_rlm%istack_rlm_kr_smp(ip-1)
        nkrs(ip) = ncomp*nkr(ip)
        nkrt(ip) = 2*nvector*nkr(ip)
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
          WK_l_bsm%time_omp(ip,0) = MPI_WTIME()
          call set_sp_rlm_vec_sym_matmul_big                            &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r, g_sph_rlm, &
     &        kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, comm_rlm%irev_sr, n_WR, WR,               &
     &        WK_l_bsm%pol_e(1,ip), WK_l_bsm%tor_e(1,ip),               &
     &        WK_l_bsm%pol_o(1,ip), WK_l_bsm%tor_o(1,ip) )
          call set_sp_rlm_scl_sym_matmul_big                            &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WR, WR,      &
     &        WK_l_bsm%pol_e(1,ip), WK_l_bsm%pol_o(1,ip) )
          WK_l_bsm%time_omp(ip,1) = WK_l_bsm%time_omp(ip,1)             &
     &                    + MPI_WTIME() - WK_l_bsm%time_omp(ip,0)
!
!   even l-m
          WK_l_bsm%time_omp(ip,0) = MPI_WTIME()
          call matmul_bwd_leg_trans                                     &
     &       (iflag_matmul, nl_rtm, nkrs(ip), n_jk_e(ip),               &
     &        WK_l_bsm%Ps_tj(1,jst(ip)+1), WK_l_bsm%pol_e(1,ip),        &
     &        WK_l_bsm%symp_r(1,ip))
          call matmul_bwd_leg_trans                                     &
     &       (iflag_matmul, nl_rtm, nkrt(ip), n_jk_e(ip),               &
     &        WK_l_bsm%dPsdt_tj(1,jst(ip)+1), WK_l_bsm%tor_e(1,ip),     &
     &        WK_l_bsm%asmp_p(1,ip))
!   odd l-m
          call matmul_bwd_leg_trans                                     &
     &       (iflag_matmul, nl_rtm, nkrs(ip), n_jk_o(ip),               &
     &        WK_l_bsm%Ps_tj(1,jst_h(ip)), WK_l_bsm%pol_o(1,ip),        &
     &        WK_l_bsm%asmp_r(1,ip))
          call matmul_bwd_leg_trans                                     &
     &       (iflag_matmul, nl_rtm, nkrt(ip), n_jk_o(ip),               &
     &        WK_l_bsm%dPsdt_tj(1,jst_h(ip)), WK_l_bsm%tor_o(1,ip),     &
     &        WK_l_bsm%symp_p(1,ip))
          WK_l_bsm%time_omp(ip,2) = WK_l_bsm%time_omp(ip,2)             &
     &                    + MPI_WTIME() - WK_l_bsm%time_omp(ip,0)
!
          WK_l_bsm%time_omp(ip,0) = MPI_WTIME()
          call cal_vr_rtm_vec_sym_matmul_big                            &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm,                      &
     &        kst(ip), nkr(ip), mp_rlm, mn_rlm, nl_rtm,                 &
     &        WK_l_bsm%symp_r(1,ip), WK_l_bsm%asmp_p(1,ip),             &
     &        WK_l_bsm%asmp_r(1,ip), WK_l_bsm%symp_p(1,ip),             &
     &        ncomp, nvector, comm_rtm%irev_sr, n_WS, WS)
          call cal_vr_rtm_scl_sym_matmul_big                            &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, kst(ip), nkr(ip), mp_rlm, nl_rtm,       &
     &        WK_l_bsm%symp_r(1,ip), WK_l_bsm%asmp_r(1,ip),             &
     &        ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WS, WS)
          WK_l_bsm%time_omp(ip,3) = WK_l_bsm%time_omp(ip,3)             &
     &                    + MPI_WTIME() - WK_l_bsm%time_omp(ip,0)
!
        end do
      end do
!$omp end parallel do
!
      if(iflag_SDT_time) then
        do ip = 2, np_smp
          WK_l_bsm%time_omp(1,1:3)                                      &
     &          = WK_l_bsm%time_omp(ip,1:3) + WK_l_bsm%time_omp(ip,1:3)
        end do
        elps1%elapsed(ist_elapsed_SDT+ 9)                               &
     &          = elps1%elapsed(ist_elapsed_SDT+ 9)                     &
     &           + WK_l_bsm%time_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_SDT+11)                               &
     &          = elps1%elapsed(ist_elapsed_SDT+11)                     &
     &           + WK_l_bsm%time_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_SDT+12)                               &
     &          = elps1%elapsed(ist_elapsed_SDT+12)                     &
     &           + WK_l_bsm%time_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine leg_bwd_trans_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      end module leg_b_trans_sym_matmul_big
