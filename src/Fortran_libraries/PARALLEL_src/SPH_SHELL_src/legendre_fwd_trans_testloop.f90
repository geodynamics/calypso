!>@file   legendre_fwd_trans_testloop.f90
!!@brief  module legendre_fwd_trans_testloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_test(ncomp, nvector, nscalar,&
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,             &
!!     &          n_WR, n_WS, WR, WS, WK_l_tst)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_fwd_trans_testloop
!
      use m_precision
!
      use m_constants
      use m_work_time
      use calypso_mpi
!
      use m_machine_parameter
      use matmul_for_legendre_trans
!
      use t_legendre_work_testlooop
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use m_elapsed_labels_SPH_TRNS
!
      implicit none
!
      integer, external :: omp_get_max_threads
!
      private :: leg_fwd_trans_8latitude, leg_fwd_trans_4latitude
      private :: leg_fwd_trans_2latitude, leg_fwd_trans_1latitude
      private :: leg_fwd_trans_at_equator
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_vector_test                           &
     &         (iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns, leg,    &
     &          n_WR, n_WS, WR, WS, WK_l_tst)
!
      use t_schmidt_poly_on_rtm
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: mm, mp_rlm, lp_rtm, ln_rtm
      integer(kind = kint) :: nkrs, nkrt, lt, lt2
      integer(kind = kint) :: ip, jst
      integer(kind = kint) :: lst
!
!
!$omp parallel do
      do ip = 1, np_smp
        WK_l_tst%wk_plm(ip)%time_omp(1:3) = 0
      end do
!$omp end parallel do
!
!$omp parallel workshare
      WS(1:ncomp*comm_rlm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nkrs = (3*nvector+nscalar) * sph_rlm%nidx_rlm(1)
      nkrt = 2*nvector * sph_rlm%nidx_rlm(1)
!
      do mp_rlm = 1, sph_rtm%nidx_rtm(3)
        mm = abs(sph_rtm%idx_gl_1d_rtm_m(mp_rlm,2))
        jst = idx_trns%lstack_rlm(mp_rlm-1)
!
!$omp parallel do private(ip,lt,lp_rtm,ln_rtm,lt2,lst)
        do ip = 1, np_smp
          WK_l_tst%Smat(ip)%pol_e(1:WK_l_tst%n_pol_e) = 0.0d0
          WK_l_tst%Smat(ip)%tor_e(1:WK_l_tst%n_tor_e) = 0.0d0
          WK_l_tst%Smat(ip)%pol_o(1:WK_l_tst%n_pol_e) = 0.0d0
          WK_l_tst%Smat(ip)%tor_o(1:WK_l_tst%n_tor_e) = 0.0d0
!
          do lt2 = 1, WK_l_tst%nlo_rtm(ip) / 8
            call leg_fwd_trans_8latitude                               &
     &         (lt2, jst, mm, mp_rlm, idx_trns%mn_rlm(mp_rlm),         &
     &          nkrs, nkrt, iflag_matmul,                              &
     &          ncomp, nvector, nscalar,                               &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,             &
     &          WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),      &
     &          WK_l_tst%lst_rtm(ip), WK_l_tst%Fmat(ip),               &
     &          WK_l_tst%Ptj_mat(ip), WK_l_tst%Smat(ip),               &
     &          WK_l_tst%wk_plm(ip))
          end do
          lst = 1 + int(WK_l_tst%nlo_rtm(ip)/8) * 8
!
          do lt2 = 1 + lst/4, WK_l_tst%nlo_rtm(ip) / 4
            call leg_fwd_trans_4latitude                              &
     &         (lt2, jst, mm, mp_rlm, idx_trns%mn_rlm(mp_rlm),        &
     &          nkrs, nkrt, iflag_matmul,                             &
     &          ncomp, nvector, nscalar,                              &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,            &
     &          WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),     &
     &          WK_l_tst%lst_rtm(ip), WK_l_tst%Fmat(ip),              &
     &          WK_l_tst%Ptj_mat(ip), WK_l_tst%Smat(ip),              &
     &          WK_l_tst%wk_plm(ip))
          end do
!
          lst = 1 + int(WK_l_tst%nlo_rtm(ip)/4) * 4
          do lt2 = 1 + lst/2, WK_l_tst%nlo_rtm(ip) / 2
            call leg_fwd_trans_2latitude                              &
     &         (lt2, jst, mm, mp_rlm, idx_trns%mn_rlm(mp_rlm),        &
     &          nkrs, nkrt, iflag_matmul,                             &
     &          ncomp, nvector, nscalar,                              &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,            &
     &          WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),     &
     &          WK_l_tst%lst_rtm(ip), WK_l_tst%Fmat(ip),              &
     &          WK_l_tst%Ptj_mat(ip), WK_l_tst%Smat(ip),              &
     &          WK_l_tst%wk_plm(ip))
          end do
!
          lst = 1 + int(WK_l_tst%nlo_rtm(ip)/2) * 2
          do lt = lst, WK_l_tst%nlo_rtm(ip)
            lp_rtm = WK_l_tst%lst_rtm(ip) + lt
            ln_rtm = sph_rtm%nidx_rtm(2) - lp_rtm + 1
            call leg_fwd_trans_1latitude                                &
     &         (lp_rtm, ln_rtm, jst, mm,                                &
     &          mp_rlm, idx_trns%mn_rlm(mp_rlm), nkrs, nkrt,            &
     &          iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,              &
     &          WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),       &
     &          WK_l_tst%Fmat(ip), WK_l_tst%Pjt_mat(ip),                &
     &          WK_l_tst%Smat(ip), WK_l_tst%wk_plm(ip))
          end do
!
!   Equator (if necessary)
          if(WK_l_tst%nle_rtm(ip) .gt. WK_l_tst%nlo_rtm(ip)) then
            lp_rtm = WK_l_tst%lst_rtm(ip) + WK_l_tst%nle_rtm(ip)
            call leg_fwd_trans_at_equator                               &
     &         (lp_rtm, jst, mm, mp_rlm, idx_trns%mn_rlm(mp_rlm),       &
     &          nkrs, nkrt, iflag_matmul, ncomp, nvector, nscalar,      &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,              &
     &          WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),       &
     &          WK_l_tst%Fmat(ip), WK_l_tst%Pjt_mat(ip),                &
     &          WK_l_tst%Smat(ip), WK_l_tst%wk_plm(ip))
          end if
        end do
!$omp end parallel do
!
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+16)
!$omp parallel private(ip)
        do ip = 2, np_smp
!$omp workshare
          WK_l_tst%Smat(1)%pol_e(1:nkrs*WK_l_tst%n_jk_e(mp_rlm))        &
     &      =  WK_l_tst%Smat(1)%pol_e(1:nkrs*WK_l_tst%n_jk_e(mp_rlm))   &
     &       + WK_l_tst%Smat(ip)%pol_e(1:nkrs*WK_l_tst%n_jk_e(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tst%Smat(1)%tor_e(1:nkrt*WK_l_tst%n_jk_e(mp_rlm))        &
     &      =  WK_l_tst%Smat(1)%tor_e(1:nkrt*WK_l_tst%n_jk_e(mp_rlm))   &
     &       + WK_l_tst%Smat(ip)%tor_e(1:nkrt*WK_l_tst%n_jk_e(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tst%Smat(1)%pol_o(1:nkrs*WK_l_tst%n_jk_o(mp_rlm))        &
     &      =  WK_l_tst%Smat(1)%pol_o(1:nkrs*WK_l_tst%n_jk_o(mp_rlm))   &
     &       + WK_l_tst%Smat(ip)%pol_o(1:nkrs*WK_l_tst%n_jk_o(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tst%Smat(1)%tor_o(1:nkrt*WK_l_tst%n_jk_o(mp_rlm))        &
     &      =  WK_l_tst%Smat(1)%tor_o(1:nkrt*WK_l_tst%n_jk_o(mp_rlm))   &
     &       + WK_l_tst%Smat(ip)%tor_o(1:nkrt*WK_l_tst%n_jk_o(mp_rlm))
!$omp end workshare nowait
        end do
!$omp end parallel
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+16)
!
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+17)
          call cal_sp_rlm_sym_mat_rin                                   &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,                       &
     &        sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,               &
     &        sph_rlm%radius_1d_rlm_r, leg%g_sph_rlm, jst,              &
     &        WK_l_tst%n_jk_o(mp_rlm), WK_l_tst%n_jk_e(mp_rlm),         &
     &        WK_l_tst%Smat(1)%pol_e(1), WK_l_tst%Smat(1)%pol_o(1),     &
     &        WK_l_tst%Smat(1)%tor_e(1), WK_l_tst%Smat(1)%tor_o(1),     &
     &        ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WS, WS)
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+17)
      end do
!
      if(iflag_SDT_time) then
        do ip = 2, np_smp
          WK_l_tst%wk_plm(1)%time_omp(1:3)                              &
     &          = WK_l_tst%wk_plm(1)%time_omp(1:3)                      &
     &           + WK_l_tst%wk_plm(ip)%time_omp(1:3)
        end do
        elps1%elapsed(ist_elapsed_SDT+13)                               &
     &          = elps1%elapsed(ist_elapsed_SDT+13)                     &
     &           + WK_l_tst%wk_plm(1)%time_omp(1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_SDT+14)                               &
     &          = elps1%elapsed(ist_elapsed_SDT+14)                     &
     &           + WK_l_tst%wk_plm(1)%time_omp(2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_SDT+15)                               &
     &          = elps1%elapsed(ist_elapsed_SDT+15)                     &
     &           + WK_l_tst%wk_plm(1)%time_omp(3) / dble(np_smp)
      end if
!
      end subroutine legendre_f_trans_vector_test
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_8latitude                                &
     &         (lt2, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,               &
     &          iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,              &
     &          n_jk_e, n_jk_o, lst_rtm, Fmat, Ptj_mat, Smat, wk_plm)
!
      use t_schmidt_poly_on_rtm
      use t_set_legendre_4_sph_trans
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use small_matmul_leg_trans_krin
!
      integer(kind = kint), intent(in) :: lt2
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: mm, jst
      integer(kind = kint), intent(in) :: nkrs, nkrt
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o, lst_rtm
      type(field_matrix_omp), intent(inout) :: Fmat
      type(leg_tj_omp_matrix), intent(inout) :: Ptj_mat
      type(spectr_matrix_omp), intent(inout) :: Smat
      type(work_make_legendre), intent(inout) :: wk_plm
!
      integer(kind = kint) :: lt, lp_rtm, ln_rtm
      integer(kind = kint) :: kst_s, kst_t
!
!   Pull data from recieve buffer
      wk_plm%st_time_omp = MPI_WTIME()
      do lt = 1, n_AVX512
        lp_rtm = lst_rtm + (lt2-1)*n_AVX512 + lt
        ln_rtm = sph_rtm%nidx_rtm(2) - lp_rtm + 1
        kst_s = (lt-1) * nkrs + 1
        kst_t = (lt-1) * nkrt + 1
!
        call set_vr_rtm_lt_sym_mat_rin                                  &
     &     (lp_rtm, ln_rtm, sph_rtm%nnod_rtm,                           &
     &      sph_rtm%istep_rtm, sph_rlm%nidx_rlm,                        &
     &      leg%asin_t_rtm(lp_rtm), leg%weight_rtm(lp_rtm),             &
     &      mp_rlm, mn_rlm, ncomp, nvector, nscalar,                    &
     &      comm_rtm%irev_sr, n_WR, WR, Fmat%symp_r(kst_s),             &
     &      Fmat%asmp_p(kst_t), Fmat%asmp_r(kst_s), Fmat%symp_p(kst_t))
      end do
      wk_plm%time_omp(1) = wk_plm%time_omp(1)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!      Set Legendre polynomials
      wk_plm%st_time_omp = MPI_WTIME()
      lp_rtm = lst_rtm + (lt2-1)*n_AVX512 + 1
      call set_each_sym_leg_omp_mat_tj(sph_rlm, mm, jst, n_AVX512,      &
     &    leg%g_colat_rtm(lp_rtm), n_jk_e, n_jk_o,                      &
     &    Ptj_mat%Pse_tj(1), Ptj_mat%dPsedt_tj(1),                      &
     &    Ptj_mat%Pso_tj(1), Ptj_mat%dPsodt_tj(1), wk_plm)
      wk_plm%time_omp(2) = wk_plm%time_omp(2)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!   Matrix products
      wk_plm%st_time_omp = MPI_WTIME()
      call matmul8_fwd_leg_trans_Ptj(iflag_matmul, nkrs, n_jk_e,        &
     &    Fmat%symp_r(1), Ptj_mat%Pse_tj(1),    Smat%pol_e(1))
      call matmul8_fwd_leg_trans_Ptj(iflag_matmul, nkrt, n_jk_e,        &
     &    Fmat%asmp_p(1), Ptj_mat%dPsedt_tj(1), Smat%tor_e(1))
!
!  odd l-m
      call matmul8_fwd_leg_trans_Ptj(iflag_matmul, nkrs, n_jk_o,        &
     &    Fmat%asmp_r(1), Ptj_mat%Pso_tj(1),    Smat%pol_o(1))
      call matmul8_fwd_leg_trans_Ptj(iflag_matmul, nkrt, n_jk_o,        &
     &    Fmat%symp_p(1), Ptj_mat%dPsodt_tj(1), Smat%tor_o(1))
      wk_plm%time_omp(3) = wk_plm%time_omp(3)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
      end subroutine leg_fwd_trans_8latitude
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_4latitude                                &
     &         (lt2, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,               &
     &          iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,              &
     &          n_jk_e, n_jk_o, lst_rtm, Fmat, Ptj_mat, Smat, wk_plm)
!
      use t_schmidt_poly_on_rtm
      use t_set_legendre_4_sph_trans
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use small_matmul_leg_trans_krin
!
      integer(kind = kint), intent(in) :: lt2
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: mm, jst
      integer(kind = kint), intent(in) :: nkrs, nkrt
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o, lst_rtm
      type(field_matrix_omp), intent(inout) :: Fmat
      type(leg_tj_omp_matrix), intent(inout) :: Ptj_mat
      type(spectr_matrix_omp), intent(inout) :: Smat
      type(work_make_legendre), intent(inout) :: wk_plm
!
      integer(kind = kint) :: lt, lp_rtm, ln_rtm
      integer(kind = kint) :: kst_s, kst_t
!
!   Pull data from recieve buffer
      wk_plm%st_time_omp = MPI_WTIME()
      do lt = 1, n_AVX
        lp_rtm = lst_rtm + (lt2-1)*n_AVX + lt
        ln_rtm = sph_rtm%nidx_rtm(2) - lp_rtm + 1
        kst_s = (lt-1) * nkrs + 1
        kst_t = (lt-1) * nkrt + 1
!
        call set_vr_rtm_lt_sym_mat_rin                                  &
     &     (lp_rtm, ln_rtm, sph_rtm%nnod_rtm,                           &
     &      sph_rtm%istep_rtm, sph_rlm%nidx_rlm,                        &
     &      leg%asin_t_rtm(lp_rtm), leg%weight_rtm(lp_rtm),             &
     &      mp_rlm, mn_rlm, ncomp, nvector, nscalar,                    &
     &      comm_rtm%irev_sr, n_WR, WR, Fmat%symp_r(kst_s),             &
     &      Fmat%asmp_p(kst_t), Fmat%asmp_r(kst_s), Fmat%symp_p(kst_t))
      end do
      wk_plm%time_omp(1) = wk_plm%time_omp(1)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!      Set Legendre polynomials
      wk_plm%st_time_omp = MPI_WTIME()
      lp_rtm = lst_rtm + (lt2-1)*n_AVX + 1
      call set_each_sym_leg_omp_mat_tj(sph_rlm, mm, jst, n_AVX,         &
     &    leg%g_colat_rtm(lp_rtm), n_jk_e, n_jk_o,                      &
     &    Ptj_mat%Pse_tj(1), Ptj_mat%dPsedt_tj(1),                      &
     &    Ptj_mat%Pso_tj(1), Ptj_mat%dPsodt_tj(1), wk_plm)
      wk_plm%time_omp(2) = wk_plm%time_omp(2)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!   Matrix products
      wk_plm%st_time_omp = MPI_WTIME()
      call matmul4_fwd_leg_trans_Ptj(iflag_matmul, nkrs, n_jk_e,        &
     &    Fmat%symp_r(1), Ptj_mat%Pse_tj(1),    Smat%pol_e(1))
      call matmul4_fwd_leg_trans_Ptj(iflag_matmul, nkrt, n_jk_e,        &
     &    Fmat%asmp_p(1), Ptj_mat%dPsedt_tj(1), Smat%tor_e(1))
!
!  odd l-m
      call matmul4_fwd_leg_trans_Ptj(iflag_matmul, nkrs, n_jk_o,        &
     &    Fmat%asmp_r(1), Ptj_mat%Pso_tj(1),    Smat%pol_o(1))
      call matmul4_fwd_leg_trans_Ptj(iflag_matmul, nkrt, n_jk_o,        &
     &    Fmat%symp_p(1), Ptj_mat%dPsodt_tj(1), Smat%tor_o(1))
      wk_plm%time_omp(3) = wk_plm%time_omp(3)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
      end subroutine leg_fwd_trans_4latitude
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_2latitude                                &
     &         (lt2, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,               &
     &          iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,              &
     &          n_jk_e, n_jk_o, lst_rtm, Fmat, Ptj_mat, Smat, wk_plm)
!
      use t_schmidt_poly_on_rtm
      use t_set_legendre_4_sph_trans
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use small_matmul_leg_trans_krin
!
      integer(kind = kint), intent(in) :: lt2
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: mm, jst
      integer(kind = kint), intent(in) :: nkrs, nkrt
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o, lst_rtm
      type(field_matrix_omp), intent(inout) :: Fmat
      type(leg_tj_omp_matrix), intent(inout) :: Ptj_mat
      type(spectr_matrix_omp), intent(inout) :: Smat
      type(work_make_legendre), intent(inout) :: wk_plm
!
      integer(kind = kint) :: lt, lp_rtm, ln_rtm
      integer(kind = kint) :: kst_s, kst_t
!
!   Pull data from recieve buffer
      wk_plm%st_time_omp = MPI_WTIME()
      do lt = 1, n_SSE2
        lp_rtm = lst_rtm + (lt2-1)*n_SSE2 + lt
        ln_rtm = sph_rtm%nidx_rtm(n_SSE2) - lp_rtm + 1
        kst_s = (lt-1) * nkrs + 1
        kst_t = (lt-1) * nkrt + 1
!
        call set_vr_rtm_lt_sym_mat_rin                                  &
     &     (lp_rtm, ln_rtm, sph_rtm%nnod_rtm,                           &
     &      sph_rtm%istep_rtm, sph_rlm%nidx_rlm,                        &
     &      leg%asin_t_rtm(lp_rtm), leg%weight_rtm(lp_rtm),             &
     &      mp_rlm, mn_rlm, ncomp, nvector, nscalar,                    &
     &      comm_rtm%irev_sr, n_WR, WR, Fmat%symp_r(kst_s),             &
     &      Fmat%asmp_p(kst_t), Fmat%asmp_r(kst_s), Fmat%symp_p(kst_t))
      end do
      wk_plm%time_omp(1) = wk_plm%time_omp(1)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!      Set Legendre polynomials
      wk_plm%st_time_omp = MPI_WTIME()
      lp_rtm = lst_rtm + (lt2-1)*n_SSE2 + 1
      call set_each_sym_leg_omp_mat_tj(sph_rlm, mm, jst, n_SSE2,        &
     &    leg%g_colat_rtm(lp_rtm), n_jk_e, n_jk_o,                      &
     &    Ptj_mat%Pse_tj(1), Ptj_mat%dPsedt_tj(1),                      &
     &    Ptj_mat%Pso_tj(1), Ptj_mat%dPsodt_tj(1), wk_plm)
      wk_plm%time_omp(2) = wk_plm%time_omp(2)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!   Matrix products
      wk_plm%st_time_omp = MPI_WTIME()
      call matmul2_fwd_leg_trans_Ptj(iflag_matmul, nkrs, n_jk_e,        &
     &    Fmat%symp_r(1), Ptj_mat%Pse_tj(1),    Smat%pol_e(1))
      call matmul2_fwd_leg_trans_Ptj(iflag_matmul, nkrt, n_jk_e,        &
     &    Fmat%asmp_p(1), Ptj_mat%dPsedt_tj(1), Smat%tor_e(1))
!
!  odd l-m
      call matmul2_fwd_leg_trans_Ptj(iflag_matmul, nkrs, n_jk_o,        &
     &    Fmat%asmp_r(1), Ptj_mat%Pso_tj(1),    Smat%pol_o(1))
      call matmul2_fwd_leg_trans_Ptj(iflag_matmul, nkrt, n_jk_o,        &
     &    Fmat%symp_p(1), Ptj_mat%dPsodt_tj(1), Smat%tor_o(1))
      wk_plm%time_omp(3) = wk_plm%time_omp(3)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
      end subroutine leg_fwd_trans_2latitude
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_1latitude                                &
     &         (lp_rtm, ln_rtm, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,    &
     &          iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,              &
     &           n_jk_e, n_jk_o, Fmat, Pjt_mat, Smat, wk_plm)
!
      use t_schmidt_poly_on_rtm
      use t_set_legendre_4_sph_trans
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use small_matmul_leg_trans_krin
!
      integer(kind = kint), intent(in) :: lp_rtm, ln_rtm
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: mm, jst
      integer(kind = kint), intent(in) :: nkrs, nkrt
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(field_matrix_omp), intent(inout) :: Fmat
      type(leg_jt_omp_matrix), intent(inout) :: Pjt_mat
      type(spectr_matrix_omp), intent(inout) :: Smat
      type(work_make_legendre), intent(inout) :: wk_plm
!
!
!   Pull data from recieve buffer
      wk_plm%st_time_omp = MPI_WTIME()
      call set_vr_rtm_lt_sym_mat_rin                                    &
     &   (lp_rtm, ln_rtm, sph_rtm%nnod_rtm,                             &
     &    sph_rtm%istep_rtm, sph_rlm%nidx_rlm,                          &
     &    leg%asin_t_rtm(lp_rtm), leg%weight_rtm(lp_rtm),               &
     &    mp_rlm, mn_rlm, ncomp, nvector, nscalar,                      &
     &    comm_rtm%irev_sr, n_WR, WR, Fmat%symp_r(1),                   &
     &    Fmat%asmp_p(1), Fmat%asmp_r(1), Fmat%symp_p(1))
      wk_plm%time_omp(1) = wk_plm%time_omp(1)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!      Set Legendre polynomials
      wk_plm%st_time_omp = MPI_WTIME()
      call set_each_sym_leg_omp_mat_j1                                  &
     &   (sph_rlm, mm, jst, leg%g_colat_rtm(lp_rtm), n_jk_e, n_jk_o,    &
     &    Pjt_mat%Pse_jt(1), Pjt_mat%dPsedt_jt(1),                      &
     &    Pjt_mat%Pso_jt(1), Pjt_mat%dPsodt_jt(1), wk_plm)
      wk_plm%time_omp(2) = wk_plm%time_omp(2)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!   Matrix products
      wk_plm%st_time_omp = MPI_WTIME()
      call matvec_fwd_leg_trans_Pj(nkrs, n_jk_e,                        &
     &    Fmat%symp_r(1), Pjt_mat%Pse_jt(1),    Smat%pol_e(1))
      call matvec_fwd_leg_trans_Pj(nkrt, n_jk_e,                        &
     &    Fmat%asmp_p(1), Pjt_mat%dPsedt_jt(1), Smat%tor_e(1))
!
!  odd l-m
      call matvec_fwd_leg_trans_Pj(nkrs, n_jk_o,                        &
     &    Fmat%asmp_r(1), Pjt_mat%Pso_jt(1),    Smat%pol_o(1))
      call matvec_fwd_leg_trans_Pj(nkrt, n_jk_o,                        &
     &    Fmat%symp_p(1), Pjt_mat%dPsodt_jt(1), Smat%tor_o(1))
      wk_plm%time_omp(3) = wk_plm%time_omp(3)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
      end subroutine leg_fwd_trans_1latitude
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_at_equator                               &
     &         (lp_rtm, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,            &
     &          iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,              &
     &          n_jk_e, n_jk_o, Fmat, Pjt_mat, Smat, wk_plm)
!
      use t_schmidt_poly_on_rtm
      use t_set_legendre_4_sph_trans
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use small_matmul_leg_trans_krin
!
      integer(kind = kint), intent(in) :: lp_rtm
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: mm, jst
      integer(kind = kint), intent(in) :: nkrs, nkrt
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(field_matrix_omp), intent(inout) :: Fmat
      type(leg_jt_omp_matrix), intent(inout) :: Pjt_mat
      type(spectr_matrix_omp), intent(inout) :: Smat
      type(work_make_legendre), intent(inout) :: wk_plm
!
!
!   Equator (if necessary)
!   Pull data from recieve buffer
      wk_plm%st_time_omp = MPI_WTIME()
      call set_vr_rtm_eq_sym_mat_rin                                    &
     &   (lp_rtm, sph_rtm%nnod_rtm,                                     &
     &    sph_rtm%istep_rtm, sph_rlm%nidx_rlm,                          &
     &    leg%asin_t_rtm(lp_rtm), leg%weight_rtm(lp_rtm),               &
     &    mp_rlm, mn_rlm, ncomp, nvector, nscalar,                      &
     &    comm_rtm%irev_sr, n_WR, WR, Fmat%symp_r(1),                   &
     &    Fmat%asmp_p(1), Fmat%asmp_r(1), Fmat%symp_p(1))
      wk_plm%time_omp(1) = wk_plm%time_omp(1)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!      Set Legendre polynomials
      wk_plm%st_time_omp = MPI_WTIME()
      call set_each_sym_leg_omp_mat_j1                                  &
     &   (sph_rlm, mm, jst, leg%g_colat_rtm(lp_rtm), n_jk_e, n_jk_o,    &
     &    Pjt_mat%Pse_jt(1), Pjt_mat%dPsedt_jt(1),                      &
     &    Pjt_mat%Pso_jt(1), Pjt_mat%dPsodt_jt(1), wk_plm)
      wk_plm%time_omp(2) = wk_plm%time_omp(2)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!   Matrix products
      wk_plm%st_time_omp = MPI_WTIME()
      call matvec_fwd_leg_trans_Pj(nkrs, n_jk_e,                        &
     &    Fmat%symp_r(1), Pjt_mat%Pse_jt(1),    Smat%pol_e(1))
      call matvec_fwd_leg_trans_Pj(nkrt, n_jk_e,                        &
     &    Fmat%asmp_p(1), Pjt_mat%dPsedt_jt(1), Smat%tor_e(1))
!
!  odd l-m
      call matvec_fwd_leg_trans_Pj(nkrs, n_jk_o,                        &
     &    Fmat%asmp_r(1), Pjt_mat%Pso_jt(1),    Smat%pol_o(1))
      call matvec_fwd_leg_trans_Pj(nkrt, n_jk_o,                        &
     &    Fmat%symp_p(1), Pjt_mat%dPsodt_jt(1), Smat%tor_o(1))
      wk_plm%time_omp(3) = wk_plm%time_omp(3)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
      end subroutine leg_fwd_trans_at_equator
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_testloop
