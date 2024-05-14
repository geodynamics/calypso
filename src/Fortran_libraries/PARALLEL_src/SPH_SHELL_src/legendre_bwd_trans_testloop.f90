!>@file   legendre_bwd_trans_testloop.f90
!!@brief  module legendre_bwd_trans_testloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_test(ncomp, nvector, nscalar,&
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm,                         &
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
      module legendre_bwd_trans_testloop
!
      use m_precision
!
      use m_constants
      use m_work_time
      use calypso_mpi
!
      use m_machine_parameter
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use t_legendre_work_testlooop
      use m_elapsed_labels_SPH_TRNS
!
      use matmul_for_legendre_trans
!
      implicit none
!
      private :: leg_bwd_trans_1latitude
      integer, external :: omp_get_max_threads
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_vector_test                           &
     &         (iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,  idx_trns, leg,   &
     &          n_WR, n_WS, WR, WS, WK_l_tst)
!
      use t_schmidt_poly_on_rtm
      use set_sp_rlm_sym_mat_tsmp
      use cal_vr_rtm_sym_mat_tsmp
      use small_matmul_leg_trans_krin
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: mp_rlm, mm
      integer(kind = kint) :: nkrs, nkrt, lp_rtm
      integer(kind = kint) :: ip, jst, jed, jnum
      integer(kind = kint) :: lt, kst_s, kst_t
!
!$omp parallel do
      do ip = 1, np_smp
        WK_l_tst%wk_plm(ip)%time_omp(1:3) = 0
      end do
!$omp end parallel do
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nkrs = (3*nvector + nscalar) * sph_rlm%nidx_rlm(1)
      nkrt = 2*nvector * sph_rlm%nidx_rlm(1)
!
      do mp_rlm = 1, sph_rtm%nidx_rtm(3)
        mm = abs(sph_rtm%idx_gl_1d_rtm_m(mp_rlm,2))
        jst = idx_trns%lstack_rlm(mp_rlm-1)
        jed = idx_trns%lstack_rlm(mp_rlm)
        jnum = idx_trns%lstack_rlm(mp_rlm) - jst
!
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+9)
          call set_sp_rlm_sym_mat_rin                                   &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r,            &
     &        leg%g_sph_rlm, &
     &        jst, WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),    &
     &        ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WR, WR,      &
     &        WK_l_tst%Smat(1)%pol_e(1), WK_l_tst%Smat(1)%tor_e(1),     &
     &        WK_l_tst%Smat(1)%pol_o(1), WK_l_tst%Smat(1)%tor_o(1) )
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+9)
!
!
!$omp parallel do private(ip,lt,kst_s,kst_t,lp_rtm)
        do ip = 1, np_smp
!   even l-m
          do lt = 1, WK_l_tst%nlo_rtm(ip)
!            kst_s = (lt-1) * nkrs + 1
!            kst_t = (lt-1) * nkrt + 1
!
!      Set Legendre polynomials
            lp_rtm = WK_l_tst%lst_rtm(ip) + lt
            call leg_bwd_trans_1latitude(lp_rtm, jst, mm,               &
     &          mp_rlm, idx_trns%mn_rlm(mp_rlm), nkrs, nkrt,            &
     &          iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rlm, sph_rtm, comm_rtm, leg, n_WR, n_WS, WR, WS,    &
     &          WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),       &
     &          WK_l_tst%Smat(1), WK_l_tst%Pjt_mat(ip),                 &
     &          WK_l_tst%Fmat(ip), WK_l_tst%wk_plm(ip))
          end do
!
!     Equator (if necessary)
          if(WK_l_tst%nle_rtm(ip) .gt. WK_l_tst%nlo_rtm(ip)) then
            lp_rtm = WK_l_tst%lst_rtm(ip) + WK_l_tst%nle_rtm(ip)
            call leg_bwd_trans_at_equator(lp_rtm, jst, mm,              &
     &          mp_rlm, idx_trns%mn_rlm(mp_rlm), nkrs, nkrt,            &
     &          iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rlm, sph_rtm, comm_rtm, leg, n_WS, WS,              &
     &          WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),       &
     &          WK_l_tst%Smat(1), WK_l_tst%Pjt_mat(ip),                 &
     &          WK_l_tst%Fmat(ip), WK_l_tst%wk_plm(ip))
          end if
        end do
!$omp end parallel do
      end do
!
      do ip = 2, np_smp
        WK_l_tst%wk_plm(1)%time_omp(1:3)                                &
     &        = WK_l_tst%wk_plm(1)%time_omp(1:3)                        &
     &         + WK_l_tst%wk_plm(ip)%time_omp(1:3)
      end do
      elps1%elapsed(ist_elapsed_SDT+10)                                 &
     &        = elps1%elapsed(ist_elapsed_SDT+10)                       &
     &         + WK_l_tst%wk_plm(1)%time_omp(1) / dble(np_smp)
      elps1%elapsed(ist_elapsed_SDT+11)                                 &
     &        = elps1%elapsed(ist_elapsed_SDT+11)                       &
     &         + WK_l_tst%wk_plm(1)%time_omp(2) / dble(np_smp)
      elps1%elapsed(ist_elapsed_SDT+12)                                 &
     &        = elps1%elapsed(ist_elapsed_SDT+12)                       &
     &         + WK_l_tst%wk_plm(1)%time_omp(3) / dble(np_smp)
!
      end subroutine legendre_b_trans_vector_test
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_1latitude                                &
     &         (lp_rtm, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,            &
     &          iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rlm, sph_rtm, comm_rtm, leg, n_WR, n_WS, WR, WS,    &
     &          n_jk_e, n_jk_o, Smat, Pjt_mat, Fmat, wk_plm)
!
      use t_schmidt_poly_on_rtm
      use set_sp_rlm_sym_mat_tsmp
      use cal_vr_rtm_sym_mat_tsmp
      use small_matmul_leg_trans_krin
!
      integer(kind = kint), intent(in) :: lp_rtm
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: mm, jst
      integer(kind = kint), intent(in) :: nkrs, nkrt
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(spectr_matrix_omp), intent(inout) :: Smat
      type(leg_jt_omp_matrix), intent(inout) :: Pjt_mat
      type(field_matrix_omp), intent(inout) :: Fmat
      type(work_make_legendre), intent(inout) :: wk_plm
!
!      Set Legendre polynomials
      wk_plm%st_time_omp = MPI_WTIME()
      call set_each_sym_leg_omp_mat_j1(sph_rlm, mm, jst,                &
     &    leg%g_colat_rtm(lp_rtm), n_jk_e, n_jk_o,                      &
     &    Pjt_mat%Pse_jt(1), Pjt_mat%dPsedt_jt(1),                      &
     &    Pjt_mat%Pso_jt(1), Pjt_mat%dPsodt_jt(1), wk_plm)
      wk_plm%time_omp(1) = wk_plm%time_omp(1)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!   Matrix products
      wk_plm%st_time_omp = MPI_WTIME()
      call matvec_bwd_leg_trans_Pj(iflag_matmul, nkrs, n_jk_e,          &
     &    Smat%pol_e(1), Pjt_mat%Pse_jt(1), Fmat%symp_r(1))
      call matvec_bwd_leg_trans_Pj(iflag_matmul, nkrt, n_jk_e,          &
     &    Smat%tor_e(1), Pjt_mat%dPsedt_jt(1), Fmat%asmp_p(1))
!   odd l-m
      call matvec_bwd_leg_trans_Pj(iflag_matmul, nkrs, n_jk_o,          &
     &    Smat%pol_o(1), Pjt_mat%Pso_jt(1), Fmat%asmp_r(1))
      call matvec_bwd_leg_trans_Pj(iflag_matmul, nkrt, n_jk_o,          &
     &    Smat%tor_o(1), Pjt_mat%dPsodt_jt(1), Fmat%symp_p(1))
      wk_plm%time_omp(2) = wk_plm%time_omp(2)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!   Substitute to send buffer
      wk_plm%st_time_omp = MPI_WTIME()
      call cal_vr_rtm_sym_mat_lt_rin(lp_rtm, sph_rtm%nnod_rtm,          &
     &    sph_rtm%nidx_rtm, sph_rtm%istep_rtm, sph_rlm%nidx_rlm,        &
     &    leg%asin_t_rtm, mp_rlm, mn_rlm, Fmat%symp_r(1),               &
     &    Fmat%asmp_p(1), Fmat%asmp_r(1), Fmat%symp_p(1),               &
     &    ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WS, WS)
      wk_plm%time_omp(3) = wk_plm%time_omp(3)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
      end subroutine leg_bwd_trans_1latitude
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_at_equator                               &
     &         (lp_rtm, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,            &
     &          iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rlm, sph_rtm, comm_rtm, leg, n_WS, WS,              &
     &          n_jk_e, n_jk_o, Smat, Pjt_mat, Fmat, wk_plm)
!
      use t_schmidt_poly_on_rtm
      use set_sp_rlm_sym_mat_tsmp
      use cal_vr_rtm_sym_mat_tsmp
      use small_matmul_leg_trans_krin
!
      integer(kind = kint), intent(in) :: lp_rtm
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: mm, jst
      integer(kind = kint), intent(in) :: nkrs, nkrt
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(spectr_matrix_omp), intent(inout) :: Smat
      type(leg_jt_omp_matrix), intent(inout) :: Pjt_mat
      type(field_matrix_omp), intent(inout) :: Fmat
      type(work_make_legendre), intent(inout) :: wk_plm
!
!      Set Legendre polynomials
      wk_plm%st_time_omp = MPI_WTIME()
      call set_each_sym_leg_omp_mat_j1(sph_rlm, mm, jst,                &
     &    leg%g_colat_rtm(lp_rtm), n_jk_e, n_jk_o,                      &
     &    Pjt_mat%Pse_jt(1), Pjt_mat%dPsedt_jt(1),                      &
     &    Pjt_mat%Pso_jt(1), Pjt_mat%dPsodt_jt(1), wk_plm)
      wk_plm%time_omp(1) = wk_plm%time_omp(1)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!   Matrix products
      wk_plm%st_time_omp = MPI_WTIME()
      call matvec_bwd_leg_trans_Pj(iflag_matmul, nkrs, n_jk_e,          &
     &    Smat%pol_e(1), Pjt_mat%Pse_jt(1), Fmat%symp_r(1))
!      call matvec_bwd_leg_trans_Pj(iflag_matmul, nkrt, n_jk_e,         &
!     &    Smat%tor_e(1), Pjt_mat%dPsedt_jt(1), Fmat%asmp_p(1))
!   odd l-m
      call matvec_bwd_leg_trans_Pj(iflag_matmul, nkrs, n_jk_o,          &
     &    Smat%pol_o(1), Pjt_mat%Pso_jt(1), Fmat%asmp_r(1))
      call matvec_bwd_leg_trans_Pj(iflag_matmul, nkrt, n_jk_o,          &
     &    Smat%tor_o(1), Pjt_mat%dPsodt_jt(1), Fmat%symp_p(1))
      wk_plm%time_omp(2) = wk_plm%time_omp(2)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
!   Substitute to send buffer
      wk_plm%st_time_omp = MPI_WTIME()
      call cal_vr_rtm_sym_mat_eq_rin(lp_rtm, sph_rtm%nnod_rtm,          &
     &    sph_rtm%nidx_rtm, sph_rtm%istep_rtm, sph_rlm%nidx_rlm,        &
     &    leg%asin_t_rtm, mp_rlm, mn_rlm,                               &
     &    Fmat%symp_r(1), Fmat%asmp_r(1), Fmat%symp_p(1),               &
     &    ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WS, WS)
      wk_plm%time_omp(3) = wk_plm%time_omp(3)                           &
     &                    + MPI_WTIME() - wk_plm%st_time_omp
!
      end subroutine leg_bwd_trans_at_equator
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_testloop
