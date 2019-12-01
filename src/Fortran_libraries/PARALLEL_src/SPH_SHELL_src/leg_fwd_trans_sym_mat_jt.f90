!>@file   leg_fwd_trans_sym_mat_jt.f90
!!@brief  module leg_fwd_trans_sym_mat_jt
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine legendre_f_trans_sym_mat_jt                          &
!!     &         (iflag_matmul, ncomp, nvector, nscalar,                &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,             &
!!     &          n_WR, n_WS, WR, WS, WK_l_tsp)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_theta_omp_work), intent(inout) :: WK_l_tsp
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module leg_fwd_trans_sym_mat_jt
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
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use t_legendre_work_sym_mat_jt
      use m_elapsed_labels_SPH_TRNS
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
      subroutine legendre_f_trans_sym_mat_jt                            &
     &         (iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,               &
     &          n_WR, n_WS, WR, WS, WK_l_tsp)
!
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in)                                    &
     &           :: asin_theta_1d_rtm(sph_rtm%nidx_rtm(2))
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
      real(kind = kreal), intent(in) :: weight_rtm(sph_rtm%nidx_rtm(2))
      type(leg_trns_theta_omp_work), intent(inout) :: WK_l_tsp
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: mp_rlm
      integer(kind = kint) :: nkrs, nkrt, lst_rtm
      integer(kind = kint) :: ip, jst
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rlm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nkrs = (3*nvector+nscalar) * sph_rlm%nidx_rlm(1)
      nkrt = 2*nvector * sph_rlm%nidx_rlm(1)
!
      do mp_rlm = 1, sph_rtm%nidx_rtm(3)
        jst = idx_trns%lstack_rlm(mp_rlm-1)
!
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+15)
!$omp parallel do private(ip,lst_rtm)
        do ip = 1, np_smp
          lst_rtm = WK_l_tsp%lst_rtm(ip)
          call set_vr_rtm_sym_mat_rout                                  &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm, weight_rtm,          &
     &        mp_rlm, WK_l_tsp%lst_rtm(ip),                             &
     &        WK_l_tsp%nle_rtm(ip), WK_l_tsp%nlo_rtm(ip),               &
     &        ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WR, WR,      &
     &        WK_l_tsp%Fmat(ip)%symp_r(1), WK_l_tsp%Fmat(ip)%asmp_p(1), &
     &        WK_l_tsp%Fmat(ip)%asmp_r(1), WK_l_tsp%Fmat(ip)%symp_p(1))
        end do
!$omp end parallel do
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+15)
!
!  even l-m
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+16)
!$omp parallel do private(ip,lst_rtm)
        do ip = 1, np_smp
          lst_rtm = WK_l_tsp%lst_rtm(ip)
          call matmul_fwd_leg_trans_Pjl(iflag_matmul,                   &
     &        WK_l_tsp%n_jk_e(mp_rlm), nkrs, WK_l_tsp%nle_rtm(ip),      &
     &        WK_l_tsp%Pmat(mp_rlm,ip)%Pse_jt,                          &
     &        WK_l_tsp%Fmat(ip)%symp_r(1), WK_l_tsp%Smat(ip)%pol_e(1))
          call matmul_fwd_leg_trans_Pjl(iflag_matmul,                   &
     &        WK_l_tsp%n_jk_e(mp_rlm), nkrt, WK_l_tsp%nle_rtm(ip),      &
     &        WK_l_tsp%Pmat(mp_rlm,ip)%dPsedt_jt,                       &
     &        WK_l_tsp%Fmat(ip)%asmp_p(1), WK_l_tsp%Smat(ip)%tor_e(1))
!
!  odd l-m
          call matmul_fwd_leg_trans_Pjl(iflag_matmul,                   &
     &        WK_l_tsp%n_jk_o(mp_rlm), nkrs, WK_l_tsp%nle_rtm(ip),      &
     &        WK_l_tsp%Pmat(mp_rlm,ip)%Pso_jt,                          &
     &        WK_l_tsp%Fmat(ip)%asmp_r(1), WK_l_tsp%Smat(ip)%pol_o(1))
          call matmul_fwd_leg_trans_Pjl(iflag_matmul,                   &
     &        WK_l_tsp%n_jk_o(mp_rlm), nkrt, WK_l_tsp%nle_rtm(ip),      &
     &        WK_l_tsp%Pmat(mp_rlm,ip)%dPsodt_jt,                       &
     &        WK_l_tsp%Fmat(ip)%symp_p(1), WK_l_tsp%Smat(ip)%tor_o(1))
        end do 
!$omp end parallel do
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+16)
!
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+17)
!$omp parallel private(ip)
        do ip = 2, np_smp
!$omp workshare
          WK_l_tsp%Smat(1)%pol_e(1:nkrs*WK_l_tsp%n_jk_e(mp_rlm))        &
     &      =  WK_l_tsp%Smat(1)%pol_e(1:nkrs*WK_l_tsp%n_jk_e(mp_rlm))   &
     &       + WK_l_tsp%Smat(ip)%pol_e(1:nkrs*WK_l_tsp%n_jk_e(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tsp%Smat(1)%tor_e(1:nkrt*WK_l_tsp%n_jk_e(mp_rlm))        &
     &      =  WK_l_tsp%Smat(1)%tor_e(1:nkrt*WK_l_tsp%n_jk_e(mp_rlm))   &
     &       + WK_l_tsp%Smat(ip)%tor_e(1:nkrt*WK_l_tsp%n_jk_e(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tsp%Smat(1)%pol_o(1:nkrs*WK_l_tsp%n_jk_o(mp_rlm))        &
     &      =  WK_l_tsp%Smat(1)%pol_o(1:nkrs*WK_l_tsp%n_jk_o(mp_rlm))   &
     &       + WK_l_tsp%Smat(ip)%pol_o(1:nkrs*WK_l_tsp%n_jk_o(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tsp%Smat(1)%tor_o(1:nkrt*WK_l_tsp%n_jk_o(mp_rlm))        &
     &      =  WK_l_tsp%Smat(1)%tor_o(1:nkrt*WK_l_tsp%n_jk_o(mp_rlm))   &
     &       + WK_l_tsp%Smat(ip)%tor_o(1:nkrt*WK_l_tsp%n_jk_o(mp_rlm))
!$omp end workshare nowait
        end do
!$omp end parallel
!
        call cal_sp_rlm_sym_mat_rout                                    &
     &     (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,                         &
     &      sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,                 &
     &      sph_rlm%radius_1d_rlm_r, g_sph_rlm, jst,                    &
     &      WK_l_tsp%n_jk_o(mp_rlm), WK_l_tsp%n_jk_e(mp_rlm),           &
     &      WK_l_tsp%Smat(1)%pol_e(1), WK_l_tsp%Smat(1)%pol_o(1),       &
     &      WK_l_tsp%Smat(1)%tor_e(1), WK_l_tsp%Smat(1)%tor_o(1),       &
     &      ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WS, WS)
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+17)
!
      end do
!
      end subroutine legendre_f_trans_sym_mat_jt
!
! ----------------------------------------------------------------------
!
      end module leg_fwd_trans_sym_mat_jt
