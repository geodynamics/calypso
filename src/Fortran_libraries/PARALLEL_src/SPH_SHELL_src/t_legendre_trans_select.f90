!>@file   t_legendre_trans_select.f90
!!@brief  module t_legendre_trans_select
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transform selector
!!
!!
!!@verbatim
!!      subroutine sel_init_legendre_trans(ncomp, nvector, nscalar,     &
!!     &          sph_rtm, sph_rlm, leg, idx_trns, WK_leg)
!!      subroutine sel_finalize_legendre_trans(WK_leg)
!!
!!    Backward transforms
!!      subroutine sel_backward_legendre_trans                          &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_leg)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine sel_forward_legendre_trans                           &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_leg)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module t_legendre_trans_select
!
      use m_precision
      use m_legendre_transform_list
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_work_4_sph_trans_spin
      use t_legendre_work_sym_matmul
      use t_leg_trans_sym_matmul_big
      use t_legendre_work_sym_mat_jt
      use t_legendre_work_testlooop
!
      use legendre_transform_org
      use legendre_transform_spin
      use legendre_trans_sym_matmul
      use legendre_trans_matmul_big
      use legendre_transform_sym_tomp
      use legendre_transform_testloop
!
      use matmul_for_legendre_trans
!
      implicit none
!
!>      Work structures for various Legendre trasform
      type legendre_trns_works
!>      Integer flag for Legendre transform
        integer(kind = kint) :: id_legendre = iflag_leg_undefined
!
!>        Work structure for Legendre trasform
        type(leg_trns_spin_work) :: WK_spin
!>        Work structure for Legendre trasform by matmul with symmetry
        type(leg_trns_sym_mul_work) :: WK_l_sml
!>        Work structure for Legendre trasform by large matmul
        type(leg_trns_bsym_mul_work) :: WK_l_bsm
!>        Structure for Legendre trasdorm for test
        type(leg_trns_testloop_work) :: WK_l_tst
!
!>        Structure for Legendre trasdorm for test
        type(leg_trns_theta_omp_work) :: WK_l_tsp
      end type legendre_trns_works
!
!
!>      vector length for legendre transform
      integer(kind = kint) :: nvector_legendre = 0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_init_legendre_trans(ncomp, nvector, nscalar,       &
     &          sph_rtm, sph_rlm, leg, idx_trns, WK_leg)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(legendre_trns_works), intent(inout) :: WK_leg
!
!
      if     (WK_leg%id_legendre .eq. iflag_leg_sym_matmul              &
     &   .or. WK_leg%id_legendre .eq. iflag_leg_sym_dgemm) then
        call init_legendre_sym_matmul(sph_rtm, sph_rlm, leg,            &
     &      idx_trns, nvector, nscalar, WK_leg%WK_l_sml)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_matmul_big          &
     &   .or. WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_big) then
        call init_leg_sym_matmul_big(sph_rtm, sph_rlm, leg,             &
     &      idx_trns, nvector, nscalar, WK_leg%WK_l_bsm)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_spin_loop) then
        call init_legendre_symmetry                                     &
     &     (sph_rtm, sph_rlm, leg, idx_trns, WK_leg%WK_l_sml)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_mat_jt              &
     &   .or. WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_jt) then
        call init_legendre_sym_mat_jt(sph_rtm, sph_rlm, leg,            &
     &      idx_trns, nvector, nscalar, WK_leg%WK_l_tsp)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_mat_tj              &
     &   .or. WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_tj) then
        call init_legendre_sym_mat_tj(sph_rtm, sph_rlm, leg,            &
     &      idx_trns, nvector, nscalar, WK_leg%WK_l_tsp)
      else if(WK_leg%id_legendre .eq. iflag_leg_test_loop) then
        call init_legendre_testloop(sph_rtm, sph_rlm, leg,              &
     &      idx_trns, nvector, nscalar, WK_leg%WK_l_tst)
      else
        call init_legendre_symmetry                                     &
     &     (sph_rtm, sph_rlm, leg, idx_trns, WK_leg%WK_l_sml)
      end if
!
      end subroutine sel_init_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine sel_finalize_legendre_trans(WK_leg)
!
      type(legendre_trns_works), intent(inout) :: WK_leg
!
!
      if     (WK_leg%id_legendre .eq. iflag_leg_sym_matmul              &
     &   .or. WK_leg%id_legendre .eq. iflag_leg_sym_dgemm               &
     &   .or. WK_leg%id_legendre .eq. iflag_leg_sym_spin_loop) then
        call finalize_legendre_sym_matmul(WK_leg%WK_l_sml)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_matmul_big          &
     &   .or. WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_big) then
        call dealloc_leg_sym_matmul_big(WK_leg%WK_l_bsm)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_mat_jt              &
     &   .or. WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_jt) then
        call dealloc_leg_sym_mat_jt(WK_leg%WK_l_tsp)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_mat_tj              &
     &   .or. WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_tj) then
        call dealloc_leg_sym_mat_tj(WK_leg%WK_l_tsp)
      else if(WK_leg%id_legendre .eq. iflag_leg_test_loop) then
        call dealloc_leg_vec_test(WK_leg%WK_l_tst)
      else
        call finalize_legendre_sym_matmul(WK_leg%WK_l_sml)
      end if
!
      end subroutine sel_finalize_legendre_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_backward_legendre_trans                            &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_leg)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(legendre_trns_works), intent(inout) :: WK_leg
!
!
      if(ncomp .le. 0) return
      if(WK_leg%id_legendre .eq. iflag_leg_test_loop) then
        call leg_backward_trans_test(ncomp, nvector, nscalar,           &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_tst)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_spin_loop) then
        call leg_backward_trans_sym_spin(ncomp, nvector, nscalar,       &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_sml)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_matmul) then
        call leg_backward_trans_sym_matmul                              &
     &     (iflag_INTRINSIC, ncomp, nvector, nscalar,                   &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_sml)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_dgemm) then
        call leg_backward_trans_sym_matmul                              &
     &     (iflag_DGEMM, ncomp, nvector, nscalar,                       &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_sml)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_matmul_big) then
        call leg_backward_trans_matmul_big                              &
     &     (iflag_INTRINSIC, ncomp, nvector, nscalar,                   &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_bsm)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_big) then
        call leg_backward_trans_matmul_big                              &
     &     (iflag_DGEMM, ncomp, nvector, nscalar,                       &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_bsm)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_mat_jt) then
        call leg_backward_trans_smat_jt                                 &
     &     (iflag_INTRINSIC, ncomp, nvector, nscalar,                   &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_tsp)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_jt) then
      call leg_backward_trans_smat_jt                                   &
     &     (iflag_DGEMM, ncomp, nvector, nscalar,                       &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_tsp)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_mat_tj) then
        call leg_backward_trans_smat_tj                                 &
     &     (iflag_INTRINSIC, ncomp, nvector, nscalar,                   &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_tsp)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_tj) then
        call leg_backward_trans_smat_tj                                 &
     &     (iflag_DGEMM, ncomp, nvector, nscalar,                       &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_tsp)
      else
        call leg_backward_trans_sym_org(ncomp, nvector, nscalar,        &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_sml)
      end if
!
      end subroutine sel_backward_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine sel_forward_legendre_trans                             &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_leg)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(legendre_trns_works), intent(inout) :: WK_leg
!
!
      if(ncomp .le. 0) return
      if(WK_leg%id_legendre .eq. iflag_leg_test_loop) then
        call leg_forward_trans_test(ncomp, nvector, nscalar,            &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_tst)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_spin_loop) then
        call leg_forward_trans_sym_spin(ncomp, nvector, nscalar,        &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_sml)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_matmul) then
        call leg_forward_trans_sym_matmul                               &
     &     (iflag_INTRINSIC, ncomp, nvector, nscalar,                   &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_sml)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_dgemm) then
        call leg_forward_trans_sym_matmul                               &
     &     (iflag_DGEMM, ncomp, nvector, nscalar,                       &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_sml)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_matmul_big) then
        call leg_forward_trans_matmul_big                               &
     &     (iflag_INTRINSIC, ncomp, nvector, nscalar,                   &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_bsm)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_big) then
        call leg_forward_trans_matmul_big                               &
     &     (iflag_DGEMM, ncomp, nvector, nscalar,                       &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_bsm)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_mat_jt) then
        call leg_forward_trans_smat_jt                                  &
     &     (iflag_INTRINSIC, ncomp, nvector, nscalar,                   &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_tsp)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_jt) then
        call leg_forward_trans_smat_jt                                  &
     &     (iflag_DGEMM, ncomp, nvector, nscalar,                       &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_tsp)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_mat_tj) then
        call leg_forward_trans_smat_tj                                  &
     &     (iflag_INTRINSIC, ncomp, nvector, nscalar,                   &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_tsp)
      else if(WK_leg%id_legendre .eq. iflag_leg_sym_dgemm_tj) then
        call leg_forward_trans_smat_tj                                  &
     &     (iflag_DGEMM, ncomp, nvector, nscalar,                       &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_tsp)
      else
        call leg_forward_trans_sym_org(ncomp, nvector, nscalar,         &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK_leg%WK_l_sml)
      end if
!
      end subroutine sel_forward_legendre_trans
!
! -----------------------------------------------------------------------
!
      end module t_legendre_trans_select
