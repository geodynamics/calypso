!>@file   t_set_legendre_4_sph_trans.f90
!!@brief  module t_set_legendre_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2020
!
!>@brief  Set Legendre polynomials into matrix
!!
!!@verbatim
!!      subroutine alloc_cal_legendre_work(l_truncation, wk_plm)
!!      subroutine dealloc_cal_legendre_work(wk_plm)
!!        type(work_make_legendre), intent(inout) :: wk_plm
!!
!!      subroutine set_each_sym_leg_omp_mat_tj                          &
!!     &        (sph_rlm, mm, jst_rlm, nlt, g_colat_rtm, n_jk_e, n_jk_o,&
!!     &         Pse_tj, dPsedt_tj, Pso_tj, dPsodt_tj, wk_plm)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(work_make_legendre), intent(inout) :: wk_plm
!!      subroutine set_each_sym_leg_omp_mat_jt                          &
!!     &        (sph_rlm, mm, jst_rlm, nlt, g_colat_rtm, n_jk_e, n_jk_o,&
!!     &         Pse_jt, dPsedt_jt, Pso_jt, dPsodt_jt, wk_plm)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(work_make_legendre), intent(inout) :: wk_plm
!!      subroutine set_each_sym_leg_omp_mat_j1                          &
!!     &        (sph_rlm, mm, jst_rlm, g_colat_rtm, n_jk_e, n_jk_o,     &
!!     &         Pse_jt, dPsedt_jt, Pso_jt, dPsodt_jt, wk_plm)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(work_make_legendre), intent(inout) :: wk_plm
!!
!!      subroutine copy_each_sym_leg_omp_mat_jt                         &
!!     &         (nth_rtm, jmax_rlm, jst_rlm, P_rtm, dPdt_rtm,          &
!!     &          lst_rtm, nle_rtm, n_jk_e, n_jk_o,                     &
!!     &          Pse_jt, dPsedt_jt, Pso_jt, dPsodt_jt)
!!      subroutine copy_each_sym_leg_omp_mat_tj                         &
!!     &         (nth_rtm, jmax_rlm, jst_rlm, P_rtm, dPdt_rtm,          &
!!     &          lst_rtm, nle_rtm, n_jk_e, n_jk_o,                     &
!!     &          Pse_tj, dPsedt_tj, Pso_tj, dPsodt_tj)
!!@endverbatim
!!
      module t_set_legendre_4_sph_trans
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_spheric_rlm_data
!
      implicit none
!
      type work_make_legendre
        integer(kind = kint) :: ltr_wk
!
        real(kind = kreal), allocatable :: p_m(:)
        real(kind = kreal), allocatable :: dp_m(:)
        real(kind = kreal), allocatable :: pmp1(:)
        real(kind = kreal), allocatable :: pmn1(:)
        real(kind = kreal), allocatable :: df_m(:)
!
        real(kind = kreal) :: st_time_omp
        real(kind = kreal) :: time_omp(3)
      end type work_make_legendre
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_cal_legendre_work(l_truncation, wk_plm)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(work_make_legendre), intent(inout) :: wk_plm
!
      wk_plm%ltr_wk = l_truncation
!
      allocate(wk_plm%p_m (0:wk_plm%ltr_wk))
      allocate(wk_plm%dp_m(0:wk_plm%ltr_wk))
      allocate(wk_plm%pmp1(0:wk_plm%ltr_wk))
      allocate(wk_plm%pmn1(0:wk_plm%ltr_wk))
      allocate(wk_plm%df_m(0:wk_plm%ltr_wk+2))
!
!$omp parallel workshare
      wk_plm%p_m (0:wk_plm%ltr_wk) = 0.0d0
      wk_plm%dp_m(0:wk_plm%ltr_wk) = 0.0d0
      wk_plm%pmp1(0:wk_plm%ltr_wk) = 0.0d0
      wk_plm%pmn1(0:wk_plm%ltr_wk) = 0.0d0
      wk_plm%df_m(0:wk_plm%ltr_wk) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_cal_legendre_work
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_cal_legendre_work(wk_plm)
!
      type(work_make_legendre), intent(inout) :: wk_plm
!
      deallocate(wk_plm%p_m, wk_plm%dp_m)
      deallocate(wk_plm%pmp1, wk_plm%pmn1, wk_plm%df_m)
!
      end subroutine dealloc_cal_legendre_work
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_each_sym_leg_omp_mat_tj                            &
     &        (sph_rlm, mm, jst_rlm, nlt, g_colat_rtm, n_jk_e, n_jk_o,  &
     &         Pse_tj, dPsedt_tj, Pso_tj, dPsodt_tj, wk_plm)
!
      use schmidt_fix_m
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      integer(kind = kint), intent(in) :: jst_rlm
!
      integer(kind = kint), intent(in) :: mm, nlt
      real(kind= kreal), intent(in) :: g_colat_rtm(nlt)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      real(kind = kreal), intent(inout) :: Pse_tj(nlt,n_jk_e)
      real(kind = kreal), intent(inout) :: dPsedt_tj(nlt,n_jk_e)
      real(kind = kreal), intent(inout) :: Pso_tj(nlt,n_jk_o)
      real(kind = kreal), intent(inout) :: dPsodt_tj(nlt,n_jk_o)
      type(work_make_legendre), intent(inout) :: wk_plm
!
      integer(kind = kint) :: j_rlm, jj
      integer(kind = kint) :: l, lt
!
!
      do lt = 1, nlt
        call schmidt_legendres_m(wk_plm%ltr_wk, mm, g_colat_rtm(lt),    &
     &                           wk_plm%p_m, wk_plm%dp_m,               &
     &                           wk_plm%pmn1, wk_plm%pmp1, wk_plm%df_m)
!
        do jj = 1, n_jk_e
          j_rlm = 2*jj - 1
          l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
          Pse_tj(lt,jj) =     wk_plm%p_m(l)
          dPsedt_tj(lt,jj) =  wk_plm%dp_m(l)
        end do
!
        do jj = 1, n_jk_o
          j_rlm = 2*jj
          l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
          Pso_tj(lt,jj) =     wk_plm%p_m(l)
          dPsodt_tj(lt,jj) =  wk_plm%dp_m(l)
        end do
      end do
!
      end subroutine set_each_sym_leg_omp_mat_tj
!
! -----------------------------------------------------------------------
!
      subroutine set_each_sym_leg_omp_mat_jt                            &
     &        (sph_rlm, mm, jst_rlm, nlt, g_colat_rtm, n_jk_e, n_jk_o,  &
     &         Pse_jt, dPsedt_jt, Pso_jt, dPsodt_jt, wk_plm)
!
      use schmidt_fix_m
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      integer(kind = kint), intent(in) :: jst_rlm
!
      integer(kind = kint), intent(in) :: mm, nlt
      real(kind= kreal), intent(in) :: g_colat_rtm(nlt)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      real(kind = kreal), intent(inout) :: Pse_jt(n_jk_e,nlt)
      real(kind = kreal), intent(inout) :: dPsedt_jt(n_jk_e,nlt)
      real(kind = kreal), intent(inout) :: Pso_jt(n_jk_o,nlt)
      real(kind = kreal), intent(inout) :: dPsodt_jt(n_jk_o,nlt)
      type(work_make_legendre), intent(inout) :: wk_plm
!
      integer(kind = kint) :: j_rlm, jj
      integer(kind = kint) :: l, lt
!
!
      do lt = 1, nlt
        call schmidt_legendres_m(wk_plm%ltr_wk, mm, g_colat_rtm(lt),    &
     &                           wk_plm%p_m, wk_plm%dp_m,               &
     &                           wk_plm%pmn1, wk_plm%pmp1, wk_plm%df_m)
!
        do jj = 1, n_jk_e
          j_rlm = 2*jj - 1
          l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
          Pse_jt(jj,lt) =     wk_plm%p_m(l)
          dPsedt_jt(jj,lt) =  wk_plm%dp_m(l)
        end do
!
        do jj = 1, n_jk_o
          j_rlm = 2*jj
          l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
          Pso_jt(jj,lt) =     wk_plm%p_m(l)
          dPsodt_jt(jj,lt) =  wk_plm%dp_m(l)
        end do
      end do
!
      end subroutine set_each_sym_leg_omp_mat_jt
!
! -----------------------------------------------------------------------
!
      subroutine set_each_sym_leg_omp_mat_j1                            &
     &        (sph_rlm, mm, jst_rlm, g_colat_rtm, n_jk_e, n_jk_o,       &
     &         Pse_jt, dPsedt_jt, Pso_jt, dPsodt_jt, wk_plm)
!
      use schmidt_fix_m
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      integer(kind = kint), intent(in) :: jst_rlm
!
      integer(kind = kint), intent(in) :: mm
      real(kind= kreal), intent(in) :: g_colat_rtm
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      real(kind = kreal), intent(inout) :: Pse_jt(n_jk_e)
      real(kind = kreal), intent(inout) :: dPsedt_jt(n_jk_e)
      real(kind = kreal), intent(inout) :: Pso_jt(n_jk_o)
      real(kind = kreal), intent(inout) :: dPsodt_jt(n_jk_o)
      type(work_make_legendre), intent(inout) :: wk_plm
!
      integer(kind = kint) :: j_rlm, jj
      integer(kind = kint) :: l
!
!
      call schmidt_legendres_m(wk_plm%ltr_wk, mm, g_colat_rtm,          &
     &                         wk_plm%p_m, wk_plm%dp_m,                 &
     &                         wk_plm%pmn1, wk_plm%pmp1, wk_plm%df_m)
!
      do jj = 1, n_jk_e
        j_rlm = 2*jj - 1
        l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
        Pse_jt(jj) =     wk_plm%p_m(l)
        dPsedt_jt(jj) =  wk_plm%dp_m(l)
      end do
!
      do jj = 1, n_jk_o
        j_rlm = 2*jj
        l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
        Pso_jt(jj) =     wk_plm%p_m(l)
        dPsodt_jt(jj) =  wk_plm%dp_m(l)
      end do
!
      end subroutine set_each_sym_leg_omp_mat_j1
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_each_sym_leg_omp_mat_jt                           &
     &         (nth_rtm, jmax_rlm, jst_rlm, P_rtm, dPdt_rtm,            &
     &          lst_rtm, nle_rtm, n_jk_e, n_jk_o,                       &
     &          Pse_jt, dPsedt_jt, Pso_jt, dPsodt_jt)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: jst_rlm
!
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      integer(kind = kint), intent(in) :: lst_rtm, nle_rtm
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      real(kind = kreal), intent(inout) :: Pse_jt(n_jk_e,nle_rtm)
      real(kind = kreal), intent(inout) :: dPsedt_jt(n_jk_e,nle_rtm)
      real(kind = kreal), intent(inout) :: Pso_jt(n_jk_o,nle_rtm)
      real(kind = kreal), intent(inout) :: dPsodt_jt(n_jk_o,nle_rtm)
!
      integer(kind = kint) :: lt, l_rtm, j_rlm, jj
!
!
!$omp parallel do private(lt,l_rtm,jj,j_rlm)
        do lt = 1, nle_rtm
          l_rtm = lst_rtm + lt
          do jj = 1, n_jk_e
            j_rlm = 2*jj + jst_rlm - 1
            Pse_jt(jj,lt) =     P_rtm(l_rtm,j_rlm)
            dPsedt_jt(jj,lt) =  dPdt_rtm(l_rtm,j_rlm)
          end do
!
          do jj = 1, n_jk_o
            j_rlm = 2*jj + jst_rlm
            Pso_jt(jj,lt) =     P_rtm(l_rtm,j_rlm)
            dPsodt_jt(jj,lt) =  dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
!$omp end parallel do
!
      end subroutine copy_each_sym_leg_omp_mat_jt
!
! -----------------------------------------------------------------------
!
      subroutine copy_each_sym_leg_omp_mat_tj                           &
     &         (nth_rtm, jmax_rlm, jst_rlm, P_rtm, dPdt_rtm,            &
     &          lst_rtm, nle_rtm, n_jk_e, n_jk_o,                       &
     &          Pse_tj, dPsedt_tj, Pso_tj, dPsodt_tj)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: jst_rlm
!
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      integer(kind = kint), intent(in) :: lst_rtm, nle_rtm
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      real(kind = kreal), intent(inout) :: Pse_tj(nle_rtm,n_jk_e)
      real(kind = kreal), intent(inout) :: dPsedt_tj(nle_rtm,n_jk_e)
      real(kind = kreal), intent(inout) :: Pso_tj(nle_rtm,n_jk_o)
      real(kind = kreal), intent(inout) :: dPsodt_tj(nle_rtm,n_jk_o)
!
      integer(kind = kint) :: lt, l_rtm, j_rlm, jj
!
!
!$omp parallel do private(lt,l_rtm,jj,j_rlm)
        do lt = 1, nle_rtm
          l_rtm = lst_rtm + lt
          do jj = 1, n_jk_e
            j_rlm = 2*jj + jst_rlm - 1
            Pse_tj(lt,jj) =     P_rtm(l_rtm,j_rlm)
            dPsedt_tj(lt,jj) =  dPdt_rtm(l_rtm,j_rlm)
          end do
!
          do jj = 1, n_jk_o
            j_rlm = 2*jj + jst_rlm
            Pso_tj(lt,jj) =     P_rtm(l_rtm,j_rlm)
            dPsodt_tj(lt,jj) =  dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
!$omp end parallel do
!
      end subroutine copy_each_sym_leg_omp_mat_tj
!
! -----------------------------------------------------------------------
!
      end module t_set_legendre_4_sph_trans
