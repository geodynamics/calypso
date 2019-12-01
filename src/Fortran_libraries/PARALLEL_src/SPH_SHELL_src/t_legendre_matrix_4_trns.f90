!>@file   t_legendre_matrix_4_trns.f90
!!@brief  module t_legendre_matrix_4_trns
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine alloc_each_sym_leg_omp_mat_jt                        &
!!     &         (nle_rtm, n_jk_e, n_jk_o, Pmat)
!!      subroutine alloc_each_sym_leg_omp_mat_tj                        &
!!     &         (nle_rtm, n_jk_e, n_jk_o, Pmat)
!!        type(leg_omp_matrix), intent(inout) :: Pmat
!!
!!      subroutine dealloc_each_sym_leg_mat_jt(Pmat)
!!      subroutine dealloc_each_sym_leg_mat_tj(Pmat)
!!
!!      subroutine set_each_sym_leg_omp_mat_jt                          &
!!     &         (nth_rtm, jmax_rlm, jst_rlm, P_rtm, dPdt_rtm,          &
!!     &          lst_rtm, nle_rtm, n_jk_e, n_jk_o, Pmat)
!!      subroutine set_each_sym_leg_omp_mat_tj                          &
!!     &         (nth_rtm, jmax_rlm, jst_rlm, P_rtm, dPdt_rtm,          &
!!     &          lst_rtm, nle_rtm, n_jk_e, n_jk_o, Pmat)
!!        type(leg_omp_matrix), intent(inout) :: Pmat
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module t_legendre_matrix_4_trns
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_field_matrices_4_legendre
!
      implicit none
!
!
      type leg_omp_matrix
!>          @$f P_{l}{m} @$f
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Pse_jt(:,:)
!>          @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsedt_jt(:,:)
!
!>          @$f P_{l}{m} @$f
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Pso_jt(:,:)
!>          @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsodt_jt(:,:)
!
!
!>          @$f P_{l}{m} @$f
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Pse_tj(:,:)
!>          @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsedt_tj(:,:)
!
!>          @$f P_{l}{m} @$f
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Pso_tj(:,:)
!>          @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsodt_tj(:,:)
      end type leg_omp_matrix
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_each_sym_leg_omp_mat_jt                          &
     &         (nle_rtm, n_jk_e, n_jk_o, Pmat)
!
      integer(kind = kint), intent(in) :: nle_rtm
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
!
      type(leg_omp_matrix), intent(inout) :: Pmat
!
!
      allocate(Pmat%Pse_jt(n_jk_e,nle_rtm))
      allocate(Pmat%dPsedt_jt(n_jk_e,nle_rtm))
!
!$omp parallel workshare
      Pmat%Pse_jt(1:n_jk_e,1:nle_rtm) =    0.0d0
      Pmat%dPsedt_jt(1:n_jk_e,1:nle_rtm) = 0.0d0
!$omp end parallel workshare
!
      allocate(Pmat%Pso_jt(n_jk_o,nle_rtm))
      allocate(Pmat%dPsodt_jt(n_jk_o,nle_rtm))
!
!$omp parallel workshare
      Pmat%Pso_jt(1:n_jk_o,1:nle_rtm) =    0.0d0
      Pmat%dPsodt_jt(1:n_jk_o,1:nle_rtm) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_each_sym_leg_omp_mat_jt
!
! -----------------------------------------------------------------------
!
      subroutine alloc_each_sym_leg_omp_mat_tj                          &
     &         (nle_rtm, n_jk_e, n_jk_o, Pmat)
!
      integer(kind = kint), intent(in) :: nle_rtm
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
!
      type(leg_omp_matrix), intent(inout) :: Pmat
!
!
!
      allocate(Pmat%Pse_tj(nle_rtm,n_jk_e))
      allocate(Pmat%dPsedt_tj(nle_rtm,n_jk_e))
!
!$omp parallel workshare
      Pmat%Pse_tj(1:nle_rtm,1:n_jk_e) =    0.0d0
      Pmat%dPsedt_tj(1:nle_rtm,1:n_jk_e) = 0.0d0
!$omp end parallel workshare
!
      allocate(Pmat%Pso_tj(nle_rtm,n_jk_o))
      allocate(Pmat%dPsodt_tj(nle_rtm,n_jk_o))
!
!$omp parallel workshare
      Pmat%Pso_tj(1:nle_rtm,1:n_jk_o) =    0.0d0
      Pmat%dPsodt_tj(1:nle_rtm,1:n_jk_o) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_each_sym_leg_omp_mat_tj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_each_sym_leg_mat_jt(Pmat)
!
      type(leg_omp_matrix), intent(inout) :: Pmat
!
!
      deallocate(Pmat%Pse_jt, Pmat%dPsedt_jt)
      deallocate(Pmat%Pso_jt, Pmat%dPsodt_jt)
!
      end subroutine dealloc_each_sym_leg_mat_jt
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_each_sym_leg_mat_tj(Pmat)
!
      type(leg_omp_matrix), intent(inout) :: Pmat
!
!
      deallocate(Pmat%Pse_tj, Pmat%dPsedt_tj)
      deallocate(Pmat%Pso_tj, Pmat%dPsodt_tj)
!
      end subroutine dealloc_each_sym_leg_mat_tj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_each_sym_leg_omp_mat_jt                            &
     &         (nth_rtm, jmax_rlm, jst_rlm, P_rtm, dPdt_rtm,            &
     &          lst_rtm, nle_rtm, n_jk_e, n_jk_o, Pmat)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: jst_rlm
!
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      integer(kind = kint), intent(in) :: lst_rtm, nle_rtm
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(leg_omp_matrix), intent(inout) :: Pmat
!
      integer(kind = kint) :: lt, l_rtm, j_rlm, jj
!
!
!$omp parallel do private(lt,l_rtm,jj,j_rlm)
        do lt = 1, nle_rtm
          l_rtm = lst_rtm + lt
          do jj = 1, n_jk_e
            j_rlm = 2*jj + jst_rlm - 1
            Pmat%Pse_jt(jj,lt) =     P_rtm(l_rtm,j_rlm)
            Pmat%dPsedt_jt(jj,lt) =  dPdt_rtm(l_rtm,j_rlm)
          end do
!
          do jj = 1, n_jk_o
            j_rlm = 2*jj + jst_rlm
            Pmat%Pso_jt(jj,lt) =     P_rtm(l_rtm,j_rlm)
            Pmat%dPsodt_jt(jj,lt) =  dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
!$omp end parallel do
!
      end subroutine set_each_sym_leg_omp_mat_jt
!
! -----------------------------------------------------------------------
!
      subroutine set_each_sym_leg_omp_mat_tj                            &
     &         (nth_rtm, jmax_rlm, jst_rlm, P_rtm, dPdt_rtm,            &
     &          lst_rtm, nle_rtm, n_jk_e, n_jk_o, Pmat)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: jst_rlm
!
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      integer(kind = kint), intent(in) :: lst_rtm, nle_rtm
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(leg_omp_matrix), intent(inout) :: Pmat
!
      integer(kind = kint) :: lt, l_rtm, j_rlm, jj
!
!
!$omp parallel do private(lt,l_rtm,jj,j_rlm)
        do lt = 1, nle_rtm
          l_rtm = lst_rtm + lt
          do jj = 1, n_jk_e
            j_rlm = 2*jj + jst_rlm - 1
            Pmat%Pse_tj(lt,jj) =     P_rtm(l_rtm,j_rlm)
            Pmat%dPsedt_tj(lt,jj) =  dPdt_rtm(l_rtm,j_rlm)
          end do
!
          do jj = 1, n_jk_o
            j_rlm = 2*jj + jst_rlm
            Pmat%Pso_tj(lt,jj) =     P_rtm(l_rtm,j_rlm)
            Pmat%dPsodt_tj(lt,jj) =  dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
!$omp end parallel do
!
      end subroutine set_each_sym_leg_omp_mat_tj
!
! -----------------------------------------------------------------------
!
      end module t_legendre_matrix_4_trns
