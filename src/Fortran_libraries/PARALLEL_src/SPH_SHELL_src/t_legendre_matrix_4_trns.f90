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
!!     &         (nle_rtm, n_jk_e, n_jk_o, Pjt_mat)
!!        type(leg_jt_omp_matrix), intent(inout) :: Pjt_mat
!!      subroutine alloc_each_sym_leg_omp_mat_tj                        &
!!     &         (nle_rtm, n_jk_e, n_jk_o, Ptj_mat)
!!        type(leg_tj_omp_matrix), intent(inout) :: Ptj_mat
!!
!!      subroutine dealloc_each_sym_leg_mat_jt(Pjt_mat)
!!        type(leg_jt_omp_matrix), intent(inout) :: Pjt_mat
!!      subroutine dealloc_each_sym_leg_mat_tj(Ptj_mat)
!!        type(leg_tj_omp_matrix), intent(inout) :: Ptj_mat
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
!>      Structure of Legendre polynomial P(l, \theta)
      type leg_jt_omp_matrix
!>          @$f P_{l}{m} @$f
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Pse_jt(:)
!>          @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsedt_jt(:)
!
!>          @$f P_{l}{m} @$f
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Pso_jt(:)
!>          @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsodt_jt(:)
      end type leg_jt_omp_matrix
!
!>      Structure of Legendre polynomial P(\theta, l)
      type leg_tj_omp_matrix
!>          @$f P_{l}{m} @$f
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Pse_tj(:)
!>          @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsedt_tj(:)
!
!>          @$f P_{l}{m} @$f
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Pso_tj(:)
!>          @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsodt_tj(:)
      end type leg_tj_omp_matrix
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_each_sym_leg_omp_mat_jt                          &
     &         (nle_rtm, n_jk_e, n_jk_o, Pjt_mat)
!
      integer(kind = kint), intent(in) :: nle_rtm
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
!
      type(leg_jt_omp_matrix), intent(inout) :: Pjt_mat
!
!
      allocate(Pjt_mat%Pse_jt(n_jk_e*nle_rtm))
      allocate(Pjt_mat%dPsedt_jt(n_jk_e*nle_rtm))
!
!$omp parallel workshare
      Pjt_mat%Pse_jt(1:n_jk_e*nle_rtm) =    0.0d0
      Pjt_mat%dPsedt_jt(1:n_jk_e*nle_rtm) = 0.0d0
!$omp end parallel workshare
!
      allocate(Pjt_mat%Pso_jt(n_jk_o*nle_rtm))
      allocate(Pjt_mat%dPsodt_jt(n_jk_o*nle_rtm))
!
!$omp parallel workshare
      Pjt_mat%Pso_jt(1:n_jk_o*nle_rtm) =    0.0d0
      Pjt_mat%dPsodt_jt(1:n_jk_o*nle_rtm) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_each_sym_leg_omp_mat_jt
!
! -----------------------------------------------------------------------
!
      subroutine alloc_each_sym_leg_omp_mat_tj                          &
     &         (nle_rtm, n_jk_e, n_jk_o, Ptj_mat)
!
      integer(kind = kint), intent(in) :: nle_rtm
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
!
      type(leg_tj_omp_matrix), intent(inout) :: Ptj_mat
!
!
      allocate(Ptj_mat%Pse_tj(nle_rtm*n_jk_e))
      allocate(Ptj_mat%dPsedt_tj(nle_rtm*n_jk_e))
!
!$omp parallel workshare
      Ptj_mat%Pse_tj(1:nle_rtm*n_jk_e) =    0.0d0
      Ptj_mat%dPsedt_tj(1:nle_rtm*n_jk_e) = 0.0d0
!$omp end parallel workshare
!
      allocate(Ptj_mat%Pso_tj(nle_rtm*n_jk_o))
      allocate(Ptj_mat%dPsodt_tj(nle_rtm*n_jk_o))
!
!$omp parallel workshare
      Ptj_mat%Pso_tj(1:nle_rtm*n_jk_o) =    0.0d0
      Ptj_mat%dPsodt_tj(1:nle_rtm*n_jk_o) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_each_sym_leg_omp_mat_tj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_each_sym_leg_mat_jt(Pjt_mat)
!
      type(leg_jt_omp_matrix), intent(inout) :: Pjt_mat
!
!
      deallocate(Pjt_mat%Pse_jt, Pjt_mat%dPsedt_jt)
      deallocate(Pjt_mat%Pso_jt, Pjt_mat%dPsodt_jt)
!
      end subroutine dealloc_each_sym_leg_mat_jt
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_each_sym_leg_mat_tj(Ptj_mat)
!
      type(leg_tj_omp_matrix), intent(inout) :: Ptj_mat
!
!
      deallocate(Ptj_mat%Pse_tj, Ptj_mat%dPsedt_tj)
      deallocate(Ptj_mat%Pso_tj, Ptj_mat%dPsodt_tj)
!
      end subroutine dealloc_each_sym_leg_mat_tj
!
! -----------------------------------------------------------------------
!
      end module t_legendre_matrix_4_trns
