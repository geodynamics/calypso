!>@file   t_field_matrices_4_legendre.f90
!!@brief  module t_field_matrices_4_legendre
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine count_size_of_field_mat_omp                          &
!!     &         (np_smp, nri_rtm, istack_rtm_lt_smp, nvector, nscalar, &
!!     &          lst_rtm, nle_rtm, nlo_rtm, Fmat)
!!      subroutine alloc_field_mat_omp(np_smp, Fmat)
!!      subroutine dealloc_field_mat_omp(np_smp, Fmat)
!!        type(field_matrix_omp), intent(inout) :: Fmat(np_smp)
!!
!!      subroutine alloc_spectr_mat_omp                                 &
!!     &          (n_pol_e, n_tor_e, np_smp, Smat)
!!      subroutine dealloc_spectr_mat_omp(np_smp, Smat)
!!        type(spectr_matrix_omp), intent(inout) :: Smat(np_smp)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module t_field_matrices_4_legendre
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_work_4_sph_trans
!
      use matmul_for_legendre_trans
!
      implicit none
!
!
      type field_matrix_omp
!>        Maximum matrix size for field data
        integer(kind = kint) :: nvec_lk
!>        Maximum matrix size for field data
        integer(kind = kint) :: nscl_lk
!>        size for work area of symp_r and asmp_r
        integer(kind = kint) :: n_sym_r
!>        size for work area of symp_p and asmp_p
        integer(kind = kint) :: n_sym_p
!
!>         Symmetric radial component
!!@n        real(kind = kreal), allocatable :: symp_r(:,:)
!!@n       Symmetric theta-component with condugate order
!!@n        real(kind = kreal), allocatable :: symn_t(:,:)
!!@n       Symmetric phi-component with condugate order
!!@n        real(kind = kreal), allocatable :: symn_p(:,:)
!!@n       Symmetric scalar component
!!@n        real(kind = kreal), allocatable :: symp(:,:)
!!@n       symp_r = symp_r(          1:  nvec_lk,ip)
!!@n       symn_t = symp_r(  nvec_lk+1:2*nvec_lk,ip)
!!@n       symn_p = symp_r(2*nvec_lk+1:3*nvec_lk,ip)
!!@n       symp =   symp_r(3*nvec_lk+1:3*nvec_lk+nscl_lk,ip)
        real(kind = kreal), allocatable :: symp_r(:)
!
!>         Anti-symmetric phi-component
!!@n        real(kind = kreal), allocatable :: asmp_p(:,:)
!!@n       Anti-symmetric theta-component
!!@n        real(kind = kreal), allocatable :: asmp_t(:,:)
!!@n       asmp_p = asmp_p(          1:  nvec_lk,ip)
!!@n       asmp_t = asmp_p(  nvec_lk+1:2*nvec_lk,ip)
        real(kind = kreal), allocatable :: asmp_p(:)
!
!!         Anti-symmetric radial component
!!@n        real(kind = kreal), allocatable :: asmp_r(:,:)
!!@n       Anti-symmetric theta-component with condugate order
!!@n        real(kind = kreal), allocatable :: asmn_t(:,:)
!!@n       Anti-symmetric phi-component with condugate order
!!@n        real(kind = kreal), allocatable :: asmn_p(:,:)
!!@n       Anti-symmetric scalar component
!!@n        real(kind = kreal), allocatable :: asmp(:,:)
!!@n       asmp_r = asmp_r(          1:  nvec_lk,ip)
!!@n       asmn_t = asmp_r(  nvec_lk+1:2*nvec_lk,ip)
!!@n       asmn_p = asmp_r(2*nvec_lk+1:3*nvec_lk,ip)
!!@n       asmp =   asmp_r(3*nvec_lk+1:3*nvec_lk+nscl_lk,ip)
        real(kind = kreal), allocatable :: asmp_r(:)
!
!>        Symmetric phi-component
!!@n        real(kind = kreal), allocatable :: symp_p(:,:)
!!@n       Symmetric theta-component
!!@n        real(kind = kreal), allocatable :: symp_t(:,:)
!!@n       symp_p = symp_p(          1:  nvec_lk,ip)
!!@n       symp_t = symp_p(  nvec_lk+1:2*nvec_lk,ip)
        real(kind = kreal), allocatable :: symp_p(:)
      end type field_matrix_omp
!
!
      type spectr_matrix_omp
!>         Poloidal component with evem (l-m)  
!!@n        real(kind = kreal), allocatable :: pol_e(:,:)
!!@n       Phi derivative of toroidal component with evem (l-m)
!!@n        real(kind = kreal), allocatable :: dtordp_e(:,:)
!!@n       Phi derivative of poloidal component with evem (l-m)
!!@n        real(kind = kreal), allocatable :: dpoldp_e(:,:)
!!@n       Scalar with evem (l-m)
!!@n        real(kind = kreal), allocatable :: scl_e(:,:)
!!@n       pol_e =    Pol_e(          1:  nvec_jk,ip)
!!@n       dtordp_e = Pol_e(  nvec_jk+1:2*nvec_jk,ip)
!!@n       dpoldp_e = Pol_e(2*nvec_jk+1:3*nvec_jk,ip)
!!@n       scl_e =    Pol_e(3*nvec_jk+1:3*nvec_jk+nscl_jk,ip)
        real(kind = kreal), allocatable :: pol_e(:)
!
!>      Theta derivative of poloidal component with evem (l-m)
!!@n        real(kind = kreal), allocatable :: dtordt_e(:,:)
!!@n    Theta derivative of Toroidal component with evem (l-m)
!!@n        real(kind = kreal), allocatable :: dpoldt_e(:,:)
!!@n       dtordt_e = tor_e(          1:  nvec_jk,ip)
!!@n       dpoldt_e = tor_e(  nvec_jk+1:2*nvec_jk,ip)
        real(kind = kreal), allocatable :: tor_e(:)
!
!>       Poloidal component with odd (l-m)
!!@n         real(kind = kreal), allocatable :: pol_o(:,:)
!!@n       Phi derivative of toroidal component with odd (l-m)
!!@n        real(kind = kreal), allocatable :: dtordp_o(:,:)
!!@n       Phi derivative of Poloidal component with odd (l-m)
!!@n        real(kind = kreal), allocatable :: dpoldp_o(:,:)
!!@n       Scalar with odd (l-m)
!!@n        real(kind = kreal), allocatable :: scl_o(:,:)
!!@n       pol_o =    pol_o(          1:  nvec_jk,ip)
!!@n       dtordp_o = pol_o(  nvec_jk+1:2*nvec_jk,ip)
!!@n       dpoldp_o = pol_o(2*nvec_jk+1:3*nvec_jk,ip)
!!@n       scl_o =    pol_o(3*nvec_jk+1:3*nvec_jk+nscl_jk,ip)
        real(kind = kreal), allocatable :: pol_o(:)
!
!>       Theta derivative of Toroidal component with odd (l-m)
!!@n        real(kind = kreal), allocatable :: dtordt_o(:,:)
!!@n       Theta derivative of Poloidal component with odd (l-m)
!!@n        real(kind = kreal), allocatable :: dpoldt_o(:,:)
!!@n       dtordt_o = tor_o(          1:  nvec_jk,ip)
!!@n       dpoldt_o = tor_o(  nvec_jk+1:2*nvec_jk,ip)
        real(kind = kreal), allocatable :: tor_o(:)
      end type spectr_matrix_omp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_size_of_field_mat_omp                            &
     &         (np_smp, nri_rtm, istack_rtm_lt_smp, nvector, nscalar,   &
     &          lst_rtm, nle_rtm, nlo_rtm, Fmat)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: nri_rtm
      integer(kind = kint), intent(in) :: istack_rtm_lt_smp(0:np_smp)
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      integer(kind = kint), intent(inout) :: lst_rtm(np_smp)
      integer(kind = kint), intent(inout) :: nle_rtm(np_smp)
      integer(kind = kint), intent(inout) :: nlo_rtm(np_smp)
      type(field_matrix_omp), intent(inout) :: Fmat(np_smp)
!
      integer(kind = kint) :: ip, led
!
!
      led = 0
      do ip = 1, np_smp
        lst_rtm(ip) = led
        led = (istack_rtm_lt_smp(ip) + 1) / 2
        nle_rtm(ip) = led - lst_rtm(ip)
        nlo_rtm(ip) = nle_rtm(ip)
      end do
      nlo_rtm(np_smp) = istack_rtm_lt_smp(np_smp) / 2 - lst_rtm(np_smp)
!
      do ip = 1, np_smp
        Fmat(ip)%nvec_lk = nle_rtm(ip) * nri_rtm * nvector
        Fmat(ip)%nscl_lk = nle_rtm(ip) * nri_rtm * nscalar
        Fmat(ip)%n_sym_r = nle_rtm(ip) * nri_rtm                   &
     &                    * (3*nvector + nscalar)
        Fmat(ip)%n_sym_p = nle_rtm(ip) * nri_rtm * 2*nvector
      end do
!
      end subroutine count_size_of_field_mat_omp
!
! -----------------------------------------------------------------------
!
      subroutine alloc_field_mat_omp(np_smp, Fmat)
!
      integer(kind = kint), intent(in) :: np_smp
      type(field_matrix_omp), intent(inout) :: Fmat(np_smp)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        allocate(Fmat(ip)%symp_r(Fmat(ip)%n_sym_r))
        allocate(Fmat(ip)%symp_p(Fmat(ip)%n_sym_p))
        allocate(Fmat(ip)%asmp_r(Fmat(ip)%n_sym_r))
        allocate(Fmat(ip)%asmp_p(Fmat(ip)%n_sym_p))
!
!$omp parallel workshare
        Fmat(ip)%symp_r(1:Fmat(ip)%n_sym_r) = 0.0d0
        Fmat(ip)%asmp_r(1:Fmat(ip)%n_sym_r) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
        Fmat(ip)%symp_p(1:Fmat(ip)%n_sym_p) = 0.0d0
        Fmat(ip)%asmp_p(1:Fmat(ip)%n_sym_p) = 0.0d0
!$omp end parallel workshare
      end do
!
      end subroutine alloc_field_mat_omp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_field_mat_omp(np_smp, Fmat)
!
      integer(kind = kint), intent(in) :: np_smp
      type(field_matrix_omp), intent(inout) :: Fmat(np_smp)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        deallocate(Fmat(ip)%symp_r, Fmat(ip)%symp_p)
        deallocate(Fmat(ip)%asmp_r, Fmat(ip)%asmp_p)
      end do
!
      end subroutine dealloc_field_mat_omp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_spectr_mat_omp                                   &
     &          (n_pol_e, n_tor_e, np_smp, Smat)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: n_pol_e, n_tor_e
!
      type(spectr_matrix_omp), intent(inout) :: Smat(np_smp)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        allocate(Smat(ip)%pol_e(n_pol_e))
        allocate(Smat(ip)%pol_o(n_pol_e))
        allocate(Smat(ip)%tor_e(n_tor_e))
        allocate(Smat(ip)%tor_o(n_tor_e))
!
!$omp parallel workshare
        Smat(ip)%pol_e(1:n_pol_e) = 0.0d0
        Smat(ip)%pol_o(1:n_pol_e) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
        Smat(ip)%tor_o(1:n_tor_e) = 0.0d0
        Smat(ip)%tor_e(1:n_tor_e) = 0.0d0
!$omp end parallel workshare
      end do
!
      end subroutine alloc_spectr_mat_omp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_spectr_mat_omp(np_smp, Smat)
!
      integer(kind = kint), intent(in) :: np_smp
      type(spectr_matrix_omp), intent(inout) :: Smat(np_smp)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        deallocate(Smat(ip)%pol_e, Smat(ip)%pol_o)
        deallocate(Smat(ip)%tor_e, Smat(ip)%tor_o)
      end do
!
      end subroutine dealloc_spectr_mat_omp
!
! -----------------------------------------------------------------------
!
      end module t_field_matrices_4_legendre
