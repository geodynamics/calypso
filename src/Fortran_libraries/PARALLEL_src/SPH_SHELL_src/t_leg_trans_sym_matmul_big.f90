!>@file   t_leg_trans_sym_matmul_big.f90
!!@brief  module t_leg_trans_sym_matmul_big
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine init_leg_sym_matmul_big(sph_rtm, sph_rlm, leg,       &
!!     &          idx_trns, nvector, nscalar, WK_l_bsm)
!!      subroutine init_leg_sym_matmul_big2(sph_rtm, sph_rlm, leg,      &
!!     &          idx_trns, nvector, nscalar, WK_l_bsm)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!!
!!      subroutine dealloc_leg_sym_matmul_big(WK_l_bsm)
!!        type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm(l_rtm,m_rtm,k_rtm,icomp)
!!       size: vr_rtm(nth_rtm,nidx_rtm(1)*ncomp,nidx_rtm(3))
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
      module t_leg_trans_sym_matmul_big
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
!
      use matmul_for_legendre_trans
!
      implicit none
!
!>      Work structure for Legendre trasform by large matmul
      type leg_trns_bsym_mul_work
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint) :: nth_sym
!>          @$f P_{l}{m} @$f
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Ps_tj(:,:)
!>          @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsdt_tj(:,:)
!
!
!>        Maximum matrix size for spectr data
        integer(kind = kint) :: nvec_jk
!>        Maximum matrix size for spectr data
        integer(kind = kint) :: nscl_jk
!
!>       size for work area of pol_e and pol_o
        integer(kind = kint) :: n_pol_e
!>       size for work area of tor_e and tor_o
        integer(kind = kint) :: n_tor_e
!
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
        real(kind = kreal), allocatable :: pol_e(:,:)
!
!>      Theta derivative of poloidal component with evem (l-m)
!!@n        real(kind = kreal), allocatable :: dtordt_e(:,:)
!!@n    Theta derivative of Toroidal component with evem (l-m)
!!@n        real(kind = kreal), allocatable :: dpoldt_e(:,:)
!!@n       dtordt_e = tor_e(          1:  nvec_jk,ip)
!!@n       dpoldt_e = tor_e(  nvec_jk+1:2*nvec_jk,ip)
        real(kind = kreal), allocatable :: tor_e(:,:)
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
        real(kind = kreal), allocatable :: pol_o(:,:)
!
!>       Theta derivative of Toroidal component with odd (l-m)
!!@n        real(kind = kreal), allocatable :: dtordt_o(:,:)
!!@n       Theta derivative of Poloidal component with odd (l-m)
!!@n        real(kind = kreal), allocatable :: dpoldt_o(:,:)
!!@n       dtordt_o = tor_o(          1:  nvec_jk,ip)
!!@n       dpoldt_o = tor_o(  nvec_jk+1:2*nvec_jk,ip)
        real(kind = kreal), allocatable :: tor_o(:,:)
!
!
!>       Maximum matrix size for field data
        integer(kind = kint) :: nvec_lk
!>       Maximum matrix size for field data
        integer(kind = kint) :: nscl_lk
!
!>       size for work area of symp_r and asmp_r
        integer(kind = kint) :: n_sym_r
!>       size for work area of symp_p and asmp_p
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
        real(kind = kreal), allocatable :: symp_r(:,:)
!
!>         Anti-symmetric phi-component
!!@n        real(kind = kreal), allocatable :: asmp_p(:,:)
!!@n       Anti-symmetric theta-component
!!@n        real(kind = kreal), allocatable :: asmp_t(:,:)
!!@n       asmp_p = asmp_p(          1:  nvec_lk,ip)
!!@n       asmp_t = asmp_p(  nvec_lk+1:2*nvec_lk,ip)
        real(kind = kreal), allocatable :: asmp_p(:,:)
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
        real(kind = kreal), allocatable :: asmp_r(:,:)
!
!>        Symmetric phi-component
!!@n        real(kind = kreal), allocatable :: symp_p(:,:)
!!@n       Symmetric theta-component
!!@n        real(kind = kreal), allocatable :: symp_t(:,:)
!!@n       symp_p = symp_p(          1:  nvec_lk,ip)
!!@n       symp_t = symp_p(  nvec_lk+1:2*nvec_lk,ip)
        real(kind = kreal), allocatable :: symp_p(:,:)
      end type leg_trns_bsym_mul_work
!
      private :: const_symmetric_legendre_lj
      private :: alloc_leg_sym_matmul_big, alloc_leg_sym_matmul_big2
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_leg_sym_matmul_big(sph_rtm, sph_rlm, leg,         &
     &          idx_trns, nvector, nscalar, WK_l_bsm)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!
!
      call const_symmetric_legendre_lj(sph_rlm%nidx_rlm(2),             &
     &    sph_rtm%nidx_rtm(2), sph_rtm%nidx_rtm(3),                     &
     &    leg, idx_trns, WK_l_bsm)
      call alloc_leg_sym_matmul_big                                     &
     &   (sph_rtm%nidx_rtm(2), sph_rtm%maxidx_rtm_smp(1),               &
     &    nvector, nscalar, idx_trns, WK_l_bsm)
!
      end subroutine init_leg_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      subroutine init_leg_sym_matmul_big2(sph_rtm, sph_rlm, leg,        &
     &          idx_trns, nvector, nscalar, WK_l_bsm)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!
!
      call const_symmetric_legendre_lj(sph_rlm%nidx_rlm(2),             &
     &    sph_rtm%nidx_rtm(2), sph_rtm%nidx_rtm(3),                     &
     &    leg, idx_trns, WK_l_bsm)
      call alloc_leg_sym_matmul_big2                                    &
     &   (sph_rtm%nidx_rtm(1), sph_rtm%maxidx_rtm_smp(1),               &
     &    nvector, nscalar, idx_trns, WK_l_bsm)
!
      end subroutine init_leg_sym_matmul_big2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_symmetric_legendre_lj                            &
     &         (jmax_rlm, nth_rtm, mphi_rtm, leg, idx_trns, WK_l_bsm)
!
      use set_legendre_matrices
!
      integer(kind = kint), intent(in) :: nth_rtm, mphi_rtm, jmax_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!
!
      WK_l_bsm%nth_sym = (nth_rtm+1) / 2
      allocate( WK_l_bsm%Ps_tj(WK_l_bsm%nth_sym,jmax_rlm) )
      allocate( WK_l_bsm%dPsdt_tj(WK_l_bsm%nth_sym,jmax_rlm) )
!
      call set_symmetric_legendre_lj                                    &
     &   (nth_rtm, mphi_rtm, jmax_rlm, WK_l_bsm%nth_sym,                &
     &    idx_trns%lstack_rlm, idx_trns%lstack_even_rlm,                &
     &    leg%P_rtm, leg%dPdt_rtm, WK_l_bsm%Ps_tj, WK_l_bsm%dPsdt_tj)
!
      end subroutine const_symmetric_legendre_lj
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_sym_matmul_big(nth_rtm, maxidx_rtm_r_smp,    &
     &          nvector, nscalar, idx_trns, WK_l_bsm)
!
      integer(kind = kint), intent(in) :: nth_rtm
      integer(kind = kint), intent(in) :: maxidx_rtm_r_smp
      integer(kind = kint), intent(in) :: nvector, nscalar
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!
!
      WK_l_bsm%nvec_jk = ((idx_trns%maxdegree_rlm+1)/2)                 &
     &         * maxidx_rtm_r_smp * nvector
      WK_l_bsm%nscl_jk = ((idx_trns%maxdegree_rlm+1)/2)                 &
     &         * maxidx_rtm_r_smp * nscalar
!
      WK_l_bsm%n_pol_e = 3*WK_l_bsm%nvec_jk + WK_l_bsm%nscl_jk
      WK_l_bsm%n_tor_e = 2*WK_l_bsm%nvec_jk
      allocate(WK_l_bsm%pol_e(WK_l_bsm%n_pol_e,np_smp))
      allocate(WK_l_bsm%tor_e(WK_l_bsm%n_tor_e,np_smp))
      allocate(WK_l_bsm%pol_o(WK_l_bsm%n_pol_e,np_smp))
      allocate(WK_l_bsm%tor_o(WK_l_bsm%n_tor_e,np_smp))
!
      WK_l_bsm%nvec_lk = ((nth_rtm+1)/2) * maxidx_rtm_r_smp * nvector
      WK_l_bsm%nscl_lk = ((nth_rtm+1)/2) * maxidx_rtm_r_smp * nscalar
!
      WK_l_bsm%n_sym_r = 3*WK_l_bsm%nvec_lk + WK_l_bsm%nscl_lk
      WK_l_bsm%n_sym_p = 2*WK_l_bsm%nvec_lk
      allocate(WK_l_bsm%symp_r(WK_l_bsm%n_sym_r,np_smp))
      allocate(WK_l_bsm%symp_p(WK_l_bsm%n_sym_p,np_smp))
      allocate(WK_l_bsm%asmp_r(WK_l_bsm%n_sym_r,np_smp))
      allocate(WK_l_bsm%asmp_p(WK_l_bsm%n_sym_p,np_smp))
!
      end subroutine alloc_leg_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_sym_matmul_big2(nri_rtm, maxidx_rtm_t_smp,   &
     &          nvector, nscalar, idx_trns, WK_l_bsm)
!
      integer(kind = kint), intent(in) :: nri_rtm
      integer(kind = kint), intent(in) :: maxidx_rtm_t_smp
      integer(kind = kint), intent(in) :: nvector, nscalar
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!
!
      WK_l_bsm%nvec_jk = ((idx_trns%maxdegree_rlm+1)/2)                 &
     &                  * nri_rtm*nvector
      WK_l_bsm%nscl_jk = ((idx_trns%maxdegree_rlm+1)/2)                 &
     &                  * nri_rtm*nscalar
!
      WK_l_bsm%n_pol_e = 3*WK_l_bsm%nvec_jk + WK_l_bsm%nscl_jk
      WK_l_bsm%n_tor_e = 2*WK_l_bsm%nvec_jk
      allocate(WK_l_bsm%pol_e(WK_l_bsm%n_pol_e,np_smp))
      allocate(WK_l_bsm%tor_e(WK_l_bsm%n_tor_e,np_smp))
      allocate(WK_l_bsm%pol_o(WK_l_bsm%n_pol_e,np_smp))
      allocate(WK_l_bsm%tor_o(WK_l_bsm%n_tor_e,np_smp))
!
      WK_l_bsm%nvec_lk = ((maxidx_rtm_t_smp+1)/2) * nri_rtm*nvector
      WK_l_bsm%nscl_lk = ((maxidx_rtm_t_smp+1)/2) * nri_rtm*nscalar
!
      WK_l_bsm%n_sym_r = 3*WK_l_bsm%nvec_lk + WK_l_bsm%nscl_lk
      WK_l_bsm%n_sym_p = 2*WK_l_bsm%nvec_lk
      allocate(WK_l_bsm%symp_r(WK_l_bsm%n_sym_r,np_smp))
      allocate(WK_l_bsm%symp_p(WK_l_bsm%n_sym_p,np_smp))
      allocate(WK_l_bsm%asmp_r(WK_l_bsm%n_sym_r,np_smp))
      allocate(WK_l_bsm%asmp_p(WK_l_bsm%n_sym_p,np_smp))
!
      end subroutine alloc_leg_sym_matmul_big2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_sym_matmul_big(WK_l_bsm)
!
      type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!
!
      deallocate(WK_l_bsm%pol_e, WK_l_bsm%tor_e)
      deallocate(WK_l_bsm%pol_o, WK_l_bsm%tor_o)
      deallocate(WK_l_bsm%symp_r, WK_l_bsm%symp_p)
      deallocate(WK_l_bsm%asmp_r, WK_l_bsm%asmp_p)
!
      deallocate(WK_l_bsm%Ps_tj, WK_l_bsm%dPsdt_tj)
!
      end subroutine dealloc_leg_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      end module t_leg_trans_sym_matmul_big
