!>@file   t_legendre_work_sym_matmul.f90
!!@brief  module t_legendre_work_sym_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine init_legendre_sym_matmul(sph_rtm, sph_rlm, leg,      &
!!     &          idx_trns, nvector, nscalar, WK_l_sml)
!!      subroutine init_legendre_symmetry                               &
!!     &         (sph_rtm, sph_rlm, leg, idx_trns, WK_l_sml)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!!      subroutine finalize_legendre_sym_matmul(WK_l_sml)
!!        type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!!
!!      subroutine dealloc_hemi_schmidt_rtm(WK_l_sml)
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
      module t_legendre_work_sym_matmul
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
!>      Work structure for Legendre trasform by matmul with symmetry
      type leg_trns_sym_mul_work
!>        Number of meridional grid points in northern hemisphere
        integer(kind = kint) :: nth_hemi_rtm
!>         @$f P_{l}{m} @$f
!!         at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Ps_rtm(:,:)
!>         @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!         at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsdt_rtm(:,:)
!
!>         @$f P_{l}{m} @$f
!!         at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Ps_jl(:,:)
!>         @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!         at gouss points in northen hemisphere
         real(kind = kreal), allocatable :: dPsdt_jl(:,:)
!
!
!>        Maximum matrix size for spectr data
        integer(kind = kint) :: nvec_jk
!>        Poloidal component with evem (l-m)
        real(kind = kreal), allocatable :: pol_e(:,:)
!>        radial difference of Poloidal component with evem (l-m)
        real(kind = kreal), allocatable :: dpoldt_e(:,:)
!>        radial difference of Poloidal component with evem (l-m)
        real(kind = kreal), allocatable :: dpoldp_e(:,:)
!>        Toroidal component with evem (l-m)
        real(kind = kreal), allocatable :: dtordt_e(:,:)
!>        Toroidal component with evem (l-m)
        real(kind = kreal), allocatable :: dtordp_e(:,:)
!>        Poloidal component with odd (l-m)
        real(kind = kreal), allocatable :: pol_o(:,:)
!>        radial difference of Poloidal component with odd (l-m)
        real(kind = kreal), allocatable :: dpoldt_o(:,:)
!>        radial difference of Poloidal component with odd (l-m)
        real(kind = kreal), allocatable :: dpoldp_o(:,:)
!>        Toroidal component with odd (l-m)
        real(kind = kreal), allocatable :: dtordt_o(:,:)
!>        Toroidal component with odd (l-m)
        real(kind = kreal), allocatable :: dtordp_o(:,:)
!
!>        Maximum matrix size for field data
        integer(kind = kint) :: nvec_lk
!>        Symmetric radial component
        real(kind = kreal), allocatable :: symp_r(:,:)
!>        Anti-symmetric theta-component
        real(kind = kreal), allocatable :: asmp_t(:,:)
!>        Anti-symmetric phi-component
        real(kind = kreal), allocatable :: asmp_p(:,:)
!>        Symmetric theta-component with condugate order
        real(kind = kreal), allocatable :: symn_t(:,:)
!>        Symmetric phi-component with condugate order
        real(kind = kreal), allocatable :: symn_p(:,:)
!>        Anti-symmetric radial component
        real(kind = kreal), allocatable :: asmp_r(:,:)
!>        Symmetric theta-component
        real(kind = kreal), allocatable :: symp_t(:,:)
!>        Symmetric phi-component
        real(kind = kreal), allocatable :: symp_p(:,:)
!>        Anti-symmetric theta-component with condugate order
        real(kind = kreal), allocatable :: asmn_t(:,:)
!>        Anti-symmetric phi-component with condugate order
        real(kind = kreal), allocatable :: asmn_p(:,:)
!
!>        Maximum matrix size for spectr data
        integer(kind = kint) :: nscl_jk
!>        Scalar with evem (l-m)
        real(kind = kreal), allocatable :: scl_e(:,:)
!>        Scalar with odd (l-m)
        real(kind = kreal), allocatable :: scl_o(:,:)
!
!>        Maximum matrix size for field data
        integer(kind = kint) :: nscl_lk
!>        Symmetric scalar component
        real(kind = kreal), allocatable :: symp(:,:)
!>        Anti-symmetric scalar component
        real(kind = kreal), allocatable :: asmp(:,:)
      end type leg_trns_sym_mul_work
!
      private :: const_symmetric_legendres
      private :: alloc_hemi_schmidt_rtm
      private :: alloc_leg_vec_sym_matmul, dealloc_leg_vec_sym_matmul
      private :: alloc_leg_scl_sym_matmul, dealloc_leg_scl_sym_matmul
      private :: alloc_leg_vec_symmetry, alloc_leg_scl_symmetry
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_legendre_sym_matmul(sph_rtm, sph_rlm, leg,        &
     &          idx_trns, nvector, nscalar, WK_l_sml)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      call const_symmetric_legendres(sph_rlm%nidx_rlm(2),               &
     &    sph_rtm%nidx_rtm(2), sph_rtm%nidx_rtm(3),                     &
     &    leg, idx_trns, WK_l_sml)
!
      call alloc_leg_vec_sym_matmul                                     &
     &   (sph_rtm%nidx_rtm(2), sph_rtm%maxidx_rtm_smp(1),               &
     &    nvector, idx_trns, WK_l_sml)
      call alloc_leg_scl_sym_matmul                                     &
     &   (sph_rtm%nidx_rtm(2), sph_rtm%maxidx_rtm_smp(1),               &
     &    nscalar, idx_trns, WK_l_sml)
!
      end subroutine init_legendre_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine init_legendre_symmetry                                 &
     &         (sph_rtm, sph_rlm, leg, idx_trns, WK_l_sml)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      call const_symmetric_legendres(sph_rlm%nidx_rlm(2),               &
     &    sph_rtm%nidx_rtm(2), sph_rtm%nidx_rtm(3),                     &
     &    leg, idx_trns, WK_l_sml)
!
      call alloc_leg_vec_symmetry                                       &
     &   (sph_rtm%nidx_rtm(2), idx_trns, WK_l_sml)
      call alloc_leg_scl_symmetry                                       &
     &   (sph_rtm%nidx_rtm(2), idx_trns, WK_l_sml)
!
      end subroutine init_legendre_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine finalize_legendre_sym_matmul(WK_l_sml)
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      call dealloc_leg_vec_sym_matmul(WK_l_sml)
      call dealloc_leg_scl_sym_matmul(WK_l_sml)
      call dealloc_hemi_schmidt_rtm(WK_l_sml)
!
      end subroutine finalize_legendre_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_symmetric_legendres                              &
     &         (jmax_rlm, nth_rtm, mphi_rtm, leg, idx_trns, WK_l_sml)
!
      use set_legendre_matrices
!
      integer(kind = kint), intent(in) :: nth_rtm, mphi_rtm, jmax_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      call alloc_hemi_schmidt_rtm(nth_rtm, jmax_rlm, WK_l_sml)
!
      call set_symmetric_legendre_lj                                    &
     &   (nth_rtm, mphi_rtm, jmax_rlm, WK_l_sml%nth_hemi_rtm,           &
     &    idx_trns%lstack_rlm, idx_trns%lstack_even_rlm,                &
     &    leg%P_rtm, leg%dPdt_rtm, WK_l_sml%Ps_rtm, WK_l_sml%dPsdt_rtm)
      call set_symmetric_legendre_jl                                    &
     &   (nth_rtm, mphi_rtm, jmax_rlm, WK_l_sml%nth_hemi_rtm,           &
     &    idx_trns%lstack_rlm, idx_trns%lstack_even_rlm,                &
     &    leg%P_rtm, leg%dPdt_rtm, WK_l_sml%Ps_jl, WK_l_sml%dPsdt_jl)
!
      end subroutine const_symmetric_legendres
!
! -----------------------------------------------------------------------
!
      subroutine alloc_hemi_schmidt_rtm(nth_rtm, jmax_rlm, WK_l_sml)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      WK_l_sml%nth_hemi_rtm = (nth_rtm+1) / 2
      allocate( WK_l_sml%Ps_rtm(WK_l_sml%nth_hemi_rtm,jmax_rlm) )
      allocate( WK_l_sml%dPsdt_rtm(WK_l_sml%nth_hemi_rtm,jmax_rlm) )
!
      allocate( WK_l_sml%Ps_jl(jmax_rlm,WK_l_sml%nth_hemi_rtm) )
      allocate( WK_l_sml%dPsdt_jl(jmax_rlm,WK_l_sml%nth_hemi_rtm) )
!
      WK_l_sml%Ps_rtm =    0.0d0
      WK_l_sml%dPsdt_rtm = 0.0d0
!
      WK_l_sml%Ps_jl =    0.0d0
      WK_l_sml%dPsdt_jl = 0.0d0
!
      end subroutine alloc_hemi_schmidt_rtm
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_hemi_schmidt_rtm(WK_l_sml)
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
      deallocate(WK_l_sml%Ps_rtm, WK_l_sml%dPsdt_rtm)
      deallocate(WK_l_sml%Ps_jl,  WK_l_sml%dPsdt_jl)
!
      end subroutine dealloc_hemi_schmidt_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_sym_matmul                               &
     &         (nth_rtm, maxidx_rtm_r_smp, nvector, idx_trns, WK_l_sml)
!
      integer(kind = kint), intent(in) :: nth_rtm
      integer(kind = kint), intent(in) :: maxidx_rtm_r_smp
      integer(kind = kint), intent(in) :: nvector
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      WK_l_sml%nvec_jk = ((idx_trns%maxdegree_rlm+1)/2)                 &
     &         * maxidx_rtm_r_smp * nvector
      allocate(WK_l_sml%pol_e(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dpoldt_e(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dpoldp_e(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dtordt_e(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dtordp_e(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%pol_o(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dpoldt_o(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dpoldp_o(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dtordt_o(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dtordp_o(WK_l_sml%nvec_jk,np_smp))
!
      WK_l_sml%nvec_lk = ((nth_rtm + 1)/2) * maxidx_rtm_r_smp * nvector
      allocate(WK_l_sml%symp_r(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%symp_t(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%symp_p(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%symn_t(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%symn_p(WK_l_sml%nvec_lk,np_smp))
!
      allocate(WK_l_sml%asmp_r(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%asmp_t(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%asmp_p(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%asmn_t(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%asmn_p(WK_l_sml%nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_sym_matmul                               &
     &         (nth_rtm, maxidx_rtm_r_smp, nscalar, idx_trns, WK_l_sml)
!
      integer(kind = kint), intent(in) :: nth_rtm
      integer(kind = kint), intent(in) :: maxidx_rtm_r_smp
      integer(kind = kint), intent(in) :: nscalar
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      WK_l_sml%nscl_jk = ((idx_trns%maxdegree_rlm+1)/2)                 &
     &                  * maxidx_rtm_r_smp * nscalar
      allocate(WK_l_sml%scl_e(WK_l_sml%nscl_jk,np_smp))
      allocate(WK_l_sml%scl_o(WK_l_sml%nscl_jk,np_smp))
!
      WK_l_sml%nscl_lk = ((nth_rtm + 1)/2) * maxidx_rtm_r_smp * nscalar
      allocate(WK_l_sml%symp(WK_l_sml%nscl_lk,np_smp))
      allocate(WK_l_sml%asmp(WK_l_sml%nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_vec_sym_matmul(WK_l_sml)
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      deallocate(WK_l_sml%pol_e)
      deallocate(WK_l_sml%dpoldt_e, WK_l_sml%dpoldp_e)
      deallocate(WK_l_sml%dtordt_e, WK_l_sml%dtordp_e)
      deallocate(WK_l_sml%pol_o)
      deallocate(WK_l_sml%dpoldt_o, WK_l_sml%dpoldp_o)
      deallocate(WK_l_sml%dtordt_o, WK_l_sml%dtordp_o)
!
      deallocate(WK_l_sml%symp_r)
      deallocate(WK_l_sml%symp_t, WK_l_sml%symp_p)
      deallocate(WK_l_sml%symn_t, WK_l_sml%symn_p)
      deallocate(WK_l_sml%asmp_r)
      deallocate(WK_l_sml%asmp_t, WK_l_sml%asmp_p)
      deallocate(WK_l_sml%asmn_t, WK_l_sml%asmn_p)
!
      end subroutine dealloc_leg_vec_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_scl_sym_matmul(WK_l_sml)
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      deallocate(WK_l_sml%scl_e, WK_l_sml%scl_o)
      deallocate(WK_l_sml%symp,  WK_l_sml%asmp)
!
      end subroutine dealloc_leg_scl_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_symmetry(nth_rtm, idx_trns, WK_l_sml)
!
      integer(kind = kint), intent(in) :: nth_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      WK_l_sml%nvec_jk = (idx_trns%maxdegree_rlm+1)/2
      allocate(WK_l_sml%pol_e(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dpoldt_e(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dpoldp_e(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dtordt_e(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dtordp_e(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%pol_o(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dpoldt_o(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dpoldp_o(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dtordt_o(WK_l_sml%nvec_jk,np_smp))
      allocate(WK_l_sml%dtordp_o(WK_l_sml%nvec_jk,np_smp))
!
      WK_l_sml%nvec_lk = (nth_rtm + 1)/2
      allocate(WK_l_sml%symp_r(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%symp_t(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%symp_p(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%symn_t(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%symn_p(WK_l_sml%nvec_lk,np_smp))
!
      allocate(WK_l_sml%asmp_r(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%asmp_t(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%asmp_p(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%asmn_t(WK_l_sml%nvec_lk,np_smp))
      allocate(WK_l_sml%asmn_p(WK_l_sml%nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_symmetry(nth_rtm, idx_trns, WK_l_sml)
!
      integer(kind = kint), intent(in) :: nth_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      WK_l_sml%nscl_jk = (idx_trns%maxdegree_rlm+1)/2
      allocate(WK_l_sml%scl_e(WK_l_sml%nscl_jk,np_smp))
      allocate(WK_l_sml%scl_o(WK_l_sml%nscl_jk,np_smp))
!
      WK_l_sml%nscl_lk = (nth_rtm + 1)/2
      allocate(WK_l_sml%symp(WK_l_sml%nscl_lk,np_smp))
      allocate(WK_l_sml%asmp(WK_l_sml%nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_symmetry
!
! -----------------------------------------------------------------------
!
      end module t_legendre_work_sym_matmul
