!>@file   t_legendre_work_matmul.f90
!!@brief  module t_legendre_work_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine alloc_leg_vec_matmul                                 &
!!     &        (nth_rtm, maxidx_rtm_r_smp, nvector, idx_trns, WK_l_mtl)
!!      subroutine alloc_leg_scl_matmul                                 &
!!     &        (nth_rtm, maxidx_rtm_r_smp, nscalar, idx_trns, WK_l_mtl)
!!      subroutine dealloc_leg_vec_matmul(WK_l_mtl)
!!
!!      subroutine alloc_leg_vec_blocked(nth_rtm, idx_trns, WK_l_mtl)
!!      subroutine alloc_leg_scl_blocked(nth_rtm, idx_trns, WK_l_mtl)
!!        type(index_4_sph_trans), intent(in) :: idx_trns
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
      module t_legendre_work_matmul
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_work_4_sph_trans
!
      use matmul_for_legendre_trans
!
      implicit none
!
!
!>      Work structure for Legendre trasform by matmul
      type leg_trns_matmul_work
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
!
!>        Maximum matrix size for spectr data
        integer(kind = kint) :: nscl_jk
!>        Scalar with evem (l-m)
        real(kind = kreal), allocatable :: scl_e(:,:)
!
!>        Maximum matrix size for field data
        integer(kind = kint) :: nscl_lk
!>        Symmetric scalar component
        real(kind = kreal), allocatable :: symp(:,:)
      end type leg_trns_matmul_work
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_matmul                                   &
     &        (nth_rtm, maxidx_rtm_r_smp, nvector, idx_trns, WK_l_mtl)
!
      integer(kind = kint), intent(in) :: nth_rtm
      integer(kind = kint), intent(in) :: maxidx_rtm_r_smp
      integer(kind = kint), intent(in) :: nvector
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
!
      WK_l_mtl%nvec_jk = idx_trns%maxdegree_rlm                         &
     &                  * maxidx_rtm_r_smp * nvector
      allocate(WK_l_mtl%pol_e(WK_l_mtl%nvec_jk,np_smp))
      allocate(WK_l_mtl%dpoldt_e(WK_l_mtl%nvec_jk,np_smp))
      allocate(WK_l_mtl%dpoldp_e(WK_l_mtl%nvec_jk,np_smp))
      allocate(WK_l_mtl%dtordt_e(WK_l_mtl%nvec_jk,np_smp))
      allocate(WK_l_mtl%dtordp_e(WK_l_mtl%nvec_jk,np_smp))
!
      WK_l_mtl%nvec_lk = nth_rtm * maxidx_rtm_r_smp * nvector
      allocate(WK_l_mtl%symp_r(WK_l_mtl%nvec_lk,np_smp))
      allocate(WK_l_mtl%symn_t(WK_l_mtl%nvec_lk,np_smp))
      allocate(WK_l_mtl%symn_p(WK_l_mtl%nvec_lk,np_smp))
      allocate(WK_l_mtl%asmp_t(WK_l_mtl%nvec_lk,np_smp))
      allocate(WK_l_mtl%asmp_p(WK_l_mtl%nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_matmul
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_matmul                                   &
     &        (nth_rtm, maxidx_rtm_r_smp, nscalar, idx_trns, WK_l_mtl)
!
      integer(kind = kint), intent(in) :: nth_rtm
      integer(kind = kint), intent(in) :: maxidx_rtm_r_smp
      integer(kind = kint), intent(in) :: nscalar
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
!
      WK_l_mtl%nscl_jk = idx_trns%maxdegree_rlm                         &
     &                  * maxidx_rtm_r_smp * nscalar
      allocate(WK_l_mtl%scl_e(WK_l_mtl%nscl_jk,np_smp))
!
      WK_l_mtl%nscl_lk = nth_rtm * maxidx_rtm_r_smp * nscalar
      allocate(WK_l_mtl%symp(WK_l_mtl%nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_matmul
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_vec_matmul(WK_l_mtl)
!
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
!
      deallocate(WK_l_mtl%pol_e, WK_l_mtl%dpoldt_e, WK_l_mtl%dpoldp_e)
      deallocate(WK_l_mtl%dtordt_e, WK_l_mtl%dtordp_e)
      deallocate(WK_l_mtl%symp_r, WK_l_mtl%symn_t, WK_l_mtl%symn_p)
      deallocate(WK_l_mtl%asmp_t, WK_l_mtl%asmp_p)
!
      deallocate(WK_l_mtl%scl_e, WK_l_mtl%symp)
!
      end subroutine dealloc_leg_vec_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_blocked(nth_rtm, idx_trns, WK_l_mtl)
!
      integer(kind = kint), intent(in) :: nth_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
!
      WK_l_mtl%nvec_jk = idx_trns%maxdegree_rlm
      allocate(WK_l_mtl%pol_e(WK_l_mtl%nvec_jk,np_smp))
      allocate(WK_l_mtl%dpoldt_e(WK_l_mtl%nvec_jk,np_smp))
      allocate(WK_l_mtl%dpoldp_e(WK_l_mtl%nvec_jk,np_smp))
      allocate(WK_l_mtl%dtordt_e(WK_l_mtl%nvec_jk,np_smp))
      allocate(WK_l_mtl%dtordp_e(WK_l_mtl%nvec_jk,np_smp))
!
      WK_l_mtl%nvec_lk = nth_rtm
      allocate(WK_l_mtl%symp_r(WK_l_mtl%nvec_lk,np_smp))
      allocate(WK_l_mtl%symn_t(WK_l_mtl%nvec_lk,np_smp))
      allocate(WK_l_mtl%symn_p(WK_l_mtl%nvec_lk,np_smp))
      allocate(WK_l_mtl%asmp_t(WK_l_mtl%nvec_lk,np_smp))
      allocate(WK_l_mtl%asmp_p(WK_l_mtl%nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_blocked
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_blocked(nth_rtm, idx_trns, WK_l_mtl)
!
      integer(kind = kint), intent(in) :: nth_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
!
      WK_l_mtl%nscl_jk = idx_trns%maxdegree_rlm
      allocate(WK_l_mtl%scl_e(WK_l_mtl%nscl_jk,np_smp))
!
      WK_l_mtl%nscl_lk = nth_rtm
      allocate(WK_l_mtl%symp(WK_l_mtl%nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_blocked
!
! -----------------------------------------------------------------------
!
      end module t_legendre_work_matmul
