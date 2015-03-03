!>@file   m_legendre_work_sym_matmul.f90
!!@brief  module m_legendre_work_sym_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine alloc_leg_vec_sym_matmul(nvector)
!!      subroutine alloc_leg_scl_sym_matmul(nscalar)
!!      subroutine dealloc_leg_vec_sym_matmul
!!      subroutine dealloc_leg_scl_sym_matmul
!!
!!      subroutine alloc_leg_vec_matmul(nvector)
!!      subroutine alloc_leg_scl_matmul(nscalar)
!!      subroutine dealloc_leg_vec_matmul
!!      subroutine dealloc_leg_scl_matmul
!!
!!      subroutine alloc_leg_vec_symmetry
!!      subroutine alloc_leg_scl_symmetry
!!      subroutine dealloc_leg_vec_symmetry
!!      subroutine dealloc_leg_scl_symmetry
!!
!!      subroutine alloc_leg_vec_blocked
!!      subroutine alloc_leg_scl_blocked
!!      subroutine dealloc_leg_vec_blocked
!!      subroutine dealloc_leg_scl_blocked
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm(l_rtm,m_rtm,k_rtm,icomp)
!!       size: vr_rtm(nidx_rtm(2),nidx_rtm(1)*ncomp,nidx_rtm(3))
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
      module m_legendre_work_sym_matmul
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use matmul_for_legendre_trans
!
      implicit none
!
!>     Maximum matrix size for spectr data
      integer(kind = kint) :: nvec_jk
!>     Poloidal component with evem (l-m)
      real(kind = kreal), allocatable :: pol_e(:,:)
!>     radial difference of Poloidal component with evem (l-m)
      real(kind = kreal), allocatable :: dpoldt_e(:,:)
!>     radial difference of Poloidal component with evem (l-m)
      real(kind = kreal), allocatable :: dpoldp_e(:,:)
!>     Toroidal component with evem (l-m)
      real(kind = kreal), allocatable :: dtordt_e(:,:)
!>     Toroidal component with evem (l-m)
      real(kind = kreal), allocatable :: dtordp_e(:,:)
!>     Poloidal component with odd (l-m)
      real(kind = kreal), allocatable :: pol_o(:,:)
!>     radial difference of Poloidal component with odd (l-m)
      real(kind = kreal), allocatable :: dpoldt_o(:,:)
!>     radial difference of Poloidal component with odd (l-m)
      real(kind = kreal), allocatable :: dpoldp_o(:,:)
!>     Toroidal component with odd (l-m)
      real(kind = kreal), allocatable :: dtordt_o(:,:)
!>     Toroidal component with odd (l-m)
      real(kind = kreal), allocatable :: dtordp_o(:,:)
!
!>     Maximum matrix size for field data
      integer(kind = kint) :: nvec_lk
!>     Symmetric radial component
      real(kind = kreal), allocatable :: symp_r(:,:)
!>     Anti-symmetric theta-component
      real(kind = kreal), allocatable :: asmp_t(:,:)
!>     Anti-symmetric phi-component
      real(kind = kreal), allocatable :: asmp_p(:,:)
!>     Symmetric theta-component with condugate order
      real(kind = kreal), allocatable :: symn_t(:,:)
!>     Symmetric phi-component with condugate order
      real(kind = kreal), allocatable :: symn_p(:,:)
!>     Anti-symmetric radial component
      real(kind = kreal), allocatable :: asmp_r(:,:)
!>     Symmetric theta-component
      real(kind = kreal), allocatable :: symp_t(:,:)
!>     Symmetric phi-component
      real(kind = kreal), allocatable :: symp_p(:,:)
!>     Anti-symmetric theta-component with condugate order
      real(kind = kreal), allocatable :: asmn_t(:,:)
!>     Anti-symmetric phi-component with condugate order
      real(kind = kreal), allocatable :: asmn_p(:,:)
!
!>     Maximum matrix size for spectr data
      integer(kind = kint) :: nscl_jk
!>     Scalar with evem (l-m)
      real(kind = kreal), allocatable :: scl_e(:,:)
!>     Scalar with odd (l-m)
      real(kind = kreal), allocatable :: scl_o(:,:)
!
!>     Maximum matrix size for field data
      integer(kind = kint) :: nscl_lk
!>     Symmetric scalar component
      real(kind = kreal), allocatable :: symp(:,:)
!>     Anti-symmetric scalar component
      real(kind = kreal), allocatable :: asmp(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_sym_matmul(nvector)
!
      integer(kind = kint), intent(in) ::nvector
!
!
      nvec_jk = ((maxdegree_rlm+1)/2) * maxidx_rlm_smp(1)*nvector
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpoldt_e(nvec_jk,np_smp))
      allocate(dpoldp_e(nvec_jk,np_smp))
      allocate(dtordt_e(nvec_jk,np_smp))
      allocate(dtordp_e(nvec_jk,np_smp))
      allocate(pol_o(nvec_jk,np_smp))
      allocate(dpoldt_o(nvec_jk,np_smp))
      allocate(dpoldp_o(nvec_jk,np_smp))
      allocate(dtordt_o(nvec_jk,np_smp))
      allocate(dtordp_o(nvec_jk,np_smp))
!
      nvec_lk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nvector
      allocate(symp_r(nvec_lk,np_smp))
      allocate(symp_t(nvec_lk,np_smp))
      allocate(symp_p(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
!
      allocate(asmp_r(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
      allocate(asmn_t(nvec_lk,np_smp))
      allocate(asmn_p(nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_sym_matmul(nscalar)
!
      integer(kind = kint), intent(in) :: nscalar
!
!
      nscl_jk = ((maxdegree_rlm+1)/2) * maxidx_rlm_smp(1)*nscalar
      allocate(scl_e(nscl_jk,np_smp))
      allocate(scl_o(nscl_jk,np_smp))
!
      nscl_lk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nscalar
      allocate(symp(nscl_lk,np_smp))
      allocate(asmp(nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_vec_sym_matmul
!
!
      deallocate(pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e)
      deallocate(pol_o, dpoldt_o, dpoldp_o, dtordt_o, dtordp_o)
!
      deallocate(symp_r, symp_t, symp_p, symn_t, symn_p)
      deallocate(asmp_r, asmp_t, asmp_p, asmn_t, asmn_p)
!
      end subroutine dealloc_leg_vec_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_scl_sym_matmul
!
!
      deallocate(scl_e, scl_o)
      deallocate(symp, asmp)
!
      end subroutine dealloc_leg_scl_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_matmul(nvector)
!
      integer(kind = kint), intent(in) ::nvector
!
!
      nvec_jk = maxdegree_rlm * maxidx_rlm_smp(1)*nvector
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpoldt_e(nvec_jk,np_smp))
      allocate(dpoldp_e(nvec_jk,np_smp))
      allocate(dtordt_e(nvec_jk,np_smp))
      allocate(dtordp_e(nvec_jk,np_smp))
!
      nvec_lk = nidx_rtm(2) * maxidx_rlm_smp(1)*nvector
      allocate(symp_r(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_matmul
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_matmul(nscalar)
!
      integer(kind = kint), intent(in) :: nscalar
!
!
      nscl_jk = maxdegree_rlm * maxidx_rlm_smp(1)*nscalar
      allocate(scl_e(nscl_jk,np_smp))
!
      nscl_lk = nidx_rtm(2) * maxidx_rlm_smp(1)*nscalar
      allocate(symp(nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_matmul
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_vec_matmul
!
!
      deallocate(pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e)
      deallocate(symp_r, symn_t, symn_p, asmp_t, asmp_p)
!
      end subroutine dealloc_leg_vec_matmul
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_scl_matmul
!
!
      deallocate(scl_e, symp)
!
      end subroutine dealloc_leg_scl_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_symmetry
!
!
      nvec_jk = (maxdegree_rlm+1)/2
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpoldt_e(nvec_jk,np_smp))
      allocate(dpoldp_e(nvec_jk,np_smp))
      allocate(dtordt_e(nvec_jk,np_smp))
      allocate(dtordp_e(nvec_jk,np_smp))
      allocate(pol_o(nvec_jk,np_smp))
      allocate(dpoldt_o(nvec_jk,np_smp))
      allocate(dpoldp_o(nvec_jk,np_smp))
      allocate(dtordt_o(nvec_jk,np_smp))
      allocate(dtordp_o(nvec_jk,np_smp))
!
      nvec_lk = (nidx_rtm(2)+1)/2
      allocate(symp_r(nvec_lk,np_smp))
      allocate(symp_t(nvec_lk,np_smp))
      allocate(symp_p(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
!
      allocate(asmp_r(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
      allocate(asmn_t(nvec_lk,np_smp))
      allocate(asmn_p(nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_symmetry
!
!
      nscl_jk = (maxdegree_rlm+1)/2
      allocate(scl_e(nscl_jk,np_smp))
      allocate(scl_o(nscl_jk,np_smp))
!
      nscl_lk = (nidx_rtm(2)+1)/2
      allocate(symp(nscl_lk,np_smp))
      allocate(asmp(nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_vec_symmetry
!
      call dealloc_leg_vec_sym_matmul
!
      end subroutine dealloc_leg_vec_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_scl_symmetry
!
      call dealloc_leg_scl_sym_matmul
!
      end subroutine dealloc_leg_scl_symmetry
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_blocked
!
!
      nvec_jk = maxdegree_rlm
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpoldt_e(nvec_jk,np_smp))
      allocate(dpoldp_e(nvec_jk,np_smp))
      allocate(dtordt_e(nvec_jk,np_smp))
      allocate(dtordp_e(nvec_jk,np_smp))
!
      nvec_lk = nidx_rtm(2)
      allocate(symp_r(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_blocked
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_blocked
!
!
      nscl_jk = maxdegree_rlm
      allocate(scl_e(nscl_jk,np_smp))
!
      nscl_lk = nidx_rtm(2)
      allocate(symp(nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_blocked
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_vec_blocked
!
      call dealloc_leg_vec_matmul
!
      end subroutine dealloc_leg_vec_blocked
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_scl_blocked
!
      call dealloc_leg_scl_matmul
!
      end subroutine dealloc_leg_scl_blocked
!
! -----------------------------------------------------------------------
!
      end module m_legendre_work_sym_matmul
