!>@file   m_leg_trans_sym_matmul_big.f90
!!@brief  module m_leg_trans_sym_matmul_big
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine alloc_leg_sym_matmul_big(nvector, nscalar)
!!      subroutine dealloc_leg_sym_matmul_big
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
      module m_leg_trans_sym_matmul_big
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
!
!>     Maximum matrix size for spectr data
      integer(kind = kint) :: nvec_jk
!>     Maximum matrix size for spectr data
      integer(kind = kint) :: nscl_jk
!
!>       Poloidal component with evem (l-m)  
!!@n      real(kind = kreal), allocatable :: pol_e(:,:)
!!@n     Phi derivative of toroidal component with evem (l-m)
!!@n      real(kind = kreal), allocatable :: dtordp_e(:,:)
!!@n     Phi derivative of poloidal component with evem (l-m)
!!@n      real(kind = kreal), allocatable :: dpoldp_e(:,:)
!!@n     Scalar with evem (l-m)
!!@n      real(kind = kreal), allocatable :: scl_e(:,:)
!!@n     pol_e =    Pol_e(          1:  nvec_jk,ip)
!!@n     dtordp_e = Pol_e(  nvec_jk+1:2*nvec_jk,ip)
!!@n     dpoldp_e = Pol_e(2*nvec_jk+1:3*nvec_jk,ip)
!!@n     scl_e =    Pol_e(3*nvec_jk+1:3*nvec_jk+nscl_jk,ip)
      real(kind = kreal), allocatable :: pol_e(:,:)
!
!>    Theta derivative of poloidal component with evem (l-m)
!!@n      real(kind = kreal), allocatable :: dtordt_e(:,:)
!!@n  Theta derivative of Toroidal component with evem (l-m)
!!@n      real(kind = kreal), allocatable :: dpoldt_e(:,:)
!!@n     dtordt_e = tor_e(          1:  nvec_jk,ip)
!!@n     dpoldt_e = tor_e(  nvec_jk+1:2*nvec_jk,ip)
      real(kind = kreal), allocatable :: tor_e(:,:)
!
!>     Poloidal component with odd (l-m)
!!@n      real(kind = kreal), allocatable :: pol_o(:,:)
!!@n     Phi derivative of toroidal component with odd (l-m)
!!@n      real(kind = kreal), allocatable :: dtordp_o(:,:)
!!@n     Phi derivative of Poloidal component with odd (l-m)
!!@n      real(kind = kreal), allocatable :: dpoldp_o(:,:)
!!@n     Scalar with odd (l-m)
!!@n      real(kind = kreal), allocatable :: scl_o(:,:)
!!@n     pol_o =    pol_o(          1:  nvec_jk,ip)
!!@n     dtordp_o = pol_o(  nvec_jk+1:2*nvec_jk,ip)
!!@n     dpoldp_o = pol_o(2*nvec_jk+1:3*nvec_jk,ip)
!!@n     scl_o =    pol_o(3*nvec_jk+1:3*nvec_jk+nscl_jk,ip)
      real(kind = kreal), allocatable :: pol_o(:,:)
!
!>     Theta derivative of Toroidal component with odd (l-m)
!!@n      real(kind = kreal), allocatable :: dtordt_o(:,:)
!!@n     Theta derivative of Poloidal component with odd (l-m)
!!@n      real(kind = kreal), allocatable :: dpoldt_o(:,:)
!!@n     dtordt_o = tor_o(          1:  nvec_jk,ip)
!!@n     dpoldt_o = tor_o(  nvec_jk+1:2*nvec_jk,ip)
      real(kind = kreal), allocatable :: tor_o(:,:)
!
!
!>     Maximum matrix size for field data
      integer(kind = kint) :: nvec_lk
!>     Maximum matrix size for field data
      integer(kind = kint) :: nscl_lk
!
!>       Symmetric radial component
!!@n      real(kind = kreal), allocatable :: symp_r(:,:)
!!@n     Symmetric theta-component with condugate order
!!@n      real(kind = kreal), allocatable :: symn_t(:,:)
!!@n     Symmetric phi-component with condugate order
!!@n      real(kind = kreal), allocatable :: symn_p(:,:)
!!@n     Symmetric scalar component
!!@n      real(kind = kreal), allocatable :: symp(:,:)
!!@n     symp_r = symp_r(          1:  nvec_lk,ip)
!!@n     symn_t = symp_r(  nvec_lk+1:2*nvec_lk,ip)
!!@n     symn_p = symp_r(2*nvec_lk+1:3*nvec_lk,ip)
!!@n     symp =   symp_r(3*nvec_lk+1:3*nvec_lk+nscl_lk,ip)
      real(kind = kreal), allocatable :: symp_r(:,:)
!
!>       Anti-symmetric phi-component
!!@n      real(kind = kreal), allocatable :: asmp_p(:,:)
!!@n     Anti-symmetric theta-component
!!@n      real(kind = kreal), allocatable :: asmp_t(:,:)
!!@n     asmp_p = asmp_p(          1:  nvec_lk,ip)
!!@n     asmp_t = asmp_p(  nvec_lk+1:2*nvec_lk,ip)
      real(kind = kreal), allocatable :: asmp_p(:,:)
!
!!       Anti-symmetric radial component
!!@n      real(kind = kreal), allocatable :: asmp_r(:,:)
!!@n     Anti-symmetric theta-component with condugate order
!!@n      real(kind = kreal), allocatable :: asmn_t(:,:)
!!@n     Anti-symmetric phi-component with condugate order
!!@n      real(kind = kreal), allocatable :: asmn_p(:,:)
!!@n     Anti-symmetric scalar component
!!@n      real(kind = kreal), allocatable :: asmp(:,:)
!!@n     asmp_r = asmp_r(          1:  nvec_lk,ip)
!!@n     asmn_t = asmp_r(  nvec_lk+1:2*nvec_lk,ip)
!!@n     asmn_p = asmp_r(2*nvec_lk+1:3*nvec_lk,ip)
!!@n     asmp =   asmp_r(3*nvec_lk+1:3*nvec_lk+nscl_lk,ip)
      real(kind = kreal), allocatable :: asmp_r(:,:)
!
!>      Symmetric phi-component
!!@n      real(kind = kreal), allocatable :: symp_p(:,:)
!!@n     Symmetric theta-component
!!@n      real(kind = kreal), allocatable :: symp_t(:,:)
!!@n     symp_p = symp_p(          1:  nvec_lk,ip)
!!@n     symp_t = symp_p(  nvec_lk+1:2*nvec_lk,ip)
      real(kind = kreal), allocatable :: symp_p(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_sym_matmul_big(nvector, nscalar)
!
      integer(kind = kint), intent(in) ::nvector, nscalar
!
!
      nvec_jk = ((maxdegree_rlm+1)/2) * maxidx_rlm_smp(1)*nvector
      nscl_jk = ((maxdegree_rlm+1)/2) * maxidx_rlm_smp(1)*nscalar
      allocate(pol_e(3*nvec_jk+nscl_jk,np_smp))
      allocate(tor_e(2*nvec_jk,np_smp))
      allocate(pol_o(3*nvec_jk+nscl_jk,np_smp))
      allocate(tor_o(2*nvec_jk,np_smp))
!
      nvec_lk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nvector
      nscl_lk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nscalar
      allocate(symp_r(3*nvec_lk+nscl_lk,np_smp))
      allocate(symp_p(2*nvec_lk,np_smp))
      allocate(asmp_r(3*nvec_lk+nscl_lk,np_smp))
      allocate(asmp_p(2*nvec_lk,np_smp))
!
      end subroutine alloc_leg_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_sym_matmul_big2(nvector, nscalar)
!
      integer(kind = kint), intent(in) ::nvector, nscalar
!
!
      nvec_jk = ((maxdegree_rlm+1)/2) * nidx_rlm(1)*nvector
      nscl_jk = ((maxdegree_rlm+1)/2) * nidx_rlm(1)*nscalar
      allocate(pol_e(3*nvec_jk+nscl_jk,np_smp))
      allocate(tor_e(2*nvec_jk,np_smp))
      allocate(pol_o(3*nvec_jk+nscl_jk,np_smp))
      allocate(tor_o(2*nvec_jk,np_smp))
!
      nvec_lk = ((maxidx_rtm_smp(2)+1)/2) * nidx_rlm(1)*nvector
      nscl_lk = ((maxidx_rtm_smp(2)+1)/2) * nidx_rlm(1)*nscalar
      allocate(symp_r(3*nvec_lk+nscl_lk,np_smp))
      allocate(symp_p(2*nvec_lk,np_smp))
      allocate(asmp_r(3*nvec_lk+nscl_lk,np_smp))
      allocate(asmp_p(2*nvec_lk,np_smp))
!
      end subroutine alloc_leg_sym_matmul_big2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_sym_matmul_big
!
!
      deallocate(pol_e, tor_e, pol_o, tor_o)
      deallocate(symp_r, symp_p, asmp_r, asmp_p)
!
      end subroutine dealloc_leg_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      end module m_leg_trans_sym_matmul_big
