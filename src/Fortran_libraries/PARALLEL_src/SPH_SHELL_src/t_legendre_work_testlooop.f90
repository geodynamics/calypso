!>@file   t_legendre_work_testlooop.f90
!!@brief  module t_legendre_work_testlooop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine init_legendre_testloop(sph_rtm, sph_rlm, leg,        &
!!     &          idx_trns, nvector, nscalar, WK_l_tst)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!!
!!      subroutine dealloc_leg_vec_test(WK_l_tst)
!!        type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
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
      module t_legendre_work_testlooop
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
      type leg_trns_testloop_work
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint) :: nl_e_rtm
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint) :: nl_o_rtm
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint) :: narray_nl
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint), allocatable :: istack_nlo_rtm(:)
!
!>          @$f P_{l}{m} @$f
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: Ps_tj(:,:,:)
!>          @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!          at gouss points in northen hemisphere
        real(kind = kreal), allocatable :: dPsdt_tj(:,:,:)
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
        real(kind = kreal), allocatable :: pol_e(:)
        real(kind = kreal), allocatable :: pol_e_smp(:,:)
!
!>      Theta derivative of poloidal component with evem (l-m)
!!@n        real(kind = kreal), allocatable :: dtordt_e(:,:)
!!@n    Theta derivative of Toroidal component with evem (l-m)
!!@n        real(kind = kreal), allocatable :: dpoldt_e(:,:)
!!@n       dtordt_e = tor_e(          1:  nvec_jk,ip)
!!@n       dpoldt_e = tor_e(  nvec_jk+1:2*nvec_jk,ip)
        real(kind = kreal), allocatable :: tor_e(:)
        real(kind = kreal), allocatable :: tor_e_smp(:,:)
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
        real(kind = kreal), allocatable :: pol_o_smp(:,:)
!
!>       Theta derivative of Toroidal component with odd (l-m)
!!@n        real(kind = kreal), allocatable :: dtordt_o(:,:)
!!@n       Theta derivative of Poloidal component with odd (l-m)
!!@n        real(kind = kreal), allocatable :: dpoldt_o(:,:)
!!@n       dtordt_o = tor_o(          1:  nvec_jk,ip)
!!@n       dpoldt_o = tor_o(  nvec_jk+1:2*nvec_jk,ip)
        real(kind = kreal), allocatable :: tor_o(:)
        real(kind = kreal), allocatable :: tor_o_smp(:,:)
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
      end type leg_trns_testloop_work
!
      private :: const_sym_legendre_lsmp_j
      private :: alloc_leg_sym_inner_omp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_legendre_testloop(sph_rtm, sph_rlm, leg,          &
     &          idx_trns, nvector, nscalar, WK_l_tst)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
!
      call const_sym_legendre_lsmp_j(sph_rlm%nidx_rlm(2),               &
     &    sph_rtm%nidx_rtm(2), sph_rtm%nidx_rtm(3),                     &
     &    leg, idx_trns, WK_l_tst)
      call alloc_leg_sym_inner_omp                                      &
     &   (sph_rtm%nidx_rtm(1), nvector, nscalar, idx_trns, WK_l_tst)
!
      end subroutine init_legendre_testloop
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sym_legendre_lsmp_j                              &
     &         (jmax_rlm, nth_rtm, mphi_rtm, leg, idx_trns, WK_l_tst)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nth_rtm, mphi_rtm, jmax_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
!
      WK_l_tst%nl_e_rtm = (nth_rtm + 1) / 2
      WK_l_tst%nl_o_rtm = nth_rtm / 2
      allocate(WK_l_tst%istack_nlo_rtm(0:np_smp))
!
      call count_number_4_smp(np_smp, ione, WK_l_tst%nl_o_rtm,         &
     &    WK_l_tst%istack_nlo_rtm, WK_l_tst%narray_nl)
      WK_l_tst%narray_nl = WK_l_tst%narray_nl + 1
!
      allocate(WK_l_tst%Ps_tj(WK_l_tst%narray_nl,jmax_rlm,np_smp))
      allocate(WK_l_tst%dPsdt_tj(WK_l_tst%narray_nl,jmax_rlm,np_smp))
!
      call set_symmetric_legendre_lsmp_j(nth_rtm, mphi_rtm, jmax_rlm,   &
     &    np_smp, WK_l_tst%istack_nlo_rtm,                              &
     &    WK_l_tst%narray_nl, WK_l_tst%nl_e_rtm,                        &
     &    idx_trns%lstack_rlm, idx_trns%lstack_even_rlm,                &
     &    leg%P_rtm, leg%dPdt_rtm, WK_l_tst%Ps_tj, WK_l_tst%dPsdt_tj)
!
      end subroutine const_sym_legendre_lsmp_j
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_sym_inner_omp                                &
     &         (nri_rtm, nvector, nscalar, idx_trns, WK_l_tst)
!
      integer(kind = kint), intent(in) :: nri_rtm
      integer(kind = kint), intent(in) :: nvector, nscalar
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
!
      WK_l_tst%nvec_jk = ((idx_trns%maxdegree_rlm+1)/2)                 &
     &                  * nri_rtm * nvector
      WK_l_tst%nscl_jk = ((idx_trns%maxdegree_rlm+1)/2)                 &
     &                  * nri_rtm * nscalar
!
      WK_l_tst%n_pol_e = 3*WK_l_tst%nvec_jk + WK_l_tst%nscl_jk
      WK_l_tst%n_tor_e = 2*WK_l_tst%nvec_jk
      allocate(WK_l_tst%pol_e(WK_l_tst%n_pol_e))
      allocate(WK_l_tst%tor_e(WK_l_tst%n_tor_e))
      allocate(WK_l_tst%pol_o(WK_l_tst%n_pol_e))
      allocate(WK_l_tst%tor_o(WK_l_tst%n_tor_e))
!
      allocate(WK_l_tst%pol_e_smp(WK_l_tst%n_pol_e,np_smp))
      allocate(WK_l_tst%tor_e_smp(WK_l_tst%n_tor_e,np_smp))
      allocate(WK_l_tst%pol_o_smp(WK_l_tst%n_pol_e,np_smp))
      allocate(WK_l_tst%tor_o_smp(WK_l_tst%n_tor_e,np_smp))
!
      WK_l_tst%nvec_lk = WK_l_tst%narray_nl * nri_rtm * nvector
      WK_l_tst%nscl_lk = WK_l_tst%narray_nl * nri_rtm * nscalar
!
      WK_l_tst%n_sym_r = 3*WK_l_tst%nvec_lk + WK_l_tst%nscl_lk
      WK_l_tst%n_sym_p = 2*WK_l_tst%nvec_lk
      allocate(WK_l_tst%symp_r(WK_l_tst%n_sym_r,np_smp))
      allocate(WK_l_tst%symp_p(WK_l_tst%n_sym_p,np_smp))
      allocate(WK_l_tst%asmp_r(WK_l_tst%n_sym_r,np_smp))
      allocate(WK_l_tst%asmp_p(WK_l_tst%n_sym_p,np_smp))
!
      end subroutine alloc_leg_sym_inner_omp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_vec_test(WK_l_tst)
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
!
      deallocate(WK_l_tst%pol_e_smp, WK_l_tst%tor_e_smp)
      deallocate(WK_l_tst%pol_o_smp, WK_l_tst%tor_o_smp)
      deallocate(WK_l_tst%pol_e, WK_l_tst%tor_e)
      deallocate(WK_l_tst%pol_o, WK_l_tst%tor_o)
      deallocate(WK_l_tst%symp_r, WK_l_tst%symp_p)
      deallocate(WK_l_tst%asmp_r, WK_l_tst%asmp_p)
!
      deallocate(WK_l_tst%Ps_tj, WK_l_tst%dPsdt_tj)
!
      end subroutine dealloc_leg_vec_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_symmetric_legendre_lsmp_j(nth_rtm, mphi_rtm,       &
     &          jmax_rlm, np_smp, istack_nlo_rtm, narray_nl, nl_e_rtm,  &
     &          lstack_rlm, lstack_even_rlm, P_rtm, dPdt_rtm,           &
     &          Ps_rtm, dPsdt_rtm)
!
      integer(kind = kint), intent(in) :: nth_rtm, mphi_rtm, jmax_rlm
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: istack_nlo_rtm(0:np_smp)
      integer(kind = kint), intent(in) :: narray_nl, nl_e_rtm
!
      integer(kind = kint), intent(in) :: lstack_rlm(0:mphi_rtm)
      integer(kind = kint), intent(in) :: lstack_even_rlm(0:mphi_rtm)
!
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      real(kind= kreal), intent(inout)                                  &
     &                  :: Ps_rtm(narray_nl,jmax_rlm,np_smp)
      real(kind= kreal), intent(inout)                                  &
     &                  :: dPsdt_rtm(narray_nl,jmax_rlm,np_smp)
!
      integer(kind = kint) :: ip, lst, num, lnum, l_rtm
      integer(kind = kint) :: mp_rlm, jst, n_jk_e, n_jk_o, jj, j_rlm
!
!
!$omp parallel do                                                       &
!$omp& private(ip,lst,num,lnum,l_rtm,jj,j_rlm,jst,n_jk_e,n_jk_o,mp_rlm)
      do ip = 1, np_smp
        lst = istack_nlo_rtm(ip-1)
        num = istack_nlo_rtm(ip) - istack_nlo_rtm(ip-1)
!
        do mp_rlm = 1, mphi_rtm
          jst = lstack_rlm(mp_rlm-1)
          n_jk_e = lstack_even_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
          n_jk_o = lstack_rlm(mp_rlm) - lstack_even_rlm(mp_rlm)
!
          do jj = 1, n_jk_e
            j_rlm = 2*jj + jst - 1
!
            do lnum = 1, num
              l_rtm = lst + lnum
              Ps_rtm(lnum,jj+jst,ip) =     P_rtm(l_rtm,j_rlm)
              dPsdt_rtm(lnum,jj+jst,ip) =  dPdt_rtm(l_rtm,j_rlm)
            end do
            do lnum = num+1, narray_nl
              Ps_rtm(lnum,jj+jst,ip) =     0.0d0
              dPsdt_rtm(lnum,jj+jst,ip) =  0.0d0
            end do
          end do
!
          do jj = 1, n_jk_o
            j_rlm = 2*jj + jst
!
            do lnum = 1, num
              l_rtm = lst + lnum
              Ps_rtm(lnum,jj+jst+n_jk_e,ip) =     P_rtm(l_rtm,j_rlm)
              dPsdt_rtm(lnum,jj+jst+n_jk_e,ip) =  dPdt_rtm(l_rtm,j_rlm)
            end do
            do lnum = num+1, narray_nl
              Ps_rtm(lnum,jj+jst+n_jk_e,ip) =     0.0d0
              dPsdt_rtm(lnum,jj+jst+n_jk_e,ip) =  0.0d0
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!
     if(nl_e_rtm .gt. istack_nlo_rtm(np_smp)) then
        lnum = nl_e_rtm - istack_nlo_rtm(np_smp-1)
!$omp parallel do private(mp_rlm,jst,n_jk_e,n_jk_o,jj,j_rlm)
        do mp_rlm = 1, mphi_rtm
          jst = lstack_rlm(mp_rlm-1)
          n_jk_e = lstack_even_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
          n_jk_o = lstack_rlm(mp_rlm) - lstack_even_rlm(mp_rlm)
!
          do jj = 1, n_jk_e
            j_rlm = 2*jj + jst - 1
            Ps_rtm(lnum,jj+jst,np_smp) =     P_rtm(nl_e_rtm,j_rlm)
            dPsdt_rtm(lnum,jj+jst,np_smp) =  dPdt_rtm(nl_e_rtm,j_rlm)
          end do
!
          do jj = 1, n_jk_o
            j_rlm = 2*jj + jst
            Ps_rtm(lnum,jj+jst+n_jk_e,np_smp)                           &
     &           = P_rtm(nl_e_rtm,j_rlm)
            dPsdt_rtm(lnum,jj+jst+n_jk_e,np_smp)                        &
     &           = dPdt_rtm(nl_e_rtm,j_rlm)
          end do
        end do
!$omp end parallel do
      end if
!
      end subroutine set_symmetric_legendre_lsmp_j
!
! -----------------------------------------------------------------------
!
      end module t_legendre_work_testlooop
