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
!!      subroutine dealloc_leg_vec_test(mphi_rtm, WK_l_tst)
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
!
      type leg_matrix_testloop
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
      end type leg_matrix_testloop
!
!
      type field_matrix_testloop
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
      end type field_matrix_testloop
!
!
      type spectr_matrix_testloop
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
      end type spectr_matrix_testloop
!
!
!>      Work structure for Legendre trasform by large matmul
      type leg_trns_testloop_work
!>         Number of harmonics order
        integer(kind = kint) :: mphi_rtm
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint), allocatable :: n_jk_e(:)
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint), allocatable :: n_jk_o(:)
!
!
        integer(kind = kint), allocatable :: lst_rtm(:)
        integer(kind = kint), allocatable :: nle_rtm(:)
        integer(kind = kint), allocatable :: nlo_rtm(:)
!
        type(leg_matrix_testloop), allocatable :: Pmat(:,:)
!
        type(field_matrix_testloop), allocatable :: Fmat(:)
!
!
!>       size for work area of pol_e and pol_o
        integer(kind = kint) :: n_pol_e
!>       size for work area of tor_e and tor_o
        integer(kind = kint) :: n_tor_e
!
        type(spectr_matrix_testloop), allocatable :: Smat(:)
      end type leg_trns_testloop_work
!
      private :: count_symmetric_leg_lj_test
      private :: alloc_leg_sym_matmul_test
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
      WK_l_tst%mphi_rtm = sph_rtm%nidx_rtm(3)
      allocate(WK_l_tst%n_jk_e(WK_l_tst%mphi_rtm))
      allocate(WK_l_tst%n_jk_o(WK_l_tst%mphi_rtm))
      allocate(WK_l_tst%Pmat(WK_l_tst%mphi_rtm,np_smp))
!
      allocate(WK_l_tst%lst_rtm(np_smp))
      allocate(WK_l_tst%nle_rtm(np_smp))
      allocate(WK_l_tst%nlo_rtm(np_smp))
!
      allocate(WK_l_tst%Fmat(np_smp))
      allocate(WK_l_tst%Smat(np_smp))
!
      call count_size_of_field_mat_tstlop                               &
     &   (np_smp, sph_rtm%nidx_rtm(1), sph_rtm%istack_rtm_lt_smp,       &
     &    nvector, nscalar, WK_l_tst%lst_rtm, WK_l_tst%nle_rtm,         &
     &    WK_l_tst%nlo_rtm, WK_l_tst%Fmat)
      call count_symmetric_leg_lj_test                                  &
     &   (sph_rtm%nidx_rtm(3), idx_trns, WK_l_tst)
!
!
      call init_symmetric_legs_testloop                                 &
     &  (sph_rtm%nidx_rtm(2), sph_rtm%nidx_rtm(3), sph_rlm%nidx_rlm(2), &
     &   idx_trns%lstack_rlm, leg%P_rtm, leg%dPdt_rtm, WK_l_tst)
!
      call alloc_leg_sym_matmul_test                                    &
     &   (sph_rtm%nidx_rtm, nvector, nscalar, idx_trns, WK_l_tst)
!
      call alloc_field_mat_tstlop(np_smp, WK_l_tst%Fmat)
      call alloc_spectr_mat_tstlop                                      &
     &   (WK_l_tst%n_pol_e, WK_l_tst%n_tor_e, np_smp, WK_l_tst%Smat)
!
      end subroutine init_legendre_testloop
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_symmetric_leg_lj_test                            &
     &         (mphi_rtm, idx_trns, WK_l_tst)
!
      use set_legendre_matrices
!
      integer(kind = kint), intent(in) :: mphi_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: mp_rlm
!
!
      do mp_rlm = 1, mphi_rtm
        WK_l_tst%n_jk_e(mp_rlm) = idx_trns%lstack_even_rlm(mp_rlm)      &
     &                           - idx_trns%lstack_rlm(mp_rlm-1)
        WK_l_tst%n_jk_o(mp_rlm) = idx_trns%lstack_rlm(mp_rlm)           &
     &                           - idx_trns%lstack_even_rlm(mp_rlm)
      end do
!
      end subroutine count_symmetric_leg_lj_test
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_symmetric_leg_lj_test(WK_l_tst)
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: ip, mp_rlm
!
!
      do mp_rlm = 1, WK_l_tst%mphi_rtm
        do ip = 1, np_smp
          call dealloc_each_sym_leg_testloop(WK_l_tst%Pmat(mp_rlm,ip))
        end do
      end do
      deallocate(WK_l_tst%Pmat)
      deallocate(WK_l_tst%n_jk_e, WK_l_tst%n_jk_o)
!
      end subroutine dealloc_symmetric_leg_lj_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_sym_matmul_test                              &
     &         (nidx_rtm, nvector, nscalar, idx_trns, WK_l_tst)
!
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: nvector, nscalar
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
!
      WK_l_tst%n_pol_e = ((idx_trns%maxdegree_rlm+1)/2) *  nidx_rtm(1)  &
     &                  * (3*nvector + nscalar)
      WK_l_tst%n_tor_e = ((idx_trns%maxdegree_rlm+1)/2) *  nidx_rtm(1)  &
     &                  * 2*nvector
!
      end subroutine alloc_leg_sym_matmul_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_symmetric_legs_testloop                           &
     &         (nth_rtm, mphi_rtm, jmax_rlm, lstack_rlm,                &
     &          P_rtm, dPdt_rtm, WK_l_tst)
!
      integer(kind = kint), intent(in) :: nth_rtm, mphi_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: lstack_rlm(0:mphi_rtm)
!
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: mp_rlm, ip
!
!
      do mp_rlm = 1, mphi_rtm
        do ip = 1, np_smp
          call alloc_each_sym_leg_testloop(WK_l_tst%nle_rtm(ip),      &
     &        WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),       &
     &        WK_l_tst%Pmat(mp_rlm,ip))
        end do
      end do
!
      do mp_rlm = 1, mphi_rtm
        do ip = 1, np_smp
          call set_each_sym_leg_testloop(nth_rtm, jmax_rlm,             &
     &        lstack_rlm(mp_rlm-1), P_rtm, dPdt_rtm,                    &
     &        WK_l_tst%lst_rtm(ip), WK_l_tst%nle_rtm(ip),               &
     &        WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),         &
     &        WK_l_tst%Pmat(mp_rlm,ip))
        end do
      end do
!
      end subroutine init_symmetric_legs_testloop
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_each_sym_leg_testloop                            &
     &         (nle_rtm, n_jk_e, n_jk_o, Pmat)
!
      integer(kind = kint), intent(in) :: nle_rtm
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
!
      type(leg_matrix_testloop), intent(inout) :: Pmat
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
      end subroutine alloc_each_sym_leg_testloop
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_each_sym_leg_testloop(Pmat)
!
      type(leg_matrix_testloop), intent(inout) :: Pmat
!
!
      deallocate(Pmat%Pse_jt, Pmat%dPsedt_jt)
      deallocate(Pmat%Pso_jt, Pmat%dPsodt_jt)
!
      end subroutine dealloc_each_sym_leg_testloop
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_vec_test(WK_l_tst)
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
!
      call dealloc_symmetric_leg_lj_test(WK_l_tst)
!
      call dealloc_spectr_mat_tstlop(np_smp, WK_l_tst%Smat)
      call dealloc_field_mat_tstlop(np_smp, WK_l_tst%Fmat)
      deallocate(WK_l_tst%Smat)
      deallocate(WK_l_tst%Fmat)
!
      deallocate(WK_l_tst%lst_rtm, WK_l_tst%nle_rtm, WK_l_tst%nlo_rtm)
!
      end subroutine dealloc_leg_vec_test
!
! -----------------------------------------------------------------------
!
      subroutine set_each_sym_leg_testloop                              &
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
      type(leg_matrix_testloop), intent(inout) :: Pmat
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
      end subroutine set_each_sym_leg_testloop
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_size_of_field_mat_tstlop                         &
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
      type(field_matrix_testloop), intent(inout) :: Fmat(np_smp)
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
        Fmat(ip)%n_sym_r = nri_rtm * (3*nvector + nscalar)
        Fmat(ip)%n_sym_p = nri_rtm * 2*nvector
      end do
!
      end subroutine count_size_of_field_mat_tstlop
!
! -----------------------------------------------------------------------
!
      subroutine alloc_field_mat_tstlop(np_smp, Fmat)
!
      integer(kind = kint), intent(in) :: np_smp
      type(field_matrix_testloop), intent(inout) :: Fmat(np_smp)
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
      end subroutine alloc_field_mat_tstlop
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_field_mat_tstlop(np_smp, Fmat)
!
      integer(kind = kint), intent(in) :: np_smp
      type(field_matrix_testloop), intent(inout) :: Fmat(np_smp)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        deallocate(Fmat(ip)%symp_r, Fmat(ip)%symp_p)
        deallocate(Fmat(ip)%asmp_r, Fmat(ip)%asmp_p)
      end do
!
      end subroutine dealloc_field_mat_tstlop
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_spectr_mat_tstlop                                &
     &          (n_pol_e, n_tor_e, np_smp, Smat)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: n_pol_e, n_tor_e
!
      type(spectr_matrix_testloop), intent(inout) :: Smat(np_smp)
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
      end subroutine alloc_spectr_mat_tstlop
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_spectr_mat_tstlop(np_smp, Smat)
!
      integer(kind = kint), intent(in) :: np_smp
      type(spectr_matrix_testloop), intent(inout) :: Smat(np_smp)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        deallocate(Smat(ip)%pol_e, Smat(ip)%pol_o)
        deallocate(Smat(ip)%tor_e, Smat(ip)%tor_o)
      end do
!
      end subroutine dealloc_spectr_mat_tstlop
!
! -----------------------------------------------------------------------
!
      end module t_legendre_work_testlooop
