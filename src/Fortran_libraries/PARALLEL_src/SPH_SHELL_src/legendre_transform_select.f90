!>@file   legendre_transform_select.f90
!!@brief  module legendre_transform_select
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transform selector
!!
!!
!!@verbatim
!!      subroutine set_legendre_trans_mode_ctl(tranx_loop_ctl)
!!      subroutine sel_init_legendre_trans(ncomp, nvector, nscalar))
!!      subroutine sel_finalize_legendre_trans
!!
!!    Backward transforms
!!      subroutine sel_backward_legendre_trans                          &
!!     &         (ncomp, nvector, nscalar, n_WS, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine sel_forward_legendre_trans(ncomp, nvector, nscalar)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_select
!
      use m_precision
!
      use m_work_4_sph_trans_spin
!
      use legendre_transform_org
      use legendre_transform_krin
      use legendre_transform_spin
      use legendre_transform_testloop
      use legendre_transform_matmul
      use legendre_trans_sym_matmul
!
      implicit none
!
!
      integer(kind = kint), parameter :: ntype_Leg_trans_loop = 12
!
!>      Character flag to perform Legendre transform 
!@n     using original array order
      character(len = kchara), parameter                                &
     &           :: leg_orginal_loop = 'original_loop'
!>      Character flag to perform Legendre transform 
!@n     using blocked loop
      character(len = kchara), parameter                                &
     &           :: leg_blocked_loop = 'blocked_loop'
!>      Character flag to perform Legendre transform 
!!@n    using longer loop for original array order 
      character(len = kchara), parameter                                &
     &           :: leg_krloop_inner = 'inner_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      character(len = kchara), parameter                                &
     &           :: leg_krloop_outer = 'outer_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with symmetry
      character(len = kchara), parameter                                &
     &           :: leg_sym_org_loop =   'symmetric_original_loop'
!>      Character flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      character(len = kchara), parameter                                &
     &           :: leg_sym_spin_loop = 'symmetric_outer_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_matmul = 'matmul'
!>      Character flag to perform Legendre transform 
!@n     with dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_dgemm = 'BLAS'
!>      Character flag to perform Legendre transform 
!@n     with self matrix product
      character(len = kchara), parameter                                &
     &           :: leg_matprod = 'matproduct'
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_matmul =  'symmetric_matmul'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_sym_dgemm =   'symmetric_BLAS'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      character(len = kchara), parameter                                &
     &           :: leg_sym_matprod = 'symmetric_matproduct'
!>      Character flag to perform Legendre transform 
!@n     with testing loop
      character(len = kchara), parameter                                &
     &           :: leg_test_loop =    'test_loop'
!
!
!>      integer flag to run elpse time check for legendre transform
      integer(kind = kint), parameter :: iflag_leg_undefined = -1
!>      integer flag to perform Legendre transform 
!@n     using original array order
      integer(kind = kint), parameter :: iflag_leg_orginal_loop = 1
!>      integer flag to perform Legendre transform 
!@n     using blocked loop
      integer(kind = kint), parameter :: iflag_leg_blocked =      2
!>      integer flag to perform Legendre transform 
!!@n    using longer loop for original array order 
      integer(kind = kint), parameter :: iflag_leg_krloop_inner = 3
!>      integer flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      integer(kind = kint), parameter :: iflag_leg_krloop_outer = 4
!>      integer flag to perform Legendre transform with symmetry
      integer(kind = kint), parameter :: iflag_leg_symmetry =     5
!>      integer flag to perform Legendre transform 
!@n     with symmetry and inneromst Legendre polynomial loop
      integer(kind = kint), parameter :: iflag_leg_sym_spin_loop = 6
!>      integer flag to perform Legendre transform 
!@n     with mutmul function
      integer(kind = kint), parameter :: iflag_leg_matmul =        7
!>      integer flag to perform Legendre transform 
!@n     with dgemm in BLAS
      integer(kind = kint), parameter :: iflag_leg_dgemm =         8
!>      integer flag to perform Legendre transform 
!@n     with self matrix product
      integer(kind = kint), parameter :: iflag_leg_matprod =       9
!>      integer flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      integer(kind = kint), parameter :: iflag_leg_sym_matmul =   10
!>      integer flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm =    11
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_matprod =  12
!>      integer flag to perform Legendre transform 
!@n     with testing loop
      integer(kind = kint), parameter :: iflag_leg_test_loop =   99
!
!>      Integer flag for Legendre transform
      integer(kind = kint)                                              &
     &              :: id_legendre_transfer = iflag_leg_undefined
!
!>      vector length for legendre transform
      integer(kind = kint) :: nvector_legendre = 0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_legendre_trans_mode_ctl(tranx_loop_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: tranx_loop_ctl
!
!
      if(     cmp_no_case(tranx_loop_ctl, leg_test_loop)) then
        id_legendre_transfer = iflag_leg_test_loop
      else if(cmp_no_case(tranx_loop_ctl, leg_krloop_inner)) then
        id_legendre_transfer = iflag_leg_krloop_inner
      else if(cmp_no_case(tranx_loop_ctl, leg_krloop_outer)) then
        id_legendre_transfer = iflag_leg_krloop_outer
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_org_loop)) then
        id_legendre_transfer = iflag_leg_symmetry
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_spin_loop)) then
        id_legendre_transfer = iflag_leg_sym_spin_loop
      else if(cmp_no_case(tranx_loop_ctl, leg_matmul)) then
        id_legendre_transfer = iflag_leg_matmul
      else if(cmp_no_case(tranx_loop_ctl, leg_dgemm)) then
        id_legendre_transfer = iflag_leg_dgemm
      else if(cmp_no_case(tranx_loop_ctl, leg_matprod)) then
        id_legendre_transfer = iflag_leg_matprod
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_matmul)) then
        id_legendre_transfer = iflag_leg_sym_matmul
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_dgemm)) then
        id_legendre_transfer = iflag_leg_sym_dgemm
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_matprod)) then
        id_legendre_transfer = iflag_leg_sym_matprod
      else if(cmp_no_case(tranx_loop_ctl, leg_blocked_loop)) then
        id_legendre_transfer = iflag_leg_blocked
      else if(cmp_no_case(tranx_loop_ctl, leg_orginal_loop)) then
        id_legendre_transfer = iflag_leg_orginal_loop
      else
        id_legendre_transfer = iflag_leg_orginal_loop
      end if
!
      end subroutine set_legendre_trans_mode_ctl
!
! -----------------------------------------------------------------------
!
      subroutine sel_init_legendre_trans(ncomp, nvector, nscalar)
!
      use m_legendre_work_sym_matmul
      use m_legendre_work_testlooop
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      if     (id_legendre_transfer .eq. iflag_leg_sym_matmul            &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_dgemm             &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_matprod) then
        call alloc_leg_vec_sym_matmul(nvector)
        call alloc_leg_scl_sym_matmul(nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_matmul                &
     &   .or. id_legendre_transfer .eq. iflag_leg_dgemm                 &
     &   .or. id_legendre_transfer .eq. iflag_leg_matprod) then
        call alloc_leg_vec_matmul(nvector)
        call alloc_leg_scl_matmul(nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_symmetry              &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_spin_loop) then
        call alloc_leg_vec_symmetry
        call alloc_leg_scl_symmetry
      else if(id_legendre_transfer .eq. iflag_leg_blocked               &
     &   .or. id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call alloc_leg_vec_blocked
        call alloc_leg_scl_blocked
      else if(id_legendre_transfer .eq. iflag_leg_test_loop) then
        call alloc_leg_vec_test(nvector, nscalar)
      else
        call allocate_work_sph_trans(ncomp)
      end if
!
      end subroutine sel_init_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine sel_finalize_legendre_trans
!
      use m_legendre_work_sym_matmul
      use m_legendre_work_testlooop
!
!
      if     (id_legendre_transfer .eq. iflag_leg_sym_matmul            &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_dgemm             &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_matprod) then
        call dealloc_leg_vec_sym_matmul
        call dealloc_leg_scl_sym_matmul
      else if(id_legendre_transfer .eq. iflag_leg_matmul                &
     &   .or. id_legendre_transfer .eq. iflag_leg_dgemm                 &
     &   .or. id_legendre_transfer .eq. iflag_leg_matprod) then
        call dealloc_leg_vec_matmul
        call dealloc_leg_scl_matmul
      else if(id_legendre_transfer .eq. iflag_leg_symmetry              &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_spin_loop) then
        call dealloc_leg_vec_symmetry
        call dealloc_leg_scl_symmetry
      else if(id_legendre_transfer .eq. iflag_leg_blocked               &
     &   .or. id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call dealloc_leg_vec_blocked
        call dealloc_leg_scl_blocked
      else if(id_legendre_transfer .eq. iflag_leg_test_loop) then
        call dealloc_leg_vec_test
      else
        call deallocate_work_sph_trans
      end if
!
      end subroutine sel_finalize_legendre_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_backward_legendre_trans                            &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
!
      if(id_legendre_transfer .eq. iflag_leg_test_loop) then
        call leg_backward_trans_test(ncomp, nvector, nscalar,           &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call leg_backward_trans_spin(ncomp, nvector, nscalar,           &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call leg_bwd_trans_fields_krin(ncomp, nvector, nscalar,         &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_symmetry) then
        call leg_backward_trans_sym_org(ncomp, nvector, nscalar,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_spin_loop) then
        call leg_backward_trans_sym_spin(ncomp, nvector, nscalar,       &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_matmul) then
        call leg_backward_trans_matmul(ncomp, nvector, nscalar,         &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_dgemm) then
        call leg_backward_trans_dgemm(ncomp, nvector, nscalar,          &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_matprod) then
        call leg_backward_trans_matprod(ncomp, nvector, nscalar,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matmul) then
        call leg_backward_trans_sym_matmul(ncomp, nvector, nscalar,     &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_dgemm) then
        call leg_backward_trans_sym_dgemm(ncomp, nvector, nscalar,      &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matprod) then
        call leg_backward_trans_sym_matprod(ncomp, nvector, nscalar,    &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_blocked) then
        call leg_backward_trans_blocked(ncomp, nvector, nscalar,        &
     &      n_WR, n_WS, WR, WS)
      else
        call leg_backward_trans_org(ncomp, nvector, nscalar,            &
     &      n_WR, n_WS, WR, WS)
      end if
!
      end subroutine sel_backward_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine sel_forward_legendre_trans                             &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
!
      if(ncomp .le. 0) return
      if(id_legendre_transfer .eq. iflag_leg_test_loop) then
        call leg_forward_trans_test(ncomp, nvector, nscalar,            &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call leg_forward_trans_spin(ncomp, nvector, nscalar,            &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call leg_fwd_trans_fields_krin(ncomp, nvector, nscalar,         &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_symmetry) then
        call leg_forward_trans_sym_org(ncomp, nvector, nscalar,         &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_spin_loop) then
        call leg_forward_trans_sym_spin(ncomp, nvector, nscalar,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_matmul) then
        call leg_forward_trans_matmul(ncomp, nvector, nscalar,          &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_dgemm) then
        call leg_forward_trans_dgemm(ncomp, nvector, nscalar,           &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_matprod) then
        call leg_forward_trans_matprod(ncomp, nvector, nscalar,         &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matmul) then
        call leg_forward_trans_sym_matmul(ncomp, nvector, nscalar,      &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_dgemm) then
        call leg_forward_trans_sym_dgemm(ncomp, nvector, nscalar,       &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matprod) then
        call leg_forward_trans_sym_matprod(ncomp, nvector, nscalar,     &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_blocked) then
        call leg_forwawd_trans_blocked(ncomp, nvector, nscalar,         &
     &      n_WR, n_WS, WR, WS)
      else
        call leg_forwawd_trans_org(ncomp, nvector, nscalar,             &
     &      n_WR, n_WS, WR, WS)
      end if
!
!
      end subroutine sel_forward_legendre_trans
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_select
