!>@file   m_legendre_transform_list.f90
!!@brief  module m_legendre_transform_list
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transform selector
!!
!!@verbatim
!!      integer(kind = kint) function                                   &
!!     &                    set_legendre_trans_mode_ctl(tranx_loop_ctl)
!!      character(len = kchara) function chosen_legendre_name(i_mode)
!!      subroutine write_elapsed_4_legendre                             &
!!     &         (i_mode, etime_max, etime_trans)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module m_legendre_transform_list
!
      use m_precision
!
      implicit none
!
!>      Number of LEgendre transform types
      integer(kind = kint), parameter :: maxindex_Leg_trans_loop = 17
!
!>      Character flag to perform Legendre transform 
!@n     using original array order
      character(len = kchara), parameter                                &
     &           :: leg_orginal_loop = 'Original_loop'
!>      Character flag to perform Legendre transform 
!@n     using blocked loop
      character(len = kchara), parameter                                &
     &           :: leg_blocked_loop = 'Blocked_loop'
!>      Character flag to perform Legendre transform 
!!@n    using longer loop for original array order 
      character(len = kchara), parameter                                &
     &           :: leg_krloop_inner = 'Inner_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      character(len = kchara), parameter                                &
     &           :: leg_krloop_outer = 'Outer_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with symmetry
      character(len = kchara), parameter                                &
     &           :: leg_sym_org_loop =   'Symmetric_original_loop'
!>      Character flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      character(len = kchara), parameter                                &
     &           :: leg_sym_spin_loop = 'Symmetric_outer_radial_loop'
!
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
     &           :: leg_matprod = 'Matproduct'
!
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_matmul =  'Symmetric_matmul'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_sym_dgemm =   'Symmetric_BLAS'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      character(len = kchara), parameter                                &
     &           :: leg_sym_matprod = 'Symmetric_matproduct'
!
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_matmul_big =  'Symmetric_matmul_big'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_sym_dgemm_big =   'Symmetric_BLAS_big'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      character(len = kchara), parameter                                &
     &           :: leg_sym_matprod_big = 'Symmetric_matproduct_big'
!
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_mat_jt =     'Pjt_matmul_theta_OMP'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_sym_dgemm_jt =   'Pjt_BLAS_w_theta_OMP'
!
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_mat_tj =  'Ptj_matmul_theta_OMP'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_dgemm_tj =   'Ptj_BLAS_w_theta_OMP'
!
!>      Character flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      character(len = kchara), parameter                                &
     &           :: on_the_fly_matmul =   'On_the_fly_Plm_matmul'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      character(len = kchara), parameter                                &
     &           :: on_the_fly_dgemm =    'On_the_fly_Plm_BLAS'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      character(len = kchara), parameter                                &
     &           :: on_the_fly_matprod =  'On_the_fly_Plm'
!
!>      Character flag to sarch fastest Legendre transform loop
      character(len = kchara), parameter                                &
     &           :: leg_search_fastest =  'Search_fastest'
!>      Character flag to perform Legendre transform
!@n     with testing loop
      character(len = kchara), parameter                                &
     &           :: leg_test_loop =      'Test_Loop'
!
!
!
!>      integer undefined flag for Legendre transform
      integer(kind = kint), parameter :: iflag_leg_undefined =  -999
!>      integer flag to run elpse time check for legendre transform
      integer(kind = kint), parameter :: iflag_leg_compare =      -1
!
!>      integer flag to perform Legendre transform with symmetry
      integer(kind = kint), parameter :: iflag_leg_symmetry =      1
!>      integer flag to perform Legendre transform 
!@n     with symmetry and inneromst Legendre polynomial loop
      integer(kind = kint), parameter :: iflag_leg_sym_spin_loop = 2
!>      integer flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      integer(kind = kint), parameter :: iflag_leg_sym_matmul =    3
!>      integer flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm =    13
!>      integer flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      integer(kind = kint), parameter :: iflag_leg_sym_matmul_big =  4
!>      integer flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm_big = 14
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_mat_jt = 5
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm_jt = 15
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_mat_tj = 6
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm_tj = 16
!
!>      integer flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      integer(kind = kint), parameter :: iflag_on_the_fly_matmul = 7
!>      integer flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      integer(kind = kint), parameter :: iflag_on_the_fly_dgemm = 17
!>      integer flag to perform Legendre transform 
!@n     with symmetry and on-the-fly Legendre polynomial
      integer(kind = kint), parameter :: iflag_on_the_fly_matprod = 8
!
!>      integer flag to perform Legendre transform 
!@n     with testing loop
      integer(kind = kint), parameter :: iflag_leg_test_loop =   99
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    set_legendre_trans_mode_ctl(tranx_loop_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: tranx_loop_ctl
      integer(kind = kint) :: iflag
!
!
      iflag = iflag_leg_undefined
      if(     cmp_no_case(tranx_loop_ctl, leg_test_loop)) then
        iflag = iflag_leg_test_loop
      else if(cmp_no_case(tranx_loop_ctl, leg_search_fastest)) then
        iflag = iflag_leg_compare
!
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_org_loop)) then
        iflag = iflag_leg_symmetry
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_spin_loop)) then
        iflag = iflag_leg_sym_spin_loop
!
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_matmul)) then
        iflag = iflag_leg_sym_matmul
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_dgemm)) then
        iflag = iflag_leg_sym_dgemm
!
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_matmul_big)) then
        iflag = iflag_leg_sym_matmul_big
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_dgemm_big)) then
        iflag = iflag_leg_sym_dgemm_big
!
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_mat_jt)) then
        iflag = iflag_leg_sym_mat_jt
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_dgemm_jt)) then
        iflag = iflag_leg_sym_dgemm_jt
!
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_mat_tj)) then
        iflag = iflag_leg_sym_mat_tj
      else if(cmp_no_case(tranx_loop_ctl, leg_dgemm_tj)) then
        iflag = iflag_leg_sym_dgemm_tj
!
      else if(cmp_no_case(tranx_loop_ctl, on_the_fly_matmul)) then
        iflag = iflag_on_the_fly_matmul
      else if(cmp_no_case(tranx_loop_ctl, on_the_fly_dgemm)) then
        iflag = iflag_on_the_fly_dgemm
      else if(cmp_no_case(tranx_loop_ctl, on_the_fly_matprod)) then
        iflag = iflag_on_the_fly_matprod
      end if
      set_legendre_trans_mode_ctl = iflag
!
      end function set_legendre_trans_mode_ctl
!
! -----------------------------------------------------------------------
!
      character(len = kchara) function chosen_legendre_name(i_mode)
!
      integer(kind = kint), intent(in) :: i_mode
      character(len = kchara) :: tmpchara
!
!
      if     (i_mode .eq. iflag_leg_symmetry) then
        tmpchara = leg_sym_org_loop
      else if(i_mode .eq. iflag_leg_sym_spin_loop) then
        tmpchara = leg_sym_spin_loop
!
      else if(i_mode .eq. iflag_leg_sym_matmul) then
        tmpchara = leg_sym_matmul
      else if(i_mode .eq. iflag_leg_sym_dgemm) then
        tmpchara = leg_sym_dgemm
!
      else if(i_mode .eq. iflag_leg_sym_matmul_big) then
        tmpchara = leg_sym_matmul_big
      else if(i_mode .eq. iflag_leg_sym_dgemm_big) then
        tmpchara = leg_sym_dgemm_big
!
      else if(i_mode .eq. iflag_leg_sym_mat_jt) then
        tmpchara = leg_sym_mat_jt
      else if(i_mode .eq. iflag_leg_sym_dgemm_jt) then
        tmpchara = leg_sym_dgemm_jt
!
      else if(i_mode .eq. iflag_leg_sym_mat_tj) then
        tmpchara = leg_sym_mat_tj
      else if(i_mode .eq. iflag_leg_sym_dgemm_tj) then
        tmpchara = leg_dgemm_tj
!
      else if(i_mode .eq. iflag_on_the_fly_matmul) then
        tmpchara = on_the_fly_matmul
      else if(i_mode .eq. iflag_on_the_fly_dgemm) then
        tmpchara = on_the_fly_dgemm
      else if(i_mode .eq. iflag_on_the_fly_matprod) then
        tmpchara = on_the_fly_matprod
!
      else if(i_mode .eq. iflag_leg_test_loop) then
        tmpchara = leg_test_loop
      end if
      chosen_legendre_name = tmpchara
!
      end function chosen_legendre_name
!
! ------------------------------------------------------------------
!
      subroutine write_elapsed_4_legendre                               &
     &         (i_mode, etime_max, etime_trans)
!
      integer(kind = kint), intent(in) :: i_mode
      real(kind = kreal), intent(in) :: etime_max, etime_trans
!
!
      if     (i_mode .eq. iflag_leg_symmetry) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by original loop with symmetric    '
      else if(i_mode .eq. iflag_leg_sym_spin_loop) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by sym. outer radius               '
!
      else if(i_mode .eq. iflag_leg_sym_matmul) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by using matmul with radial SMP    '
      else if(i_mode .eq. iflag_leg_sym_dgemm) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by using BLAS with radial SMP      '
!
      else if(i_mode .eq. iflag_leg_sym_matmul_big) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by big matmul with radial SMP      '
      else if(i_mode .eq. iflag_leg_sym_dgemm_big) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by big BLAS with radial SMP        '
!
      else if(i_mode .eq. iflag_leg_sym_mat_jt) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by matmul for Pjt with theta SMP   '
      else if(i_mode .eq. iflag_leg_sym_dgemm_jt) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by BLAS for Pjt with theta SMP     '
!
      else if(i_mode .eq. iflag_leg_sym_mat_tj) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by matmul for Ptj with theta SMP   '
      else if(i_mode .eq. iflag_leg_sym_dgemm_tj) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by BLAS for Ptj with theta SMP     '
!
      else if(i_mode .eq. iflag_on_the_fly_matmul) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by matmul with on-the-fly Plm      '
      else if(i_mode .eq. iflag_on_the_fly_dgemm) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by BLAS with on-the-fly Plm        '
      else if(i_mode .eq. iflag_on_the_fly_matprod) then
          write(*,'(i3,a)', advance='NO') i_mode,                       &
     &          ': elapsed by simple loop with on-the-fly Plm '
      end if
!
      write(*,'(a1,a,a4,1p2e16.6)')                                     &
     &       '(', trim(chosen_legendre_name(i_mode)), '):  ',           &
     &       etime_max, etime_trans
!
      end subroutine write_elapsed_4_legendre
!
! ------------------------------------------------------------------
!
      end module m_legendre_transform_list
