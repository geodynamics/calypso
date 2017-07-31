!>@file   const_fdm_coefs.f90
!!@brief  module const_fdm_coefs
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!!      subroutine const_2nd_fdm_matrices(sph_params, sph_rj, r_2nd)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(inout) :: r_2nd
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!    2nd order derivatives on node by nodal field
!!      dfdr =    r_2nd%fdm(1)%dmat(-1) * d_nod(k-1)
!!              + r_2nd%fdm(1)%dmat( 0) * d_nod(k  )
!!              + r_2nd%fdm(1)%dmat( 1) * d_nod(k+1)
!!      d2fdr2 =  r_2nd%fdm(2)%dmat(-1) * d_nod(k-1)
!!              + r_2nd%fdm(2)%dmat( 0) * d_nod(k  )
!!              + r_2nd%fdm(2)%dmat( 1) * d_nod(k+1)
!!
!!       r_2nd%fdm(1)%dmat = d1nod_mat_fdm_2
!!       r_2nd%fdm(2)%dmat = d2nod_mat_fdm_2
!
!! ----------------------------------------------------------------------!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by nodal field
!!      dfdr =    r_2nd%wk_mat(2,1,k) * d_nod(k  )
!!              + r_2nd%wk_mat(2,2,k) * d_nod(k+1)
!!              + r_2nd%wk_mat(2,3,k) * d_nod(k-1)
!!      d2fdr2 =  r_2nd%wk_mat(3,1,k) * d_nod(k  )
!!              + r_2nd%wk_mat(3,2,k) * d_nod(k+1)
!!              + r_2nd%wk_mat(3,3,k) * d_nod(k-1)
!!
!!      r_2nd%wk_mat = mat_fdm_2
!! ----------------------------------------------------------------------
!!
!!      subroutine const_4th_fdm_coefs(nlayer_ICB, sph_rj, r_4th)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(inout) :: r_4th
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k-1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k-1) + d_nod(k))
!!
!!    4th order derivatives on node by nodal field
!!      dfdr =    r_4th%fdm(1)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(1)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(1)%dmat( 0,3) *  d_nod(k  )
!!              + r_4th%fdm(1)%dmat( 1,3) *  d_nod(k+1)
!!              + r_4th%fdm(1)%dmat( 2,4) *  d_nod(k+2)
!!      d2fdr2 =  r_4th%fdm(2)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(2)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(2)%dmat( 1,3) *  d_nod(k  )
!!              + r_4th%fdm(2)%dmat( 0,3) *  d_nod(k+1)
!!              + r_4th%fdm(2)%dmat( 2,4) *  d_nod(k+2)
!!      d3fdr3 =  r_4th%fdm(3)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(3)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(3)%dmat( 0,3) *  d_nod(k  )
!!              + r_4th%fdm(3)%dmat( 1,3) *  d_nod(k+1)
!!              + r_4th%fdm(3)%dmat( 2,4) *  d_nod(k+2)
!!      d4fdr4 =  r_4th%fdm(4)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(4)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(4)%dmat( 0,3) *  d_nod(k  )
!!              + r_4th%fdm(4)%dmat( 1,3) *  d_nod(k+1)
!!              + r_4th%fdm(4)%dmat( 2,4) *  d_nod(k+2)
!!
!!       r_4th%fdm(1)%dmat = d1nod_mat_fdm_4
!!       r_4th%fdm(2)%dmat = d2nod_mat_fdm_4
!!       r_4th%fdm(3)%dmat = d3nod_mat_fdm_4
!!       r_4th%fdm(4)%dmat = d4nod_mat_fdm_4
!!
!! ----------------------------------------------------------------------
!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by nodal field
!!      dfdr =    r_4th%wk_mat(2,1,k) * d_nod(k  )
!!              + r_4th%wk_mat(2,2,k) * d_nod(k+1)
!!              + r_4th%wk_mat(2,3,k) * d_nod(k-1)
!!              + r_4th%wk_mat(2,4,k) * d_nod(k+2)
!!              + r_4th%wk_mat(2,5,k) * d_nod(k-2)
!!      d2fdr2 =  r_4th%wk_mat(3,1,k) * d_nod(k  )
!!              + r_4th%wk_mat(3,2,k) * d_nod(k+1)
!!              + r_4th%wk_mat(3,3,k) * d_nod(k-1)
!!              + r_4th%wk_mat(3,4,k) * d_nod(k+2)
!!              + r_4th%wk_mat(3,5,k) * d_nod(k-2)
!!      d3fdr3 =  r_4th%wk_mat(4,1,k) * d_nod(k  )
!!              + r_4th%wk_mat(4,2,k) * d_nod(k+1)
!!              + r_4th%wk_mat(4,3,k) * d_nod(k-1)
!!              + r_4th%wk_mat(4,4,k) * d_nod(k+2)
!!              + r_4th%wk_mat(4,5,k) * d_nod(k-2)
!!      d4fdr4 =  r_4th%wk_mat(5,1,k) * d_nod(k  )
!!              + r_4th%wk_mat(5,2,k) * d_nod(k+1)
!!              + r_4th%wk_mat(5,3,k) * d_nod(k-1)
!!              + r_4th%wk_mat(5,4,k) * d_nod(k+2)
!!              + r_4th%wk_mat(5,5,k) * d_nod(k-2)
!!
!!      r_4th%wk_mat = mat_fdm_4
!! ----------------------------------------------------------------------
!!
!!      subroutine const_2e_fdm_coefs(nlayer_ICB, sph_rj, r_2nd_ele)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(inout) :: r_2nd_ele
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k+1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k+1) + d_nod(k))
!!
!!    derivatives on node by element field
!!      dfdr =    r_2nd_ele%fdm(1)%dmat(0) * d_ele(k  )
!!              + r_2nd_ele%fdm(1)%dmat(1) * d_ele(k+1)
!!
!!    r_2nd_ele%fdm(1)%dmat = d1nod_mat_fdm_2e
!!
!! ----------------------------------------------------------------------
!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by element field
!!      dfdr =       r_2nd_ele%wk_mat(2,1) * d_ele(k+1)
!!                 + r_2nd_ele%wk_mat(2,2) * d_ele(k  )
!!
!!      r_2nd_ele%wk_mat = mat_fdm_2e
!! ----------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module const_fdm_coefs
!
      use m_precision
      use m_constants
      use t_spheric_parameter
      use t_fdm_coefs
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine const_2nd_fdm_matrices(sph_params, sph_rj, r_2nd)
!
      use set_radius_func_noequi
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(fdm_matrices), intent(inout) :: r_2nd
!
!
      call alloc_nod_fdm_matrices(sph_rj%nidx_rj(1), itwo, r_2nd)
      call alloc_fdm_work(sph_rj%nidx_rj(1), r_2nd)
!   Choose radial differences
      call nod_r_2nd_fdm_coefs_nonequi(sph_params%nlayer_ICB,           &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, r_2nd%wk_mat)
      call deallocate_dr_rj_noequi
!
      call copy_fdm2_nod_coefs_from_mat(sph_rj%nidx_rj(1), r_2nd)
      call dealloc_fdm_work(r_2nd)
!
      if(iflag_debug .eq. iflag_full_msg) then
        call check_fdm_coefs                                            &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, r_2nd)
      end if
!
      end subroutine const_2nd_fdm_matrices
!
! -----------------------------------------------------------------------
!
      subroutine const_4th_fdm_coefs(nlayer_ICB, sph_rj, r_4th)
!
      use const_radial_4th_fdm_noequi
!
      integer(kind = kint), intent(in) :: nlayer_ICB
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(fdm_matrices), intent(inout) :: r_4th
!
!
      call alloc_nod_fdm_matrices(sph_rj%nidx_rj(1), ifour, r_4th)
      call alloc_fdm_work(sph_rj%nidx_rj(1), r_4th)
!
!   Choose radial differences
      call nod_r_4th_fdm_coefs_nonequi(nlayer_ICB, sph_rj%nidx_rj(1),   &
     &    sph_rj%radius_1d_rj_r, r_4th%wk_mat)
!
      call copy_fdm4_nod_coefs_from_mat(sph_rj%nidx_rj(1), r_4th)
      call dealloc_fdm_work(r_4th)
!
      if(iflag_debug .eq. iflag_full_msg) then
        call check_fdm_coefs(sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,  &
     &      r_4th)
      end if
!
      end subroutine const_4th_fdm_coefs
!
! -----------------------------------------------------------------------
!
      subroutine const_2e_fdm_coefs(nlayer_ICB, sph_rj, r_2nd_ele)
!
      use cal_sph_exp_1st_diff_ele
!
      integer(kind = kint), intent(in) :: nlayer_ICB
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(fdm_matrices), intent(inout) :: r_2nd_ele
!
!
      call alloc_nod_fdm_matrices(sph_rj%nidx_rj(1), ione, r_2nd_ele)
      call alloc_fdm_work(sph_rj%nidx_rj(1), r_2nd_ele)
      call cal_2nd_ele_r_fdm_coefs(nlayer_ICB,                          &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, r_2nd_ele%wk_mat)
!
      call copy_fdm2_ele_coefs_from_mat(sph_rj%nidx_rj(1), r_2nd_ele)
!
      call  dealloc_fdm_work(r_2nd_ele)
!
      end subroutine const_2e_fdm_coefs
!
! -----------------------------------------------------------------------
!
      end module const_fdm_coefs
