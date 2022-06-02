!>@file   cal_sph_exp_1st_diff_ele.f90
!!@brief  module cal_sph_exp_1st_diff_ele
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate first radial derivative for spectr data
!!        for center of the element
!!
!!@verbatim
!!      subroutine cal_sph_vect_dr_ele_2(kr_in, kr_out, sph_rj,         &
!!     &          dele_rj, dnod_dr, d1nod_mat_fdm_2e)
!!      subroutine cal_2nd_ele_r_fdm_coefs                              &
!!     &         (nlayer_ICB, nri, r, mat_fdm_2e)
!!
!!        Element order: node(n-1), elemenr(n), node(n)
!!
!!@endverbatim
!!
!!@n @param kr_in      Address of inner boundary
!!@n @param kr_out     Address of outer boundary
!!@n @param jmax_rj    Number of spherical harmonic nodes
!!@n @param dnod_rj(nnod_rj)      Input spectr data
!!@n @param dnod_dr(nnod_rj,nd)   Gradient or radial derivative of field
!
      module cal_sph_exp_1st_diff_ele
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_vect_dr_ele_2(kr_in, kr_out, sph_rj,           &
     &          dele_rj, dnod_dr, d1nod_mat_fdm_2e)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: dele_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in)                                    &
     &                    :: d1nod_mat_fdm_2e(sph_rj%nidx_rj(1),0:1)
!
      real(kind = kreal), intent(inout) :: dnod_dr(sph_rj%nnod_rj)
!
      integer(kind = kint) :: inod, i_n1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = kr_in * sph_rj%nidx_rj(2) + 1
      ied = kr_out * sph_rj%nidx_rj(2)
!$omp parallel do private(inod,i_n1,j,k)
      do inod = ist, ied
        i_n1 = inod - sph_rj%nidx_rj(2)
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
!
        dnod_dr(inod) =  d1nod_mat_fdm_2e(k, 0) * dele_rj(i_n1)         &
     &                 + d1nod_mat_fdm_2e(k, 1) * dele_rj(inod)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_vect_dr_ele_2
!
!
      end module cal_sph_exp_1st_diff_ele
