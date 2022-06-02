!>@file   cal_sph_exp_1st_diff.f90
!!@brief  module cal_sph_exp_1st_diff
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate first radial derivative for spectr data
!!
!!@verbatim
!!      subroutine cal_sph_nod_gradient_2(kr_in, kr_out,                &
!!     &          is_fld, is_grad, nidx_rj, radius_1d_rj_r, g_sph_rj,   &
!!     &          d1nod_mat_fdm_2, n_point, ntot_phys_rj, d_rj)
!!      subroutine normalize_sph_average_grad                           &
!!     &         (is_fld, idx_rj_degree_zero, nidx_rj,                  &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_vect_dr_2(kr_in, kr_out, is_fld,         &
!!     &          nidx_rj, d1nod_mat_fdm_2, n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine cal_sph_nod_gradient_1d(kr_in, kr_out, nri,          &
!!     &                                   d1nod_mat_fdm_2, d_r, grad_r)
!!@endverbatim
!!
!!@n @param kr_in    radial ID for inner boundary
!!@n @param kr_out   radial ID for outer boundary
!!@n @param dnod_rj(n_point)      Input spectr data
!!@n @param dnod_dr(n_point,nd)   Gradient of field
!!@n                 dnod_dr(n_point,1) = r^2 l(l+1) d phi / dr
!!@n                 dnod_dr(n_point,2) = phi
!!@n                 dnod_dr(n_point,3) = 0
!
      module cal_sph_exp_1st_diff
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_gradient_2(kr_in, kr_out,                  &
     &          is_fld, is_grad, nidx_rj, radius_1d_rj_r, g_sph_rj,     &
     &          d1nod_mat_fdm_2, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_grad
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),13)
      real(kind = kreal), intent(in)                                    &
     &                   :: d1nod_mat_fdm_2(nidx_rj(1),-1:1)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
      real(kind = kreal) :: d1sdr
!
!
      ist  = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel
!$omp do private(inod)
      do inod = ist, ied
        d_rj(inod,is_grad+1) = d_rj(inod,is_fld)
        d_rj(inod,is_grad+2) = zero
      end do
!$omp end do
!
!$omp do private(inod,i_p1,i_n1,j,k,d1sdr)
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d1sdr =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld)              &
     &         + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld)              &
     &         + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld)
!
        d_rj(inod,is_grad  ) = d1sdr * g_sph_rj(j,13)                   &
     &                        * radius_1d_rj_r(k)**2

      end do
!$omp end do
!$omp end parallel
!
      end subroutine cal_sph_nod_gradient_2
!
! -----------------------------------------------------------------------
!
      subroutine normalize_sph_average_grad                             &
     &         (is_fld, idx_rj_degree_zero, nidx_rj,                    &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, k
!
!
      if(idx_rj_degree_zero .eq. 0) return
!
!$omp parallel do private(inod,k)
      do k = 1, nidx_rj(1)
        inod = (k-1) * nidx_rj(2) + idx_rj_degree_zero
        d_rj(inod,is_fld  ) = two * d_rj(inod,is_fld)
        d_rj(inod,is_fld+1) = zero
        d_rj(inod,is_fld+2) = zero
      end do
!$omp end parallel do
!
      end subroutine normalize_sph_average_grad
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_vect_dr_2(kr_in, kr_out, is_fld,           &
     &          nidx_rj, d1nod_mat_fdm_2, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: d1nod_mat_fdm_2(nidx_rj(1),-1:1)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
      integer(kind = kint) :: is_dr
!
!
      is_dr = is_fld + 1
      ist  = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k)
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod-j) / nidx_rj(2)
!
        d_rj(inod,is_dr) =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld)   &
     &                    + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld)   &
     &                    + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_vect_dr_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_gradient_1d(kr_in, kr_out, nri,            &
     &                                   d1nod_mat_fdm_2, d_r, grad_r)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out, nri
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
!
      real (kind=kreal), intent(in) :: d_r(0:nri)
      real(kind = kreal), intent(inout) :: grad_r(0:nri)
!
      integer(kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = kr_in+1, kr_out-1
        grad_r(k) =  d1nod_mat_fdm_2(k,-1) * d_r(k-1)                   &
     &             + d1nod_mat_fdm_2(k, 0) * d_r(k  )                   &
     &             + d1nod_mat_fdm_2(k, 1) * d_r(k+1)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_gradient_1d
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_1st_diff
