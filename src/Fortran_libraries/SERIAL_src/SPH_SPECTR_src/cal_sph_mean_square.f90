!>@file   cal_sph_mean_square.f90
!!@brief  module cal_sph_mean_square
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine one_mode_scalar_mean_square(j, nri, jmax,            &
!!     &          inod_rj_center, radius_1d_rj_r,                       &
!!     &          g_rj_11, n_point, d_rj, sq_r)
!!      subroutine each_scalar_sph_spec(nri, jmax,                      &
!!     &          idx_rj_degree_zero, inod_rj_center, radius_1d_rj_r,   &
!!     &          g_sph_rj, n_point, d_rj, sq_rj)
!!        real(kind = kreal), intent(inout) :: sq_rj(0:nri,jmax)
!!
!!      subroutine one_mode_vector_mean_square(j, nri, jmax,            &
!!     &          a_r_1d_rj_r, g_rj_3, g_rj_12, n_point, d_rj, sq_r)
!!      subroutine degree_zero_vector_mean_square(nri, jmax,            &
!!     &          idx_rj_degree_zero, inod_rj_center, a_r_1d_rj_r,      &
!!     &          n_point, d_rj, sq_r)
!!      subroutine each_vector_sph_spec(nri, jmax,                      &
!!     &          idx_rj_degree_zero, inod_rj_center, a_r_1d_rj_r,      &
!!     &          g_sph_rj, n_point, d_rj, sq_rj)
!!        real(kind = kreal), intent(inout) :: sq_rj(0:nri,jmax,3)
!!
!!      subroutine one_mode_mean_sq_to_energy(nri, ene_r)
!!      subroutine one_field_mean_sq_to_energy(nri, jmax, ene_rj)
!!@endverbatim
!!
      module cal_sph_mean_square
!
      use m_precision
      use m_constants
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine one_mode_scalar_mean_square(j, nri, jmax,              &
     &          inod_rj_center, radius_1d_rj_r,                         &
     &          g_rj_11, n_point, d_rj, sq_r)
!
      integer(kind = kint), intent(in) :: j, n_point, nri, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: g_rj_11
      real(kind = kreal), intent(in) :: d_rj(n_point)
!
      real(kind = kreal), intent(inout) :: sq_r(0:nri)
!
      integer(kind = kint) :: k, inod
!
!
!$omp parallel do private(k,inod)
        do k = 1, nri
          inod = j + (k-1) * jmax
          sq_r(k) = g_rj_11 * ( d_rj(inod) * radius_1d_rj_r(k))**2
        end do
!$omp end parallel do
!
      if(inod_rj_center .eq. 0) then
        sq_r(0) = 0.0d0
      else
        sq_r(0) = d_rj(inod_rj_center)**2
      end if
!
      end subroutine one_mode_scalar_mean_square
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine each_scalar_sph_spec(nri, jmax,                        &
     &          idx_rj_degree_zero, inod_rj_center, radius_1d_rj_r,     &
     &          g_sph_rj, n_point, d_rj, sq_rj)
!
      integer(kind = kint), intent(in) :: n_point, nri, jmax
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: d_rj(n_point)
!
      real(kind = kreal), intent(inout) :: sq_rj(0:nri,jmax)
!
      integer(kind = kint) :: k, j, inod
!
!
!$omp parallel do private(k,j,inod)
      do j = 1, jmax
        sq_rj(0,j) = 0.0d0
        do k = 1, nri
          inod = j + (k-1) * jmax
          sq_rj(k,j) = d_rj(inod)*d_rj(inod)*g_sph_rj(j,11)             &
     &         * radius_1d_rj_r(k) * radius_1d_rj_r(k)
        end do
      end do
!$omp end parallel do
!
      if(inod_rj_center .eq. 0) return
      j = idx_rj_degree_zero
      sq_rj(0,j) = d_rj(inod_rj_center)**2
!
      end subroutine each_scalar_sph_spec
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine one_mode_vector_mean_square(j, nri, jmax,              &
     &          a_r_1d_rj_r, g_rj_3, g_rj_12, n_point, d_rj, sq_r)
!
      integer(kind = kint), intent(in) :: j, n_point, nri, jmax
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: g_rj_3, g_rj_12
      real(kind = kreal), intent(in) :: d_rj(n_point,3)
!
      real(kind = kreal), intent(inout) :: sq_r(0:nri,3)
!
      integer(kind = kint) :: k, inod
!
!
!$omp parallel do private(k,inod)
      do k = 1, nri
        inod = j + (k-1) * jmax
        sq_r(k,1) = g_rj_12 * ( g_rj_3 * a_r_1d_rj_r(k)**2              &
     &                        * d_rj(inod,1)**2 + d_rj(inod,2)**2)
        sq_r(k,2) = g_rj_12 * d_rj(inod,3)**2
        sq_r(k,3) =  sq_r(k,1) + sq_r(k,2)
      end do
!$omp end parallel do
!
      sq_r(0,1) = 0.0d0
      sq_r(0,2) = 0.0d0
      sq_r(0,3) = 0.0d0
!
      end subroutine one_mode_vector_mean_square
!
! -----------------------------------------------------------------------
!
      subroutine degree_zero_vector_mean_square(nri, jmax,              &
     &          idx_rj_degree_zero, inod_rj_center, a_r_1d_rj_r,        &
     &          n_point, d_rj, sq_r)
!
      integer(kind = kint), intent(in) :: n_point, nri, jmax
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: d_rj(n_point,3)
!
      real(kind = kreal), intent(inout) :: sq_r(0:nri,3)
!
      integer(kind = kint) :: k, inod
!
!
!$omp parallel do private(k,inod)
      do k = 1, nri
        inod = idx_rj_degree_zero + (k-1) * jmax
        sq_r(k,1) = (half * d_rj(inod,1) * a_r_1d_rj_r(k))**2
        sq_r(k,2) = zero
        sq_r(k,3) = sq_r(k,1)
      end do
!$omp end parallel do
!
      if(inod_rj_center .eq. 0) then
        sq_r(0,1) = 0.0d0
        sq_r(0,2) = 0.0d0
        sq_r(0,3) = 0.0d0
      else
        sq_r(0,1) = (half*d_rj(inod_rj_center,1))**2
        sq_r(0,2) = zero
        sq_r(0,3) = sq_r(0,1)
      end if
!
      end subroutine degree_zero_vector_mean_square
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine each_vector_sph_spec(nri, jmax,                        &
     &          idx_rj_degree_zero, inod_rj_center, a_r_1d_rj_r,        &
     &          g_sph_rj, n_point, d_rj, sq_rj)
!
      integer(kind = kint), intent(in) :: n_point, nri, jmax
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: d_rj(n_point,3)
!
      real(kind = kreal), intent(inout) :: sq_rj(0:nri,jmax,3)
!
      integer(kind = kint) :: k, j, inod
!
!
!$omp parallel do private(k,j,inod)
      do j = 1, jmax
        sq_rj(0,j,1) = 0.0d0
        sq_rj(0,j,2) = 0.0d0
        sq_rj(0,j,3) = 0.0d0
        do k = 1, nri
          inod = j + (k-1) * jmax
          sq_rj(k,j,1) = g_sph_rj(j,12) * ( g_sph_rj(j,3)               &
     &      * (a_r_1d_rj_r(k) * d_rj(inod,1))**2 + d_rj(inod,2)**2)
          sq_rj(k,j,2) = g_sph_rj(j,12) * d_rj(inod,3)**2
          sq_rj(k,j,3) =  sq_rj(k,j,1) + sq_rj(k,j,2)
        end do
      end do
!$omp end parallel do
!
      if(idx_rj_degree_zero .eq. izero) return
!
      j = idx_rj_degree_zero
!$omp parallel do private(k,inod)
      do k = 1, nri
        inod = idx_rj_degree_zero + (k-1) * jmax
        sq_rj(k,j,1) = (half * d_rj(inod,1)* a_r_1d_rj_r(k))**2
        sq_rj(k,j,2) = zero
        sq_rj(k,j,3) = sq_rj(k,j,1)
      end do
!$omp end parallel do
!
      if(inod_rj_center .eq. 0) return
      j = idx_rj_degree_zero
      sq_rj(0,j,1) = (half*d_rj(inod_rj_center,1))**2
      sq_rj(0,j,2) = zero
      sq_rj(0,j,3) = sq_rj(0,j,1)
!
      end subroutine each_vector_sph_spec
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine one_mode_mean_sq_to_energy(nri, ene_r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(inout) :: ene_r(0:nri,3)
!
!
!$omp parallel workshare
      ene_r(0:nri,1) = half * ene_r(0:nri,1)
      ene_r(0:nri,2) = half * ene_r(0:nri,2)
      ene_r(0:nri,3) = half * ene_r(0:nri,3)
!$omp end parallel workshare
!
      end subroutine one_mode_mean_sq_to_energy
!
! -----------------------------------------------------------------------
!
      subroutine one_field_mean_sq_to_energy(nri, jmax, ene_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(inout) :: ene_rj(0:nri,jmax,3)
!
      integer(kind = kint) :: j
!
!
!$omp parallel private(j)
      do j = 1, jmax
!$omp workshare
        ene_rj(0:nri,j,1) = half * ene_rj(0:nri,j,1)
        ene_rj(0:nri,j,2) = half * ene_rj(0:nri,j,2)
        ene_rj(0:nri,j,3) = half * ene_rj(0:nri,j,3)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine one_field_mean_sq_to_energy
!
! -----------------------------------------------------------------------
!
      end module cal_sph_mean_square
