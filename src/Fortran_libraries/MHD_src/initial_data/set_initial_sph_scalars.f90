!>@file   set_initial_sph_scalars.f90
!!@brief  module set_initial_sph_scalars
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine initial_sph_reference_scalar                         &
!!     &         (sph, nri_ref, reftemp_j, n_point, temp_rj)
!!      subroutine initial_sph_ref_temp_dbench(sph, n_point, temp_rj)
!!      subroutine init_sph_sectorial_temp(isig, sph, n_point, temp_rj)
!!      subroutine initital_sph_noize_temp(sph, n_point, temp_rj) 
!!        type(sph_grids), intent(in) :: sph
!!        integer(kind = kint), intent(in) :: nri_ref
!!        real(kind=kreal), intent(in) :: reftemp_j(0:nri_ref-1)
!!        integer(kind = kint), intent(in) :: n_point
!!        real (kind=kreal), intent(inout) :: temp_rj(n_point)
!!@endverbatim
!
!
      module set_initial_sph_scalars
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_reference_scalar_param
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine initial_sph_reference_scalar                           &
     &         (sph, nri_ref, reftemp_j, n_point, temp_rj)
!
      use spherical_indices_wrapper
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: nri_ref
      real(kind=kreal), intent(in) :: reftemp_j(0:nri_ref-1)
      integer(kind = kint), intent(in) :: n_point
!
      real (kind=kreal), intent(inout) :: temp_rj(n_point)
!
      integer(kind = kint) :: inod
      integer :: k, jj
!
!
!$omp parallel workshare
        temp_rj(1:n_point) = zero
!$omp end parallel workshare
!
!   set reference temperature (l = m = 0)
      jj = idx_rj_degree_zero(sph)
      if(jj .gt. 0) then
        do k = 1, num_rj_radial_point(sph)
          inod = local_sph_data_address(sph, k, jj)
          temp_rj(inod) = reftemp_j(k)
        end do
      end if
!
!    Center
      inod = inod_rj_center(sph)
      if(inod .gt. 0) temp_rj(inod) = reftemp_j(0)
!
      end subroutine initial_sph_reference_scalar
!
!-----------------------------------------------------------------------
!
      subroutine initial_sph_ref_temp_dbench(sph, n_point, temp_rj)
!
      use spherical_indices_wrapper
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: n_point
!
      real (kind=kreal), intent(inout) :: temp_rj(n_point)
!
      integer(kind = kint) :: inod
      integer(kind = kint) :: k, jj
      real(kind = kreal) :: r_in, r_out, shell
!
!
      r_in =  r_ICB(sph)
      r_out = r_CMB(sph)
      shell = r_CMB(sph) - r_ICB(sph)
!
!$omp parallel workshare
      temp_rj(1:n_point) = zero
!$omp end parallel workshare
!
!   set reference temperature (l = m = 0)
      jj = idx_rj_degree_zero(sph)
      if(jj .gt. 0) then
        do k = 1, nlayer_ICB(sph)-1
          inod = local_sph_data_address(sph, k, jj)
          temp_rj(inod) = 1.0d0
        end do
!
        do k = nlayer_ICB(sph), nlayer_CMB(sph)
          inod = local_sph_data_address(sph, k, jj)
          temp_rj(inod)                                                 &
     &         = (r_out*r_in / radius_1d_rj_r(sph,k) - r_in) / shell
        end do
!
        do k = nlayer_CMB(sph)+1, num_rj_radial_point(sph)
          inod = local_sph_data_address(sph, k, jj)
          temp_rj(inod) = 0.0d0
        end do
      end if
!
!    Center
      inod = inod_rj_center(sph)
      if(inod .gt. 0) temp_rj(inod) = 1.0d0
!
      end subroutine initial_sph_ref_temp_dbench
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_sectorial_temp(isig, sph, n_point, temp_rj)
!
      use spherical_indices_wrapper
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: isig
      integer(kind = kint), intent(in) :: n_point
!
      real(kind=kreal), intent(inout) :: temp_rj(n_point)
!
      integer(kind = kint) :: m, k, jj
      integer(kind = kint) :: inod
      real(kind = kreal) :: pi, xr, shell, r_in, r_out
!
!
      pi = four * atan(one)
      r_in =  r_ICB(sph)
      r_out = r_CMB(sph)
      shell = r_CMB(sph) - r_ICB(sph)
!
      m = int(mod(isig,100000) / icent)
      jj = find_local_sph_mode_address(sph, m, m)
!
      if (jj .gt. 0) then
!$omp parallel do private(k,inod,xr)
        do k = nlayer_ICB(sph), nlayer_CMB(sph)
          xr = two * radius_1d_rj_r(sph,k) - (r_in + r_out) / shell
          inod = jj + (k-1) * num_rj_horiz_point(sph)
!
          temp_rj(inod) = (one-three*xr**2+three*xr**4-xr**6)           &
     &                            * 0.1d0 * three / (sqrt(two*pi))
        end do
!$omp end parallel do
      end if
!
!    Center
      inod = inod_rj_center(sph)
      if(inod .gt. 0) temp_rj(inod) = temp_rj(inod)
!
      end subroutine init_sph_sectorial_temp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine initital_sph_noize_temp(sph, n_point, temp_rj) 
!
      use spherical_indices_wrapper
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: n_point
!
      real(kind=kreal), intent(inout) :: temp_rj(n_point)
!
      integer(kind = kint) :: k, kst, ked
      integer(kind = kint) :: inod, j
      real(kind = kreal) :: pi, xr, shell, r_in, r_out
!
!
      pi = four * atan(one)
      kst = nlayer_ICB(sph) + 2
      ked = nlayer_CMB(sph) - 2
      r_in =  radius_1d_rj_r(sph,kst)
      r_out = radius_1d_rj_r(sph,ked)
      shell = r_CMB(sph) - r_ICB(sph)
!
!$omp parallel do private(j,k,inod,xr)
      do j = 1, num_rj_horiz_point(sph)
        if(j .eq. idx_rj_degree_zero(sph)) cycle
        do k = kst, ked
          inod = local_sph_data_address(sph, k, j)
!
          xr = two * radius_1d_rj_r(sph,k) - (r_in + r_out) / shell
          temp_rj(inod) = (one - three*xr**2 + three*xr**4 - xr**6)     &
     &                   * 1.0d-4 * six / (sqrt(pi))
        end do
      end do
!$omp end parallel do
!
      end subroutine initital_sph_noize_temp
!
!-----------------------------------------------------------------------
!
      end module set_initial_sph_scalars
