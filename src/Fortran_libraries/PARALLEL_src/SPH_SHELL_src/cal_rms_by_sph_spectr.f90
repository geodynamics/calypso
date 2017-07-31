!>@file   cal_rms_by_sph_spectr.f90
!!@brief  module cal_rms_by_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief  Evaluate mean square data for each spherical harmonics mode
!!
!!@verbatim
!!      subroutine cal_rms_sph_spec_one_field                           &
!!     &         (sph_rj, ipol, ncomp_rj, g_sph_rj, icomp_rj,           &
!!     &          n_point, ntot_phys_rj, d_rj, rms_sph_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!        (1/4\pi) \int (\bf{u}_{l}^{m})^2 sin \theta d\theta d\phi
!!          = r^{-2} [ l(l+1) / (2l+1) 
!!           ( l(l+1)/r^2 (S_{l}^{m})^2 + (dS_{l}^{m}/dr)^2)
!!            + (T_{l}^{m})^2 ) ]
!!@endverbatim
!!
!!@n @param  d_rj         spectrum data
!!@n @param  rms_sph_rj   mean square data
!
      module cal_rms_by_sph_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: cal_rms_each_scalar_sph_spec
      private :: cal_rms_each_vector_sph_spec
      private :: set_sph_energies_by_rms
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_rms_sph_spec_one_field                             &
     &         (sph_rj, ipol, ncomp_rj, g_sph_rj, icomp_rj,             &
     &          n_point, ntot_phys_rj, d_rj, rms_sph_rj)
!
      use t_spheric_rj_data
      use t_phys_address
      use m_phys_constants
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: ncomp_rj, icomp_rj
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout)                                 &
     &    :: rms_sph_rj(0:sph_rj%nidx_rj(1),sph_rj%nidx_rj(2),ncomp_rj)
!
!
      if     (ncomp_rj .eq. n_scalar) then
        call cal_rms_each_scalar_sph_spec                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,           &
     &      sph_rj%radius_1d_rj_r, g_sph_rj, n_point,                   &
     &      d_rj(1,icomp_rj), rms_sph_rj(0,1,1))
      else if(ncomp_rj .eq. n_vector) then
        call cal_rms_each_vector_sph_spec                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,           &
     &      sph_rj%a_r_1d_rj_r, g_sph_rj, n_point, d_rj(1,icomp_rj),    &
     &      rms_sph_rj(0,1,1))
!
        if (   icomp_rj .eq. ipol%i_velo                                &
     &      .or. icomp_rj .eq. ipol%i_magne                             &
     &      .or. icomp_rj .eq. ipol%i_filter_velo                       &
     &      .or. icomp_rj .eq. ipol%i_filter_magne                      &
     &      .or. icomp_rj .eq. ipol%i_wide_fil_velo                     &
     &      .or. icomp_rj .eq. ipol%i_wide_fil_magne) then
          call set_sph_energies_by_rms                                  &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), rms_sph_rj(0,1,1))
        end if
      end if
!
      end subroutine cal_rms_sph_spec_one_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rms_each_scalar_sph_spec(nri, jmax,                &
     &          idx_rj_degree_zero, inod_rj_center, radius_1d_rj_r,     &
     &          g_sph_rj, n_point, d_rj, rms_sph_rj)
!
      integer(kind = kint), intent(in) :: n_point, nri, jmax
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: d_rj(n_point)
      real(kind = kreal), intent(inout) :: rms_sph_rj(0:nri,jmax)
!
      integer(kind = kint) :: k, j, inod
!
!
!$omp parallel do private(k,j,inod)
      do j = 1, jmax
        rms_sph_rj(0,j) = 0.0d0
        do k = 1, nri
          inod = j + (k-1) * jmax
          rms_sph_rj(k,j) = d_rj(inod)*d_rj(inod)*g_sph_rj(j,11)        &
     &         * radius_1d_rj_r(k) * radius_1d_rj_r(k)
        end do
      end do
!$omp end parallel do
!
      if(inod_rj_center .eq. 0) return
      j = idx_rj_degree_zero
      rms_sph_rj(0,j) = d_rj(inod_rj_center)**2
!
      end subroutine cal_rms_each_scalar_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine cal_rms_each_vector_sph_spec(nri, jmax,                &
     &          idx_rj_degree_zero, inod_rj_center, a_r_1d_rj_r,        &
     &          g_sph_rj, n_point, d_rj, rms_sph_rj)
!
      integer(kind = kint), intent(in) :: n_point, nri, jmax
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: d_rj(n_point,3)
      real(kind = kreal), intent(inout) :: rms_sph_rj(0:nri,jmax,3)
!
      integer(kind = kint) :: k, j, inod
!
!
!$omp parallel do private(k,j,inod)
      do j = 1, jmax
        rms_sph_rj(0,j,1) = 0.0d0
        rms_sph_rj(0,j,2) = 0.0d0
        rms_sph_rj(0,j,3) = 0.0d0
        do k = 1, nri
          inod = j + (k-1) * jmax
          rms_sph_rj(k,j,1) = g_sph_rj(j,12)                            &
     &                        * ( g_sph_rj(j,3)                         &
     &                          * a_r_1d_rj_r(k)*a_r_1d_rj_r(k)         &
     &                          * d_rj(inod,1)*d_rj(inod,1)             &
     &                         +  d_rj(inod,2)*d_rj(inod,2))
          rms_sph_rj(k,j,2) = g_sph_rj(j,12)                            &
     &                          * d_rj(inod,3)*d_rj(inod,3)
          rms_sph_rj(k,j,3) =  rms_sph_rj(k,j,1)                        &
     &                         + rms_sph_rj(k,j,2)
        end do
      end do
!$omp end parallel do
!
      if(idx_rj_degree_zero .eq. izero) return
!
      j = idx_rj_degree_zero
      do k = 1, nri
        inod = idx_rj_degree_zero + (k-1) * jmax
        rms_sph_rj(k,j,1) = (half * d_rj(inod,1))**2                   &
     &                            * a_r_1d_rj_r(k)*a_r_1d_rj_r(k)
        rms_sph_rj(k,j,2) = zero
        rms_sph_rj(k,j,3) = rms_sph_rj(k,j,1)
      end do
!
      if(inod_rj_center .eq. 0) return
      j = idx_rj_degree_zero
      rms_sph_rj(0,j,1) = (half*d_rj(inod_rj_center,1))**2
      rms_sph_rj(0,j,2) = zero
      rms_sph_rj(0,j,3) = rms_sph_rj(0,j,1)
!
      end subroutine cal_rms_each_vector_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_energies_by_rms(nri, jmax, rms_sph_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(inout) :: rms_sph_rj(0:nri,jmax,3)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private(k, j)
      do j = 1, jmax
        do k = 0, nri
          rms_sph_rj(k,j,1) = half * rms_sph_rj(k,j,1)
          rms_sph_rj(k,j,2) = half * rms_sph_rj(k,j,2)
          rms_sph_rj(k,j,3) = half * rms_sph_rj(k,j,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_sph_energies_by_rms
!
! -----------------------------------------------------------------------
!
      end module cal_rms_by_sph_spectr
