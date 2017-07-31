!>@file   correlation_by_sph_spectr.f90
!!@brief  module correlation_by_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief  Evaluate mean square data for each spherical harmonics mode
!!
!!@verbatim
!!      subroutine correlate_sph_spec_one_field                         &
!!     &         (sph_rj, ncomp_x, g_sph_rj,                            &
!!     &          icomp1_rj, n1_point, ntot_phys1_rj, d1_rj,            &
!!     &          icomp2_rj, n2_point, ntot_phys2_rj, d2_rj, x_sph_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!        (1/4\pi) \int (\bf{u}_{l}^{m})^2 sin \theta d\theta d\phi
!!          = r^{-2} [ l(l+1) / (2l+1) 
!!           ( l(l+1)/r^2 (S_{l}^{m})^2 + (dS_{l}^{m}/dr)^2)
!!            + (T_{l}^{m})^2 ) ]
!!@endverbatim
!!
!!@n @param  d_rj         spectrum data
!!@n @param  x_sph_rj   cross spectr data
!
      module correlation_by_sph_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: correlate_each_scalar_sph_spec
      private :: correlate_each_vector_sph_spec
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine correlate_sph_spec_one_field                           &
     &         (sph_rj, ncomp_x, g_sph_rj,                              &
     &          icomp1_rj, n1_point, ntot_phys1_rj, d1_rj,              &
     &          icomp2_rj, n2_point, ntot_phys2_rj, d2_rj, x_sph_rj)
!
      use t_spheric_rj_data
      use t_phys_address
      use m_phys_constants
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: n1_point, n2_point, ncomp_x
      integer(kind = kint), intent(in) :: icomp1_rj, icomp2_rj
      integer(kind = kint), intent(in) :: ntot_phys1_rj, ntot_phys2_rj
      real(kind = kreal), intent(in) :: d1_rj(n1_point,ntot_phys1_rj)
      real(kind = kreal), intent(in) :: d2_rj(n2_point,ntot_phys2_rj)
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout)                                 &
     &    :: x_sph_rj(0:sph_rj%nidx_rj(1),sph_rj%nidx_rj(2),ncomp_x)
!
!
      if     (ncomp_x .eq. n_scalar) then
        call correlate_each_scalar_sph_spec                             &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,           &
     &      sph_rj%radius_1d_rj_r, g_sph_rj, sph_rj%nnod_rj,            &
     &      d1_rj(1,icomp1_rj), d2_rj(1,icomp2_rj), x_sph_rj(0,1,1))
      else if(ncomp_x .eq. n_vector) then
        call correlate_each_vector_sph_spec                             &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,           &
     &      sph_rj%a_r_1d_rj_r, g_sph_rj, sph_rj%nnod_rj,               &
     &      d1_rj(1,icomp1_rj), d2_rj(1,icomp2_rj), x_sph_rj(0,1,1))
      end if
!
      end subroutine correlate_sph_spec_one_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine correlate_each_scalar_sph_spec(nri, jmax,              &
     &          idx_rj_degree_zero, inod_rj_center, radius_1d_rj_r,     &
     &          g_sph_rj, n_point, d1_rj, d2_rj, x_sph_rj)
!
      integer(kind = kint), intent(in) :: n_point, nri, jmax
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: d1_rj(n_point)
      real(kind = kreal), intent(in) :: d2_rj(n_point)
      real(kind = kreal), intent(inout) :: x_sph_rj(0:nri,jmax)
!
      integer(kind = kint) :: k, j, inod
!
!
!$omp parallel do private(k,j,inod)
      do j = 1, jmax
        x_sph_rj(0,j) = 0.0d0
        do k = 1, nri
          inod = j + (k-1) * jmax
          x_sph_rj(k,j) = d1_rj(inod)*d2_rj(inod)*g_sph_rj(j,11)      &
     &         * radius_1d_rj_r(k) * radius_1d_rj_r(k)
        end do
      end do
!$omp end parallel do
!
      if(inod_rj_center .eq. 0) return
      j = idx_rj_degree_zero
      inod = inod_rj_center
      x_sph_rj(0,j) = d1_rj(inod)*d2_rj(inod)
!
      end subroutine correlate_each_scalar_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine correlate_each_vector_sph_spec(nri, jmax,              &
     &          idx_rj_degree_zero, inod_rj_center, a_r_1d_rj_r,        &
     &          g_sph_rj, n_point, d1_rj, d2_rj, x_sph_rj)
!
      integer(kind = kint), intent(in) :: n_point, nri, jmax
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: d1_rj(n_point,3)
      real(kind = kreal), intent(in) :: d2_rj(n_point,3)
      real(kind = kreal), intent(inout) :: x_sph_rj(0:nri,jmax,3)
!
      integer(kind = kint) :: k, j, inod
!
!
!$omp parallel do private(k,j,inod)
      do j = 1, jmax
        x_sph_rj(0,j,1) = 0.0d0
        x_sph_rj(0,j,2) = 0.0d0
        x_sph_rj(0,j,3) = 0.0d0
        do k = 1, nri
          inod = j + (k-1) * jmax
          x_sph_rj(k,j,1) = g_sph_rj(j,12)                              &
     &                        * ( g_sph_rj(j,3)                         &
     &                          * a_r_1d_rj_r(k)*a_r_1d_rj_r(k)         &
     &                          * d1_rj(inod,1)*d2_rj(inod,1)           &
     &                         +  d1_rj(inod,2)*d2_rj(inod,2))
          x_sph_rj(k,j,2) = g_sph_rj(j,12)                              &
     &                          * d1_rj(inod,3)*d2_rj(inod,3)
          x_sph_rj(k,j,3) =  x_sph_rj(k,j,1) + x_sph_rj(k,j,2)
        end do
      end do
!$omp end parallel do
!
      if(idx_rj_degree_zero .eq. izero) return
!
      j = idx_rj_degree_zero
      do k = 1, nri
        inod = idx_rj_degree_zero + (k-1) * jmax
        x_sph_rj(k,j,1) = (half * d1_rj(inod,1) * d2_rj(inod,1))        &
     &                            * a_r_1d_rj_r(k)*a_r_1d_rj_r(k)
        x_sph_rj(k,j,2) = zero
        x_sph_rj(k,j,3) = x_sph_rj(k,j,1)
      end do
!
      if(inod_rj_center .eq. 0) return
      j = idx_rj_degree_zero
      inod = inod_rj_center
      x_sph_rj(0,j,1) = (half*d1_rj(inod,1)*d2_rj(inod,1))
      x_sph_rj(0,j,2) = zero
      x_sph_rj(0,j,3) = x_sph_rj(0,j,1)
!
      end subroutine correlate_each_vector_sph_spec
!
! -----------------------------------------------------------------------
!
      end module correlation_by_sph_spectr
