!>@file   set_initial_sph_scalars.f90
!!@brief  module set_initial_sph_scalars
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine set_ini_reference_temp_sph                           &
!!     &         (is_temp, reftemp_rj, sph_rj, ref_param,               &
!!     &          nlayer_ICB, nlayer_CMB, n_point, ntot_phys_rj, d_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(reference_scalar_param), intent(in) :: ref_param
!!      subroutine set_ini_ref_temp_benchmark                           &
!!     &         (sph_rj, nlayer_ICB, nlayer_CMB, temp_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!      subroutine set_initial_temp_sph(isig, sph_rj,                   &
!!     &          r_ICB, r_CMB, nlayer_ICB, nlayer_CMB, temp_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!      subroutine set_noize_scalar_sph                                 &
!!     &         (sph_rj, r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,temp_rj) 
!!        type(sph_rj_grid), intent(in) :: sph_rj
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
      subroutine set_ini_reference_temp_sph(sph_rj, ref_param,          &
     &          reftemp_j, nlayer_ICB, nlayer_CMB, temp_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(reference_scalar_param), intent(in) :: ref_param
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind=kreal), intent(in)                                      &
     &                 :: reftemp_j(0:sph_rj%nidx_rj(1))
!
      real (kind=kreal), intent(inout) :: temp_rj(sph_rj%nnod_rj)
!
      integer(kind = kint) :: inod
      integer :: k, jj
!
!
!$omp parallel do
      do inod = 1, sph_rj%nnod_rj
        temp_rj(inod) = zero
      end do
!$omp end parallel do
!
!   set reference temperature (l = m = 0)
      if (sph_rj%idx_rj_degree_zero .gt. 0) then
        if (ref_param%iflag_reference .eq. id_sphere_ref_temp) then
          do k = 1, sph_rj%nidx_rj(1)
            inod = sph_rj%idx_rj_degree_zero + (k-1) * sph_rj%nidx_rj(2)
            temp_rj(inod) = reftemp_j(k)
          end do
        else
          do k = 1, nlayer_ICB-1
            inod = local_sph_node_address(sph_rj, k, jj)
            temp_rj(inod) = 1.0d0
          end do
          do k = nlayer_ICB, nlayer_CMB
            inod = sph_rj%idx_rj_degree_zero                            &
     &            + (k-1) * sph_rj%nidx_rj(2)
            temp_rj(inod)                                               &
     &           = (sph_rj%ar_1d_rj(k,1) * 20.d0/13.0d0 - 1.0d0 )       &
     &            * 7.0d0 / 13.0d0
          end do
        end if
      end if
!
!    Center
      if(sph_rj%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj, 0, 0)
        inod = local_sph_node_address(sph_rj, 1, jj)
        temp_rj(sph_rj%inod_rj_center) = reftemp_j(0)
      end if
!
      end subroutine set_ini_reference_temp_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_ini_ref_temp_benchmark                             &
     &         (sph_rj, nlayer_ICB, nlayer_CMB, temp_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!
      real (kind=kreal), intent(inout) :: temp_rj(sph_rj%nnod_rj)
!
      integer(kind = kint) :: inod
      integer :: k, jj
!
!
!$omp parallel do
      do inod = 1, sph_rj%nnod_rj
        temp_rj(inod) = zero
      end do
!$omp end parallel do
!
!   set reference temperature (l = m = 0)
      if(sph_rj%idx_rj_degree_zero .gt. 0) then
        do k = 1, nlayer_ICB-1
          inod = local_sph_node_address(sph_rj, k, jj)
          temp_rj(inod) = 1.0d0
        end do
        do k = nlayer_ICB, nlayer_CMB
          inod = sph_rj%idx_rj_degree_zero                              &
     &          + (k-1) * sph_rj%nidx_rj(2)
          temp_rj(inod)                                                 &
     &         = (sph_rj%ar_1d_rj(k,1) * 20.d0/13.0d0 - 1.0d0 )         &
     &          * 7.0d0 / 13.0d0
        end do
      end if
!
!    Center
      if(sph_rj%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj, 0, 0)
        inod = local_sph_node_address(sph_rj, 1, jj)
        temp_rj(sph_rj%inod_rj_center) = 1.0d0
      end if
!
      end subroutine set_ini_ref_temp_benchmark
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temp_sph(isig, sph_rj,                     &
     &          r_ICB, r_CMB, nlayer_ICB, nlayer_CMB, temp_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      integer ( kind = kint), intent(in) :: isig
      real(kind=kreal), intent(inout) :: temp_rj(sph_rj%nnod_rj)
!
      integer :: m, k, jj
      integer(kind = kint) :: inod
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
      m = int( mod(isig,ikilo) / icent )
      jj = find_local_sph_address(sph_rj, m, m)
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          xr = two * sph_rj%radius_1d_rj_r(k)                           &
     &        - one * (r_CMB+r_ICB) / shell
          inod = jj + (k-1) * sph_rj%nidx_rj(2)
!
          temp_rj(inod) = (one-three*xr**2+three*xr**4-xr**6)           &
     &                            * 0.1d0 * three / (sqrt(two*pi))
        end do
      end if
!
!    Center
      if(sph_rj%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj, 0, 0)
        inod = local_sph_node_address(sph_rj, 1, jj)
        temp_rj(sph_rj%inod_rj_center) = temp_rj(inod)
      end if
!
      end subroutine set_initial_temp_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_noize_scalar_sph                                   &
     &         (sph_rj, r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,temp_rj) 
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
!
      real(kind=kreal), intent(inout) :: temp_rj(sph_rj%nnod_rj)
!
      integer :: k, jj
      integer( kind = kint) :: inod, j
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
      do j = 1, sph_rj%nidx_rj(2)
        if(j .eq. sph_rj%idx_rj_degree_zero) cycle
        do k = nlayer_ICB+2, nlayer_CMB-2
          inod = j + (k-1) * sph_rj%nidx_rj(2)
!
          xr = two * sph_rj%radius_1d_rj_r(k)                           &
     &       - (sph_rj%radius_1d_rj_r(nlayer_ICB+2)                     &
     &         + sph_rj%radius_1d_rj_r(nlayer_CMB-2) ) / shell
          temp_rj(inod) = (one-three*xr**2+three*xr**4-xr**6)           &
     &                       * 1.0d-4 * six / (sqrt(pi))
        end do
      end do
!
      end subroutine set_noize_scalar_sph
!
!-----------------------------------------------------------------------
!
      end module set_initial_sph_scalars
