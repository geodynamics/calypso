!>@file   initial_magne_dbench_qvc.f90
!!@brief  module initial_magne_dbench_qvc
!!
!!@author H. Matsui
!!@date Programmed in March, 2024
!
!> @brief Set initial magnetic field for 
!!         pseudo vacuume boundary banchmark
!!
!!@verbatim
!!      subroutine initial_b_dynamobench_qcv                            &
!!     &         (sph_rj, ipol, r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,   &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!@endverbatim
!
      module initial_magne_dbench_qvc
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine initial_b_dynamobench_qcv                              &
     &         (sph_rj, ipol, r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,     &
     &          n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: is, it, k, js, jt
!
!
      js = find_local_sph_address(sph_rj, 1,0)
      jt = find_local_sph_address(sph_rj, 2,0)
!
      pi = four * atan(one)
!
!$omp parallel do
      do is = 1, n_point
        d_rj(is,ipol%base%i_magne  ) = zero
        d_rj(is,ipol%base%i_magne+1) = zero
        d_rj(is,ipol%base%i_magne+2) = zero
        d_rj(is,ipol%base%i_current  ) = zero
        d_rj(is,ipol%base%i_current+1) = zero
        d_rj(is,ipol%base%i_current+2) = zero
      end do
!$omp end parallel do
!
!        Poloidal magnetic field
      if (js .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          is = js + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
!
          d_rj(is,ipol%base%i_magne  )                                  &
     &      =  (five / eight) * (dnine*half * rr**4                     &
     &        - (three*r_ICB + three*r_CMB + four) * two * rr**3        &
     &        + (four*r_ICB + four*r_CMB + three*r_ICB*r_CMB)           &
     &         * three * rr**2                                          &
     &        - four*six * r_ICB*r_CMB * rr)
          d_rj(is,ipol%base%i_magne+1)                                  &
     &      =  (five / eight) * (two*dnine * rr**3                      &
     &        - (three*r_ICB + three*r_CMB + four) * six * rr**2        &
     &        + (four*r_ICB + four*r_CMB + three*r_ICB*r_CMB)*six * rr  &
     &        - four*six * r_ICB*r_CMB)
          d_rj(is,ipol%base%i_current+2)                                &
     &      =  (five / eight) * (-four*dnine * rr**2                    &
     &        + (three*r_ICB + three*r_CMB + four) * eight * rr         &
     &        - eight*six * r_ICB*r_CMB / rr)
        end do
      end if
!
!        Toroidal magnetic field
     if (jt .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          it = jt + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
          d_rj(it,ipol%base%i_magne+2)                                  &
     &             = (ten/eight) * rr * sin(pi*(rr-r_ICB))
          d_rj(it,ipol%base%i_current  ) = d_rj(it,ipol%base%i_magne+2)
          d_rj(it,ipol%base%i_current+1)                                &
     &             = (ten / eight) * (sin(pi*(rr-r_ICB))                &
     &              + pi * rr * cos(pi*(rr-r_ICB)) )
        end do
      end if
!
      end subroutine initial_b_dynamobench_qcv
!
!-----------------------------------------------------------------------
!
      end module initial_magne_dbench_qvc
