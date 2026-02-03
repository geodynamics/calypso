!>@file   initial_magne_dynamobench.f90
!!@brief  module initial_magne_dynamobench
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial magnetic field for 
!!        pseudo vacuume boundary banchmark
!!
!!@verbatim
!!      subroutine initial_b_dynamobench_1                              &
!!     &         (sph_rj, ipol, r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,   &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!      subroutine initial_b_dynamobench_2(sph_rj, ipol,                &
!!     &          nlayer_CMB, r_CMB, n_point, ntot_phys_rj, d_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!
!!      subroutine initial_magne_sph_dbench_case1                       &
!!     &         (sph_rj, nlayer_ICB, nlayer_CMB, r_ICB, r_CMB,         &
!!     &          is_magne, is_current, n_point, ntot_phys_rj, d_rj)
!!      subroutine initial_magne_sph_dbench_case2                       &
!!     &         (sph_rj, nlayer_CMB, r_CMB, is_magne, is_current,      &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine initial_magne_sph_dbench_qcv                         &
!!     &         (sph_rj, nlayer_ICB, nlayer_CMB, r_ICB, r_CMB,         &
!!     &          is_magne, is_current, n_point, ntot_phys_rj, d_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        integer(kind = kint), intent(in) :: is_magne, is_current
!!        integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!!        real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!!
!!      subroutine reduce_initial_magne_sph(reduce_ratio,               &
!!     &          is_magne, is_current, nnod_rj, ntot_phys_rj, d_rj)
!!        real(kind = kreal), intent(in) :: reduce_ratio
!!        integer(kind = kint), intent(in) :: is_magne, is_current
!!        integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
!!        real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!!@endverbatim
!
      module initial_magne_dynamobench
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine initial_magne_sph_dbench_case1                         &
     &         (sph_rj, nlayer_ICB, nlayer_CMB, r_ICB, r_CMB,           &
     &          is_magne, is_current, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      integer(kind = kint), intent(in) :: is_magne, is_current
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: inod, k, js, jt
!
!
      pi = four * atan(one)
      js = find_local_sph_address(sph_rj, 1,0)
      jt = find_local_sph_address(sph_rj, 2,0)
!
!         Poloidal magnetic field
      if (js .gt. 0) then
!$omp parallel do private(k,inod,rr)
        do k = nlayer_ICB, nlayer_CMB
          inod = js + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
!
          d_rj(inod,is_magne  ) = (five / eight)                        &
     &        * (-three * rr**3 + four * r_CMB * rr**2 - r_ICB**4/rr)
          d_rj(inod,is_magne+1) = (five / eight)                        &
     &        * (-dnine * rr**2 + eight * r_CMB * rr + r_ICB**4/rr**2)
          d_rj(inod,is_magne+2  ) = zero
          d_rj(inod,is_current  ) = zero
          d_rj(inod,is_current+1) = zero
          d_rj(inod,is_current+2) = (five*three / two) * rr
        end do
!$omp end parallel do
      end if
!
!         Toroidal magnetic field
      if (jt .gt. 0) then
!$omp parallel do private(k,inod,rr)
        do k = nlayer_ICB, nlayer_CMB
          inod = jt + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
          d_rj(inod,is_magne  ) = zero
          d_rj(inod,is_magne+1) = zero
          d_rj(inod,is_magne+2) =  (ten/three) * rr * sin(pi*(rr-r_ICB))
          d_rj(inod,is_current  ) = d_rj(inod,is_magne+2)
          d_rj(inod,is_current+1) = (ten / three) * sin(pi*(rr-r_ICB))  &
     &                  + (ten / three) * pi * rr * cos(pi*(rr-r_ICB))
          d_rj(inod,is_current+2) = zero
        end do
!$omp end parallel do
      end if
!
      end subroutine initial_magne_sph_dbench_case1
!
!-----------------------------------------------------------------------
!
      subroutine initial_magne_sph_dbench_case2                         &
     &         (sph_rj, nlayer_CMB, r_CMB, is_magne, is_current,        &
     &          n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nlayer_CMB
      real(kind = kreal), intent(in) :: r_CMB
      integer(kind = kint), intent(in) :: is_magne, is_current
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: inod, k, js, jt
!
!
      pi = four * atan(one)
      js = find_local_sph_address(sph_rj, 1,0)
      jt = find_local_sph_address(sph_rj, 2,0)
!
!         Poloidal magnetic field
      if(js .gt. 0) then
!$omp parallel do private(k,inod,rr)
        do k = 1, nlayer_CMB
          inod = js + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
          d_rj(inod,is_magne  ) = (five / two) * rr**2                  &
     &                       * (four*r_CMB - three*rr) / (r_CMB+three)
          d_rj(inod,is_magne+1) = (five / two) * rr                     &
     &                       * (eight*r_CMB - dnine*rr) / (r_CMB+three)
          d_rj(inod,is_magne+2  ) = zero
          d_rj(inod,is_current  ) = zero
          d_rj(inod,is_current+1) = zero
          d_rj(inod,is_current+2) = five*six * rr / (three +r_CMB)
        end do
!$omp end parallel do
      end if
!
!         Toroidal magnetic field
      if (jt .gt. 0) then
!$omp parallel do private(k,inod,rr)
        do k = 1, nlayer_CMB
          inod = jt + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
!
          d_rj(inod,is_magne  ) = zero
          d_rj(inod,is_magne+1) = zero
          d_rj(inod,is_magne+2) = (ten / three) * rr * sin(pi*rr/r_CMB)
          d_rj(inod,is_current  ) = d_rj(inod,is_magne+2)
          d_rj(inod,is_current+1) = (ten / three) * sin(pi*rr/r_CMB)    &
     &          + (ten / three) * (pi/r_CMB) * rr * cos(pi*rr/r_CMB)
          d_rj(inod,is_current+2) = zero
        end do
!$omp end parallel do
      end if
!
      end subroutine initial_magne_sph_dbench_case2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine initial_magne_sph_dbench_qcv                           &
     &         (sph_rj, nlayer_ICB, nlayer_CMB, r_ICB, r_CMB,           &
     &          is_magne, is_current, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      integer(kind = kint), intent(in) :: is_magne, is_current
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: inod, k, js, jt
!
!
      js = find_local_sph_address(sph_rj, 1,0)
      jt = find_local_sph_address(sph_rj, 2,0)
!
      pi = four * atan(one)
!
!        Poloidal magnetic field
      if(js .gt. 0) then
!$omp parallel do private(k,inod,rr)
        do k = nlayer_ICB, nlayer_CMB
          inod = js + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
!
          d_rj(inod,is_magne  )                                         &
     &      =  (five / eight) * (dnine*half * rr**4                     &
     &        - (three*r_ICB + three*r_CMB + four) * two * rr**3        &
     &        + (four*r_ICB + four*r_CMB + three*r_ICB*r_CMB)           &
     &         * three * rr**2                                          &
     &        - four*six * r_ICB*r_CMB * rr)
          d_rj(inod,is_magne+1)                                         &
     &      =  (five / eight) * (two*dnine * rr**3                      &
     &        - (three*r_ICB + three*r_CMB + four) * six * rr**2        &
     &        + (four*r_ICB + four*r_CMB + three*r_ICB*r_CMB)*six * rr  &
     &        - four*six * r_ICB*r_CMB)
          d_rj(inod,is_magne+2  ) = zero
          d_rj(inod,is_current  ) = zero
          d_rj(inod,is_current+1) = zero
          d_rj(inod,is_current+2)                                       &
     &      =  (five / eight) * (-four*dnine * rr**2                    &
     &        + (three*r_ICB + three*r_CMB + four) * eight * rr         &
     &        - eight*six * r_ICB*r_CMB / rr)
        end do
!$omp end parallel do
      end if
!
!        Toroidal magnetic field
     if(jt .gt. 0) then
!$omp parallel do private(k,inod,rr)
        do k = nlayer_ICB, nlayer_CMB
          inod = jt + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
          d_rj(inod,is_magne  ) = zero
          d_rj(inod,is_magne+1) = zero
          d_rj(inod,is_magne+2)                                         &
     &             = (ten/eight) * rr * sin(pi*(rr-r_ICB))
          d_rj(inod,is_current  ) = d_rj(inod,is_magne+2)
          d_rj(inod,is_current+1)                                       &
     &             = (ten / eight) * (sin(pi*(rr-r_ICB))                &
     &              + pi * rr * cos(pi*(rr-r_ICB)) )
          d_rj(inod,is_current+2) = zero
        end do
!$omp end parallel do
      end if
!
      end subroutine initial_magne_sph_dbench_qcv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reduce_initial_magne_sph(reduce_ratio,                 &
     &          is_magne, is_current, nnod_rj, ntot_phys_rj, d_rj)
!
      real(kind = kreal), intent(in) :: reduce_ratio
      integer(kind = kint), intent(in) :: is_magne, is_current
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: is
!
!
!$omp parallel do
      do is = 1, nnod_rj
        d_rj(is,is_magne  ) =   reduce_ratio * d_rj(is,is_magne  )
        d_rj(is,is_magne+1) =   reduce_ratio * d_rj(is,is_magne+1)
        d_rj(is,is_magne+2) =   reduce_ratio * d_rj(is,is_magne+2)
        d_rj(is,is_current  ) = reduce_ratio * d_rj(is,is_current  )
        d_rj(is,is_current+1) = reduce_ratio * d_rj(is,is_current+1)
        d_rj(is,is_current+2) = reduce_ratio * d_rj(is,is_current+2)
      end do
!$omp end parallel do
!
      end subroutine reduce_initial_magne_sph
!
!-----------------------------------------------------------------------
!
      end module initial_magne_dynamobench
