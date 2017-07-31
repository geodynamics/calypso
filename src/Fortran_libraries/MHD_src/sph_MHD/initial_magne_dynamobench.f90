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
!!      subroutine initial_b_dynamobench_1(sph_rj,  ipol, idpdr, itor,  &
!!     &          r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,                 &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine initial_b_dynamobench_2(sph_rj, ipol, idpdr, itor,   &
!!     &          nlayer_CMB, r_CMB, n_point, ntot_phys_rj, d_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!@endverbatim
!
      module initial_magne_dynamobench
!
      use m_precision
      use m_constants
!
      use t_phys_address
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
      subroutine initial_b_dynamobench_1(sph_rj,  ipol, idpdr, itor,    &
     &          r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,                   &
     &          n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol, idpdr, itor
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
        d_rj(is,ipol%i_magne  ) = zero
        d_rj(is,ipol%i_magne+1) = zero
        d_rj(is,ipol%i_magne+2) = zero
        d_rj(is,ipol%i_current  ) = zero
        d_rj(is,ipol%i_current+1) = zero
        d_rj(is,ipol%i_current+2) = zero
      end do
!$omp end parallel do
!
      if (js .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          is = js + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
!
          d_rj(is,ipol%i_magne) =  (five / eight) * (-three * rr**3     &
     &                     + four * r_CMB * rr**2 - r_ICB**4 / rr)
          d_rj(is,idpdr%i_magne) = (five / eight) * (-dnine * rr**2     &
     &                       + eight * r_CMB * rr + r_ICB**4 / rr**2)
          d_rj(is,itor%i_current) =  (five*three / two) * rr
        end do
      end if
!
     if (jt .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          it = jt + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
          d_rj(it,itor%i_magne) = (ten/three) * rr * sin(pi*(rr-r_ICB))
          d_rj(it,ipol%i_current) =  d_rj(it,itor%i_magne)
          d_rj(it,idpdr%i_current)                                      &
     &             = (ten / three) * (sin(pi*(rr-r_ICB))                &
     &              + pi * rr * cos(pi*(rr-r_ICB)) )
        end do
      end if
!
      end subroutine initial_b_dynamobench_1
!
!-----------------------------------------------------------------------
!
      subroutine initial_b_dynamobench_2(sph_rj, ipol, idpdr, itor,     &
     &          nlayer_CMB, r_CMB, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol, idpdr, itor
      integer(kind = kint), intent(in) ::  nlayer_CMB
      real(kind = kreal), intent(in) :: r_CMB
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
        d_rj(is,ipol%i_magne  ) = zero
        d_rj(is,ipol%i_magne+1) = zero
        d_rj(is,ipol%i_magne+2) = zero
        d_rj(is,ipol%i_current  ) = zero
        d_rj(is,ipol%i_current+1) = zero
        d_rj(is,ipol%i_current+2) = zero
      end do
!$omp end parallel do
!
      if (js .gt. 0) then
        do k = 1, nlayer_CMB
          is = js + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
          d_rj(is,ipol%i_magne) =  (five / two) * rr**2                 &
     &                       * (four*r_CMB - three*rr) / (r_CMB+three)
          d_rj(is,idpdr%i_magne) = (five / two) * rr                    &
     &                       * (eight*r_CMB - dnine*rr) / (r_CMB+three)
          d_rj(is,itor%i_current) =  five*six * rr / (three +r_CMB)
        end do
      end if
!
      if (jt .gt. 0) then
        do k = 1, nlayer_CMB
          it = jt + (k-1) * sph_rj%nidx_rj(2)
          rr = sph_rj%radius_1d_rj_r(k)
!
          d_rj(it,itor%i_magne) = (ten / three) * rr * sin(pi*rr/r_CMB)
          d_rj(it,ipol%i_current) =  d_rj(it,itor%i_magne)
          d_rj(it,idpdr%i_current)                                      &
     &              = (ten / three) * (sin(pi*rr/r_CMB)      &
     &               + (pi/r_CMB) * rr * cos(pi*rr/r_CMB) )
        end do
      end if
!
      end subroutine initial_b_dynamobench_2
!
!-----------------------------------------------------------------------
!
      end module initial_magne_dynamobench
