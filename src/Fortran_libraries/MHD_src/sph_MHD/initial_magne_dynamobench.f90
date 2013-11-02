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
!!      subroutine initial_b_dynamobench_1
!!      subroutine initial_b_dynamobench_2
!!@endverbatim
!
      module initial_magne_dynamobench
!
      use m_precision
!
      use m_constants
      use m_sph_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine initial_b_dynamobench_1
!
      use m_control_params_sph_MHD
      use m_spheric_parameter
      use m_sph_spectr_data
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: is, it, k, j, js, jt
      integer(kind = kint), parameter :: ls = 1, lt = 2
!
!
      js = find_local_sph_mode_address(ls,izero)
      jt = find_local_sph_mode_address(lt,izero)
!
      pi = four * atan(one)
!
!$omp parallel do
      do is = 1, nnod_rj
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
          is = js + (k-1)*nidx_rj(2)
          rr = radius_1d_rj_r(k)
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
          it = jt + (k-1)*nidx_rj(2)
          rr = radius_1d_rj_r(k)
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
      subroutine initial_b_dynamobench_2
!
      use m_control_params_sph_MHD
      use m_spheric_parameter
      use m_sph_spectr_data
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: is, it, k, j, js, jt
      integer(kind = kint), parameter :: ls = 1, lt = 2
!
!
      js = find_local_sph_mode_address(ls,izero)
      jt = find_local_sph_mode_address(lt,izero)
!
      pi = four * atan(one)
!
!$omp parallel do
      do is = 1, nnod_rj
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
          is = js + (k-1)*nidx_rj(2)
          rr = radius_1d_rj_r(k)
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
          it = jt + (k-1)*nidx_rj(2)
          rr = radius_1d_rj_r(k)
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
