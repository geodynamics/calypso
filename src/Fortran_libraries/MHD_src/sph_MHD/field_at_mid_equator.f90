!>@file   field_at_mid_equator.f90
!!@brief  module field_at_mid_equator
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  data at mid-depth of the shell at equator for dynamo benchmark
!!
!!@verbatim
!!      subroutine set_mid_equator_point_global                         &
!!     &         (sph_params, sph_rtp, sph_rj, cdat)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_data), intent(in) :: rj_fld
!!        type(circle_fld_maker), intent(inout) :: cdat
!!
!!      subroutine cal_drift_by_v44(time, circle, ibench_velo,          &
!!     &          t_prev, phase_vm4, phase_vm4_prev, omega_vm4)
!!      subroutine cal_field_4_dynamobench(circle, d_circle,            &
!!     &          ibench_velo, phi_zero, phi_prev, d_zero)
!!@endverbatim
!
      module field_at_mid_equator
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_field_on_circle
      use t_circle_transform
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_mid_equator_point_global                           &
     &         (sph_params, sph_rtp, sph_rj, cdat)
!
      use m_phys_labels
      use t_spheric_parameter
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(circle_fld_maker), intent(inout) :: cdat
!
      real(kind = kreal) :: r_MID
!
!
      r_MID = half * (sph_rj%radius_1d_rj_r(sph_params%nlayer_ICB)      &
     &              + sph_rj%radius_1d_rj_r(sph_params%nlayer_CMB) )
!
      cdat%circle%s_circle = r_MID
      cdat%circle%z_circle = zero
      call const_circle_point_global(sph_params%l_truncation,           &
     &    sph_rtp, sph_rj, cdat)
!
      end subroutine set_mid_equator_point_global
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_drift_by_v44(time, circle, ibench_velo,            &
     &          t_prev, phase_vm4, phase_vm4_prev, omega_vm4)
!
      use t_circle_transform
!
      real(kind=kreal), intent(in) :: time
      type(fields_on_circle), intent(in) :: circle
      integer(kind = kint), intent(in) :: ibench_velo
!
      real(kind = kreal), intent(inout) :: t_prev
      real(kind = kreal), intent(inout) :: phase_vm4(2)
      real(kind = kreal), intent(inout) :: phase_vm4_prev(2)
      real(kind = kreal), intent(inout) :: omega_vm4(2)
!
      integer(kind = kint) :: j4c, j4s
      real(kind = kreal) :: vp44c, vp44s
      real(kind = kreal) :: vt54c, vt54s
!
!
      if(time .eq. t_prev) return
!
      j4c = ifour*(ifour+1) + ifour
      j4s = ifour*(ifour+1) - ifour
      vp44c = circle%d_rj_circle(j4c,ibench_velo  )
      vp44s = circle%d_rj_circle(j4s,ibench_velo  )
!
      j4c = ifive*(ifive+1) + ifour
      j4s = ifive*(ifive+1) - ifour
      vt54c = circle%d_rj_circle(j4c,ibench_velo+2)
      vt54s = circle%d_rj_circle(j4s,ibench_velo+2)
!
      phase_vm4(1) = atan2(vp44s,vp44c)
      phase_vm4(2) = atan2(vt54s,vt54c)
!
      omega_vm4(1:2) = quad * (phase_vm4(1:2) - phase_vm4_prev(1:2))    &
     &                / (time - t_prev)
!
      t_prev = time
      phase_vm4_prev(1:2) = phase_vm4(1:2)
!
      end subroutine cal_drift_by_v44
!
! ----------------------------------------------------------------------
!
      subroutine cal_field_4_dynamobench(circle, d_circle,              &
     &          ibench_velo, phi_zero, phi_prev, d_zero)
!
      use calypso_mpi
      use t_phys_data
!
      type(fields_on_circle), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
      integer(kind = kint), intent(in) :: ibench_velo
!
      real(kind = kreal), intent(inout) :: phi_zero(4), phi_prev(4)
      real(kind = kreal), intent(inout) :: d_zero(0:4,7)
!
      integer(kind = kint) :: nd, mphi, mp_next, icou
      real(kind = kreal) :: coef
!
!
      icou = 0
      do mphi = 1, circle%mphi_circle
        mp_next = mod(mphi,circle%mphi_circle) + 1
        if(      d_circle%d_fld(mphi,ibench_velo)  .le.  zero           &
     &     .and. d_circle%d_fld(mp_next,ibench_velo) .gt. zero) then
          icou = icou + 1
          coef = d_circle%d_fld(mp_next,ibench_velo)                    &
     &          / (d_circle%d_fld(mp_next,ibench_velo)                  &
     &              - d_circle%d_fld(mphi,ibench_velo))
!
          phi_zero(icou) = two*four*atan(one)                           &
     &                * (dble(mphi) - coef) / dble(circle%mphi_circle)
          do nd = 1, 7
            d_zero(icou,nd) = coef * d_circle%d_fld(mphi,nd)            &
     &                     + (one - coef) * d_circle%d_fld(mp_next,nd)
          end do
!
          if(icou .eq. 4) exit
        end if
      end do
!
      phi_prev(1:4) = phi_zero(1:4)
!
      do nd = 1, 7
        d_zero(0,nd) = quad * (d_zero(1,nd) + d_zero(2,nd)              &
     &                       + d_zero(3,nd) + d_zero(4,nd))
      end do
!
      end subroutine cal_field_4_dynamobench
!
! ----------------------------------------------------------------------
!
      end module field_at_mid_equator
