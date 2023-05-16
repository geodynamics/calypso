!>@file   field_at_mid_equator.f90
!!@brief  module field_at_mid_equator
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  data at mid-depth of the shell at equator for dynamo benchmark
!!
!!@verbatim
!!      subroutine init_mid_equator_point_global(sph, cdat)
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: ipol
!!        type(circle_fld_maker), intent(inout) :: cdat
!!
!!      subroutine cal_drift_by_v44(time, sph_rj, rj_fld, ipol,         &
!!     &          circle, t_prev, phase_vm4, phase_vm4_prev, omega_vm4)
!!        real(kind=kreal), intent(in) :: time
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_address), intent(in) :: ipol
!!        type(circle_parameters), intent(in) :: circle
!!        real(kind = kreal), intent(inout) :: t_prev
!!        real(kind = kreal), intent(inout) :: phase_vm4(2)
!!        real(kind = kreal), intent(inout) :: phase_vm4_prev(2)
!!        real(kind = kreal), intent(inout) :: omega_vm4(2)
!!      subroutine cal_field_4_dynamobench                              &
!!     &         (time, t_prev, circle, d_circle, ibench_velo,          &
!!     &          phi_zero, phi_prev, drift, ave_drift, d_zero)
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
      subroutine init_mid_equator_point_global(sph, cdat)
!
      use t_spheric_parameter
!
      type(sph_grids), intent(in) :: sph
      type(circle_fld_maker), intent(inout) :: cdat
!
      integer(kind = kint) :: kr_ICB, kr_CMB
      real(kind = kreal) :: r_MID
!
!
      kr_ICB = sph%sph_params%nlayer_ICB
      kr_CMB = sph%sph_params%nlayer_CMB
      r_MID = half * (sph%sph_rj%radius_1d_rj_r(kr_ICB)                 &
     &              + sph%sph_rj%radius_1d_rj_r(kr_CMB))
!
      cdat%circle%s_circle = r_MID
      cdat%circle%z_circle = zero
!
      end subroutine init_mid_equator_point_global
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_drift_by_v44(time, sph_rj, rj_fld, ipol,           &
     &          circle, t_prev, phase_vm4, phase_vm4_prev, omega_vm4)
!
      use calypso_mpi
      use calypso_mpi_real
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_sph_circle_parameters
      use transfer_to_long_integers
!
      real(kind=kreal), intent(in) :: time
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(in) :: ipol
      type(circle_parameters), intent(in) :: circle
!
      real(kind = kreal), intent(inout) :: t_prev
      real(kind = kreal), intent(inout) :: phase_vm4(2)
      real(kind = kreal), intent(inout) :: phase_vm4_prev(2)
      real(kind = kreal), intent(inout) :: omega_vm4(2)
!
      integer(kind = kint) :: j4c, j4s, kr_in, kr_out, i_in, i_out
      real(kind = kreal) :: c_in, c_out
      real(kind = kreal) :: v4_lc(4), v4_gl(4)
!
!
      if(time .eq. t_prev) return
!
      kr_in =  circle%kr_gl_rcirc_in
      kr_out = circle%kr_gl_rcirc_out
      c_in =   circle%coef_gl_rcirc_in
      c_out =  circle%coef_gl_rcirc_out
      v4_lc(1:4) = 0.0d0
!
      j4c = find_local_sph_address(sph_rj, 4, 4)
      if(j4c .gt. 0) then
        i_in =  local_sph_node_address(sph_rj, kr_in, j4c)
        i_out = local_sph_node_address(sph_rj, kr_out, j4c)
        v4_lc(1) = c_in *   rj_fld%d_fld(i_in, ipol%base%i_velo)        &
     &            + c_out * rj_fld%d_fld(i_out,ipol%base%i_velo)
      end if
!
      j4s = find_local_sph_address(sph_rj, 4,-4)
      if(j4s .gt. 0) then
        i_in =  local_sph_node_address(sph_rj, kr_in, j4s)
        i_out = local_sph_node_address(sph_rj, kr_out, j4s)
        v4_lc(2) = c_in *   rj_fld%d_fld(i_in, ipol%base%i_velo)        &
     &            + c_out * rj_fld%d_fld(i_out,ipol%base%i_velo)
      end if
!
      j4c = find_local_sph_address(sph_rj, 5, 4)
      if(j4c .gt. 0) then
        i_in =  local_sph_node_address(sph_rj, kr_in, j4c)
        i_out = local_sph_node_address(sph_rj, kr_out, j4c)
        v4_lc(3) = c_in *   rj_fld%d_fld(i_in, ipol%base%i_velo+2)      &
     &            + c_out * rj_fld%d_fld(i_out,ipol%base%i_velo+2)
      end if
!
      j4s = find_local_sph_address(sph_rj, 5,-4)
      if(j4s .gt. 0) then
        i_in =  local_sph_node_address(sph_rj, kr_in, j4s)
        i_out = local_sph_node_address(sph_rj, kr_out, j4s)
        v4_lc(4) = c_in *   rj_fld%d_fld(i_in, ipol%base%i_velo+2)      &
     &            + c_out * rj_fld%d_fld(i_out,ipol%base%i_velo+2)
      end if
!
      call calypso_mpi_reduce_real                                      &
     &   (v4_lc, v4_gl, cast_long(ifour), MPI_SUM, 0)
!
      if(my_rank .ne. 0) return
!
      phase_vm4(1) = atan2(v4_gl(2), v4_gl(1))
      phase_vm4(2) = atan2(v4_gl(4), v4_gl(3))
!
      if(t_prev .eq. time) then
        omega_vm4(1:2) = 0.0d0
      else
        omega_vm4(1:2) = quad * (phase_vm4(1:2) - phase_vm4_prev(1:2))  &
     &                / (time - t_prev)
      end if
!
      phase_vm4_prev(1:2) = phase_vm4(1:2)
!
      end subroutine cal_drift_by_v44
!
! ----------------------------------------------------------------------
!
      subroutine cal_field_4_dynamobench                                &
     &         (time, t_prev, circle, d_circle, ibench_velo,            &
     &          phi_zero, phi_prev, drift, ave_drift, d_zero)
!
      use calypso_mpi
      use t_phys_data
      use t_sph_circle_parameters
!
      real(kind=kreal), intent(in) :: time, t_prev
      type(circle_parameters), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
      integer(kind = kint), intent(in) :: ibench_velo
!
      real(kind = kreal), intent(inout) :: phi_zero(4), phi_prev(4)
      real(kind = kreal), intent(inout) :: drift(4), ave_drift
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_zero(0:4,d_circle%ntot_phys)
!
      integer(kind = kint) :: nd, mphi, mp_next, icou
      real(kind = kreal) :: coef, pi
!
      pi = four*atan(one)
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
          phi_zero(icou) = two*pi                                       &
     &                * (dble(mphi) - coef) / dble(circle%mphi_circle)
          do nd = 1, d_circle%ntot_phys
            d_zero(icou,nd) = coef * d_circle%d_fld(mphi,nd)            &
     &                     + (one - coef) * d_circle%d_fld(mp_next,nd)
          end do
!
          if(icou .eq. 4) exit
        end if
      end do
!
      drift(1:4) = (phi_zero(1:4) - phi_prev(1:4))
      do icou = 1, 4
        if(drift(icou) .lt. -pi) drift(icou) = drift(icou) + two*pi
        if(drift(icou) .gt.  pi) drift(icou) = drift(icou) - two*pi
      end do
!
      if(t_prev .eq. time) then
        drift(1:4)  = 0.0d0
      else
        drift(1:4) = drift(1:4) / (t_prev - time)
      end if
      phi_prev(1:4) = phi_zero(1:4)
!
      ave_drift = quad * (drift(1) + drift(2) + drift(3) + drift(4))
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
