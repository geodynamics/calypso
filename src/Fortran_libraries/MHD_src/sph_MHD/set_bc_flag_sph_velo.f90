!>@file   set_bc_flag_sph_velo.f90
!!@brief  module set_bc_flag_sph_velo
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions flags for velocity
!!
!!@verbatim
!!      subroutine set_sph_bc_velo_sph                                  &
!!     &         (sph_rj, radial_rj_grp, r_ICB, r_CMB,                  &
!!     &          velo_nod, torque_surf, sph_bc_U, bc_Uspectr)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(boundary_condition_list), intent(in) :: velo_nod
!!        type(boundary_condition_list), intent(in) :: torque_surf
!!        type(sph_boundary_type), intent(inout) :: sph_bc_U
!!        type(sph_velocity_BC_spectr), intent(inout) :: bc_Uspectr
!!@endverbatim
!
      module set_bc_flag_sph_velo
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_boundary_condition_IDs
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
!
      implicit none
!
      private :: set_sph_velo_ICB_flag, set_sph_velo_CMB_flag
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_velo_sph                                    &
     &         (sph_rj, radial_rj_grp, r_ICB, r_CMB,                    &
     &          velo_nod, torque_surf, sph_bc_U, bc_Uspectr)
!
      use t_spheric_rj_data
      use t_group_data
      use t_bc_data_list
      use set_bc_sph_scalars
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rj_grp
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      type(boundary_condition_list), intent(in) :: velo_nod
      type(boundary_condition_list), intent(in) :: torque_surf
!
      type(sph_boundary_type), intent(inout) :: sph_bc_U
      type(sph_velocity_BC_spectr), intent(inout) :: bc_Uspectr
!
      integer(kind = kint) :: i
      integer(kind = kint) :: igrp_icb, igrp_cmb
!
!
      call find_both_sides_of_boundaries(sph_rj, radial_rj_grp,        &
     &   velo_nod, torque_surf, sph_bc_U, igrp_icb, igrp_cmb)
!
      call alloc_vsp_bc_array(sph_rj%nidx_rj(2), bc_Uspectr)
!
!
      i = abs(igrp_icb)
      if(igrp_icb .lt. 0) then
        call set_sph_velo_ICB_flag                                      &
     &    (sph_rj%nidx_rj(2), sph_rj%idx_rj_degree_one, r_ICB,          &
     &     torque_surf%ibc_type(i), torque_surf%bc_magnitude(i),        &
     &      sph_bc_U, bc_Uspectr%vt_ICB_bc)
      else
        call set_sph_velo_ICB_flag                                      &
     &    (sph_rj%nidx_rj(2), sph_rj%idx_rj_degree_one, r_ICB,          &
     &     velo_nod%ibc_type(i), velo_nod%bc_magnitude(i),              &
     &      sph_bc_U, bc_Uspectr%vt_ICB_bc)
      end if
!
      i = abs(igrp_cmb)
      if(igrp_icb .lt. 0) then
        call set_sph_velo_CMB_flag                                      &
     &    (sph_rj%nidx_rj(2), sph_rj%idx_rj_degree_one, r_CMB,          &
     &      torque_surf%ibc_type(i), torque_surf%bc_magnitude(i),       &
     &      sph_bc_U, bc_Uspectr%vt_CMB_bc)
      else
        call set_sph_velo_CMB_flag                                      &
     &    (sph_rj%nidx_rj(2), sph_rj%idx_rj_degree_one, r_CMB,          &
     &      velo_nod%ibc_type(i), velo_nod%bc_magnitude(i),             &
     &      sph_bc_U, bc_Uspectr%vt_CMB_bc)
      end if
!
      end subroutine set_sph_bc_velo_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_velo_ICB_flag(jmax, idx_rj_degree_one,         &
     &          r_ICB, ibc_type, bc_mag, sph_bc_U, vt_ICB_bc)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: idx_rj_degree_one(-1:1)
      real(kind = kreal), intent(in) :: r_ICB
      integer(kind = kint), intent(in) :: ibc_type
      real(kind = kreal), intent(in) :: bc_mag
!
      type(sph_boundary_type), intent(inout) :: sph_bc_U
      real(kind= kreal), intent(inout) :: vt_ICB_bc(jmax)
!
!
      if      (ibc_type .eq. iflag_free_sph) then
        sph_bc_U%iflag_icb = iflag_free_slip
      else if (ibc_type .eq. iflag_non_slip_sph) then
        sph_bc_U%iflag_icb = iflag_fixed_velo
      else if (ibc_type .eq. iflag_rotatable_icore) then
        sph_bc_U%iflag_icb = iflag_rotatable_ic
      else if (ibc_type .eq. iflag_sph_2_center) then
        sph_bc_U%iflag_icb = iflag_sph_fill_center
      else if (ibc_type .eq. iflag_sph_clip_center) then
        sph_bc_U%iflag_icb = iflag_sph_fix_center
!
      else if (ibc_type .eq. (iflag_bc_rot+1)) then
        sph_bc_U%iflag_icb = iflag_fixed_velo
        if(idx_rj_degree_one( 1) .gt.0 ) then
          vt_ICB_bc( idx_rj_degree_one( 1) ) = r_ICB*r_ICB * bc_mag
        end if
      else if (ibc_type .eq. (iflag_bc_rot+2)) then
        sph_bc_U%iflag_icb = iflag_fixed_velo
        if(idx_rj_degree_one(-1) .gt. 0) then
          vt_ICB_bc( idx_rj_degree_one(-1) ) = r_ICB*r_ICB * bc_mag
        end if
      else if (ibc_type .eq. (iflag_bc_rot+3)) then
        sph_bc_U%iflag_icb = iflag_fixed_velo
        if(idx_rj_degree_one( 0) .gt. 0) then
          vt_ICB_bc( idx_rj_degree_one( 0) ) = r_ICB*r_ICB * bc_mag
        end if
      end if
!
      end subroutine set_sph_velo_ICB_flag
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_velo_CMB_flag(jmax, idx_rj_degree_one,         &
     &          r_CMB, ibc_type, bc_mag, sph_bc_U, vt_CMB_bc)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: idx_rj_degree_one(-1:1)
      real(kind = kreal), intent(in) :: r_CMB
      integer(kind = kint), intent(in) :: ibc_type
      real(kind = kreal), intent(in) :: bc_mag
!
      type(sph_boundary_type), intent(inout) :: sph_bc_U
      real(kind= kreal), intent(inout) :: vt_CMB_bc(jmax)
!
!
      if      (ibc_type .eq. iflag_free_sph) then
        sph_bc_U%iflag_cmb = iflag_free_slip
      else if (ibc_type .eq. iflag_non_slip_sph) then
        sph_bc_U%iflag_cmb = iflag_fixed_velo
!
      else if (ibc_type .eq. (iflag_bc_rot+1)) then
        sph_bc_U%iflag_cmb = iflag_fixed_velo
        if(idx_rj_degree_one( 1) .gt.0 ) then
          vt_CMB_bc( idx_rj_degree_one( 1) ) = r_CMB*r_CMB * bc_mag
        end if
      else if (ibc_type .eq. (iflag_bc_rot+2)) then
        sph_bc_U%iflag_cmb = iflag_fixed_velo
        if(idx_rj_degree_one(-1) .gt. 0) then
          vt_CMB_bc( idx_rj_degree_one(-1) ) = r_CMB*r_CMB * bc_mag
        end if
      else if (ibc_type .eq. (iflag_bc_rot+3)) then
        sph_bc_U%iflag_cmb = iflag_fixed_velo
        if(idx_rj_degree_one( 0) .gt. 0) then
          vt_CMB_bc( idx_rj_degree_one( 0) ) = r_CMB*r_CMB * bc_mag
        end if
      end if
!
      end subroutine set_sph_velo_CMB_flag
!
! -----------------------------------------------------------------------
!
      end module set_bc_flag_sph_velo
