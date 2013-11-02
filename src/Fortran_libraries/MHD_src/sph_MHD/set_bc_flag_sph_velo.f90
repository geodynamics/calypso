!>@file   set_bc_flag_sph_velo.f90
!!@brief  module set_bc_flag_sph_velo
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions flags for velocity
!!
!!@verbatim
!!      subroutine s_set_bc_sph_mhd
!!@endverbatim
!
      module set_bc_flag_sph_velo
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_boundary_condition_IDs
      use m_control_parameter
      use m_control_params_sph_MHD
!
      use m_spheric_parameter
!
      use m_bc_data_list
      use m_surf_data_list
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
      subroutine set_sph_bc_velo_sph
!
      integer(kind = kint) :: i
!
!
      call allocate_vsp_bc_array( nidx_rj(2) )
!
!
      do i = 1, velo_nod%num_bc
        if(iflag_icb_velocity .ne. iflag_fixed_velo) exit
        if(velo_nod%bc_name(i) .eq. ICB_nod_grp_name                    &
     &    .or. velo_nod%bc_name(i) .eq. CTR_nod_grp_name) then
          call set_sph_velo_ICB_flag(velo_nod%ibc_type(i),              &
     &        velo_nod%bc_magnitude(i))
        end if
      end do
!
      do i = 1, torque_surf%num_bc
        if(iflag_icb_velocity .ne. iflag_fixed_velo) exit
        if    (torque_surf%bc_name(i) .eq. ICB_sf_grp_name              &
     &    .or. torque_surf%bc_name(i) .eq. ICB_nod_grp_name             &
     &    .or. torque_surf%bc_name(i) .eq. CTR_sf_grp_name              &
     &    .or. torque_surf%bc_name(i) .eq. CTR_nod_grp_name) then
          call set_sph_velo_ICB_flag(torque_surf%ibc_type(i),           &
     &        torque_surf%bc_magnitude(i))
        end if
      end do
!
!
!
      do i = 1, velo_nod%num_bc
        if(iflag_cmb_velocity .ne. iflag_fixed_velo) exit
        if(velo_nod%bc_name(i) .eq. CMB_nod_grp_name) then
          call set_sph_velo_CMB_flag(velo_nod%ibc_type(i),              &
     &        velo_nod%bc_magnitude(i))
        end if
      end do
!
      do i = 1, torque_surf%num_bc
        if(iflag_cmb_velocity .ne. iflag_fixed_velo) exit
        if(     torque_surf%bc_name(i) .eq. CMB_sf_grp_name             &
     &     .or. torque_surf%bc_name(i) .eq. CMB_nod_grp_name) then
          call set_sph_velo_CMB_flag(torque_surf%ibc_type(i),           &
     &        torque_surf%bc_magnitude(i))
        end if
      end do
!
      end subroutine set_sph_bc_velo_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_velo_ICB_flag(ibc_type, bc_mag)
!
      integer(kind = kint), intent(in) :: ibc_type
      real(kind = kreal), intent(in) :: bc_mag
!
!
      if      (ibc_type .eq. iflag_free_sph) then
        iflag_icb_velocity = iflag_free_slip
      else if (ibc_type .eq. iflag_non_slip_sph) then
        iflag_icb_velocity = iflag_fixed_velo
      else if (ibc_type .eq. iflag_rotatable_icore) then
        iflag_icb_velocity = iflag_rotatable_ic
      else if (ibc_type .eq. iflag_sph_2_center) then
        iflag_icb_velocity = iflag_sph_fill_center
      else if (ibc_type .eq. iflag_sph_clip_center) then
        iflag_icb_velocity = iflag_sph_fix_center
!
      else if (ibc_type .eq. (iflag_bc_rot+1)) then
        iflag_icb_velocity = iflag_fixed_velo
        if(idx_rj_degree_one( 1) .gt.0 ) then
          vt_ICB_bc( idx_rj_degree_one( 1) ) = r_ICB*r_ICB * bc_mag
        end if
      else if (ibc_type .eq. (iflag_bc_rot+2)) then
        iflag_icb_velocity = iflag_fixed_velo
        if(idx_rj_degree_one(-1) .gt. 0) then
          vt_ICB_bc( idx_rj_degree_one(-1) ) = r_ICB*r_ICB * bc_mag
        end if
      else if (ibc_type .eq. (iflag_bc_rot+3)) then
        iflag_icb_velocity = iflag_fixed_velo
        if(idx_rj_degree_one( 0) .gt. 0) then
          vt_ICB_bc( idx_rj_degree_one( 0) ) = r_ICB*r_ICB * bc_mag
        end if
      end if
!
      end subroutine set_sph_velo_ICB_flag
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_velo_CMB_flag(ibc_type, bc_mag)
!
      integer(kind = kint), intent(in) :: ibc_type
      real(kind = kreal), intent(in) :: bc_mag
!
!
      if      (ibc_type .eq. iflag_free_sph) then
        iflag_cmb_velocity = iflag_free_slip
      else if (ibc_type .eq. iflag_non_slip_sph) then
        iflag_cmb_velocity = iflag_fixed_velo
!
      else if (ibc_type .eq. (iflag_bc_rot+1)) then
        iflag_cmb_velocity = iflag_fixed_velo
        if(idx_rj_degree_one( 1) .gt.0 ) then
          vt_CMB_bc( idx_rj_degree_one( 1) ) = r_CMB*r_CMB * bc_mag
        end if
      else if (ibc_type .eq. (iflag_bc_rot+2)) then
        iflag_cmb_velocity = iflag_fixed_velo
        if(idx_rj_degree_one(-1) .gt. 0) then
          vt_CMB_bc( idx_rj_degree_one(-1) ) = r_CMB*r_CMB * bc_mag
        end if
      else if (ibc_type .eq. (iflag_bc_rot+3)) then
        iflag_cmb_velocity = iflag_fixed_velo
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
