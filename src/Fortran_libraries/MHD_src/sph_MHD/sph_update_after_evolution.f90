!>@file   sph_update_after_evolution.f90
!!@brief  module sph_update_after_evolution
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update fields after time integration
!!
!!@verbatim
!!      subroutine update_after_vorticity_sph(sph_rj, r_2nd, fl_prop,   &
!!     &          sph_bc_U, fdm2_free_ICB, fdm2_free_CMB, leg,          &
!!     &          ipol, rj_fld)
!!      subroutine update_after_magne_sph(sph_rj, r_2nd,                &
!!     &          cd_prop, sph_bc_B, leg, ipol, rj_fld)
!!      subroutine update_after_heat_sph(sph_rj, r_2nd, ht_prop,        &
!!     &          sph_bc_T, bcs_T, fdm2_center, leg, ipol, rj_fld)
!!      subroutine update_after_composit_sph(sph_rj, r_2nd, cp_prop,    &
!!     &          sph_bc_C, bcs_C, fdm2_center, leg, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop
!!        type(scalar_property), intent(in) :: cp_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
!!        type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        type(sph_boundary_type), intent(in) :: sph_bc_C
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_C
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module sph_update_after_evolution
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use const_sph_radial_grad
      use const_sph_rotation
      use const_sph_diffusion
!
      use t_control_parameter
      use t_physical_property
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_radial_matrices_sph_MHD
      use t_coef_fdm2_MHD_boundaries
      use t_work_4_sph_trans
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine update_after_vorticity_sph(sph_rj, r_2nd, fl_prop,     &
     &          sph_bc_U, fdm2_free_ICB, fdm2_free_CMB, leg,            &
     &          ipol, rj_fld)
!
      use cal_inner_core_rotation
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
      type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call set_inner_core_rotation(sph_bc_U%kr_in, sph_rj, ipol%base, &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
!       Input: ipol%base%i_vort, ipol%base%i_vort+2
!       Solution: ipol%diffusion%i_v_diffuse, 
!                 ipol%diffusion%i_v_diffuse+2,
!                 ipol%diffusion%i_v_diffuse+1
      if(ipol%diffusion%i_v_diffuse .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'const_sph_viscous_by_vort2'
        call const_sph_viscous_by_vort2(sph_rj, r_2nd,                  &
     &      sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,                     &
     &      leg%g_sph_rj, fl_prop%coef_diffuse,                         &
     &      ipol%base%i_velo, ipol%base%i_vort,                         &
     &      ipol%diffusion%i_v_diffuse, rj_fld)
      end if
!
!       Input:    ipol%base%i_vort, ipol%base%i_vort+2
!       Solution: ipol%diffusion%i_w_diffuse
!                 ipol%diffusion%i_w_diffuse+2,
!                 ipol%diffusion%i_w_diffuse+1
      if(ipol%diffusion%i_w_diffuse .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)'const_sph_vorticirty_diffusion'
        call const_sph_vorticirty_diffusion(sph_rj, r_2nd,              &
     &      sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,                     &
     &      leg%g_sph_rj, fl_prop%coef_diffuse,                         &
     &      ipol%base%i_vort, ipol%diffusion%i_w_diffuse, rj_fld)
      end if
!
      end subroutine update_after_vorticity_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine update_after_magne_sph(sph_rj, r_2nd,                  &
     &          cd_prop, sph_bc_B, leg, ipol, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(conductive_property), intent(in) :: cd_prop
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
!       Input:    ipol%base%i_current to ipol%base%i_current+2
!       Solution: ipol%diffusion%i_b_diffuse
!                to ipol%diffusion%i_b_diffuse+2
      if(ipol%diffusion%i_b_diffuse .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*) 'const_sph_mag_diffuse_by_j'
        call const_sph_mag_diffuse_by_j(sph_rj, r_2nd, sph_bc_B,        &
     &      leg%g_sph_rj, cd_prop%coef_diffuse,                         &
     &      ipol%base%i_magne, ipol%base%i_current,                     &
     &      ipol%diffusion%i_b_diffuse, rj_fld)
      end if
!
      end subroutine update_after_magne_sph
!
! -----------------------------------------------------------------------
!
      subroutine update_after_heat_sph(sph_rj, r_2nd, ht_prop,          &
     &          sph_bc_T, bcs_T, fdm2_center, leg, ipol, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: ht_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
!         Input: ipol%base%i_temp,  Solution: ipol%grad_fld%i_grad_temp
      if(iflag_debug .gt. 0)  write(*,*)                                &
     &           'const_radial_grad_temp', ipol%grad_fld%i_grad_temp
      if(ipol%grad_fld%i_grad_temp .gt. 0) then
        call const_radial_grad_scalar                                   &
     &     (sph_rj, r_2nd, sph_bc_T, bcs_T, fdm2_center, leg%g_sph_rj,  &
     &      ipol%base%i_temp, ipol%grad_fld%i_grad_temp, rj_fld)
      end if
!
!         Input: ipol%base%i_temp,  Solution: ipol%diffusion%i_t_diffuse
      if(ipol%diffusion%i_t_diffuse .gt. 0) then
        if(iflag_debug .gt. 0)  write(*,*)                              &
     &         'const_sph_scalar_diffusion', ipol%diffusion%i_t_diffuse
        call const_sph_scalar_diffusion                                 &
     &     (sph_rj, r_2nd, sph_bc_T, bcs_T, fdm2_center,                &
     &      leg%g_sph_rj, ht_prop%coef_diffuse,                         &
     &      ipol%base%i_temp, ipol%diffusion%i_t_diffuse, rj_fld)
      end if
!
      end subroutine update_after_heat_sph
!
! -----------------------------------------------------------------------
!
      subroutine update_after_composit_sph(sph_rj, r_2nd, cp_prop,      &
     &          sph_bc_C, bcs_C, fdm2_center, leg, ipol, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(sph_scalar_boundary_data), intent(in) :: bcs_C
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
!         Input: ipol%base%i_light
!         Solution: ipol%grad_fld%i_grad_composit
      if(ipol%grad_fld%i_grad_composit .gt. 0) then
        call const_radial_grad_scalar                                   &
     &     (sph_rj, r_2nd, sph_bc_C, bcs_C, fdm2_center, leg%g_sph_rj,  &
     &      ipol%base%i_light, ipol%grad_fld%i_grad_composit,           &
     &      rj_fld)
      end if
!
!         Input: ipol%base%i_light
!         Solution: ipol%diffusion%i_c_diffuse
      if(ipol%diffusion%i_c_diffuse .gt. 0) then
        if(iflag_debug .gt. 0)  write(*,*)                              &
     &         'const_sph_scalar_diffusion', ipol%diffusion%i_c_diffuse
        call const_sph_scalar_diffusion                                 &
     &     (sph_rj, r_2nd, sph_bc_C, bcs_C, fdm2_center,                &
     &      leg%g_sph_rj, cp_prop%coef_diffuse, ipol%base%i_light,      &
     &      ipol%diffusion%i_c_diffuse, rj_fld)
      end if
!
      end subroutine update_after_composit_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_vs_spectr(sph_rj, ipol, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint) :: j, k, inod
!
      write(150+my_rank,*) 'j, k, s_velo, ds_velo, t_velo'
      do j = 1, sph_rj%nidx_rj(2)
         do k = 1, sph_rj%nidx_rj(1)
          inod = j + (k-1) * sph_rj%nidx_rj(2)
          write(150+my_rank,'(2i16,1p20E25.15e3)') j, k,                &
     &        rj_fld%d_fld(inod,ipol%base%i_velo),                      &
     &        rj_fld%d_fld(inod,ipol%base%i_velo+1),                    &
     &        rj_fld%d_fld(inod,ipol%base%i_velo+2)
        end do
      end do
!
      end subroutine check_vs_spectr
!
! -----------------------------------------------------------------------
!
      subroutine check_ws_spectr(sph_rj, ipol, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint) :: j, k, inod
!
      write(150+my_rank,*) 'j, k, s_vort, ds_vort, t_vort'
      do j = 1, sph_rj%nidx_rj(2)
         do k = 1, sph_rj%nidx_rj(1)
          inod = j + (k-1) * sph_rj%nidx_rj(2)
          write(150+my_rank,'(2i16,1p20E25.15e3)') j, k,                &
     &        rj_fld%d_fld(inod,ipol%base%i_vort),                      &
     &        rj_fld%d_fld(inod,ipol%base%i_vort+1),                    &
     &        rj_fld%d_fld(inod,ipol%base%i_vort+2)
        end do
      end do
!
      end subroutine check_ws_spectr
!
! -----------------------------------------------------------------------
!
      end module sph_update_after_evolution
