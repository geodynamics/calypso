!>@file   cal_sol_sph_MHD_crank.f90
!!@brief  module cal_sol_sph_MHD_crank
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update fields for MHD dynamo model
!!
!!@verbatim
!!      subroutine s_cal_sol_sph_MHD_crank                              &
!!     &         (dt, sph_rj, r_2nd, MHD_prop, sph_MHD_bc, leg,         &
!!     &          ipol, sph_MHD_mat, rj_fld)
!!      subroutine set_sph_field_to_start(sph_rj, r_2nd,                &
!!     &          MHD_prop, sph_MHD_bc, leg, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine check_vs_spectr(sph_rj, ipol, rj_fld)
!!      subroutine check_ws_spectr(sph_rj, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_sol_sph_MHD_crank
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
      subroutine s_cal_sol_sph_MHD_crank                                &
     &         (dt, sph_rj, r_2nd, MHD_prop, sph_MHD_bc, leg,           &
     &          ipol, sph_MHD_mat, rj_fld)
!
      use cal_sol_sph_fluid_crank
      use sph_radial_grad_4_velocity
      use sph_radial_grad_4_magne
      use sph_update_after_evolution
!
      real(kind = kreal), intent(in) :: dt
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
!
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(phys_data), intent(inout) :: rj_fld
!
!      integer(kind = kint) :: i, j, k, inod
!
!*-----  time evolution   -------------
!*
!      call check_ws_spectr(sph_rj, ipol, rj_fld)
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
!         Input:    ipol%base%i_vort to ipol%base%i_vort+2
!         Solution: ipol%base%i_velo to ipol%base%i_velo+2
        if (iflag_debug .gt. 0)                                         &
     &       write(*,*) 'cal_sol_velo_by_vort_sph_crank'
        call cal_sol_velo_by_vort_sph_crank                             &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      sph_MHD_mat%band_vp_evo, sph_MHD_mat%band_vt_evo,           &
     &      ipol, rj_fld)
        call const_grad_vp_and_vorticity                                &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      leg%g_sph_rj, ipol%base%i_velo, ipol%base%i_vort, rj_fld)
      end if
!
!  Input: ipol%base%i_temp,  Solution: ipol%base%i_temp
      if(iflag_debug.gt.0) write(*,*) 'cal_sol_scalar_sph_crank'
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        call cal_sol_scalar_sph_crank(dt, sph_rj,                       &
     &      MHD_prop%ht_prop, sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T,    &
     &      sph_MHD_mat%band_temp_evo, sph_MHD_mat%band_temp00_evo,     &
     &      ipol%base%i_temp, rj_fld, sph_MHD_mat%x00_w_center)
      end if
!
!  Input: ipol%base%i_light,  Solution: ipol%base%i_light
      if(iflag_debug.gt.0) write(*,*) 'cal_sol_scalar_sph_crank'
      if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        call cal_sol_scalar_sph_crank(dt, sph_rj,                       &
     &      MHD_prop%cp_prop, sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C,    &
     &      sph_MHD_mat%band_comp_evo, sph_MHD_mat%band_comp00_evo,     &
     &      ipol%base%i_light, rj_fld, sph_MHD_mat%x00_w_center)
      end if
!
!  Input:    ipol%base%i_magne, to ipol%base%i_magne+2
!  Solution: ipol%base%i_magne, to ipol%base%i_magne+2
      if(iflag_debug.gt.0) write(*,*) 'cal_sol_magne_sph_crank'
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call cal_sol_magne_sph_crank                                    &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, sph_MHD_bc%bcs_B,       &
     &      sph_MHD_mat%band_bp_evo, sph_MHD_mat%band_bt_evo,           &
     &      leg%g_sph_rj, ipol, rj_fld)
        call const_grad_bp_and_current                                  &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, sph_MHD_bc%bcs_B,       &
     &      leg%g_sph_rj, ipol%base%i_magne, ipol%base%i_current,       &
     &      rj_fld)
      end if
!
!*  ---- update after evolution ------------------
!      call check_vs_spectr(sph_rj, ipol, rj_fld)
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call update_after_vorticity_sph                                 &
     &     (sph_rj, r_2nd, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      leg, ipol, rj_fld)
      end if
!
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        call update_after_heat_sph(sph_rj, r_2nd, MHD_prop%ht_prop,     &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T,                      &
     &      sph_MHD_bc%fdm2_center, leg, ipol, rj_fld)
      end if
      if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        call update_after_composit_sph(sph_rj, r_2nd, MHD_prop%cp_prop, &
     &      sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C,                      &
     &      sph_MHD_bc%fdm2_center, leg, ipol, rj_fld)
      end if
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call update_after_magne_sph(sph_rj, r_2nd, MHD_prop%cd_prop,    &
     &      sph_MHD_bc%sph_bc_B, leg, ipol, rj_fld)
      end if
!
      end subroutine s_cal_sol_sph_MHD_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_field_to_start(sph_rj, r_2nd,                  &
     &          MHD_prop, sph_MHD_bc, leg, ipol, rj_fld)
!
      use sph_radial_grad_4_velocity
      use sph_radial_grad_4_magne
      use sph_update_after_evolution
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol%base%i_vort .gt. 0) then
        call const_grad_vp_and_vorticity                                &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      leg%g_sph_rj, ipol%base%i_velo, ipol%base%i_vort, rj_fld)
      end if
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug.gt.0) write(*,*) 'update_after_vorticity_sph'
        call update_after_vorticity_sph                                 &
     &     (sph_rj, r_2nd, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      leg, ipol, rj_fld)
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'update_after_heat_sph'
      call update_after_heat_sph(sph_rj, r_2nd, MHD_prop%ht_prop,       &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T,                        &
     &    sph_MHD_bc%fdm2_center, leg, ipol, rj_fld)
      if(iflag_debug.gt.0) write(*,*) 'update_after_composit_sph'
      call update_after_composit_sph(sph_rj, r_2nd, MHD_prop%cp_prop,   &
     &    sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C,                        &
     &    sph_MHD_bc%fdm2_center, leg, ipol, rj_fld)
!
      if(ipol%base%i_current .gt. 0) then
        call const_grad_bp_and_current                                  &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, sph_MHD_bc%bcs_B,       &
     &      leg%g_sph_rj, ipol%base%i_magne, ipol%base%i_current,       &
     &      rj_fld)
      end if
!
      call update_after_magne_sph(sph_rj, r_2nd, MHD_prop%cd_prop,      &
     &    sph_MHD_bc%sph_bc_B, leg, ipol, rj_fld)
!
      end subroutine set_sph_field_to_start
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
      end module cal_sol_sph_MHD_crank
