!> @file  select_exp_velocity_bc.f90
!!      module select_exp_velocity_bc
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Select boundary condition routines for velocity
!!
!!@verbatim
!!      subroutine sel_bc_grad_vp_and_vorticity(is_velo, is_vort)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: idpdr%i_velo, ipol%i_vort, itor%i_vort, idpdr%i_vort
!!      subroutine sel_bc_grad_poloidal_moment(is_fld)
!!        Input:    is_fld, is_fld+2
!!        Solution: is_fld+1
!!
!!      subroutine sel_bc_sph_vorticity(sph_bc_U, is_fld, is_rot)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: ipol%i_vort, itor%i_vort, idpdr%i_vort
!!
!!      subroutine sel_bc_sph_viscous_diffusion(sph_bc_U, coef_diffuse, &
!!     &          is_velo, it_velo, is_viscous, ids_viscous)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse
!!      subroutine sel_bc_sph_vort_diffusion(sph_bc_U,                  &
!!     &          coef_diffuse, is_vort, is_w_diffuse, ids_w_diffuse)
!!        Input:    ipol%i_vort, itor%i_vort
!!        Solution: ipol%i_w_diffuse, itor%i_w_diffuse, idpdr%i_w_diffuse
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!@param sph_bc_U  Structure for basic boundary condition parameters
!!                 for velocity
!!
!!@param coef_diffuse   Diffusion coefficient
!!
!!@param is_fld      Spherical hermonics data address for input vector
!!@param is_rot      Spherical hermonics data address for curl of field
!!@param is_grad     Spherical hermonics data address for gradient
!!@param is_velo     Spherical hermonics data address
!!                   for poloidal velocity
!!@param is_velo     Spherical hermonics data address
!!                   for toroidal velocity (or poloical vorticity)
!!@param is_vort     Spherical hermonics data address
!!                   for poloidal vorticity
!!@param is_press    Spherical hermonics data address
!!                   for pressure
!!@param is_viscous  Spherical hermonics data address
!!                   for poloidal visous diffusion
!!@param ids_viscous Spherical hermonics data address
!!                   for radial derivative of poloidal visous diffusion
!!@param is_w_diffuse    Spherical hermonics data address
!!                       for poloidal vorticity diffusion
!!@param ids_w_diffuse Spherical hermonics data address
!!                   for radial derivative of vorticity diffusion
!
      module select_exp_velocity_bc
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
!
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use set_sph_exp_nod_center
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_bc_grad_vp_and_vorticity(is_velo, is_vort)
!
      use m_boundary_params_sph_MHD
!
      integer(kind = kint), intent(in) :: is_velo, is_vort
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call sph_center_fld_and_curl(is_velo, is_vort)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_v_and_w(nidx_rj(2), sph_bc_U%kr_in,   &
     &      fdm2_free_vp_ICB, fdm2_free_vt_ICB, is_velo, is_vort)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (idx_rj_degree_zero, idx_rj_degree_one, nidx_rj(2),          &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, is_velo)
        call cal_sph_nod_icb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_velo, is_vort)
      else
        call cal_sph_nod_icb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, is_velo)
        call cal_sph_nod_icb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_velo, is_vort)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_v_and_w(nidx_rj(2), sph_bc_U%kr_out,  &
     &      fdm2_free_vp_CMB, fdm2_free_vt_CMB, is_velo, is_vort)
      else
        call cal_sph_nod_cmb_rigid_v_and_w                              &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      vt_CMB_bc, is_velo, is_vort)
      end if
!
      end subroutine sel_bc_grad_vp_and_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine sel_bc_grad_poloidal_moment(is_fld)
!
      use m_boundary_params_sph_MHD
!
      integer(kind = kint), intent(in) :: is_fld
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_dsdr_sph_center_2(is_fld)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_vpol2(nidx_rj(2), sph_bc_U%kr_in,     &
     &      fdm2_free_vp_ICB, is_fld)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (idx_rj_degree_zero, idx_rj_degree_one, nidx_rj(2),          &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, is_fld)
      else
        call cal_sph_nod_icb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, is_fld)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_vpol2(nidx_rj(2), sph_bc_U%kr_out,    &
     &      fdm2_free_vp_CMB, is_fld)
      else
        call cal_sph_nod_cmb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, vt_CMB_bc, is_fld)
      end if
!
      end subroutine sel_bc_grad_poloidal_moment
!
! -----------------------------------------------------------------------
!
      subroutine sel_bc_sph_vorticity(sph_bc_U, is_fld, is_rot)
!
      use t_boundary_params_sph_MHD
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_fld, is_rot
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_rot2(is_fld, is_rot)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_rot2                                  &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      fdm2_free_vp_ICB, fdm2_free_vt_ICB, is_fld, is_rot)
      else
        call cal_sph_nod_icb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_fld, is_rot)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_rot2                                  &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      fdm2_free_vp_CMB, fdm2_free_vt_CMB, is_fld, is_rot)
      else
        call cal_sph_nod_cmb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      is_fld, is_rot)
      end if
!
      end subroutine sel_bc_sph_vorticity
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_bc_sph_viscous_diffusion(sph_bc_U, coef_diffuse,   &
     &          is_velo, it_velo, is_viscous, ids_viscous)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_inner_core_rotation
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_velo, it_velo
      integer(kind = kint), intent(in) :: is_viscous, ids_viscous
      real(kind = kreal), intent(in) :: coef_diffuse
!
      integer(kind = kint) :: it_diffuse
!
!
      it_diffuse =  is_viscous + 2
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2(coef_diffuse,                  &
     &      is_velo, is_viscous)
        call cal_dsdr_sph_center_2(is_viscous)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_diffuse2                              &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      fdm2_free_vp_ICB, fdm2_free_vt_ICB,                         &
     &      coef_diffuse, is_velo, is_viscous)
      else
        call cal_sph_nod_icb_rigid_diffuse2                             &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, is_velo, is_viscous)
      end if
      call cal_dsdr_sph_no_bc_in_2(nidx_rj(2),                          &
     &    sph_bc_U%kr_in, sph_bc_U%fdm2_fix_fld_ICB,                    &
     &    is_viscous, ids_viscous)
!
!   Ovewrite rotatable inner core 
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_icore_viscous_drag_explicit(sph_bc_U%kr_in,            &
     &      coef_diffuse, it_velo, it_diffuse)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_diffuse2                              &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      fdm2_free_vp_CMB, fdm2_free_vt_CMB,                         &
     &      coef_diffuse, is_velo, is_viscous)
      else
        call cal_sph_nod_cmb_rigid_diffuse2                             &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_velo, is_viscous)
      end if
      call cal_dsdr_sph_no_bc_out_2(nidx_rj(2),                         &
     &    sph_bc_U%kr_out, sph_bc_U%fdm2_fix_fld_CMB,                   &
     &    is_viscous, ids_viscous)
!
      end subroutine sel_bc_sph_viscous_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_bc_sph_vort_diffusion(sph_bc_U,                    &
     &          coef_diffuse, is_vort, is_w_diffuse, ids_w_diffuse)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_inner_core_rotation
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_vort
      integer(kind = kint), intent(in) :: is_w_diffuse, ids_w_diffuse
      real(kind = kreal), intent(in) :: coef_diffuse
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2(coef_diffuse,                  &
     &      is_vort, is_w_diffuse)
        call cal_dsdr_sph_center_2(is_w_diffuse)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_w_diffuse2                            &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, fdm2_free_vt_ICB,                &
     &      coef_diffuse, is_vort, is_w_diffuse)
      else
        call cal_sph_nod_icb_rgd_w_diffuse2(nidx_rj(2),                 &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_fld_ICB,  &
     &      coef_diffuse, is_vort, is_w_diffuse)
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_icore_viscous_drag_explicit(sph_bc_U%kr_in,            &
     &      coef_diffuse, is_vort, is_w_diffuse)
      end if
!
      call cal_dsdr_sph_no_bc_in_2(nidx_rj(2),                          &
     &    sph_bc_U%kr_in, sph_bc_U%fdm2_fix_fld_ICB,                    &
     &    is_w_diffuse, ids_w_diffuse)
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_w_diffuse2                            &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      sph_bc_U%fdm2_fix_fld_CMB, fdm2_free_vt_CMB,                &
     &      coef_diffuse, is_vort, is_w_diffuse)
      else
        call cal_sph_nod_cmb_rgd_w_diffuse2(nidx_rj(2),                 &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_fld_CMB, &
     &      coef_diffuse, is_vort, is_w_diffuse)
      end if
!
      call cal_dsdr_sph_no_bc_out_2(nidx_rj(2),                         &
     &    sph_bc_U%kr_out, sph_bc_U%fdm2_fix_fld_CMB,                   &
     &    is_w_diffuse, ids_w_diffuse)
!
      end subroutine sel_bc_sph_vort_diffusion
!
! -----------------------------------------------------------------------
!
      end module select_exp_velocity_bc
