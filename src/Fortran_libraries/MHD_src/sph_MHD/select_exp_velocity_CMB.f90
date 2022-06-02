!> @file  select_exp_velocity_CMB.f90
!!      module select_exp_velocity_CMB
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Select boundary condition routines for velocity
!!
!!@verbatim
!!      subroutine sel_CMB_grad_vp_and_vorticity                        &
!!     &         (sph_rj, sph_bc_U, CMB_Uspec, fdm2_free_CMB, g_sph_rj, &
!!     &          is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_velo
!!        Address for solution: is_velo+1, 
!!                              is_vort, is_vort+2, is_vort+1
!!      subroutine sel_CMB_grad_poloidal_moment                         &
!!     &         (sph_rj, sph_bc_U, CMB_Uspec, fdm2_free_CMB,           &
!!     &          is_fld, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_fld, is_fld+2
!!        Address for solution: is_fld+1
!!
!!      subroutine sel_CMB_sph_vorticity                                &
!!     &         (sph_rj, sph_bc_U, fdm2_free_CMB, g_sph_rj,            &
!!     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_fld, is_fld+2
!!        Address for solution: is_rot, is_rot+2, is_rot+1
!!
!!      subroutine sel_CMB_sph_viscous_diffusion(sph_rj, sph_bc_U,      &
!!     &          fdm2_free_CMB, g_sph_rj, coef_diffuse,                &
!!     &          is_velo, is_viscous, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_velo, is_velo+2, is_velo+1
!!        Address for solution: is_viscous, is_viscous+2, is_viscous+1
!!      subroutine sel_CMB_sph_vort_diffusion(sph_rj, sph_bc_U,         &
!!     &          fdm2_free_CMB, g_sph_rj, coef_diffuse,                &
!!     &          is_vort, is_w_diffuse, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_vort, is_vort+2
!!        Address for solution: is_w_diffuse, is_w_diffuse+2, 
!!                              is_w_diffuse+1
!!          type(sph_rj_grid), intent(in) :: sph_rj
!!          type(sph_boundary_type), intent(in) :: sph_bc_U
!!          type(sph_vector_BC_coef), intent(in) :: CMB_Uspec
!!          type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
!!          type(fdm_matrices), intent(in) :: r_2nd
!!@endverbatim
!!
      module select_exp_velocity_CMB
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_boundary_sph_spectr
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_CMB
      use set_sph_exp_fix_vector_CMB
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_grad_vp_and_vorticity                          &
     &         (sph_rj, sph_bc_U, CMB_Uspec, fdm2_free_CMB, g_sph_rj,   &
     &          is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_BC_coef), intent(in) :: CMB_Uspec
      type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_velo, is_vort
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_v_and_w                               &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_out,                         &
     &      fdm2_free_CMB%dmat_vp, fdm2_free_CMB%dmat_vt,               &
     &      is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_field                 &
     &   .or. sph_bc_U%iflag_cmb .eq. iflag_evolve_field) then
        call cal_sph_nod_cmb_rigid_vect                                 &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_out,                         &
     &      CMB_Uspec%Vp_BC, CMB_Uspec%Dp_BC, CMB_Uspec%Vt_BC,          &
     &      is_velo, n_point, ntot_phys_rj, d_rj)
        call cal_sph_nod_cmb_fixed_rot2(sph_rj%nidx_rj(2), g_sph_rj,    &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB,                            &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_velo) then
      else
        call cal_sph_nod_cmb_rigid_v_and_w                              &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_out,                         &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_CMB_grad_vp_and_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_grad_poloidal_moment                           &
     &         (sph_rj, sph_bc_U, CMB_Uspec, fdm2_free_CMB,             &
     &          is_fld, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_BC_coef), intent(in) :: CMB_Uspec
      type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_fld
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_vpol2                                 &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_out, fdm2_free_CMB%dmat_vp,  &
     &      is_fld, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_field                 &
     &   .or. sph_bc_U%iflag_cmb .eq. iflag_evolve_field) then
        call cal_sph_nod_cmb_rigid_vect                                 &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_out,                         &
     &      CMB_Uspec%Vp_BC, CMB_Uspec%Dp_BC, CMB_Uspec%Vt_BC,          &
     &      is_fld, n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_velo) then
      else
        call cal_sph_nod_cmb_rigid_velo                                 &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_out, is_fld,                 &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_CMB_grad_poloidal_moment
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_vorticity                                  &
     &         (sph_rj, sph_bc_U, fdm2_free_CMB, g_sph_rj,              &
     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_rot2(sph_rj%nidx_rj(2), g_sph_rj,     &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB,                            &
     &      fdm2_free_CMB%dmat_vp, fdm2_free_CMB%dmat_vt,               &
     &      is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_cmb .eq. iflag_evolve_field                &
     &   .or. sph_bc_U%iflag_cmb .eq. iflag_fixed_field) then
        call cal_sph_nod_cmb_fixed_rot2(sph_rj%nidx_rj(2), g_sph_rj,    &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB,                            &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_velo) then
      else
        call cal_sph_nod_cmb_rigid_rot2                                 &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_out,                         &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_CMB_sph_vorticity
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_viscous_diffusion(sph_rj, sph_bc_U,        &
     &          fdm2_free_CMB, g_sph_rj, coef_diffuse,                  &
     &          is_velo, is_viscous, n_point, ntot_phys_rj, d_rj)
!
      use cal_sph_exp_fixed_scalar
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: is_viscous
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: ids_viscous
!
!
      ids_viscous = is_viscous + 1
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_diffuse2(sph_rj%nidx_rj(2), g_sph_rj, &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB,                            &
     &      fdm2_free_CMB%dmat_vp, fdm2_free_CMB%dmat_vt,               &
     &      coef_diffuse, is_velo, is_viscous,                          &
     &      n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_cmb .eq. iflag_evolve_field                &
     &   .or. sph_bc_U%iflag_cmb .eq. iflag_fixed_field) then
        call cal_sph_nod_cmb_fixed_diffuse2                             &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB,                            &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_velo, is_viscous,                          &
     &      n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_velo) then
      else
        call cal_sph_nod_cmb_rigid_diffuse2                             &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_out,                         &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_velo, is_viscous,                          &
     &      n_point, ntot_phys_rj, d_rj)
      end if
      call cal_dsdr_sph_no_bc_out_2(sph_rj%nidx_rj(2), sph_bc_U%kr_out, &
     &    sph_bc_U%fdm2_fix_fld_CMB, is_viscous, ids_viscous,           &
     &    n_point, ntot_phys_rj, d_rj)
!
      end subroutine sel_CMB_sph_viscous_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_vort_diffusion(sph_rj, sph_bc_U,           &
     &          fdm2_free_CMB, g_sph_rj, coef_diffuse,                  &
     &          is_vort, is_w_diffuse, n_point, ntot_phys_rj, d_rj)
!
      use cal_sph_exp_fixed_scalar
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_vort
      integer(kind = kint), intent(in) :: is_w_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: ids_w_diffuse
!
!
      ids_w_diffuse = is_w_diffuse + 1
!
      call cal_dsdr_sph_no_bc_in_2(sph_rj%nidx_rj(2), sph_bc_U%kr_in,   &
     &    sph_bc_U%fdm2_fix_fld_ICB, is_w_diffuse, ids_w_diffuse,       &
     &    n_point, ntot_phys_rj, d_rj)
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_w_diffuse2                            &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_fld_CMB, &
     &      fdm2_free_CMB%dmat_vt, coef_diffuse, is_vort, is_w_diffuse, &
     &      n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_U%iflag_cmb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_velo) then
      else
        call cal_sph_nod_cmb_fixed_diffuse2(sph_rj%nidx_rj(2),          &
     &      g_sph_rj, sph_bc_U%kr_out, sph_bc_U%r_CMB,                  &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_vort, is_w_diffuse,                        &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      call cal_dsdr_sph_no_bc_out_2(sph_rj%nidx_rj(2), sph_bc_U%kr_out, &
     &    sph_bc_U%fdm2_fix_fld_CMB, is_w_diffuse, ids_w_diffuse,       &
     &    n_point, ntot_phys_rj, d_rj)
!
      end subroutine sel_CMB_sph_vort_diffusion
!
! -----------------------------------------------------------------------
!
      end module select_exp_velocity_CMB
