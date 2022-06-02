!> @file  select_exp_velocity_ICB.f90
!!      module select_exp_velocity_ICB
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Select boundary condition routines for velocity
!!
!!@verbatim
!!      subroutine sel_ICB_grad_vp_and_vorticity(sph_rj, r_2nd,         &
!!     &          sph_bc_U, ICB_Uspec, fdm2_free_ICB, g_sph_rj,         &
!!     &          is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_velo, is_velo+2
!!        Address for solution: is_velo+1, 
!!                              is_vort, is_vort+2, is_vort+1
!!      subroutine sel_ICB_grad_poloidal_moment                         &
!!     &         (sph_rj, r_2nd, sph_bc_U, ICB_Uspec, fdm2_free_ICB,    &
!!     &          is_fld, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_fld, is_fld+2
!!        Address for solution: is_fld+1
!!
!!      subroutine sel_ICB_sph_vorticity                                &
!!     &         (sph_rj, r_2nd, sph_bc_U, fdm2_free_ICB, g_sph_rj,     &
!!     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_fld, is_fld+2
!!        Address for solution: is_rot, is_rot+2, is_rot+1
!!
!!      subroutine sel_ICB_sph_viscous_diffusion(sph_rj, r_2nd,         &
!!     &          sph_bc_U, fdm2_free_ICB, g_sph_rj, coef_diffuse,      &
!!     &          is_velo, is_viscous, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_velo, is_velo+2
!!        Address for solution: is_viscous, is_viscous+2, is_viscous+1
!!      subroutine sel_ICB_sph_vort_diffusion(sph_rj, r_2nd,            &
!!     &          sph_bc_U, fdm2_free_ICB, g_sph_rj, coef_diffuse,      &
!!     &          is_vort, is_w_diffuse, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_vort, is_vort+2
!!        Address for solution: is_w_diffuse, is_w_diffuse+2,
!!                              is_w_diffuse+1
!!          type(sph_rj_grid), intent(in) :: sph_rj
!!          type(sph_boundary_type), intent(in) :: sph_bc_U
!!          type(sph_vector_BC_coef), intent(in) :: ICB_Uspec
!!          type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
!!          type(fdm_matrices), intent(in) :: r_2nd
!!@endverbatim
!!
      module select_exp_velocity_ICB
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
      use set_sph_exp_fix_vector_ICB
      use set_sph_exp_rigid_ICB
      use set_sph_exp_free_ICB
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
      subroutine sel_ICB_grad_vp_and_vorticity(sph_rj, r_2nd,           &
     &          sph_bc_U, ICB_Uspec, fdm2_free_ICB, g_sph_rj,           &
     &          is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_BC_coef), intent(in) :: ICB_Uspec
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_velo, is_vort
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call sph_center_fld_and_curl                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(1,2), &
     &      g_sph_rj, r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat,             &
     &      is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_v_and_w                               &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_in,                          &
     &      fdm2_free_ICB%dmat_vp, fdm2_free_ICB%dmat_vt,               &
     &      is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%idx_rj_degree_one,        &
     &      sph_rj%nidx_rj, sph_bc_U%kr_in, sph_bc_U%r_ICB,             &
     &      sph_rj%radius_1d_rj_r, is_velo,                             &
     &      n_point, ntot_phys_rj, d_rj)
        call cal_sph_nod_icb_rigid_rot2(sph_rj%nidx_rj(2), g_sph_rj,    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_field                 &
     &   .or. sph_bc_U%iflag_icb .eq. iflag_evolve_field) then
        call cal_sph_nod_icb_rigid_vect(sph_rj%nidx_rj,                 &
     &      sph_rj%idx_gl_1d_rj_j, sph_rj%radius_1d_rj_r,               &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      ICB_Uspec%Vp_BC, ICB_Uspec%Dp_BC, ICB_Uspec%Vt_BC,          &
     &      is_velo, n_point, ntot_phys_rj, d_rj)
        call cal_sph_nod_icb_fixed_rot2(sph_rj%nidx_rj(2), g_sph_rj,    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_velo) then
      else
        call cal_sph_nod_icb_rigid_velo2                                &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_in, is_velo,                 &
     &      n_point, ntot_phys_rj, d_rj)
        call cal_sph_nod_icb_rigid_rot2(sph_rj%nidx_rj(2), g_sph_rj,    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_velo, is_vort, n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_ICB_grad_vp_and_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_grad_poloidal_moment                           &
     &         (sph_rj, r_2nd, sph_bc_U, ICB_Uspec, fdm2_free_ICB,      &
     &          is_fld, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_BC_coef), intent(in) :: ICB_Uspec
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_fld
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_dsdr_sph_center_2                                      &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), r_2nd%fdm(1)%dmat,    &
     &      is_fld, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_vpol2                                 &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_in, fdm2_free_ICB%dmat_vp,   &
     &      is_fld, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%idx_rj_degree_one,        &
     &      sph_rj%nidx_rj, sph_bc_U%kr_in, sph_bc_U%r_ICB,             &
     &      sph_rj%radius_1d_rj_r, is_fld, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_field                 &
     &   .or. sph_bc_U%iflag_icb .eq. iflag_evolve_field) then
        call cal_sph_nod_icb_rigid_vect(sph_rj%nidx_rj,                 &
     &      sph_rj%idx_gl_1d_rj_j, sph_rj%radius_1d_rj_r,               &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      ICB_Uspec%Vp_BC, ICB_Uspec%Dp_BC, ICB_Uspec%Vt_BC,          &
     &      is_fld, n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_velo) then
      else
        call cal_sph_nod_icb_rigid_velo2                                &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_in, is_fld,                  &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_ICB_grad_poloidal_moment
!
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_sph_vorticity                                  &
     &         (sph_rj, r_2nd, sph_bc_U, fdm2_free_ICB, g_sph_rj,       &
     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_rot2                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(1,2), &
     &      g_sph_rj, r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat,             &
     &      is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_rot2(sph_rj%nidx_rj(2), g_sph_rj,     &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      fdm2_free_ICB%dmat_vp, fdm2_free_ICB%dmat_vt,               &
     &      is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field                &
     &   .or. sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
        call cal_sph_nod_icb_fixed_rot2(sph_rj%nidx_rj(2), g_sph_rj,    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_velo) then
      else
        call cal_sph_nod_icb_rigid_rot2(sph_rj%nidx_rj(2), g_sph_rj,    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_ICB_sph_vorticity
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_sph_viscous_diffusion(sph_rj, r_2nd,           &
     &          sph_bc_U, fdm2_free_ICB, g_sph_rj, coef_diffuse,        &
     &          is_velo, is_viscous, n_point, ntot_phys_rj, d_rj)
!
      use cal_sph_exp_fixed_scalar
      use cal_inner_core_rotation
      use set_sph_exp_fix_vector_CMB
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: is_viscous
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: it_velo, it_viscous, ids_viscous
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(1,2), &
     &      g_sph_rj, r_2nd%fdm(2)%dmat, coef_diffuse,                  &
     &      is_velo, is_viscous, n_point, ntot_phys_rj, d_rj)
        call cal_dsdr_sph_center_2                                      &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), r_2nd%fdm(1)%dmat,    &
     &      is_viscous, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_diffuse2                              &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      fdm2_free_ICB%dmat_vp, fdm2_free_ICB%dmat_vt,               &
     &      coef_diffuse, is_velo, is_viscous,                          &
     &      n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field                &
     &   .or. sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
        call cal_sph_nod_icb_fixed_diffuse2(sph_rj%nidx_rj(2),          &
     &      g_sph_rj, sph_bc_U%kr_in, sph_bc_U%r_ICB,                   &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, is_velo, is_viscous,                          &
     &      n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_velo) then
      else
        call cal_sph_nod_icb_rigid_diffuse2                             &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, is_velo, is_viscous,                          &
     &      n_point, ntot_phys_rj, d_rj)
      end if

      ids_viscous = is_viscous + 1
      call cal_dsdr_sph_no_bc_in_2(sph_rj%nidx_rj(2), sph_bc_U%kr_in,   &
     &    sph_bc_U%fdm2_fix_fld_ICB, is_viscous, ids_viscous,           &
     &    n_point, ntot_phys_rj, d_rj)
!
!   Ovewrite rotatable inner core 
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        it_velo =     is_velo + 2
        it_viscous =  is_viscous + 2
        call cal_icore_viscous_drag_explicit(sph_bc_U%kr_in,            &
     &      sph_bc_U%fdm1_fix_fld_ICB, sph_rj, coef_diffuse,            &
     &      it_velo, it_viscous, n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_ICB_sph_viscous_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_sph_vort_diffusion(sph_rj, r_2nd,              &
     &          sph_bc_U, fdm2_free_ICB, g_sph_rj, coef_diffuse,        &
     &          is_vort, is_w_diffuse, n_point, ntot_phys_rj, d_rj)
!
      use cal_sph_exp_fixed_scalar
      use cal_inner_core_rotation
      use set_sph_exp_fix_vector_CMB
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_vort
      integer(kind = kint), intent(in) :: is_w_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(1,2), &
     &      g_sph_rj, r_2nd%fdm(2)%dmat, coef_diffuse,                  &
     &      is_vort, is_w_diffuse, n_point, ntot_phys_rj, d_rj)
        call cal_dsdr_sph_center_2                                      &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), r_2nd%fdm(1)%dmat,    &
     &      is_w_diffuse, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_w_diffuse2                            &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB,                             &
     &      sph_bc_U%fdm2_fix_fld_ICB, fdm2_free_ICB%dmat_vt,           &
     &      coef_diffuse, is_vort, is_w_diffuse,                        &
     &      n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_velo) then
      else
        call cal_sph_nod_icb_fixed_diffuse2(sph_rj%nidx_rj(2),          &
     &      g_sph_rj, sph_bc_U%kr_in, sph_bc_U%r_ICB,                   &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, is_vort, is_w_diffuse,                        &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_icore_viscous_drag_explicit                            &
     &     (sph_bc_U%kr_in, sph_bc_U%fdm1_fix_fld_ICB, sph_rj,          &
     &      coef_diffuse, is_vort, is_w_diffuse,                        &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_ICB_sph_vort_diffusion
!
! -----------------------------------------------------------------------
!
      end module select_exp_velocity_ICB
