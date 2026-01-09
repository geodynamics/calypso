!> @file  select_exp_viscosity_ICB.f90
!!      module select_exp_viscosity_ICB
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Select boundary condition routines for velocity
!!
!!@verbatim
!!      subroutine sel_ICB_sph_viscous_diffusion(sph_rj, r_2nd,         &
!!     &          sph_bc_U, fdm2_free_ICB, fdm1_fix_fld_ICB, g_sph_rj,  &
!!     &          coef_diffuse, is_velo, is_viscous,                    &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_velo, is_velo+2
!!        Address for solution: is_viscous, is_viscous+2, is_viscous+1
!!      subroutine sel_ICB_sph_vort_diffusion(sph_rj, r_2nd,            &
!!     &          sph_bc_U, fdm2_free_ICB, fdm1_fix_fld_ICB,            &
!!     &          g_sph_rj, coef_diffuse, is_vort, is_w_diffuse,        &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(fdm2_ICB_free_slip), intent(in) :: fdm2_free_ICB
!!        real(kind = kreal), intent(in) :: fdm1_fix_fld_ICB(0:1,2)
!!        integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!!        integer(kind = kint), intent(in) :: is_vort
!!        integer(kind = kint), intent(in) :: is_w_diffuse
!!        real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        real(kind = kreal), intent(in) :: coef_diffuse
!!        real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!!          Address for input:    is_vort, is_vort+2
!!          Address for solution: is_w_diffuse, is_w_diffuse+2,
!!                                is_w_diffuse+1
!!
!!      subroutine sel_ICB_sph_wtor_evo_RHS(dt, sph_rj, g_sph_rj, r_2nd,&
!!     &          fl_prop, sph_bc_U, ICB_Uspec, it_w_diffuse,           &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        real(kind = kreal), intent(in) :: dt
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_vector_BC_coef), intent(in) :: ICB_Uspec
!!        real(kind=kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!!        integer(kind = kint), intent(in) :: is_w_diffuse
!!        real(kind = kreal), intent(in) :: coef_diffuse
!!        real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!!@endverbatim
!!
      module select_exp_viscosity_ICB
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_boundary_sph_spectr
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_free_slip_ICB
!
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
      subroutine sel_ICB_sph_viscous_diffusion(sph_rj, r_2nd,           &
     &          sph_bc_U, fdm2_free_ICB, fdm1_fix_fld_ICB,              &
     &          g_sph_rj, coef_diffuse, is_velo, is_viscous,            &
     &          n_point, ntot_phys_rj, d_rj)
!
      use cal_inner_core_rotation
      use sph_exp_rigid_ICB
      use sph_exp_free_slip_ICB
      use sph_exp_fix_scalar_ICB
      use sph_exp_fix_vector_ICB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm2_ICB_free_slip), intent(in) :: fdm2_free_ICB
      real(kind = kreal), intent(in) :: fdm1_fix_fld_ICB(0:1,2)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: is_viscous
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: it_velo, it_viscous
!
!
      if(     (sph_bc_U%iflag_icb .eq. iflag_sph_fill_center)           &
     &   .or. (sph_bc_U%iflag_icb .eq. iflag_sph_filter_center)) then
        call cal_sph_nod_center_diffuse2                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(1,2), &
     &      g_sph_rj, r_2nd%fdm(2)%dmat, coef_diffuse,                  &
     &      n_point, d_rj(1,is_velo), d_rj(1,is_viscous))
        call cal_dsdr_sph_center_2                                      &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), r_2nd%fdm(1)%dmat,    &
     &      n_point, d_rj(1,is_viscous), d_rj(1,is_viscous+1))
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_diffuse2(sph_rj%nidx_rj(2),           &
     &      g_sph_rj, sph_bc_U%kr_in, sph_bc_U%r_ICB,                   &
     &      fdm2_free_ICB%dmat_vp, fdm2_free_ICB%dmat_vt, coef_diffuse, &
     &      n_point, d_rj(1,is_velo), d_rj(1,is_viscous))
      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field                &
     &   .or. sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
        call cal_sph_nod_icb_fixed_diffuse2(sph_rj%nidx_rj(2),          &
     &      g_sph_rj, sph_bc_U%kr_in, sph_bc_U%r_ICB,                   &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, n_point, d_rj(1,is_velo), d_rj(1,is_viscous))
!      else if(sph_bc_U%iflag_icb .eq. iflag_non_slip) then
      else
        call cal_sph_nod_icb_rigid_diffuse2(sph_rj%nidx_rj(2),          &
     &      g_sph_rj, sph_bc_U%kr_in, sph_bc_U%r_ICB,                   &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, n_point, d_rj(1,is_velo), d_rj(1,is_viscous))
      end if

      call cal_dsdr_sph_no_bc_in_2(sph_rj%nidx_rj(2),                   &
     &    sph_bc_U%kr_in, sph_bc_U%fdm2_fix_fld_ICB,                    &
     &    n_point, d_rj(1,is_viscous), d_rj(1,is_viscous+1))
!
!   Ovewrite rotatable inner core 
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        it_velo =     is_velo + 2
        it_viscous =  is_viscous + 2
        call cal_icore_viscous_drag_explicit(sph_bc_U%kr_in,            &
     &      fdm1_fix_fld_ICB, sph_rj, coef_diffuse,                     &
     &      it_velo, it_viscous, n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_ICB_sph_viscous_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_sph_vort_diffusion(sph_rj, r_2nd,              &
     &          sph_bc_U, fdm2_free_ICB, fdm1_fix_fld_ICB,              &
     &          g_sph_rj, coef_diffuse, is_vort, is_w_diffuse,          &
     &          n_point, ntot_phys_rj, d_rj)
!
      use cal_inner_core_rotation
      use sph_exp_rigid_ICB
      use sph_exp_free_slip_ICB
      use sph_exp_fix_scalar_ICB
      use sph_exp_fix_vector_ICB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm2_ICB_free_slip), intent(in) :: fdm2_free_ICB
      real(kind = kreal), intent(in) :: fdm1_fix_fld_ICB(0:1,2)
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
      if(     (sph_bc_U%iflag_icb .eq. iflag_sph_fill_center)           &
     &   .or. (sph_bc_U%iflag_icb .eq. iflag_sph_filter_center)) then
        call cal_sph_nod_center_diffuse2                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(1,2), &
     &      g_sph_rj, r_2nd%fdm(2)%dmat, coef_diffuse,                  &
     &      n_point, d_rj(1,is_vort), d_rj(1,is_w_diffuse))
        call cal_dsdr_sph_center_2                                      &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), r_2nd%fdm(1)%dmat,    &
     &      n_point, d_rj(1,is_w_diffuse), d_rj(1,is_w_diffuse+1))
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_w_diffuse2(sph_rj%nidx_rj(2),         &
     &      g_sph_rj, sph_bc_U%kr_in, sph_bc_U%r_ICB,                   &
     &      sph_bc_U%fdm2_fix_fld_ICB, fdm2_free_ICB%dmat_vt,           &
     &      coef_diffuse, n_point, d_rj(1,is_vort),                     &
     &      d_rj(1,is_w_diffuse))
!      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_non_slip) then
      else
        call cal_sph_nod_icb_fixed_diffuse2(sph_rj%nidx_rj(2),          &
     &      g_sph_rj, sph_bc_U%kr_in, sph_bc_U%r_ICB,                   &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, n_point, d_rj(1,is_vort),                     &
     &      d_rj(1,is_w_diffuse))
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_icore_viscous_drag_explicit                            &
     &     (sph_bc_U%kr_in, fdm1_fix_fld_ICB, sph_rj, coef_diffuse,     &
     &      is_vort, is_w_diffuse, n_point, ntot_phys_rj, d_rj)
      end if
!
      call cal_dsdr_sph_no_bc_in_2(sph_rj%nidx_rj(2),                   &
     &    sph_bc_U%kr_in, sph_bc_U%fdm2_fix_fld_ICB,                    &
     &    n_point, d_rj(1,is_w_diffuse), d_rj(1,is_w_diffuse+1))
!
      end subroutine sel_ICB_sph_vort_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_sph_wtor_evo_RHS(dt, sph_rj, g_sph_rj, r_2nd,  &
     &          fl_prop, sph_bc_U, ICB_Uspec, it_w_diffuse,             &
     &          n_point, ntot_phys_rj, d_rj)
!
      use t_physical_property
      use add_sph_ICB_w_diffuse
!
      real(kind = kreal), intent(in) :: dt
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_BC_coef), intent(in) :: ICB_Uspec
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: it_w_diffuse
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: coef_dvt
!
!
      if(      (sph_bc_U%iflag_cmb .eq. iflag_evolve_field)             &
     &    .or. (sph_bc_U%iflag_cmb .eq. iflag_fixed_field)) then
        if(fl_prop%coef_velo .eq. zero) then
          coef_dvt = one
        else
          coef_dvt = fl_prop%coef_exp * fl_prop%coef_diffuse * dt
        end if
!
        call add_sph_ICB_w_diffuse_by_dpdr(sph_rj%nidx_rj(1),           &
     &     sph_rj%nidx_rj(2), sph_rj%ar_1d_rj, g_sph_rj,                &
     &      sph_bc_U%kr_in, ICB_Uspec%Dp_BC, r_2nd%fdm(2)%dmat,         &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      coef_dvt, n_point, d_rj(1,it_w_diffuse))
!      else if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_non_slip) then
!      else
      end if
!
      end subroutine sel_ICB_sph_wtor_evo_RHS
!
! -----------------------------------------------------------------------
!
      end module select_exp_viscosity_ICB
