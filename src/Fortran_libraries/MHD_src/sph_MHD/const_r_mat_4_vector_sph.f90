!>@file   const_r_mat_4_vector_sph.f90
!!@brief  module const_r_mat_4_vector_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of vector fields
!!
!!@verbatim
!!      subroutine const_radial_mat_vort_2step
!!      subroutine const_radial_mat_4_magne_sph
!!@endverbatim
!
      module const_r_mat_4_vector_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_t_int_parameter
      use m_spheric_param_smp
      use m_spheric_parameter
      use m_radial_matrices_sph
      use m_physical_property
      use m_ludcmp_3band
      use set_radial_mat_sph
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_vort_2step
!
      use m_boundary_params_sph_MHD
      use m_coef_fdm_to_center
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use m_ludcmp_band
      use set_sph_scalar_mat_bc
      use cal_inner_core_rotation
      use center_sph_matrices
      use mat_product_3band_mul
!
      integer(kind = kint) :: ip, jst, jed, j
      integer(kind = kint) :: ierr
      real(kind = kreal) :: coef_dvt, coef_p
!
!
      if(coef_d_velo .eq. zero) then
        coef_dvt = one
        coef_p = - coef_press
        call set_unit_mat_4_poisson(nidx_rj(1), nidx_rj(2),             &
     &      sph_bc_U%kr_in, sph_bc_U%kr_out, vt_evo_mat)
        call set_unit_mat_4_poisson(nidx_rj(1), nidx_rj(2),             &
     &      sph_bc_U%kr_in, sph_bc_U%kr_out, wt_evo_mat)
      else
        coef_dvt = coef_imp_v * coef_d_velo * dt
        coef_p = - coef_press
        call set_unit_mat_4_time_evo(nidx_rj(1), nidx_rj(2),            &
     &      vt_evo_mat)
        call set_unit_mat_4_time_evo(nidx_rj(1), nidx_rj(2),            &
     &      wt_evo_mat)
      end if
!
      call set_unit_mat_4_poisson(nidx_rj(1), nidx_rj(2),               &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, vs_poisson_mat)
      call set_unit_mat_4_poisson(nidx_rj(1), nidx_rj(2),               &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, p_poisson_mat)
!
      call add_vector_poisson_mat_sph(nidx_rj(1), nidx_rj(2),           &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, coef_dvt, vt_evo_mat)
      call add_vector_poisson_mat_sph(nidx_rj(1), nidx_rj(2),           &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, coef_dvt, wt_evo_mat)
      call add_vector_poisson_mat_sph(nidx_rj(1), nidx_rj(2),           &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, one, vs_poisson_mat)
      call add_scalar_poisson_mat_sph(nidx_rj(1), nidx_rj(2),           &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, coef_p, p_poisson_mat)
!
!   Boundary condition for ICB
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call add_scalar_poisson_mat_filled(idx_rj_degree_zero,          &
     &      nidx_rj(1), nidx_rj(2),  sph_bc_U%r_ICB,                    &
     &      fdm2_fix_fld_center, fdm2_fix_dr_center,                    &
     &      coef_p, p_poisson_mat)
        call add_vector_poisson_mat_center(nidx_rj(1), nidx_rj(2),      &
     &      sph_bc_U%r_ICB, fdm2_fix_fld_center, coef_dvt, vt_evo_mat)
        call add_vector_poisson_mat_center(nidx_rj(1), nidx_rj(2),      &
     &      sph_bc_U%r_ICB, fdm2_fix_fld_center, coef_dvt, wt_evo_mat)
        call add_vector_poisson_mat_center(nidx_rj(1), nidx_rj(2),      &
     &      sph_bc_U%r_ICB, fdm2_fix_fld_center, one, vs_poisson_mat)
      else
        call add_fix_flux_icb_poisson_mat(nidx_rj(1), nidx_rj(2),       &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB,   &
     &      coef_dvt, wt_evo_mat)
        call add_icb_scalar_poisson_mat(nidx_rj(1), nidx_rj(2),         &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB,   &
     &      coef_p, p_poisson_mat)
!
        if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
          call add_fix_flux_icb_poisson_mat(nidx_rj(1), nidx_rj(2),     &
     &        sph_bc_U%kr_in, sph_bc_U%r_ICB, fdm2_free_vt_ICB,         &
     &        coef_dvt, vt_evo_mat)
          call add_fix_flux_icb_poisson_mat(nidx_rj(1), nidx_rj(2),     &
     &        sph_bc_U%kr_in, sph_bc_U%r_ICB, fdm2_free_vp_ICB,         &
     &        one, vs_poisson_mat)
        else
          call set_fix_fld_icb_poisson_mat(nidx_rj(1), nidx_rj(2),      &
     &        sph_bc_U%kr_in, vt_evo_mat)
          call add_fix_flux_icb_poisson_mat(nidx_rj(1), nidx_rj(2),     &
     &        sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB, &
     &        one, vs_poisson_mat)
        end if
      end if
!
!   Overwrite rotation for inner core
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call set_icore_viscous_matrix                                   &
     &     (sph_bc_U%kr_in, sph_bc_U%fdm1_fix_fld_ICB)
      end if
!
!   Boundary condition for CMB
!
      call add_fix_flux_cmb_poisson_mat(nidx_rj(1), nidx_rj(2),         &
     &    sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,    &
     &    coef_dvt, wt_evo_mat)
      call add_cmb_scalar_poisson_mat(nidx_rj(1), nidx_rj(2),           &
     &    sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,    &
     &    coef_p, p_poisson_mat)
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call add_fix_flux_cmb_poisson_mat(nidx_rj(1), nidx_rj(2),       &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, fdm2_free_vt_CMB,          &
     &      coef_dvt, vt_evo_mat)
        call add_fix_flux_cmb_poisson_mat(nidx_rj(1), nidx_rj(2),       &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, fdm2_free_vp_CMB,          &
     &      one, vs_poisson_mat)
      else
        call set_fix_fld_cmb_poisson_mat(nidx_rj(1), nidx_rj(2),        &
     &      sph_bc_U%kr_out, vt_evo_mat)
        call add_fix_flux_cmb_poisson_mat(nidx_rj(1), nidx_rj(2),       &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,  &
     &      one, vs_poisson_mat)
      end if
!
!
      call cal_mat_product_3band_mul(nidx_rj(1), nidx_rj(2),            &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, wt_evo_mat, vs_poisson_mat,  &
     &    vp_evo_mat)
!
      if(i_debug .eq. iflag_full_msg)                                   &
     &          call check_vorticity_matrices_sph(my_rank)
!
!$omp parallel do private(jst,jed,j)
      do ip = 1, np_smp
        jst = idx_rj_smp_stack(ip-1,2) + 1
        jed = idx_rj_smp_stack(ip,  2)
        do j = jst, jed
          call ludcmp_band(nidx_rj(1), ifive, vp_evo_mat(1,1,j),        &
     &        vp_evo_lu(1,1,j) ,i_vp_pivot(1,j), vp_evo_det(1,j))
          call ludcmp_3band(nidx_rj(1), vt_evo_mat(1,1,j),              &
     &        i_vt_pivot(1,j), ierr, vt_evo_lu(1,1,j), vt_evo_det(1,j))
          call ludcmp_3band(nidx_rj(1), wt_evo_mat(1,1,j),              &
     &        i_wt_pivot(1,j), ierr, wt_evo_lu(1,1,j), wt_evo_det(1,j))
          call ludcmp_3band(nidx_rj(1), vs_poisson_mat(1,1,j),          &
     &        i_vs_pivot(1,j), ierr, vs_poisson_lu(1,1,j),              &
     &        vs_poisson_det(1,j) )
          call ludcmp_3band(nidx_rj(1), p_poisson_mat(1,1,j),           &
     &        i_p_pivot(1,j), ierr, p_poisson_lu(1,1,j),                &
     &        p_poisson_det(1,j) )
        end do
      end do
!$omp end parallel do
!
!      do j = 1, nidx_rj(2)
!        do k = 1, nidx_rj(1)
!          vp_evo_det(1,j) = vp_evo_det(1,j) * vp_evo_lu(5,k,j)
!          vt_evo_det(1,j) = vt_evo_det(1,j) * vt_evo_lu(3,k,j)
!        end do
!        write(my_rank+60,*) 'det vp', j,                               &
!     &                       vp_evo_det(1,j), vt_evo_det(1,j)
!      end do
!
      end subroutine const_radial_mat_vort_2step
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_magne_sph
!
      use m_boundary_params_sph_MHD
      use m_coef_fdm_to_center
      use set_sph_scalar_mat_bc
      use set_sph_magne_mat_bc
      use center_sph_matrices
!
      integer(kind = kint) :: ip, jst, jed, j
      integer(kind = kint) :: ierr
      real(kind = kreal) :: coef_dbt
!
!
      if(coef_d_magne .eq. zero) then
        coef_dbt = one
        call set_unit_mat_4_poisson(nidx_rj(1), nidx_rj(2),             &
     &      sph_bc_B%kr_in, sph_bc_B%kr_out, bs_evo_mat)
        call set_unit_mat_4_poisson(nidx_rj(1), nidx_rj(2),             &
     &      sph_bc_B%kr_in, sph_bc_B%kr_out, bt_evo_mat)
      else
        coef_dbt = coef_imp_b * coef_d_magne * dt
        call set_unit_mat_4_time_evo(nidx_rj(1), nidx_rj(2),            &
     &      bs_evo_mat)
        call set_unit_mat_4_time_evo(nidx_rj(1), nidx_rj(2),            &
     &      bt_evo_mat)
      end if
!
      call add_vector_poisson_mat_sph(nidx_rj(1), nidx_rj(2),           &
     &    sph_bc_B%kr_in, sph_bc_B%kr_out, coef_dbt, bs_evo_mat)
      call add_vector_poisson_mat_sph(nidx_rj(1), nidx_rj(2),           &
     &    sph_bc_B%kr_in, sph_bc_B%kr_out, coef_dbt, bt_evo_mat)
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call add_vector_poisson_mat_center(nidx_rj(1), nidx_rj(2),      &
     &      sph_bc_B%r_ICB, fdm2_fix_fld_center, coef_dbt, bs_evo_mat)
        call add_vector_poisson_mat_center(nidx_rj(1), nidx_rj(2),      &
     &      sph_bc_B%r_ICB, fdm2_fix_fld_center, coef_dbt, bt_evo_mat)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call add_fix_flux_icb_poisson_mat(nidx_rj(1), nidx_rj(2),       &
     &      sph_bc_B%kr_in, sph_bc_B%r_ICB, sph_bc_B%fdm2_fix_dr_ICB,   &
     &      coef_dbt, bs_evo_mat)
        call set_fix_fld_icb_poisson_mat(nidx_rj(1), nidx_rj(2),        &
     &      sph_bc_B%kr_in, bt_evo_mat)
      else
        call set_ins_magne_icb_rmat_sph(nidx_rj(1), nidx_rj(2),         &
     &      sph_bc_B%kr_in, sph_bc_B%r_ICB, sph_bc_B%fdm2_fix_dr_ICB,   &
     &      coef_dbt, bs_evo_mat)
        call set_fix_fld_icb_poisson_mat(nidx_rj(1), nidx_rj(2),        &
     &      sph_bc_B%kr_in, bt_evo_mat)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call add_fix_flux_cmb_poisson_mat(nidx_rj(1), nidx_rj(2),       &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB, sph_bc_B%fdm2_fix_dr_CMB,  &
     &      coef_dbt, bs_evo_mat)
      else
        call set_ins_magne_cmb_rmat_sph(nidx_rj(1), nidx_rj(2),         &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB, sph_bc_B%fdm2_fix_dr_CMB,  &
     &      coef_dbt, bs_evo_mat)
      end if
      call set_fix_fld_cmb_poisson_mat(nidx_rj(1), nidx_rj(2),          &
     &    sph_bc_B%kr_out, bt_evo_mat)
!
!
!$omp parallel do private(jst,jed,j)
      do ip = 1, np_smp
        jst = idx_rj_smp_stack(ip-1,2) + 1
        jed = idx_rj_smp_stack(ip,  2)
        do j = jst, jed
          call ludcmp_3band(nidx_rj(1), bs_evo_mat(1,1,j),              &
     &        i_bs_pivot(1,j), ierr, bs_evo_lu(1,1,j), bs_evo_det(1,j))
        end do
!
        do j = jst, jed
          call ludcmp_3band(nidx_rj(1), bt_evo_mat(1,1,j),              &
     &        i_bt_pivot(1,j), ierr, bt_evo_lu(1,1,j), bt_evo_det(1,j))
        end do
      end do
!$omp end parallel do
!
      end subroutine const_radial_mat_4_magne_sph
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_4_vector_sph
