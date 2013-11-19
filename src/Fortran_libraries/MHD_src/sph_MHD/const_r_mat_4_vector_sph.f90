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
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use m_ludcmp_band
      use set_free_slip_sph_mat_bc
      use set_non_slip_sph_mat_bc
      use set_sph_mom_mat_bc
      use cal_inner_core_rotation
      use mat_product_3band_mul
!
      integer(kind = kint) :: ip, jst, jed, j
      integer(kind = kint) :: ierr
!
!
!$omp parallel
      call set_radial_vect_evo_mat_sph(nidx_rj(1), nidx_rj(2),          &
     &    nlayer_ICB, nlayer_CMB, coef_imp_v, coef_d_velo, vt_evo_mat)
      call set_radial_vect_evo_mat_sph(nidx_rj(1), nidx_rj(2),          &
     &    nlayer_ICB, nlayer_CMB, coef_imp_v, coef_d_velo, wt_evo_mat)
      call set_radial_vp3_mat_sph(nidx_rj(1), nidx_rj(2),               &
     &    nlayer_ICB, nlayer_CMB, vs_poisson_mat)
      call set_radial_press_mat_sph(nidx_rj(1), nidx_rj(2),             &
     &    nlayer_ICB, nlayer_CMB, coef_press, p_poisson_mat)
!$omp end parallel
!
!   Boundary condition for ICB
!
      call set_icb_wt_sph_evo_mat(nidx_rj(1), nidx_rj(2),               &
     &    sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB,     &
     &    wt_evo_mat)
      call set_icb_p_sph_poisson_mat(nidx_rj(1), nidx_rj(2),            &
     &    sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB,     &
     &    p_poisson_mat)
!
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call free_slip_icb_vt_sph_mat(nidx_rj(1), nidx_rj(2),           &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, fdm2_free_vt_ICB,           &
     &      vt_evo_mat)
        call free_icb_vp_poisson3_mat(nidx_rj(1), nidx_rj(2),           &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, fdm2_free_vp_ICB,           &
     &      vs_poisson_mat)
      else
        call set_non_slip_icb_vt_sph_mat(nidx_rj(1), nidx_rj(2),        &
     &      sph_bc_U%kr_in, vt_evo_mat)
        call set_rgd_icb_vp_poisson3_mat(nidx_rj(1), nidx_rj(2),        &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB,   &
     &      vs_poisson_mat)
      end if
!
!   Overwrite rotation for inner core
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call set_icore_viscous_matrix(sph_bc_U%kr_in)
      end if
!
!   Boundary condition for CMB
!
      call set_cmb_wt_sph_evo_mat(nidx_rj(1), nidx_rj(2),               &
     &    sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,    &
     &    wt_evo_mat)
      call set_cmb_p_sph_poisson_mat(nidx_rj(1), nidx_rj(2),            &
     &    sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,    &
     &    p_poisson_mat)
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call free_slip_cmb_vt_sph_mat(nidx_rj(1), nidx_rj(2),           &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, fdm2_free_vt_CMB,          &
     &      vt_evo_mat)
        call free_cmb_vp_poisson3_mat(nidx_rj(1), nidx_rj(2),           &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, fdm2_free_vp_CMB,          &
     &      vs_poisson_mat)
      else
        call set_non_slip_cmb_vt_sph_mat(nidx_rj(1), nidx_rj(2),        &
     &      sph_bc_U%kr_out, vt_evo_mat)
        call set_rgd_cmb_vp_poisson3_mat(nidx_rj(1), nidx_rj(2),        &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,  &
     &      vs_poisson_mat)
      end if
!
!
      call cal_mat_product_3band_mul(nidx_rj(1), nidx_rj(2),            &
     &    nlayer_ICB, nlayer_CMB, wt_evo_mat, vs_poisson_mat,           &
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
      use set_sph_magne_mat_bc
!
      integer(kind = kint) :: ip, jst, jed, j
      integer(kind = kint) :: ierr
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call set_magne_center_rmat_sph
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call no_r_poynting_cmb_rmat_sph(nidx_rj(1), nidx_rj(2),         &
     &      sph_bc_B%kr_in, sph_bc_B%r_ICB, sph_bc_B%fdm2_fix_dr_ICB,   &
     &      bs_evo_mat, bt_evo_mat)
      else
        call set_ins_magne_icb_rmat_sph(nidx_rj(1), nidx_rj(2),         &
     &      sph_bc_B%kr_in, sph_bc_B%r_ICB, sph_bc_B%fdm2_fix_dr_ICB,   &
     &      bs_evo_mat, bt_evo_mat)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call no_r_poynting_cmb_rmat_sph(nidx_rj(1), nidx_rj(2),         &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB, sph_bc_B%fdm2_fix_dr_CMB,  &
     &      bs_evo_mat, bt_evo_mat)
      else
        call set_ins_magne_cmb_rmat_sph(nidx_rj(1), nidx_rj(2),         &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB, sph_bc_B%fdm2_fix_dr_CMB,  &
     &      bs_evo_mat, bt_evo_mat)
      end if
!
!$omp parallel
      call set_radial_vect_evo_mat_sph(nidx_rj(1), nidx_rj(2),          &
     &    sph_bc_B%kr_in, sph_bc_B%kr_out, coef_imp_b, coef_d_magne,    &
     &    bs_evo_mat)
      call set_radial_vect_evo_mat_sph(nidx_rj(1), nidx_rj(2),          &
     &    sph_bc_B%kr_in, sph_bc_B%kr_out, coef_imp_b, coef_d_magne,    &
     &    bt_evo_mat)
!$omp end parallel
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
