!>@file   cal_sol_sph_fluid_crank.f90
!!@brief  module cal_sol_sph_fluid_crank
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update each field for MHD dynamo model
!!
!!@verbatim
!!      subroutine cal_sol_velo_by_vort_sph_crank
!!        Input address:    ipol%i_vort, itor%i_vort
!!        Solution address: ipol%i_velo, itor%i_velo, idpdr%i_velo
!!
!!      subroutine cal_sol_pressure_by_div_v
!!        Solution address: ipol%i_press
!!
!!
!!      subroutine cal_sol_magne_sph_crank
!!        Input address:    ipol%i_magne, itor%i_magne
!!        Solution address: ipol%i_magne, itor%i_magne, idpdr%i_magne
!!
!!      subroutine cal_sol_temperature_sph_crank
!!        Input address:    ipol%i_temp
!!        Solution address: ipol%i_temp
!!      subroutine cal_sol_composition_sph_crank
!!        Input address:    ipol%i_light
!!        Solution address: ipol%i_light
!!@endverbatim
!
      module cal_sol_sph_fluid_crank
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_radial_matrices_sph
      use set_reference_sph_mhd
!
      use lubksb_357band_mul
!
      implicit none
!
      private :: cal_sol_scalar_sph_crank
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_by_vort_sph_crank
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_boundary_params_sph_MHD
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use const_sph_radial_grad
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
!
      integer(kind = kint) :: inod
!       integer(kind = kint) :: k, j
!
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_velo) = d_rj(inod,itor%i_vort)
        d_rj(inod,itor%i_velo) = d_rj(inod,ipol%i_vort)
      end do
!$omp end parallel do
!
      call delete_zero_degree_comp(ipol%i_velo)
!
      if     (sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_vpol2(nidx_rj(2), sph_bc_U%kr_in,     &
     &      fdm2_free_vp_ICB, ipol%i_velo)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (idx_rj_degree_zero, idx_rj_degree_one, nidx_rj(2),          &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, radius_1d_rj_r(1),          &
     &      vt_ICB_bc, ipol%i_velo)
      else
        call cal_sph_nod_icb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, ipol%i_velo)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_vpol2(nidx_rj(2), sph_bc_U%kr_out,    &
     &      fdm2_free_vp_CMB, ipol%i_velo)
      else
        call cal_sph_nod_cmb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, vt_CMB_bc, ipol%i_velo)
      end if
!
!      write(my_rank+70,*) 'k, j, inod, vp_rhs, vt_rhs'
!      do j = 1, nidx_rj(2)
!        j = 3
!        do k = 1, sph_bc_U%kr_out
!          inod = (k-1)*nidx_rj(2) + j
!          write(my_rank+70,*) k, j, inod,                              &
!     &                 d_rj(inod,ipol%i_velo), d_rj(inod,itor%i_velo)
!        end do
!      end do
!
      call lubksb_5band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), vp_evo_lu,                            &
     &    i_vp_pivot, d_rj(1,ipol%i_velo) )
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), vt_evo_lu,                            &
     &    i_vt_pivot, d_rj(1,itor%i_velo) )
!
      call const_grad_vp_and_vorticity(ipol%i_velo, ipol%i_vort)
!
!      write(my_rank+170,*) 'k, j, vt2, wp2, dwp2'
!      do j = 1, nidx_rj(2)
!         j = 6
!        do k = 1, sph_bc_U%kr_out
!            inod = (k-1)*nidx_rj(2) + j
!            write(my_rank+170,'(2i10,1p8E25.15e3)') k, j,              &
!     &              d_rj(inod,ipol%i_velo),d_rj(inod,itor%i_velo)
!     &          d_rj(inod,ipol%i_velo), d_rj(inod,idpdr%i_velo),       &
!     &          d_rj(inod,itor%i_velo), d_rj(inod,itor%i_vort)
!        end do
!      end do
!
      end subroutine cal_sol_velo_by_vort_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_pressure_by_div_v
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_boundary_params_sph_MHD
      use set_reference_sph_mhd
!
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), p_poisson_lu, i_p_pivot,              &
     &    d_rj(1,ipol%i_press) )
!
      call adjust_by_ave_pressure_on_CMB                                &
     &   (sph_bc_U%kr_in, sph_bc_U%kr_out)
!
      end subroutine cal_sol_pressure_by_div_v
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_sph_crank
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_boundary_params_sph_MHD
      use const_sph_radial_grad
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_cmb_qvac
      use cal_sph_exp_nod_icb_qvac
!
!
      call delete_zero_degree_comp(ipol%i_magne)
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call cal_sph_nod_icb_ins_mag2(nidx_rj(2), sph_bc_B%kr_in,       &
     &      sph_bc_B%r_ICB, ipol%i_magne)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_mag2(nidx_rj(2), sph_bc_B%kr_in,       &
     &      ipol%i_magne)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_mag2(nidx_rj(2), sph_bc_B%kr_out,      &
     &      ipol%i_magne)
      else
        call cal_sph_nod_cmb_ins_mag2(nidx_rj(2), sph_bc_B%kr_out,      &
     &      sph_bc_B%r_CMB, ipol%i_magne)
      end if
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), bs_evo_lu, i_bs_pivot,                &
     &    d_rj(1,ipol%i_magne) )
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), bt_evo_lu, i_bt_pivot,                &
     &    d_rj(1,itor%i_magne) )
!
      call const_grad_bp_and_current                                    &
     &   (sph_bc_B, ipol%i_magne, ipol%i_current)
!
      end subroutine cal_sol_magne_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temperature_sph_crank
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_t_int_parameter
      use m_physical_property
      use m_boundary_params_sph_MHD
      use m_radial_mat_sph_w_center
!
!
      call cal_sol_scalar_sph_crank(nidx_rj(1), nidx_rj(2),             &
     &    idx_rj_smp_stack, sph_bc_T, coef_temp, coef_d_temp,           &
     &    coef_imp_t, temp_evo_mat, temp_evo_lu, i_temp_pivot,          &
     &    t00_evo_lu, i_t00_pivot, t00_solution, ipol%i_temp)
!
      end subroutine cal_sol_temperature_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_composition_sph_crank
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_t_int_parameter
      use m_physical_property
      use m_boundary_params_sph_MHD
      use m_radial_mat_sph_w_center
!
!
      call cal_sol_scalar_sph_crank(nidx_rj(1), nidx_rj(2),             &
     &    idx_rj_smp_stack, sph_bc_C, coef_light, coef_d_light,         &
     &    coef_imp_c, composit_evo_mat, composit_evo_lu,                &
     &    i_composit_pivot, c00_evo_lu, i_c00_pivot, c00_solution,      &
     &    ipol%i_light)
!
      end subroutine cal_sol_composition_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine check_temperature(l, m, is_field)
!
      integer(kind = kint), intent(in) :: l, m, is_field
!
      integer(kind = kint) :: j,k,inod
!
!
      j = find_local_sph_mode_address(l, m)
      if(j .eq. 0) return
!
      write(*,*) 'field ID, l, m: ', is_field, l, m
      do k = 1, nidx_rj(1)
        inod = j + (k-1)*nidx_rj(2)
        write(*,*) k, d_rj(inod,is_field)
      end do
!
      end subroutine check_temperature
!
! -----------------------------------------------------------------------
!
      subroutine check_NaN_temperature(is_field)
!
      integer(kind = kint), intent(in) :: is_field
!
      integer(kind = kint) :: inod, j, k, l, m
!
!
      do inod = 1, nnod_rj
        if(d_rj(inod,is_field) .ne. d_rj(inod,is_field)) then
          j = idx_global_rj(inod,2)
          k = idx_global_rj(inod,1)
          l = aint(sqrt(real(j)))
          m = j - l*(l+1)
          write(50+my_rank,*) 'Broken', inod, k, j, l, m,  &
     &              d_rj(inod,is_field)
        end if
      end do
!
end subroutine check_NaN_temperature
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_scalar_sph_crank(nri, jmax, idx_rj_smp_stack,  &
     &          sph_bc, coef_f, coef_d, coef_imp, evo_mat, evo_lu,      &
     &          i_pivot, s00_evo_lu, i_s00_pivot, sol_00, is_field)
!
      use m_t_int_parameter
      use t_boundary_params_sph_MHD
      use set_scalar_boundary_sph
      use cal_sph_exp_center
      use lubksb_357band
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: idx_rj_smp_stack(0:np_smp,2)
      type(sph_boundary_type), intent(in) :: sph_bc
      real(kind = kreal), intent(in) :: coef_imp, coef_f, coef_d
      real(kind = kreal), intent(in) :: evo_mat(3,nri,jmax)
      real(kind = kreal), intent(in) :: evo_lu(5,nri,jmax)
      integer(kind = kint), intent(in) :: i_pivot(nri,jmax)
      real(kind = kreal), intent(in) :: s00_evo_lu(5,0:nri)
      integer(kind = kint), intent(in) :: i_s00_pivot(0:nri)
!
      integer(kind = kint), intent(in) :: is_field
      real(kind = kreal), intent(inout) :: sol_00(0:nri)
!
!
!   Set RHS vector for CMB
      if (sph_bc%iflag_cmb .eq. iflag_fixed_field) then
        call set_fixed_scalar_sph(jmax, sph_bc%kr_out,                  &
     &      nri, is_field, sph_bc%CMB_fld)
      else if(coef_f .ne. 0.0d0) then
        call adjust_out_fixed_flux_sph(jmax,                            &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      sph_bc%CMB_flux, coef_d, coef_imp, dt, is_field)
      else
        call poisson_out_fixed_flux_sph(jmax,                           &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      sph_bc%CMB_flux, is_field)
      end if
!
!   Set RHS vector for ICB
      if (sph_bc%iflag_icb .eq. iflag_fixed_field) then
        call set_fixed_scalar_sph(jmax, ione, sph_bc%kr_in,             &
     &      is_field, sph_bc%ICB_fld)
      else if (sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call cal_sph_fixed_center(inod_rj_center, sph_bc%CTR_fld,       &
     &      is_field)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux                    &
     &     .and. coef_f .ne. 0.0d0) then
        call adjust_in_fixed_flux_sph(jmax,                             &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,         &
     &      sph_bc%ICB_flux, coef_d, coef_imp, dt, is_field)
      else if (sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call poisson_in_fixed_flux_sph(jmax,                            &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,         &
     &      sph_bc%ICB_flux, is_field)
      end if
!
      if(inod_rj_center .gt. 0) then
        call copy_degree0_comps_to_sol(nri, jmax,                       &
     &      inod_rj_center, idx_rj_degree_zero, is_field, sol_00)
      end if
!
!      j = find_local_sph_mode_address(30,-23)
!      if(j.gt.0) then
!        write(*,*) 'matrix'
!        call check_single_radial_3band_mat(my_rank, nri,                &
!     &      radius_1d_rj_r, evo_mat(1,1,j))
!      end if
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    jmax, nri, evo_lu, i_pivot, d_rj(1,is_field) )
!
!       write(*,*) 'solution'
!       call check_temperature(30,-23, is_field)
!       write(*,*) 'check_NaN_temperature'
!       call check_NaN_temperature(is_field)
!
!   Solve l=m=0 including center
      if(inod_rj_center .eq. 0) return
      call lubksb_3band(nri+1, s00_evo_lu, i_s00_pivot, sol_00)
      call copy_degree0_comps_from_sol(nri, jmax,                       &
     &    inod_rj_center, idx_rj_degree_zero, sol_00, is_field)
!
      end subroutine cal_sol_scalar_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine copy_degree0_comps_to_sol(nri, jmax,                   &
     &          inod_rj_center, idx_rj_degree_zero, is_field, sol_00)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_field
      real(kind = kreal), intent(inout) :: sol_00(0:nri)
!
      integer(kind = kint) :: kr, inod
!
!$omp parallel do private(inod)
      do kr = 1, nri
        inod = idx_rj_degree_zero + (kr-1) * jmax
        sol_00(kr) = d_rj(inod,is_field)
      end do
!$omp end parallel do
      sol_00(0) = d_rj(inod_rj_center,is_field)
!
!       write(*,*) 'kr, Average RHS'
!       do kr = 0, nri
!         write(*,*) kr, sol_00(kr)
!       end do

      end subroutine copy_degree0_comps_to_sol
!
! -----------------------------------------------------------------------
!
      subroutine copy_degree0_comps_from_sol(nri, jmax,                 &
     &          inod_rj_center, idx_rj_degree_zero, sol_00, is_field)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_field
      real(kind = kreal), intent(in) :: sol_00(0:nri)
!
      integer(kind = kint) :: kr, inod
!
!$omp parallel do private(inod)
      do kr = 1, nri
        inod = idx_rj_degree_zero + (kr-1) * jmax
        d_rj(inod,is_field) = sol_00(kr)
      end do
!$omp end parallel do
      d_rj(inod_rj_center,is_field) = sol_00(0)
!
!       write(*,*) 'kr, average Solution'
!       do kr = 0, nri
!         write(*,*) kr, sol_00(kr)
!      end do
!
      end subroutine copy_degree0_comps_from_sol
!
! -----------------------------------------------------------------------
!
      end module cal_sol_sph_fluid_crank
