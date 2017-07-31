!>@file   const_r_mat_4_scalar_sph.f90
!!@brief  module const_r_mat_4_scalar_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine const_radial_mat_4_press_sph                         &
!!     &         (sph_rj, r_2nd, fl_prop, sph_bc_U, fdm2_center,        &
!!     &          g_sph_rj, band_p_poisson)
!!      subroutine const_radial_mat_4_scalar_sph                        &
!!     &         (mat_name, dt, sph_rj, r_2nd, property,                &
!!     &          sph_bc, fdm2_center, g_sph_rj, band_s_evo)
!!        type(scalar_property), intent(in) :: property
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!@endverbatim
!
      module const_r_mat_4_scalar_sph
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
!
      use t_physical_property
      use t_spheric_rj_data
      use t_sph_matrices
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_press_sph                           &
     &         (sph_rj, r_2nd, fl_prop, sph_bc_U, fdm2_center,          &
     &          g_sph_rj, band_p_poisson)
!
      use m_ludcmp_3band
      use set_sph_scalar_mat_bc
      use cal_inner_core_rotation
      use center_sph_matrices
      use mat_product_3band_mul
      use set_radial_mat_sph
      use check_sph_radial_mat
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(band_matrices_type), intent(inout) :: band_p_poisson
!
      real(kind = kreal) :: coef_p
!
!
      coef_p = - fl_prop%coef_press
      call alloc_band_mat_sph(ithree, sph_rj, band_p_poisson)

      call set_unit_mat_4_poisson                                       &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, band_p_poisson%mat)
      call add_scalar_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    g_sph_rj, sph_bc_U%kr_in, sph_bc_U%kr_out, coef_p,            &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_p_poisson%mat)
!
!   Boundary condition for ICB
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call add_scalar_poisson_mat_ctr1                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%r_ICB, fdm2_center%dmat_fix_fld, coef_p,           &
     &      band_p_poisson%mat)
      else
        call add_icb_scalar_poisson_mat                                 &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB,   &
     &      coef_p, band_p_poisson%mat)
      end if
!
!   Boundary condition for CMB
!
      call add_cmb_scalar_poisson_mat                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,               &
     &    sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,    &
     &    coef_p, band_p_poisson%mat)
!
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_p_poisson)
!
      if(i_debug .eq. iflag_full_msg) then
        write(band_p_poisson%mat_name,'(a)') 'pressure_poisson'
        call check_radial_band_mat(my_rank, sph_rj, band_p_poisson)
      end if
!
      end subroutine const_radial_mat_4_press_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_scalar_sph                          &
     &         (mat_name, dt, sph_rj, r_2nd, property,                  &
     &          sph_bc, fdm2_center, g_sph_rj, band_s_evo)
!
      use m_ludcmp_3band
      use center_sph_matrices
      use set_radial_mat_sph
      use set_sph_scalar_mat_bc
      use check_sph_radial_mat
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: property
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: dt
      character(len=kchara), intent(in) :: mat_name
!
      type(band_matrices_type), intent(inout) :: band_s_evo
!
      real(kind = kreal) :: coef
!
!
      write(band_s_evo%mat_name,'(a)') trim(mat_name)
      if(property%iflag_scheme .lt. id_Crank_nicolson) return
      call alloc_band_mat_sph(ithree, sph_rj, band_s_evo)
      call set_unit_on_diag(band_s_evo)
!
      if(property%coef_advect .eq. zero) then
        coef = one
        call set_unit_mat_4_poisson                                     &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_in, sph_bc%kr_out, band_s_evo%mat)
      else
        coef = property%coef_imp * property%coef_diffuse * dt
        call set_unit_mat_4_time_evo                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), band_s_evo%mat)
      end if
!
      call add_scalar_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    g_sph_rj, sph_bc%kr_in, sph_bc%kr_out, coef,                  &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_s_evo%mat)
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center               &
     &   .or. sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call add_scalar_poisson_mat_ctr1                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc%r_ICB, fdm2_center%dmat_fix_fld, coef,               &
     &      band_s_evo%mat)
      else if (sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call add_fix_flux_icb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB, coef,   &
     &      band_s_evo%mat)
      else
        call set_fix_fld_icb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_in, band_s_evo%mat)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        call add_fix_flux_cmb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB, coef,  &
     &      band_s_evo%mat)
      else
        call set_fix_fld_cmb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_out, band_s_evo%mat)
      end if
!
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_s_evo)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_radial_band_mat(my_rank, sph_rj, band_s_evo)
      end if
!
      end subroutine const_radial_mat_4_scalar_sph
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_4_scalar_sph
