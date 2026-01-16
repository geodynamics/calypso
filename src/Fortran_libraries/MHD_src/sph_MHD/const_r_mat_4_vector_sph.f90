!>@file   const_r_mat_4_vector_sph.f90
!!@brief  module const_r_mat_4_vector_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of vector fields
!!
!!@verbatim
!!      subroutine const_radial_mat_vort_2step(dt, sph_rj, r_2nd,       &
!!     &          fl_prop, sph_bc_U, bcs_U, bc_fdms_U, fdm2_center,     &
!!     &          g_sph_rj, band_vs_poisson, band_vp_evo, band_wt_evo)
!!      subroutine const_radial_mat_toroidal_flow(dt, sph_rj, r_2nd,    &
!!     &          fl_prop, sph_bc_U, bcs_U, bc_fdms_U, fdm2_center,     &
!!     &          g_sph_rj, band_vt_evo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_vector_boundary_data), intent(in) :: bcs_U
!!        type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(band_matrices_type), intent(inout) :: band_vp_evo
!!        type(band_matrices_type), intent(inout) :: band_vt_evo
!!        type(band_matrices_type), intent(inout) :: band_wt_evo
!!        type(band_matrices_type), intent(inout) :: band_vs_poisson
!!
!!      subroutine const_radial_mat7_vpol_press                         &
!!     &         (dt, sph_rj, r_2nd, r_n2e_3rd, r_e2n_1st, fl_prop,     &
!!     &          sph_bc_U, bcs_U, bc_fdms_U, fdm2_center, g_sph_rj,    &
!!     &          radial_variation, band7_vsp_evo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(fdm_matrices), intent(in) :: r_n2e_3rd
!!        type(fdm_matrices), intent(in) :: r_e2n_1st
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_vector_boundary_data), intent(in) :: bcs_U
!!        type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!!        type(phys_data), intent(in) :: radial_variation
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        real(kind = kreal), intent(in)                                &
!!     &                     :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        real(kind = kreal), intent(in) :: dt
!!        type(band_matrices_type), intent(inout) :: band7_vsp_evo
!!@endverbatim
!
      module const_r_mat_4_vector_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_ludcmp_3band
!
      use t_physical_property
      use t_spheric_rj_data
      use t_sph_matrices
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_sph_velocity_BCs
      use t_coef_fdm2_centre
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &           :: vp_evo_name =     'poloidal_velocity_evolution'
      character(len=kchara), parameter, private                         &
     &           :: vt_evo_name =     'toroidal_velocity_evolution'
      character(len=kchara), parameter, private                         &
     &           :: wt_evo_name =     'toroidal_vorticity_evolution'
      character(len=kchara), parameter, private                         &
     &           :: wt_poison_name =  'toroidal_vorticity_Poisson'
      character(len=kchara), parameter, private                         &
     &           :: vsp_evo_name =    'velocity_pressure_evolution'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_vort_2step(dt, sph_rj, r_2nd,         &
     &          fl_prop, sph_bc_U, bcs_U, bc_fdms_U, fdm2_center,       &
     &          g_sph_rj, band_vs_poisson, band_vp_evo, band_wt_evo)
!
      use calypso_mpi
      use m_ludcmp_band
      use set_sph_unit_radial_mat
      use add_sph_vector_radial_mat
      use select_sph_r_mat_vort_BC
      use center_sph_matrices
      use mat_product_3band_mul
      use mat_product_3band_mul_bc
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: dt
!
      type(band_matrices_type), intent(inout) :: band_vp_evo
      type(band_matrices_type), intent(inout) :: band_wt_evo
      type(band_matrices_type), intent(inout) :: band_vs_poisson
!
!      integer(kind = kint) :: j
      real(kind = kreal) :: coef_dvt
!
!
!      do j = 1, sph_rj%nidx_rj(2)
!        if(sph_rj%idx_gl_1d_rj_j(j,3) .ne. 0) cycle
!        write(*,'(i4,a,2i4,f5.2,i4)') my_rank, 'BC_Lmax', j,           &
!     &          sph_rj%idx_gl_1d_rj_j(j,2), bcs_U%ICB_Vspec%Vp_BC(j),  &
!     &          int(bcs_U%ICB_Vspec%Vp_BC(j))
!      end do
!
      band_vs_poisson%mat_name = wt_poison_name
      band_wt_evo%mat_name =     wt_evo_name
      band_vp_evo%mat_name =     vp_evo_name
!
      call alloc_band_mat_sph(ifive, sph_rj, band_vp_evo)
      call alloc_band_mat_sph(ithree, sph_rj, band_wt_evo)
      call alloc_band_mat_sph(ithree, sph_rj, band_vs_poisson)
!
      call set_unit_on_diag(band_vp_evo)
      call set_unit_on_diag(band_wt_evo)
!
      if(fl_prop%coef_velo .eq. zero) then
        coef_dvt = one
        call set_unit_mat_4_poisson                                     &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc_U%kr_in, sph_bc_U%kr_out, band_wt_evo%mat)
      else
        coef_dvt = fl_prop%coef_imp * fl_prop%coef_diffuse * dt
        call set_unit_mat_4_time_evo                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), band_wt_evo%mat)
      end if
!
      call set_unit_mat_4_poisson                                       &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, band_vs_poisson%mat)
!
      call add_vector_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    g_sph_rj, sph_bc_U%kr_in, sph_bc_U%kr_out,                    &
     &    coef_dvt, r_2nd%fdm(2)%dmat, band_wt_evo%mat)
      call add_vector_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    g_sph_rj, sph_bc_U%kr_in, sph_bc_U%kr_out,                    &
     &    one, r_2nd%fdm(2)%dmat, band_vs_poisson%mat)
!
!   Boundary condition for ICB
      call sel_sph_r_mat_vort_2step_ICB                                 &
     &   (sph_rj, sph_bc_U, bcs_U, bc_fdms_U, fdm2_center,              &
     &    g_sph_rj, coef_dvt, band_vs_poisson, band_wt_evo)
!
!   Boundary condition for CMB
      call sel_sph_r_mat_vort_2step_CMB(sph_rj, sph_bc_U, bc_fdms_U,    &
     &    g_sph_rj, coef_dvt, band_vs_poisson, band_wt_evo)
!
!
      call cal_mat_product_3band_mul                                    &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, band_wt_evo%mat,             &
     &    band_vs_poisson%mat, band_vp_evo%mat)
      call cal_vp_evo_mat_product_bc                                    &
     &   (sph_bc_U, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),               &
     &    band_wt_evo%mat, band_vs_poisson%mat, band_vp_evo%mat)
!
!      call check_specific_radial_band_mat(my_rank, (100+my_rank), 2, 0, &
!     &                                     sph_rj, band_vs_poisson)
!      call check_specific_radial_band_mat(my_rank, (200+my_rank), 2, 0, &
!     &                                     sph_rj, band_wt_evo)
!      call check_specific_radial_band_mat(my_rank, (300+my_rank), 2, 0, &
!     &                                     sph_rj, band_vp_evo)

      call ludcmp_5band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_vp_evo)
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_wt_evo)
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_vs_poisson)
!
      end subroutine const_radial_mat_vort_2step
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_toroidal_flow(dt, sph_rj, r_2nd,      &
     &          fl_prop, sph_bc_U, bcs_U, bc_fdms_U, fdm2_center,       &
     &          g_sph_rj, band_vt_evo)
!
      use m_ludcmp_band
      use set_sph_unit_radial_mat
      use add_sph_vector_radial_mat
      use select_sph_r_mat_vort_BC
      use cal_inner_core_rotation
      use center_sph_matrices
      use mat_product_3band_mul
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: dt
!
      type(band_matrices_type), intent(inout) :: band_vt_evo
!
!      integer(kind = kint) :: j
      real(kind = kreal) :: coef_dvt
!
!
      band_vt_evo%mat_name = vt_evo_name
      call alloc_band_mat_sph(ithree, sph_rj, band_vt_evo)
      call set_unit_on_diag(band_vt_evo)
!
      if(fl_prop%coef_velo .eq. zero) then
        coef_dvt = one
        call set_unit_mat_4_poisson                                     &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc_U%kr_in, sph_bc_U%kr_out, band_vt_evo%mat)
      else
        coef_dvt = fl_prop%coef_imp * fl_prop%coef_diffuse * dt
        call set_unit_mat_4_time_evo                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), band_vt_evo%mat)
      end if
!
!
      call add_vector_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    g_sph_rj, sph_bc_U%kr_in, sph_bc_U%kr_out,                    &
     &    coef_dvt, r_2nd%fdm(2)%dmat, band_vt_evo%mat)
!
!   Boundary condition for ICB
      call sel_sph_r_mat_tor_flow_ICB                                   &
     &   (sph_rj, sph_bc_U, bcs_U, bc_fdms_U, fdm2_center,              &
     &    g_sph_rj, coef_dvt, band_vt_evo)
!
!   Overwrite rotation of inner core for degree 1
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call set_icore_viscous_matrix                                   &
     &     (sph_bc_U%kr_in, bc_fdms_U%fdm1_fix_fld_ICB,                 &
     &      dt, sph_rj, fl_prop, band_vt_evo)
      end if
!
!   Boundary condition for CMB
      call sel_sph_r_mat_tor_flow_CMB(sph_rj, sph_bc_U,                 &
     &          bc_fdms_U, g_sph_rj, coef_dvt, band_vt_evo)
!
!   LU decomposition
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_vt_evo)
!
      end subroutine const_radial_mat_toroidal_flow
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat7_vpol_press                           &
     &         (dt, sph_rj, r_2nd, r_n2e_3rd, r_e2n_1st, fl_prop,       &
     &          sph_bc_U, bcs_U, bc_fdms_U, fdm2_center, g_sph_rj,      &
     &          radial_variation, band7_vsp_evo)
!
      use t_phys_data
      use cal_sph_pol_hdiv_viscousity
      use cal_sph_pol_hdiv_vscs_CMB
      use cal_sph_pol_hdiv_vscs_ICB
      use set_sph_pol_hdiv_viscs_CTR
      use center_sph_matrices
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm_matrices), intent(in) :: r_n2e_3rd
      type(fdm_matrices), intent(in) :: r_e2n_1st
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
      type(phys_data), intent(in) :: radial_variation
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: dt
!
      type(band_matrices_type), intent(inout) :: band7_vsp_evo
!
!      integer(kind = kint) :: j
      real(kind = kreal) :: coef_dvt
!
      real(kind = kreal) :: mat_grad_p(sph_rj%nidx_rj(2),0:1)
      real(kind = kreal) :: mat2_viscous(sph_rj%nidx_rj(2),-1:1)
      real(kind = kreal) :: hdiv_visous_mat(sph_rj%nidx_rj(2),-2:1)
!
!
      if(fl_prop%coef_velo .eq. zero) then
        coef_dvt = one
      else
        coef_dvt = fl_prop%coef_imp * fl_prop%coef_diffuse * dt
      end if
!
      band7_vsp_evo%mat_name = vsp_evo_name
      call alloc_band_matrices_type(iseven, (2*sph_rj%nidx_rj(1)),      &
     &                              sph_rj%nidx_rj(2), band7_vsp_evo)
      call set_unit_on_diag(band7_vsp_evo)
!
      call sph_FDM2_vpol_viscosity_mat                                  &
     &   (sph_bc_U%kr_in, sph_bc_U%kr_out, sph_rj, fl_prop,             &
     &    radial_variation, g_sph_rj, fl_prop%coef_press, coef_dvt,     &
     &    r_2nd%fdm(1), r_n2e_3rd%fdm(0), r_e2n_1st%fdm(0),             &
     &    mat2_viscous, hdiv_visous_mat, band7_vsp_evo%mat)
!
      if(     (sph_bc_U%iflag_icb .eq. iflag_sph_fill_center)           &
     &   .or. (sph_bc_U%iflag_icb .eq. iflag_sph_filter_center)) then
        call sph_FDM2_vpol_viscosity_mat_CTR                            &
     &     (sph_rj, fl_prop, radial_variation, g_sph_rj, coef_dvt,      &
     &      r_n2e_3rd%fdm(0), r_e2n_1st%fdm(0),                         &
     &      fdm2_center, bc_fdms_U%fdm3e_CTR,                           &
     &      mat_grad_p, mat2_viscous, hdiv_visous_mat,                  &
     &      band7_vsp_evo%mat)
!
        if(sph_bc_U%iflag_icb .eq. iflag_sph_filter_center) then
          call set_unit_mat7_filter_to_center                           &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        bcs_U%ICB_Vspec%Vp_BC, band7_vsp_evo%mat)
        end if
      else
        call sph_FDM2_vpol_viscosity_mat_ICB(sph_rj, fl_prop,           &
     &      radial_variation, sph_bc_U, g_sph_rj, coef_dvt,             &
     &      bc_fdms_U%fdm3e_vp0_ICB, bc_fdms_U%fdm3e_free_ICB,          &
     &      hdiv_visous_mat, band7_vsp_evo%mat)
      end if
!
      call sph_FDM2_vpol_viscosity_mat_CMB                              &
     &        (sph_rj, fl_prop, radial_variation, sph_bc_U,             &
     &    g_sph_rj, fl_prop%coef_press, coef_dvt,                       &
     &    bc_fdms_U%fdm3e_vp0_CMB, bc_fdms_U%fdm3e_free_CMB,            &
     &    hdiv_visous_mat, band7_vsp_evo%mat)
!
      end subroutine const_radial_mat7_vpol_press
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat9_vpol_press(dt, sph_rj, r_2nd,        &
     &          fl_prop, sph_bc_U, bc_fdms_U, fdm2_center, g_sph_rj,    &
     &          band9_vsp_evo)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: dt
!
      type(band_matrices_type), intent(inout) :: band9_vsp_evo
!
!      integer(kind = kint) :: j
      real(kind = kreal) :: coef_dvt
!
!
      if(fl_prop%coef_velo .eq. zero) then
        coef_dvt = one
      else
        coef_dvt = fl_prop%coef_imp * fl_prop%coef_diffuse * dt
      end if
!
      band9_vsp_evo%mat_name = vsp_evo_name
      call alloc_band_matrices_type(inine, (2*sph_rj%nidx_rj(1)),       &
     &                              sph_rj%nidx_rj(2), band9_vsp_evo)
      call set_unit_on_diag(band9_vsp_evo)
!
      end subroutine const_radial_mat9_vpol_press
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_4_vector_sph
