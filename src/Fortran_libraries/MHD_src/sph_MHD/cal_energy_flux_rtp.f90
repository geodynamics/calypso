!> @file  cal_energy_flux_rtp.f90
!!      module cal_energy_flux_rtp
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine s_cal_energy_flux_rtp(sph_rtp,                       &
!!     &          fl_prop, cd_prop, ref_param_T, ref_param_C, leg,      &
!!     &          f_trns, bs_trns, be_trns, bs_difv, fe_trns,           &
!!     &          trns_f_MHD, trns_b_snap, trns_b_eflux, trns_b_difv,   &
!!     &          trns_f_eflux)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: f_trns
!!        type(phys_address), intent(in) :: bs_trns, bs_difv
!!        type(phys_address), intent(in) :: be_trns, fe_trns
!!        type(spherical_transform_data), intent(in) :: trns_f_MHD
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(in) :: trns_b_eflux
!!        type(spherical_transform_data), intent(inout) :: trns_f_eflux
!!
!!      subroutine cal_energy_fluxes_on_node                            &
!!     &         (bs_trns_base, f_trns_frc, be_trns_frc, fs_trns_eflux, &
!!     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp, &
!!     &          ntot_comp_uxb, fub_rtp, ntot_comp_flx, flx_rtp)
!!        type(base_field_address), intent(in) :: bs_trns_base
!!        type(base_force_address), intent(in) :: f_trns_frc
!!        type(base_force_address), intent(in) :: be_trns_frc
!!        type(energy_flux_address), intent(in) :: fs_trns_eflux
!!@endverbatim
!
      module cal_energy_flux_rtp
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_phys_address
      use t_spheric_rtp_data
      use t_physical_property
      use t_reference_scalar_param
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
!
      implicit  none
!
      private :: cal_square_vector_on_node, cal_lengh_scale_rtp
!
! -----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_energy_flux_rtp(sph_rtp,                         &
     &          fl_prop, cd_prop, ref_param_T, ref_param_C, leg,        &
     &          f_trns, bs_trns, be_trns, bs_difv, fe_trns,             &
     &          trns_f_MHD, trns_b_snap, trns_b_eflux, trns_b_difv,     &
     &          trns_f_eflux)
!
      use const_wz_coriolis_rtp
      use cal_buoyancy_flux_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: f_trns
      type(phys_address), intent(in) :: bs_trns, bs_difv
      type(phys_address), intent(in) :: be_trns, fe_trns
      type(spherical_transform_data), intent(in) :: trns_f_MHD
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_b_eflux
      type(spherical_transform_data), intent(in) :: trns_b_difv
!
      type(spherical_transform_data), intent(inout) :: trns_f_eflux
!
!
      if(fe_trns%forces%i_coriolis .gt. 0) then
        call sel_wz_coriolis_rtp(sph_rtp, leg, fl_prop%coef_cor,        &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),                 &
     &      trns_f_eflux%fld_rtp(1,fe_trns%forces%i_coriolis))
      end if
!
      call cal_magnetic_fluxes_rtp(sph_rtp, cd_prop,                    &
     &    f_trns%forces, bs_trns%base, bs_difv%diff_vector,             &
     &    fe_trns%forces, fe_trns%prod_fld,                             &
     &    trns_f_MHD, trns_b_snap, trns_b_difv, trns_f_eflux)
      call cal_magnetic_fluxes_pole(sph_rtp, cd_prop,                   &
     &    f_trns%forces, bs_trns%base, bs_difv%diff_vector,             &
     &    fe_trns%forces, fe_trns%prod_fld,                             &
     &    trns_f_MHD, trns_b_snap, trns_b_difv, trns_f_eflux)
!
      call cal_energy_fluxes_on_node                                    &
     &   (bs_trns%base, f_trns%forces, be_trns%forces,                  &
     &    fe_trns%ene_flux, sph_rtp%nnod_rtp,                           &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_MHD%ncomp, trns_f_MHD%fld_rtp,                         &
     &    trns_b_eflux%ncomp, trns_b_eflux%fld_rtp,                     &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
      call cal_energy_fluxes_on_node                                    &
     &   (bs_trns%base, f_trns%forces, be_trns%forces,                  &
     &    fe_trns%ene_flux, sph_rtp%nnod_pole,                          &
     &    trns_b_snap%ncomp, trns_b_snap%fld_pole,                      &
     &    trns_f_MHD%ncomp, trns_f_MHD%fld_pole,                        &
     &    trns_b_eflux%ncomp, trns_b_eflux%fld_pole,                    &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_pole)
!
      call cal_buoyancy_flux_rtp                                        &
     &   (sph_rtp, fl_prop, ref_param_T, ref_param_C,                   &
     &    bs_trns%base, bs_trns%base, fe_trns%ene_flux,                 &
     &    trns_b_snap, trns_b_snap, trns_f_eflux)
      call pole_buoyancy_flux_rtp                                       &
     &   (sph_rtp, fl_prop, ref_param_T, ref_param_C,                   &
     &    bs_trns%base, bs_trns%base, fe_trns%ene_flux,                 &
     &    trns_b_snap, trns_b_snap, trns_f_eflux)
!
      call cal_helicity_on_node                                         &
     &   (bs_trns%base, fe_trns%prod_fld, sph_rtp%nnod_rtp,             &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
      call cal_helicity_on_node                                         &
     &   (bs_trns%base, fe_trns%prod_fld, sph_rtp%nnod_pole,            &
     &    trns_b_snap%ncomp, trns_b_snap%fld_pole,                      &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_pole)
!
!       get amplitude
      call cal_square_vector_on_node                                    &
     &   (bs_trns%base, fe_trns%prod_fld, sph_rtp%nnod_rtp,             &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
      call cal_square_vector_on_node                                    &
     &   (bs_trns%base, fe_trns%prod_fld, sph_rtp%nnod_pole,            &
     &    trns_b_snap%ncomp, trns_b_snap%fld_pole,                      &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_pole)
!
!       get lengh scale
      call cal_lengh_scale_rtp                                          &
     &   (sph_rtp, bs_trns%base, be_trns%diffusion, fe_trns%prod_fld,   &
     &    trns_b_snap, trns_b_eflux, trns_f_eflux)
!
      end subroutine s_cal_energy_flux_rtp
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_energy_fluxes_on_node                              &
     &         (bs_trns_base, f_trns_frc, be_trns_frc, fs_trns_eflux,   &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp,   &
     &          ntot_comp_uxb, fub_rtp, ntot_comp_flx, flx_rtp)
!
      use cal_products_smp
!
      type(base_field_address), intent(in) :: bs_trns_base
      type(base_force_address), intent(in) :: f_trns_frc
      type(base_force_address), intent(in) :: be_trns_frc
      type(energy_flux_address), intent(in) :: fs_trns_eflux
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld, ntot_comp_frc
      integer(kind = kint), intent(in) :: ntot_comp_uxb, ntot_comp_flx
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
      real(kind = kreal), intent(in) :: frc_rtp(nnod,ntot_comp_frc)
      real(kind = kreal), intent(in) :: fub_rtp(nnod,ntot_comp_uxb)
!
      real(kind = kreal), intent(inout) :: flx_rtp(nnod,ntot_comp_flx)
!
!$omp parallel
      if(fs_trns_eflux%i_ujb .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      frc_rtp(1,f_trns_frc%i_lorentz),                            &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_ujb) )
      end if
!
      if(fs_trns_eflux%i_nega_ujb .gt. 0) then
        call cal_dot_prod_w_coef_smp(nnod, dminus,                      &
     &      frc_rtp(1,f_trns_frc%i_lorentz),                            &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_nega_ujb))
      end if
!
      if(fs_trns_eflux%i_me_gen .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      fub_rtp(1,be_trns_frc%i_induction),                         &
     &      fld_rtp(1,bs_trns_base%i_magne),                            &
     &      flx_rtp(1,fs_trns_eflux%i_me_gen))
      end if
!$omp end parallel
!
      end subroutine cal_energy_fluxes_on_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_magnetic_fluxes_rtp                                &
     &         (sph_rtp, cd_prop, f_trns_frc, bs_trns_base,             &
     &          bs_trns_diff_v, fe_trns_frc, fe_trns_prod,              &
     &          trns_f_MHD, trns_b_snap, trns_b_difv, trns_f_eflux)
!
      use poynting_flux_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(conductive_property), intent(in) :: cd_prop
      type(base_force_address), intent(in) :: f_trns_frc
      type(base_field_address), intent(in) :: bs_trns_base
      type(diff_vector_address), intent(in) :: bs_trns_diff_v
      type(base_force_address), intent(in) :: fe_trns_frc
      type(phys_products_address), intent(in) :: fe_trns_prod
      type(spherical_transform_data), intent(in) :: trns_f_MHD
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_b_difv
!
      type(spherical_transform_data), intent(inout) :: trns_f_eflux
!
!
!$omp parallel
      if(fe_trns_prod%i_electric .gt. 0) then
        call cal_electric_field_smp                                     &
     &     (sph_rtp%nnod_rtp, cd_prop%coef_diffuse,                     &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_current),              &
     &      trns_f_MHD%fld_rtp(1,f_trns_frc%i_vp_induct),               &
     &      trns_f_eflux%fld_rtp(1,fe_trns_prod%i_electric))
      end if
!
      if(fe_trns_prod%i_poynting .gt. 0) then
        call cal_poynting_flux_smp                                      &
     &     (sph_rtp%nnod_rtp, cd_prop%coef_diffuse,                     &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_current),              &
     &      trns_f_MHD%fld_rtp(1,f_trns_frc%i_vp_induct),               &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_magne),                &
     &      trns_f_eflux%fld_rtp(1,fe_trns_prod%i_poynting))
      end if
!
      if(fe_trns_frc%i_mag_stretch .gt. 0) then
        call cal_rtp_magnetic_streach                                   &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), sph_rtp%nidx_rtp(2), &
     &      sph_rtp%a_r_1d_rtp_r, sph_rtp%cot_theta_1d_rtp,             &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_magne),                &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),                 &
     &      trns_b_difv%fld_rtp(1,bs_trns_diff_v%i_grad_vx),            &
     &      trns_b_difv%fld_rtp(1,bs_trns_diff_v%i_grad_vy),            &
     &      trns_b_difv%fld_rtp(1,bs_trns_diff_v%i_grad_vz),            &
     &      trns_f_eflux%fld_rtp(1,fe_trns_frc%i_mag_stretch))
      end if
!$omp end parallel
!
      end subroutine cal_magnetic_fluxes_rtp
!
!-----------------------------------------------------------------------
!
      subroutine cal_magnetic_fluxes_pole                               &
     &         (sph_rtp, cd_prop, f_trns_frc, bs_trns_base,             &
     &          bs_trns_diff_v, fe_trns_frc, fe_trns_prod,              &
     &          trns_f_MHD, trns_b_snap, trns_b_difv, trns_f_eflux)
!
      use poynting_flux_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(conductive_property), intent(in) :: cd_prop
      type(base_force_address), intent(in) :: f_trns_frc
      type(base_field_address), intent(in) :: bs_trns_base
      type(diff_vector_address), intent(in) :: bs_trns_diff_v
      type(base_force_address), intent(in) :: fe_trns_frc
      type(phys_products_address), intent(in) :: fe_trns_prod
      type(spherical_transform_data), intent(in) :: trns_f_MHD
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_b_difv
!
      type(spherical_transform_data), intent(inout) :: trns_f_eflux
!
!
!$omp parallel
      if(fe_trns_prod%i_electric .gt. 0) then
        call cal_electric_field_smp                                     &
     &     (sph_rtp%nnod_pole, cd_prop%coef_diffuse,                    &
     &      trns_b_snap%fld_pole(1,bs_trns_base%i_current),             &
     &      trns_f_MHD%fld_pole(1,f_trns_frc%i_vp_induct),              &
     &      trns_f_eflux%fld_pole(1,fe_trns_prod%i_electric))
      end if
!
      if(fe_trns_prod%i_poynting .gt. 0) then
        call cal_poynting_flux_smp                                      &
     &     (sph_rtp%nnod_pole, cd_prop%coef_diffuse,                    &
     &      trns_b_snap%fld_pole(1,bs_trns_base%i_current),             &
     &      trns_f_MHD%fld_pole(1,f_trns_frc%i_vp_induct),              &
     &      trns_b_snap%fld_pole(1,bs_trns_base%i_magne),               &
     &      trns_f_eflux%fld_pole(1,fe_trns_prod%i_poynting))
      end if
!
      if(fe_trns_frc%i_mag_stretch .gt. 0) then
        call cal_xyz_magnetic_streach(sph_rtp%nnod_pole,                &
     &      trns_b_snap%fld_pole(1,bs_trns_base%i_magne),               &
     &      trns_b_difv%fld_pole(1,bs_trns_diff_v%i_grad_vx),           &
     &      trns_b_difv%fld_pole(1,bs_trns_diff_v%i_grad_vy),           &
     &      trns_b_difv%fld_pole(1,bs_trns_diff_v%i_grad_vz),           &
     &      trns_f_eflux%fld_pole(1,fe_trns_frc%i_mag_stretch))
      end if
!$omp end parallel
!
      end subroutine cal_magnetic_fluxes_pole
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_helicity_on_node                                   &
     &         (bs_trns_base, fe_trns_prod, nnod,                       &
     &          ntot_comp_fld, fld_rtp, ntot_comp_hls, fhls_rtp)
!
      use cal_products_smp
!
      type(base_field_address), intent(in) :: bs_trns_base
      type(phys_products_address), intent(in) :: fe_trns_prod
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld, ntot_comp_hls
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
!
      real(kind = kreal), intent(inout) :: fhls_rtp(nnod,ntot_comp_hls)
!
!
!$omp parallel
      if(fe_trns_prod%i_k_heli .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      fld_rtp(1,bs_trns_base%i_vort),                             &
     &      fhls_rtp(1,fe_trns_prod%i_k_heli))
      end if
      if(fe_trns_prod%i_c_heli .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      fld_rtp(1,bs_trns_base%i_magne),                            &
     &      fld_rtp(1,bs_trns_base%i_current),                          &
     &      fhls_rtp(1,fe_trns_prod%i_c_heli))
      end if
      if(fe_trns_prod%i_x_heli .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      fld_rtp(1,bs_trns_base%i_magne),                            &
     &      fhls_rtp(1,fe_trns_prod%i_x_heli))
      end if
!$omp end parallel
!
      end subroutine cal_helicity_on_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_square_vector_on_node                              &
     &         (bs_trns_base, fe_trns_prod, nnod,                       &
     &          ntot_comp_fld, fld_rtp, ntot_comp_fmg, fmag_rtp)
!
      use cal_products_smp
!
      type(base_field_address), intent(in) :: bs_trns_base
      type(phys_products_address), intent(in) :: fe_trns_prod
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld, ntot_comp_fmg
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
!
      real(kind = kreal), intent(inout) :: fmag_rtp(nnod,ntot_comp_fmg)
!
!
!$omp parallel
      if(fe_trns_prod%i_square_v .gt. 0) then
        call vector_vector_prod_smp(nnod,                               &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      fmag_rtp(1,fe_trns_prod%i_square_v))
      end if
      if(fe_trns_prod%i_square_w .gt. 0) then
        call vector_vector_prod_smp(nnod,                               &
     &      fld_rtp(1,bs_trns_base%i_vort),                             &
     &      fld_rtp(1,bs_trns_base%i_vort),                             &
     &      fmag_rtp(1,fe_trns_prod%i_square_w))
      end if
      if(fe_trns_prod%i_square_b .gt. 0) then
        call vector_vector_prod_smp(nnod,                               &
     &      fld_rtp(1,bs_trns_base%i_magne),                            &
     &      fld_rtp(1,bs_trns_base%i_magne),                            &
     &      fmag_rtp(1,fe_trns_prod%i_square_b))
      end if
      if(fe_trns_prod%i_square_a .gt. 0) then
        call vector_vector_prod_smp(nnod,                               &
     &      fld_rtp(1,bs_trns_base%i_vecp),                             &
     &      fld_rtp(1,bs_trns_base%i_vecp),                             &
     &      fmag_rtp(1,fe_trns_prod%i_square_a))
      end if
      if(fe_trns_prod%i_square_j .gt. 0) then
        call vector_vector_prod_smp(nnod,                               &
     &      fld_rtp(1,bs_trns_base%i_current),                          &
     &      fld_rtp(1,bs_trns_base%i_current),                          &
     &      fmag_rtp(1,fe_trns_prod%i_square_j))
      end if
      if(fe_trns_prod%i_square_t .gt. 0) then
        call cal_scalar_prod_no_coef_smp(nnod,                          &
     &      fld_rtp(1,bs_trns_base%i_temp),                             &
     &      fld_rtp(1,bs_trns_base%i_temp),                             &
     &      fmag_rtp(1,fe_trns_prod%i_square_t))
      end if
      if(fe_trns_prod%i_square_c .gt. 0) then
        call cal_scalar_prod_no_coef_smp(nnod,                          &
     &      fld_rtp(1,bs_trns_base%i_light),                            &
     &      fld_rtp(1,bs_trns_base%i_light),                            &
     &      fmag_rtp(1,fe_trns_prod%i_square_c))
      end if
!$omp end parallel
!
      end subroutine cal_square_vector_on_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_lengh_scale_rtp                                    &
     &         (sph_rtp, bs_trns_base, bs_trns_dif, fe_trns_prod,       &
     &          trns_b_snap, trns_b_eflux, trns_f_eflux)
!
      use mag_of_field_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(base_field_address), intent(in) :: bs_trns_base
      type(diffusion_address), intent(in) :: bs_trns_dif
      type(phys_products_address), intent(in) :: fe_trns_prod
!
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_b_eflux
!
      type(spherical_transform_data), intent(inout) :: trns_f_eflux
!
!
!$omp parallel
      if(fe_trns_prod%i_velo_scale .gt. 0) then
        call cal_len_scale_by_rot_smp(sph_rtp%nnod_rtp,                 &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),                 &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_vort),                 &
     &      trns_f_eflux%fld_rtp(1,fe_trns_prod%i_velo_scale))
      end if
      if(fe_trns_prod%i_magne_scale .gt. 0) then
        call cal_len_scale_by_rot_smp(sph_rtp%nnod_rtp,                 &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_magne),                &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_current),              &
     &      trns_f_eflux%fld_rtp(1,fe_trns_prod%i_magne_scale))
      end if
      if(fe_trns_prod%i_temp_scale .gt. 0) then
        call cal_len_scale_by_diffuse_smp(sph_rtp%nnod_rtp,             &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_temp),                 &
     &      trns_b_eflux%fld_rtp(1,bs_trns_dif%i_t_diffuse),            &
     &      trns_f_eflux%fld_rtp(1,fe_trns_prod%i_temp_scale))
      end if
      if(fe_trns_prod%i_comp_scale .gt. 0) then
        call cal_len_scale_by_diffuse_smp(sph_rtp%nnod_rtp,             &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_light),                &
     &      trns_b_eflux%fld_rtp(1,bs_trns_dif%i_c_diffuse),            &
     &      trns_f_eflux%fld_rtp(1,fe_trns_prod%i_comp_scale))
      end if
!$omp end parallel
!
      end subroutine cal_lengh_scale_rtp
!
!-----------------------------------------------------------------------
!
      end module cal_energy_flux_rtp
