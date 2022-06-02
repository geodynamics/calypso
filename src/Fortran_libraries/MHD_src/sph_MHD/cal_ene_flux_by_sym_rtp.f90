!> @file  cal_ene_flux_by_filter_rtp.f90
!!      module cal_ene_flux_by_filter_rtp
!!
!! @author  T. Kera (Tohoku University)
!! @date Programmed in Aug., 2021
!
!> @brief Evaluate energy fluxes by filtered field
!!
!!@verbatim
!!      subroutine s_cal_ene_flux_by_sym_rtp                            &
!!     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C,           &
!!     &          bs_trns, fs_trns, be_trns, fe_trns,                   &
!!     &          trns_b_snap, trns_f_snap, trns_b_eflux, trns_f_eflux)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(phys_address), intent(in) :: bs_trns, fs_trns
!!        type(phys_address), intent(in) :: be_trns, fe_trns
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(in) :: trns_f_snap
!!        type(spherical_transform_data), intent(in) :: trns_b_eflux
!!        type(spherical_transform_data), intent(inout) :: trns_f_eflux
!!@endverbatim
!
      module cal_ene_flux_by_sym_rtp
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
      private :: cal_work_of_lorentz_on_node
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_ene_flux_by_sym_rtp                              &
     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C,             &
     &          bs_trns, fs_trns, be_trns, fe_trns,                     &
     &          trns_b_snap, trns_f_snap, trns_b_eflux, trns_f_eflux)
!
      use cal_buoyancy_flux_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(phys_address), intent(in) :: bs_trns, fs_trns
      type(phys_address), intent(in) :: be_trns, fe_trns
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_f_snap
      type(spherical_transform_data), intent(in) :: trns_b_eflux
!
      type(spherical_transform_data), intent(inout) :: trns_f_eflux
!
!
      call cal_work_of_lorentz_on_node                                  &
     &   (bs_trns%sym_fld, fs_trns%forces_by_sym_asym,                  &
     &    fe_trns%eflux_to_sym_by_sym_asym, sph_rtp%nnod_rtp,           &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_snap%ncomp, trns_f_snap%fld_rtp,                       &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
      call cal_work_of_lorentz_on_node                                  &
     &   (bs_trns%sym_fld, fs_trns%forces_by_asym_sym,                  &
     &    fe_trns%eflux_to_sym_by_asym_sym, sph_rtp%nnod_rtp,           &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_snap%ncomp, trns_f_snap%fld_rtp,                       &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
      call cal_work_of_lorentz_on_node                                  &
     &   (bs_trns%asym_fld, fs_trns%forces_by_sym_sym,                  &
     &    fe_trns%eflux_to_asym_by_sym_sym, sph_rtp%nnod_rtp,           &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_snap%ncomp, trns_f_snap%fld_rtp,                       &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
      call cal_work_of_lorentz_on_node                                  &
     &   (bs_trns%asym_fld, fs_trns%forces_by_asym_asym,                &
     &    fe_trns%eflux_to_asym_by_asym_asym, sph_rtp%nnod_rtp,         &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_snap%ncomp, trns_f_snap%fld_rtp,                       &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
!
!
      call cal_buoyancy_flux_rtp                                        &
     &   (sph_rtp, fl_prop, ref_param_T, ref_param_C,                   &
     &    bs_trns%sym_fld, bs_trns%sym_fld,                             &
     &    fe_trns%eflux_to_sym_by_sym_asym,                             &
     &    trns_b_snap, trns_b_snap, trns_f_eflux)
      call cal_buoyancy_flux_rtp                                        &
     &   (sph_rtp, fl_prop, ref_param_T, ref_param_C,                   &
     &   bs_trns%asym_fld, bs_trns%asym_fld,                            &
     &   fe_trns%eflux_to_asym_by_sym_sym,                              &
     &   trns_b_snap, trns_b_snap, trns_f_eflux)
!
!
     call cal_work_of_inertia_on_node                                   &
     &   (bs_trns%sym_fld, fs_trns%forces_by_sym_asym,                  &
     &    fe_trns%eflux_to_sym_by_sym_asym, sph_rtp%nnod_rtp,           &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_snap%ncomp, trns_f_snap%fld_rtp,                       &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
     call cal_work_of_inertia_on_node                                   &
     &   (bs_trns%sym_fld, fs_trns%forces_by_asym_sym,                  &
     &    fe_trns%eflux_to_sym_by_asym_sym, sph_rtp%nnod_rtp,           &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_snap%ncomp, trns_f_snap%fld_rtp,                       &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
     call cal_work_of_inertia_on_node                                   &
     &   (bs_trns%asym_fld, fs_trns%forces_by_sym_sym,                  &
     &    fe_trns%eflux_to_asym_by_sym_sym, sph_rtp%nnod_rtp,           &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_snap%ncomp, trns_f_snap%fld_rtp,                       &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
     call cal_work_of_inertia_on_node                                   &
     &   (bs_trns%asym_fld, fs_trns%forces_by_asym_asym,                &
     &    fe_trns%eflux_to_asym_by_asym_asym, sph_rtp%nnod_rtp,         &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_snap%ncomp, trns_f_snap%fld_rtp,                       &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
! 
!
     call cal_ene_flux_by_induct_on_node                                &
     &   (bs_trns%asym_fld, be_trns%forces_by_sym_asym,                 &
     &    fe_trns%eflux_to_sym_by_sym_asym, sph_rtp%nnod_rtp,           &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_b_eflux%ncomp, trns_b_eflux%fld_rtp,                     &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
     call cal_ene_flux_by_induct_on_node                                &
     &   (bs_trns%asym_fld, be_trns%forces_by_asym_sym,                 &
     &    fe_trns%eflux_to_sym_by_asym_sym, sph_rtp%nnod_rtp,           &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_b_eflux%ncomp, trns_b_eflux%fld_rtp,                     &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
     call cal_ene_flux_by_induct_on_node                                &
     &   (bs_trns%sym_fld, be_trns%forces_by_sym_sym,                   &
     &    fe_trns%eflux_to_asym_by_sym_sym, sph_rtp%nnod_rtp,           &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_b_eflux%ncomp, trns_b_eflux%fld_rtp,                     &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
     call cal_ene_flux_by_induct_on_node                                &
     &   (bs_trns%sym_fld, fs_trns%forces_by_asym_asym,                 &
     &    fe_trns%eflux_to_asym_by_asym_asym, sph_rtp%nnod_rtp,         &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_b_eflux%ncomp, trns_b_eflux%fld_rtp,                     &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
!
      end subroutine s_cal_ene_flux_by_sym_rtp
!
!-----------------------------------------------------------------------
!
      subroutine cal_work_of_lorentz_on_node                            &
     &         (bs_trns_base, f_trns_frc, fs_trns_eflux,                &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp,   &
     &          ntot_comp_flx, flx_rtp)
!
      use cal_products_smp
!
      type(base_field_address), intent(in) :: bs_trns_base
      type(base_force_address), intent(in) :: f_trns_frc
      type(energy_flux_address), intent(in) :: fs_trns_eflux
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld, ntot_comp_frc
      integer(kind = kint), intent(in) :: ntot_comp_flx
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
      real(kind = kreal), intent(in) :: frc_rtp(nnod,ntot_comp_frc)
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
!$omp end parallel
!
      end subroutine cal_work_of_lorentz_on_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_work_of_inertia_on_node                            &
     &         (bs_trns_base, f_trns_frc, fs_trns_eflux,                &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp,   &
     &          ntot_comp_flx, flx_rtp)
!
      use cal_products_smp
!
      type(base_field_address), intent(in) :: bs_trns_base
      type(base_force_address), intent(in) :: f_trns_frc
      type(energy_flux_address), intent(in) :: fs_trns_eflux
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld, ntot_comp_frc
      integer(kind = kint), intent(in) :: ntot_comp_flx
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
      real(kind = kreal), intent(in) :: frc_rtp(nnod,ntot_comp_frc)
!
      real(kind = kreal), intent(inout) :: flx_rtp(nnod,ntot_comp_flx)
!
!$omp parallel
      if(fs_trns_eflux%i_m_advect_work .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      frc_rtp(1,f_trns_frc%i_m_advect),                           &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_m_advect_work) )
      end if
!$omp end parallel
!
      end subroutine cal_work_of_inertia_on_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_ene_flux_by_induct_on_node                         &
     &         (bs_trns_base, ipol_frc, fs_trns_eflux,                  &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp,   &
     &          ntot_comp_flx, flx_rtp)
!
      use cal_products_smp
!
      type(base_field_address), intent(in) :: bs_trns_base
      type(base_force_address), intent(in) :: ipol_frc
      type(energy_flux_address), intent(in) :: fs_trns_eflux
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld, ntot_comp_frc
      integer(kind = kint), intent(in) :: ntot_comp_flx
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
      real(kind = kreal), intent(in) :: frc_rtp(nnod,ntot_comp_frc)
!
      real(kind = kreal), intent(inout) :: flx_rtp(nnod,ntot_comp_flx)
!
!$omp parallel
      if(fs_trns_eflux%i_m_advect_work .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      frc_rtp(1,ipol_frc%i_induction),                            &
     &      fld_rtp(1,bs_trns_base%i_magne),                            &
     &      flx_rtp(1,fs_trns_eflux%i_me_gen) )
      end if
!$omp end parallel
!
      end subroutine cal_ene_flux_by_induct_on_node
!
!-----------------------------------------------------------------------
!
      end module cal_ene_flux_by_sym_rtp
