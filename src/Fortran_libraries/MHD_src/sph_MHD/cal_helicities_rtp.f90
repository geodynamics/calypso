!> @file  cal_helicities_rtp.f90
!!      module cal_helicities_rtp
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine s_cal_helicities_rtp(sph_rtp, fl_prop, leg,          &
!!     &          bs_trns, be_trns, fe_trns,                            &
!!     &          trns_b_snap, trns_b_eflux, trns_f_eflux)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: bs_trns
!!        type(phys_address), intent(in) :: be_trns, fe_trns
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(in) :: trns_b_eflux
!!        type(spherical_transform_data), intent(inout) :: trns_f_eflux
!!@endverbatim
!
      module cal_helicities_rtp
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
      subroutine s_cal_helicities_rtp(sph_rtp, fl_prop, leg,            &
     &          bs_trns, be_trns, fe_trns,                              &
     &          trns_b_snap, trns_b_eflux, trns_f_eflux)
!
      use const_wz_coriolis_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: bs_trns
      type(phys_address), intent(in) :: be_trns, fe_trns
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_b_eflux
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
      end subroutine s_cal_helicities_rtp
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
      end module cal_helicities_rtp
