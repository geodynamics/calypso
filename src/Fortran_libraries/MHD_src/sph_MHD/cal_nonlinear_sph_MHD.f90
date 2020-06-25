!>@file   cal_nonlinear_sph_MHD.f90
!!@brief  module cal_nonlinear_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine nonlinear_terms_in_rtp(sph_rtp, MHD_prop, leg,       &
!!     &          b_trns_base, f_trns_frc, trns_b_MHD, trns_f_MHD)
!!       Input ::  trns_b_MHD%fld_rtp(1,ib_fld)
!!               ib_fld = i_velo, base%i_vort, 
!!                        base%i_magne, base%i_current,
!!                        base%i_temp, base%i_light
!!       Output :: trns_f_MHD%fld_rtp(1,if_frc)
!!               if_frc = forces%i_m_advect, forces%i_lorentz, 
!!                        forces%i_vp_induct, forces%i_h_flux, 
!!                        forces%i_c_flux, forces%i_coriolis
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(base_field_address), intent(in) :: b_trns_base
!!        type(base_force_address), intent(in) :: f_trns_frc
!!        type(spherical_transform_data), intent(in) :: trns_b_MHD
!!        type(spherical_transform_data), intent(inout) :: trns_f_MHD
!!      subroutine cal_nonlinear_pole_MHD(sph_rtp, MHD_prop,            &
!!     &          b_trns_base, f_trns_frc, trns_b_snap, trns_f_MHD)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(base_field_address), intent(in) :: b_trns_base
!!        type(base_force_address), intent(in) :: f_trns_frc
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(inout) :: trns_f_MHD
!!      subroutine nonlinear_terms_on_node                              &
!!     &         (MHD_prop, b_trns_base, f_trns_frc,                    &
!!     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(base_field_address), intent(in) :: b_trns_base
!!        type(base_force_address), intent(in) :: f_trns_frc
!!      subroutine add_ref_advect_sph_MHD(sph_rj, sph_MHD_bc, MHD_prop, &
!!     &          leg, ref_temp, ref_comp, ipol, rj_fld)
!!       Input ::  rj_fld(1,is_fld)
!!               is_fld = i_velo, forces%i_h_advect, forces%i_c_advect
!!       Output :: rj_fld(1,is_fld)
!!               is_fld = forces%i_h_advect, forces%i_c_advect
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(reference_field), intent(in) :: ref_temp
!!        type(reference_field), intent(in) :: ref_comp
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine add_reference_advect_sph                             &
!!     &         (kr_in, kr_out, nidx_rj, ar_1d_rj, g_sph_rj,           &
!!     &          coef_advect, is_h_advect, is_velo,                    &
!!     &          nnod_rj, ntot_phys_rj, reftemp_rj, d_rj)
!!@endverbatim
!!
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out      Radial ID for outer boundary
!
      module cal_nonlinear_sph_MHD
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      use t_control_parameter
      use t_reference_scalar_param
      use t_spheric_rj_data
      use t_spheric_rtp_data
      use t_base_field_labels
      use t_base_force_labels
      use t_phys_address
      use t_phys_data
      use t_schmidt_poly_on_rtm
      use t_radial_reference_temp
      use t_boundary_data_sph_MHD
      use t_addresses_sph_transform
!
      implicit none
!
      private :: add_reference_advect_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine nonlinear_terms_in_rtp(sph_rtp, MHD_prop, leg,         &
     &          b_trns_base, f_trns_frc, trns_b_MHD, trns_f_MHD)
!
      use const_wz_coriolis_rtp
      use cal_products_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(legendre_4_sph_trans), intent(in) :: leg
      type(base_field_address), intent(in) :: b_trns_base
      type(base_force_address), intent(in) :: f_trns_frc
      type(spherical_transform_data), intent(in) :: trns_b_MHD
      type(spherical_transform_data), intent(inout) :: trns_f_MHD
!
!
      call nonlinear_terms_on_node                                      &
     &   (MHD_prop, b_trns_base, f_trns_frc, sph_rtp%nnod_rtp,          &
     &    trns_b_MHD%ncomp, trns_b_MHD%fld_rtp,                         &
     &    trns_f_MHD%ncomp, trns_f_MHD%fld_rtp)
!
      if(f_trns_frc%i_coriolis .gt. 0) then
        call cal_wz_coriolis_rtp(sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,    &
     &      leg%g_colat_rtp, MHD_prop%fl_prop%coef_cor,                 &
     &      trns_b_MHD%fld_rtp(1,b_trns_base%i_velo),                   &
     &      trns_f_MHD%fld_rtp(1,f_trns_frc%i_coriolis))
      end if
!
      end subroutine nonlinear_terms_in_rtp
!
!-----------------------------------------------------------------------
!
      subroutine cal_nonlinear_pole_MHD(sph_rtp, MHD_prop,              &
     &          b_trns_base, f_trns_frc, trns_b_snap, trns_f_MHD)
!
      use const_wz_coriolis_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(base_field_address), intent(in) :: b_trns_base
      type(base_force_address), intent(in) :: f_trns_frc
      type(spherical_transform_data), intent(in) :: trns_b_snap
!
      type(spherical_transform_data), intent(inout) :: trns_f_MHD
!
!
      call nonlinear_terms_on_node(MHD_prop,                            &
     &    b_trns_base, f_trns_frc, sph_rtp%nnod_pole,                   &
     &    trns_b_snap%ncomp, trns_b_snap%fld_pole,                      &
     &    trns_f_MHD%ncomp, trns_f_MHD%fld_pole)
!
      if(MHD_prop%fl_prop%iflag_4_coriolis) then
        call cal_wz_coriolis_pole                                       &
     &     (sph_rtp%nnod_pole, MHD_prop%fl_prop%coef_cor,               &
     &      trns_b_snap%fld_pole(1,b_trns_base%i_velo),                 &
     &      trns_f_MHD%fld_pole(1,f_trns_frc%i_coriolis))
      end if
!
      end subroutine cal_nonlinear_pole_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine nonlinear_terms_on_node                                &
     &         (MHD_prop, b_trns_base, f_trns_frc,                      &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp)
!
      use cal_products_smp
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(base_field_address), intent(in) :: b_trns_base
      type(base_force_address), intent(in) :: f_trns_frc
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld, ntot_comp_frc
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod,ntot_comp_frc)
!
!$omp parallel
      if(f_trns_frc%i_m_advect .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (nnod, MHD_prop%fl_prop%coef_velo,                           &
     &      fld_rtp(1,b_trns_base%i_vort),                              &
     &      fld_rtp(1,b_trns_base%i_velo),                              &
     &      frc_rtp(1,f_trns_frc%i_m_advect) )
      end if
!
      if(f_trns_frc%i_lorentz .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (nnod, MHD_prop%fl_prop%coef_lor,                            &
     &      fld_rtp(1,b_trns_base%i_current),                           &
     &      fld_rtp(1,b_trns_base%i_magne),                             &
     &      frc_rtp(1,f_trns_frc%i_lorentz) )
      end if
!
!
      if(f_trns_frc%i_vp_induct .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (nnod, MHD_prop%cd_prop%coef_induct,                         &
     &      fld_rtp(1,b_trns_base%i_velo),                              &
     &      fld_rtp(1,b_trns_base%i_magne),                             &
     &      frc_rtp(1,f_trns_frc%i_vp_induct) )
      end if
!
!
      if(f_trns_frc%i_h_flux .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (nnod, MHD_prop%ht_prop%coef_advect,                         &
     &      fld_rtp(1,b_trns_base%i_velo),                              &
     &      fld_rtp(1,b_trns_base%i_temp),                              &
     &      frc_rtp(1,f_trns_frc%i_h_flux) )
      end if
!
      if(f_trns_frc%i_c_flux .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (nnod, MHD_prop%cp_prop%coef_advect,                         &
     &      fld_rtp(1,b_trns_base%i_velo),                              &
     &      fld_rtp(1,b_trns_base%i_light),                             &
     &      frc_rtp(1,f_trns_frc%i_c_flux) )
      end if
!$omp end parallel
!
      end subroutine nonlinear_terms_on_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_ref_advect_sph_MHD(sph_rj, sph_MHD_bc, MHD_prop,   &
     &          leg, ref_temp, ref_comp, ipol, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
!
      type(reference_field), intent(in) :: ref_temp
      type(reference_field), intent(in) :: ref_comp
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!   ----  Lead advection of reference field
        call add_reference_advect_sph(sph_MHD_bc%sph_bc_T,              &
     &      MHD_prop%ht_prop, MHD_prop%ref_param_T,                     &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, leg%g_sph_rj,              &
     &      ipol%forces%i_h_advect, ipol%base%i_velo,                   &
     &      ref_temp%t_rj, rj_fld)
        call add_reference_advect_sph(sph_MHD_bc%sph_bc_C,              &
     &      MHD_prop%cp_prop, MHD_prop%ref_param_C,                     &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, leg%g_sph_rj,              &
     &      ipol%forces%i_c_advect, ipol%base%i_velo,                   &
     &      ref_comp%t_rj, rj_fld)
!
      end subroutine add_ref_advect_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_reference_advect_sph                               &
     &         (sph_bc_S, property, ref_param_S,                        &
     &          nidx_rj, ar_1d_rj, g_sph_rj, is_h_advect, is_velo,      &
     &          reftemp_rj, rj_fld)
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: is_h_advect, is_velo
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),13)
      real(kind = kreal), intent(in) :: ar_1d_rj(nidx_rj(1),3)
      real(kind = kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:2)
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(scalar_property), intent(in) :: property
      type(reference_scalar_param), intent(in) :: ref_param_S
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      if (ref_param_S%iflag_reference .ne. id_sphere_ref_temp) return
!
      ist = (sph_bc_S%kr_in-1) * nidx_rj(2) + 1
      ied =  sph_bc_S%kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        rj_fld%d_fld(inod,is_h_advect) = rj_fld%d_fld(inod,is_h_advect) &
     &           + property%coef_advect * g_sph_rj(j,3) * ar_1d_rj(k,2) &
     &            * reftemp_rj(k,1) * rj_fld%d_fld(inod,is_velo)
      end do
!$omp end parallel do
!
      end subroutine add_reference_advect_sph
!
!-----------------------------------------------------------------------
!
      end module cal_nonlinear_sph_MHD
