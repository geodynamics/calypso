!>@file   cal_nonlinear_sph_MHD.f90
!!@brief  module cal_nonlinear_sph_MHD
!!
!!@author H. Matsui (UC Berkeley) and T. Kera (Tohoku University)
!!@date Programmed in Oct., 2009
!>        Modified by T. Kera in Aug., 2021
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
      use t_radial_reference_field
      use t_boundary_data_sph_MHD
      use t_addresses_sph_transform
!
      implicit none
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
        call sel_wz_coriolis_rtp                                        &
     &     (sph_rtp, leg, MHD_prop%fl_prop%coef_cor,                    &
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
      if(MHD_prop%fl_prop%flag_coriolis) then
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
      use cal_vector_products
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
      if(f_trns_frc%i_m_advect .gt. 0) then
        call cal_cross_product_w_coef                                   &
     &     (nnod, MHD_prop%fl_prop%coef_velo,                           &
     &      fld_rtp(1,b_trns_base%i_vort),                              &
     &      fld_rtp(1,b_trns_base%i_velo),                              &
     &      frc_rtp(1,f_trns_frc%i_m_advect) )
      end if
!
      if(f_trns_frc%i_lorentz .gt. 0) then
        call cal_cross_product_w_coef                                   &
     &     (nnod, MHD_prop%fl_prop%coef_lor,                            &
     &      fld_rtp(1,b_trns_base%i_current),                           &
     &      fld_rtp(1,b_trns_base%i_magne),                             &
     &      frc_rtp(1,f_trns_frc%i_lorentz) )
!
        if(b_trns_base%i_back_B .gt. 0) then
          call add_cross_product_w_coef                                 &
     &       (nnod, MHD_prop%fl_prop%coef_lor,                          &
     &        fld_rtp(1,b_trns_base%i_current),                         &
     &        fld_rtp(1,b_trns_base%i_back_B),                          &
     &        frc_rtp(1,f_trns_frc%i_lorentz) )
        end if
      end if
!
!
      if(f_trns_frc%i_vp_induct .gt. 0) then
        call cal_cross_product_w_coef                                   &
     &     (nnod, MHD_prop%cd_prop%coef_induct,                         &
     &      fld_rtp(1,b_trns_base%i_velo),                              &
     &      fld_rtp(1,b_trns_base%i_magne),                             &
     &      frc_rtp(1,f_trns_frc%i_vp_induct) )
!
        if(b_trns_base%i_back_B .gt. 0) then
          call add_cross_product_w_coef                                 &
     &       (nnod, MHD_prop%cd_prop%coef_induct,                       &
     &        fld_rtp(1,b_trns_base%i_velo),                            &
     &        fld_rtp(1,b_trns_base%i_back_B),                          &
     &        frc_rtp(1,f_trns_frc%i_vp_induct) )
        end if
      end if
!
!
      if(f_trns_frc%i_h_flux .gt. 0) then
        call cal_vec_scalar_product_w_coef                              &
     &     (nnod, MHD_prop%ht_prop%coef_advect,                         &
     &      fld_rtp(1,b_trns_base%i_velo),                              &
     &      fld_rtp(1,b_trns_base%i_temp),                              &
     &      frc_rtp(1,f_trns_frc%i_h_flux) )
      end if
!
      if(f_trns_frc%i_c_flux .gt. 0) then
        call cal_vec_scalar_product_w_coef                              &
     &     (nnod, MHD_prop%cp_prop%coef_advect,                         &
     &      fld_rtp(1,b_trns_base%i_velo),                              &
     &      fld_rtp(1,b_trns_base%i_light),                             &
     &      frc_rtp(1,f_trns_frc%i_c_flux) )
      end if
!
      end subroutine nonlinear_terms_on_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine nonlinear_terms_on_node_w_sym                          &
     &         (MHD_prop, b_trns_base_1, b_trns_base_2, f_trns_frc,     &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp)
!
      use cal_products_smp
      use cal_vector_products
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(base_field_address), intent(in) :: b_trns_base_1
      type(base_field_address), intent(in) :: b_trns_base_2
      type(base_force_address), intent(in) :: f_trns_frc
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld, ntot_comp_frc
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod,ntot_comp_frc)
!
!
      if(f_trns_frc%i_m_advect .gt. 0) then
        call cal_cross_product_w_coef                                   &
     &     (nnod, MHD_prop%fl_prop%coef_velo,                           &
     &      fld_rtp(1,b_trns_base_1%i_vort),                            &
     &      fld_rtp(1,b_trns_base_2%i_velo),                            &
     &      frc_rtp(1,f_trns_frc%i_m_advect) )
      end if
!
      if(f_trns_frc%i_lorentz .gt. 0) then
        call cal_cross_product_w_coef                                   &
     &     (nnod, MHD_prop%fl_prop%coef_lor,                            &
     &      fld_rtp(1,b_trns_base_1%i_current),                         &
     &      fld_rtp(1,b_trns_base_2%i_magne),                           &
     &      frc_rtp(1,f_trns_frc%i_lorentz) )
!
        if(b_trns_base_2%i_back_B .gt. 0) then
          call add_cross_product_w_coef                                 &
     &       (nnod, MHD_prop%fl_prop%coef_lor,                          &
     &        fld_rtp(1,b_trns_base_1%i_current),                       &
     &        fld_rtp(1,b_trns_base_2%i_back_B),                        &
     &        frc_rtp(1,f_trns_frc%i_lorentz) )
        end if
      end if
!
!
      if(f_trns_frc%i_vp_induct .gt. 0) then
        call cal_cross_product_w_coef                                   &
     &     (nnod, MHD_prop%cd_prop%coef_induct,                         &
     &      fld_rtp(1,b_trns_base_1%i_velo),                            &
     &      fld_rtp(1,b_trns_base_2%i_magne),                           &
     &      frc_rtp(1,f_trns_frc%i_vp_induct) )
!
        if(b_trns_base_2%i_back_B .gt. 0) then
          call add_cross_product_w_coef                                 &
     &       (nnod, MHD_prop%cd_prop%coef_induct,                       &
     &        fld_rtp(1,b_trns_base_1%i_velo),                          &
     &        fld_rtp(1,b_trns_base_2%i_back_B),                        &
     &        frc_rtp(1,f_trns_frc%i_vp_induct) )
        end if
      end if
!
!
      if(f_trns_frc%i_h_flux .gt. 0) then
        call cal_vec_scalar_product_w_coef                              &
     &     (nnod, MHD_prop%ht_prop%coef_advect,                         &
     &      fld_rtp(1,b_trns_base_1%i_velo),                            &
     &      fld_rtp(1,b_trns_base_2%i_temp),                            &
     &      frc_rtp(1,f_trns_frc%i_h_flux) )
      end if
!
      if(f_trns_frc%i_c_flux .gt. 0) then
        call cal_vec_scalar_product_w_coef                              &
     &     (nnod, MHD_prop%cp_prop%coef_advect,                         &
     &      fld_rtp(1,b_trns_base_1%i_velo),                            &
     &      fld_rtp(1,b_trns_base_2%i_light),                           &
     &      frc_rtp(1,f_trns_frc%i_c_flux) )
      end if
!
      end subroutine nonlinear_terms_on_node_w_sym
!
!-----------------------------------------------------------------------
!
      end module cal_nonlinear_sph_MHD
