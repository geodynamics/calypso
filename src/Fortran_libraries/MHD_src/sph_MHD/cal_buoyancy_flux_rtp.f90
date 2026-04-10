!> @file  cal_buoyancy_flux_rtp.f90
!!      module cal_buoyancy_flux_rtp
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine s_cal_buoyancy_flux_rtp                              &
!!     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C,           &
!!     &          bs_trns_base, bs_trns_scalar, fs_trns_eflux,          &
!!     &          trns_b_snap, trns_b_scl, trns_f_eflux)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(base_field_address), intent(in) :: bs_trns_base
!!        type(base_field_address), intent(in) :: bs_trns_scalar
!!        type(energy_flux_address), intent(in) :: fs_trns_eflux
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(in) :: trns_b_scl
!!        type(spherical_transform_data), intent(inout) :: trns_f_eflux
!!@endverbatim
!
      module cal_buoyancy_flux_rtp
!
      use m_precision
      use m_constants
      use m_machine_parameter
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
      private :: sel_buoyancy_flux_rtp, sel_pole_sph_buoyancy_flux
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_buoyancy_flux_rtp                                &
     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C,             &
     &          bs_trns_base, bs_trns_scalar, fs_trns_eflux,            &
     &          trns_b_snap, trns_b_scl, trns_f_eflux)
!
      use cal_self_buoyancies_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(base_field_address), intent(in) :: bs_trns_base
      type(base_field_address), intent(in) :: bs_trns_scalar
      type(energy_flux_address), intent(in) :: fs_trns_eflux
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_b_scl
!
      type(spherical_transform_data), intent(inout) :: trns_f_eflux
!
      integer(kind = kint) :: ibuo_temp,  ibuo_comp
!
!
      call sel_field_address_for_buoyancies                             &
     &   (bs_trns_scalar, ref_param_T, ref_param_C,                     &
     &    ibuo_temp, ibuo_comp)
!
      if(fs_trns_eflux%i_t_buo_gen .gt. 0) then
        call sel_buoyancy_flux_rtp(fl_prop%i_grav, sph_rtp,             &
     &      fl_prop%coef_buo, trns_b_scl%fld_rtp(1,ibuo_temp),          &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),                 &
     &      trns_f_eflux%fld_rtp(1,fs_trns_eflux%i_t_buo_gen))
        call sel_pole_sph_buoyancy_flux                                 &
     &     (fl_prop%i_grav, sph_rtp%nnod_pole, sph_rtp%nidx_rtp(1),     &
     &      sph_rtp%radius_1d_rtp_r, fl_prop%coef_buo,                  &
     &      trns_b_scl%fld_pole(1,ibuo_temp),                           &
     &      trns_b_snap%fld_pole(1,bs_trns_base%i_velo),                &
     &      trns_f_eflux%fld_pole(1,fs_trns_eflux%i_t_buo_gen))
      end if
!
      if(fs_trns_eflux%i_c_buo_gen .gt. 0) then
        call sel_buoyancy_flux_rtp(fl_prop%i_grav, sph_rtp,             &
     &      fl_prop%coef_comp_buo, trns_b_scl%fld_rtp(1,ibuo_comp),     &
     &      trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),                 &
     &      trns_f_eflux%fld_rtp(1,fs_trns_eflux%i_c_buo_gen))
        call sel_pole_sph_buoyancy_flux                                 &
     &     (fl_prop%i_grav, sph_rtp%nnod_pole, sph_rtp%nidx_rtp(1),     &
     &      sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,             &
     &      trns_b_scl%fld_pole(1,ibuo_comp),                           &
     &      trns_b_snap%fld_pole(1,bs_trns_base%i_velo),                &
     &      trns_f_eflux%fld_pole(1,fs_trns_eflux%i_c_buo_gen))
      end if
!
      call cal_total_buoyancy_scalar                                    &
     &   (fs_trns_eflux%i_t_buo_gen, fs_trns_eflux%i_c_buo_gen,         &
     &    fs_trns_eflux%i_buoyancy_flux, sph_rtp%nnod_rtp,              &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_rtp)
      call cal_total_buoyancy_scalar                                    &
     &   (fs_trns_eflux%i_t_buo_gen, fs_trns_eflux%i_c_buo_gen,         &
     &    fs_trns_eflux%i_buoyancy_flux, sph_rtp%nnod_pole,             &
     &    trns_f_eflux%ncomp, trns_f_eflux%fld_pole)
!
      end subroutine s_cal_buoyancy_flux_rtp
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_buoyancy_flux_rtp(i_grav, sph_rtp, coef,           &
     &                                 scalar, vr, prod)
!
      use cal_sph_buoyancy_flux
      use cal_products_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: i_grav
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: scalar(sph_rtp%nnod_rtp)
      real(kind=kreal), intent(in) :: vr(sph_rtp%nnod_rtp)
!
      real(kind=kreal), intent(inout) :: prod(sph_rtp%nnod_rtp)
!
!
      if(i_grav .eq. iflag_radial_g) then
        call cal_scalar_product_w_coef(sph_rtp%nnod_rtp, coef,          &
     &                                 scalar, vr, prod)
      else
        if(sph_rtp%istep_rtp(1) .eq. 1) then
          call sph_self_buoyancy_flux_rin                               &
     &       (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                       &
     &        sph_rtp%radius_1d_rtp_r, coef, scalar, vr, prod)
        else
          call sph_self_buoyancy_flux_pin                               &
     &       (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                       &
     &        sph_rtp%radius_1d_rtp_r, coef, scalar, vr, prod)
        end if
      end if
!
      end subroutine sel_buoyancy_flux_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_pole_sph_buoyancy_flux                             &
     &         (i_grav, nnod_pole, nidx_rtp_r, radius, coef,            &
     &          t_pole, v_pole, d_pole)
!
      use cal_sph_buoyancy_flux
!
      integer(kind = kint), intent(in) :: i_grav
      integer(kind = kint), intent(in) :: nnod_pole
      integer(kind = kint), intent(in) :: nidx_rtp_r
      real(kind=kreal), intent(in) :: radius(nidx_rtp_r)
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: t_pole(nnod_pole)
      real(kind = kreal), intent(in) :: v_pole(nnod_pole,3)
!
      real(kind = kreal), intent(inout) :: d_pole(nnod_pole)
!
!
      if(i_grav .eq. iflag_radial_g) then
        call pole_sph_const_buoyancy_flux(nnod_pole, nidx_rtp_r, coef,  &
     &                                    t_pole, v_pole, d_pole)
      else
        call pole_sph_self_buoyancy_flux(nnod_pole, nidx_rtp_r, radius, &
     &                                   coef, t_pole, v_pole, d_pole)
      end if
!
      end subroutine sel_pole_sph_buoyancy_flux
!
! -----------------------------------------------------------------------
!
      end module cal_buoyancy_flux_rtp
