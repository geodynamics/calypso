!> @file  cal_buoyancy_flux_sph.f90
!!      module cal_buoyancy_flux_sph
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine cal_buoyancy_flux_rtp                                &
!!     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C,           &
!!     &          bs_trns_base, bs_trns_scalar, fs_trns_eflux,          &
!!     &          trns_b_snap, trns_b_scl, trns_f_eflux)
!!      subroutine pole_buoyancy_flux_rtp                               &
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
      module cal_buoyancy_flux_sph
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
      private :: pole_sph_buoyancy_flux
      private :: cal_buoyancy_flux_rin, cal_buoyancy_flux_pin
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_buoyancy_flux_rtp                                  &
     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C,             &
     &          bs_trns_base, bs_trns_scalar, fs_trns_eflux,            &
     &          trns_b_snap, trns_b_scl, trns_f_eflux)
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
!
      if(fs_trns_eflux%i_buo_gen .gt. 0) then
        if    (ref_param_T%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_T%iflag_reference .eq. id_takepiro_temp) then
          call sel_buoyancy_flux_rtp(sph_rtp, fl_prop%coef_buo,         &
     &        trns_b_scl%fld_rtp(1,bs_trns_scalar%i_per_temp),          &
     &        trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),               &
     &        trns_f_eflux%fld_rtp(1,fs_trns_eflux%i_buo_gen))
        else
          call sel_buoyancy_flux_rtp(sph_rtp, fl_prop%coef_buo,         &
     &        trns_b_scl%fld_rtp(1,bs_trns_scalar%i_temp),              &
     &        trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),               &
     &        trns_f_eflux%fld_rtp(1,fs_trns_eflux%i_buo_gen))
        end if
      end if
!
      if(fs_trns_eflux%i_c_buo_gen .gt. 0) then
        if    (ref_param_C%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_C%iflag_reference .eq. id_takepiro_temp) then
          call sel_buoyancy_flux_rtp(sph_rtp, fl_prop%coef_comp_buo,    &
     &        trns_b_scl%fld_rtp(1,bs_trns_scalar%i_per_light),         &
     &        trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),               &
     &        trns_f_eflux%fld_rtp(1,fs_trns_eflux%i_c_buo_gen))
        else
          call sel_buoyancy_flux_rtp(sph_rtp, fl_prop%coef_comp_buo,    &
     &        trns_b_scl%fld_rtp(1,bs_trns_scalar%i_light),             &
     &        trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),               &
     &        trns_f_eflux%fld_rtp(1,fs_trns_eflux%i_c_buo_gen) )
        end if
      end if
!
      end subroutine cal_buoyancy_flux_rtp
!
!-----------------------------------------------------------------------
!
      subroutine pole_buoyancy_flux_rtp                                 &
     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C,             &
     &          bs_trns_base, bs_trns_scalar, fs_trns_eflux,            &
     &          trns_b_snap, trns_b_scl, trns_f_eflux)
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
!
!$omp parallel
      if(fs_trns_eflux%i_buo_gen .gt. 0) then
        if    (ref_param_T%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_T%iflag_reference .eq. id_takepiro_temp) then
          call pole_sph_buoyancy_flux                                   &
     &       (sph_rtp%nnod_pole, sph_rtp%nidx_rtp(1),                   &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_buo,                &
     &        trns_b_scl%fld_pole(1,bs_trns_scalar%i_per_temp),         &
     &        trns_b_snap%fld_pole(1,bs_trns_base%i_velo),              &
     &        trns_f_eflux%fld_pole(1,fs_trns_eflux%i_buo_gen))
        else
          call pole_sph_buoyancy_flux                                   &
     &       (sph_rtp%nnod_pole, sph_rtp%nidx_rtp(1),                   &
     &        sph_rtp%radius_1d_rtp_r,  fl_prop%coef_buo,               &
     &        trns_b_scl%fld_pole(1,bs_trns_scalar%i_temp),             &
     &        trns_b_snap%fld_pole(1,bs_trns_base%i_velo),              &
     &        trns_f_eflux%fld_pole(1,fs_trns_eflux%i_buo_gen))
        end if
      end if
!
      if(fs_trns_eflux%i_c_buo_gen .gt. 0) then
        if    (ref_param_C%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_C%iflag_reference .eq. id_takepiro_temp) then
          call pole_sph_buoyancy_flux                                   &
     &       (sph_rtp%nnod_pole, sph_rtp%nidx_rtp(1),                   &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,           &
     &        trns_b_scl%fld_pole(1,bs_trns_scalar%i_per_light),        &
     &        trns_b_snap%fld_pole(1,bs_trns_base%i_velo),              &
     &        trns_f_eflux%fld_pole(1,fs_trns_eflux%i_c_buo_gen))
        else
          call pole_sph_buoyancy_flux                                   &
     &       (sph_rtp%nnod_pole, sph_rtp%nidx_rtp(1),                   &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,           &
     &        trns_b_scl%fld_pole(1,bs_trns_scalar%i_light),            &
     &        trns_b_snap%fld_pole(1,bs_trns_base%i_velo),              &
     &        trns_f_eflux%fld_pole(1,fs_trns_eflux%i_c_buo_gen) )
        end if
      end if
!$omp end parallel
!
      end subroutine pole_buoyancy_flux_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pole_sph_buoyancy_flux(nnod_pole, nidx_rtp_r, radius,  &
     &          coef, t_pole, v_pole, d_pole)
!
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
      integer(kind = kint) :: inod, kr
!
!
!  field for north pole (kr) and south pole (inod)
!$omp parallel do private(kr,inod)
      do kr = 1, nidx_rtp_r
        inod = kr + nidx_rtp_r
        d_pole(kr)                                                      &
     &       = coef*t_pole(kr)*v_pole(kr,3)*radius(kr)
        d_pole(inod)                                                    &
     &     = -coef*t_pole(inod)*v_pole(inod,3)*radius(kr)
      end do
!
      d_pole(2*nidx_rtp_r+1) = zero
!
      end subroutine pole_sph_buoyancy_flux
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_buoyancy_flux_rtp(sph_rtp, coef, scalar, vr, prod)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: scalar(sph_rtp%nnod_rtp)
      real(kind=kreal), intent(in) :: vr(sph_rtp%nnod_rtp)
!
      real(kind=kreal), intent(inout) :: prod(sph_rtp%nnod_rtp)
!
!
      if(sph_rtp%istep_rtp(1) .eq. 1) then
        call cal_buoyancy_flux_rin(sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,  &
     &      sph_rtp%radius_1d_rtp_r, coef, scalar, vr, prod)
      else
        call cal_buoyancy_flux_pin(sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,  &
     &      sph_rtp%radius_1d_rtp_r, coef, scalar, vr, prod)
      end if
!
      end subroutine sel_buoyancy_flux_rtp
!
!-----------------------------------------------------------------------
!
      subroutine cal_buoyancy_flux_rin(nnod_rtp, nidx_rtp,              &
     &          radius, coef, scalar, vr, prod)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: scalar(nnod_rtp), vr(nnod_rtp)
      real(kind=kreal), intent(in) :: radius(nidx_rtp(1))
!
      real(kind=kreal), intent(inout) :: prod(nnod_rtp)
!
      integer (kind=kint) :: inod, k, ml
!
!
!$omp parallel do private(ml,k,inod)
      do ml = 1, nidx_rtp(2)*nidx_rtp(3)
        do k = 1, nidx_rtp(1)
          inod = k + (ml-1) * nidx_rtp(1)
          prod(inod) =  coef*scalar(inod)*vr(inod)*radius(k)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_buoyancy_flux_rin
!
!-----------------------------------------------------------------------
!
      subroutine cal_buoyancy_flux_pin(nnod_rtp, nidx_rtp,              &
     &          radius, coef, scalar, vr, prod)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: scalar(nnod_rtp), vr(nnod_rtp)
      real(kind=kreal), intent(in) :: radius(nidx_rtp(1))
!
      real(kind=kreal), intent(inout) :: prod(nnod_rtp)
!
      integer (kind=kint) :: inod, k, l, m
!
!
!$omp parallel private(l,k)
      do l = 1, nidx_rtp(2)
        do k = 1, nidx_rtp(1)
!$omp do private(m,inod)
          do m = 1, nidx_rtp(3)
            inod = m + (k-1) * nidx_rtp(3)                              &
     &               + (l-1) * nidx_rtp(3) * nidx_rtp(1)
            prod(inod) =  coef*scalar(inod)*vr(inod)*radius(k)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine cal_buoyancy_flux_pin
!
!-----------------------------------------------------------------------
!
      end module cal_buoyancy_flux_sph
