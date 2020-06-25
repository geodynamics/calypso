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
      private :: cal_buoyancy_flux_rtp_smp, pole_sph_buoyancy_flux
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
!$omp parallel
      if(fs_trns_eflux%i_buo_gen .gt. 0) then
        if    (ref_param_T%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_T%iflag_reference .eq. id_takepiro_temp) then
          call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,      &
     &        sph_rtp%nidx_rtp(1), sph_rtp%istack_inod_rtp_smp,         &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_buo,                &
     &        trns_b_scl%fld_rtp(1,bs_trns_scalar%i_per_temp),          &
     &        trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),               &
     &        trns_f_eflux%fld_rtp(1,fs_trns_eflux%i_buo_gen))
        else
          call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,      &
     &        sph_rtp%nidx_rtp(1), sph_rtp%istack_inod_rtp_smp,         &
     &        sph_rtp%radius_1d_rtp_r,  fl_prop%coef_buo,               &
     &        trns_b_scl%fld_rtp(1,bs_trns_scalar%i_temp),              &
     &        trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),               &
     &        trns_f_eflux%fld_rtp(1,fs_trns_eflux%i_buo_gen))
        end if
      end if
!
      if(fs_trns_eflux%i_c_buo_gen .gt. 0) then
        if    (ref_param_C%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_C%iflag_reference .eq. id_takepiro_temp) then
          call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,      &
     &        sph_rtp%nidx_rtp(1), sph_rtp%istack_inod_rtp_smp,         &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,           &
     &        trns_b_scl%fld_rtp(1,bs_trns_scalar%i_per_light),         &
     &        trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),               &
     &        trns_f_eflux%fld_rtp(1,fs_trns_eflux%i_c_buo_gen))
        else
          call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,      &
     &        sph_rtp%nidx_rtp(1), sph_rtp%istack_inod_rtp_smp,         &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,           &
     &        trns_b_scl%fld_rtp(1,bs_trns_scalar%i_light),             &
     &        trns_b_snap%fld_rtp(1,bs_trns_base%i_velo),               &
     &        trns_f_eflux%fld_rtp(1,fs_trns_eflux%i_c_buo_gen) )
        end if
      end if
!$omp end parallel
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
      subroutine cal_buoyancy_flux_rtp_smp(np_smp, nnod, nr,            &
     &          inod_smp_stack, radius, coef, scalar, vr, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod, nr
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: coef, scalar(nnod), vr(nnod)
      real(kind=kreal), intent(in) :: radius(nr)
!
      real(kind=kreal), intent(inout) :: prod(nnod)
!
      integer (kind=kint) :: iproc, inod, ist, ied, k
!
!
!$omp do private(inod,ist,ied,k)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          k = mod( (inod-1),nr) + 1
          prod(inod) =  coef*scalar(inod)*vr(inod)*radius(k)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_buoyancy_flux_rtp_smp
!
! -----------------------------------------------------------------------
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
!
      end module cal_buoyancy_flux_sph
