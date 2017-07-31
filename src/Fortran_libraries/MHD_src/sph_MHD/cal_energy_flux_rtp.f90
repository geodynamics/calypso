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
!!      subroutine cal_nonlinear_pole_MHD                               &
!!     &         (sph_rtp, fl_prop, cd_prop, ht_prop, cp_prop,          &
!!     &          f_trns, bs_trns, ncomp_snap_rj_2_rtp, ncomp_rtp_2_rj, &
!!     &          fls_pl, frc_pl)
!!      subroutine s_cal_energy_flux_rtp                                &
!!     &         (sph_rtp, fl_prop, cd_prop, ref_param_T, ref_param_C,  &
!!     &          f_trns, bs_trns, fs_trns, ncomp_rtp_2_rj,             &
!!     &          ncomp_snap_rj_2_rtp, ncomp_snap_rtp_2_rj,             &
!!     &          frc_rtp, fls_rtp, frs_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: f_trns
!!        type(phys_address), intent(in) :: bs_trns, fs_trns
!!@endverbatim
!
      module cal_energy_flux_rtp
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_phys_address
      use t_spheric_rtp_data
      use t_physical_property
      use t_reference_scalar_param
!
      implicit  none
!
      private :: cal_buoyancy_flux_rtp_smp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_nonlinear_pole_MHD                                 &
     &         (sph_rtp, fl_prop, cd_prop, ht_prop, cp_prop,            &
     &          f_trns, bs_trns, ncomp_snap_rj_2_rtp, ncomp_rtp_2_rj,   &
     &          fls_pl, frc_pl)
!
      use const_wz_coriolis_rtp
      use cal_products_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: f_trns
      type(phys_address), intent(in) :: bs_trns
      integer(kind = kint), intent(in) :: ncomp_snap_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &           :: fls_pl(sph_rtp%nnod_pole,ncomp_snap_rj_2_rtp)
      real(kind = kreal), intent(inout)                                 &
     &           :: frc_pl(sph_rtp%nnod_pole,ncomp_rtp_2_rj)
!
!
!$omp parallel
      if( (f_trns%i_m_advect * fl_prop%iflag_scheme) .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (sph_rtp%nnod_pole, fl_prop%coef_velo,                       &
     &      fls_pl(1,bs_trns%i_vort), fls_pl(1,bs_trns%i_velo),         &
     &      frc_pl(1,f_trns%i_m_advect) )
      end if
!
      if( (f_trns%i_lorentz * fl_prop%iflag_4_lorentz) .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (sph_rtp%nnod_pole, fl_prop%coef_lor,                        &
     &      fls_pl(1,bs_trns%i_current), fls_pl(1,bs_trns%i_magne),     &
     &      frc_pl(1,f_trns%i_lorentz) )
      end if
!
!
!
      if((f_trns%i_vp_induct * cd_prop%iflag_Bevo_scheme) .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (sph_rtp%nnod_pole, cd_prop%coef_induct,                     &
     &      fls_pl(1,bs_trns%i_velo), fls_pl(1,bs_trns%i_magne),        &
     &      frc_pl(1,f_trns%i_vp_induct) )
      end if
!
!
      if( (f_trns%i_h_flux * ht_prop%iflag_scheme) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_pole, ht_prop%coef_advect,                     &
     &      fls_pl(1,bs_trns%i_velo), fls_pl(1,bs_trns%i_temp),         &
     &      frc_pl(1,f_trns%i_h_flux) )
      end if
!
      if( (f_trns%i_c_flux * cp_prop%iflag_scheme) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_pole, cp_prop%coef_advect,                     &
     &      fls_pl(1,bs_trns%i_velo), fls_pl(1,bs_trns%i_light),        &
     &      frc_pl(1,f_trns%i_c_flux) )
      end if
!
!      if( (f_trns%i_Coriolis*fl_prop%iflag_4_coriolis) .gt. 0) then
!        call cal_wz_coriolis_rtp                                       &
!     &     (sph_rtp%nnod_pole, sph_rtp%nidx_rtp, fl_prop%coef_cor,     &
!     &      fls_pl(1,bs_trns%i_velo), frc_pl(1,f_trns%i_Coriolis))
!      end if
!$omp end parallel
!
      end subroutine cal_nonlinear_pole_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_cal_energy_flux_rtp                                  &
     &         (sph_rtp, fl_prop, cd_prop, ref_param_T, ref_param_C,    &
     &          f_trns, bs_trns, fs_trns, ncomp_rtp_2_rj,               &
     &          ncomp_snap_rj_2_rtp, ncomp_snap_rtp_2_rj,               &
     &          frc_rtp, fls_rtp, frs_rtp)
!
      use poynting_flux_smp
      use sph_transforms_4_MHD
      use mag_of_field_smp
      use const_wz_coriolis_rtp
      use cal_products_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(phys_address), intent(in) :: f_trns
      type(phys_address), intent(in) :: bs_trns, fs_trns
      integer(kind = kint), intent(in) :: ncomp_rtp_2_rj
      integer(kind = kint), intent(in) :: ncomp_snap_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_snap_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &           :: frc_rtp(sph_rtp%nnod_rtp,ncomp_rtp_2_rj)
      real(kind = kreal), intent(in)                                    &
     &           :: fls_rtp(sph_rtp%nnod_rtp,ncomp_snap_rj_2_rtp)
      real(kind = kreal), intent(inout)                                 &
     &           :: frs_rtp(sph_rtp%nnod_rtp,ncomp_snap_rtp_2_rj)
!
!
!$omp parallel
      if(fs_trns%i_coriolis .gt. 0) then
        call cal_wz_coriolis_rtp                                        &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, fl_prop%coef_cor,       &
     &      fls_rtp(1,bs_trns%i_velo), frs_rtp(1,fs_trns%i_Coriolis))
      end if
!
      if(fs_trns%i_ujb .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      frc_rtp(1,f_trns%i_lorentz), fls_rtp(1,bs_trns%i_velo),     &
     &      frs_rtp(1,fs_trns%i_ujb) )
      end if
!
      if(fs_trns%i_nega_ujb .gt. 0) then
        call cal_dot_prod_w_coef_smp(sph_rtp%nnod_rtp, dminus,          &
     &      frc_rtp(1,f_trns%i_lorentz), fls_rtp(1,bs_trns%i_velo),     &
     &      frs_rtp(1,fs_trns%i_nega_ujb) )
      end if
!
      if(fs_trns%i_me_gen .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      fls_rtp(1,bs_trns%i_induction), fls_rtp(1,bs_trns%i_magne), &
     &      frs_rtp(1,fs_trns%i_me_gen))
      end if
!
      if(fs_trns%i_electric .gt. 0) then
        call cal_electric_field_smp                                     &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,      &
     &      cd_prop%coef_diffuse, fls_rtp(1,bs_trns%i_current),         &
     &      frc_rtp(1,f_trns%i_vp_induct),                              &
     &      frs_rtp(1,fs_trns%i_electric))
      end if
!
      if(fs_trns%i_poynting .gt. 0) then
        call cal_poynting_flux_smp                                      &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,      &
     &      cd_prop%coef_diffuse, fls_rtp(1,bs_trns%i_current),         &
     &      frc_rtp(1,f_trns%i_vp_induct), fls_rtp(1,bs_trns%i_magne),  &
     &      frs_rtp(1,fs_trns%i_poynting))
      end if
!
      if(fs_trns%i_buo_gen .gt. 0) then
        if    (ref_param_T%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_T%iflag_reference .eq. id_takepiro_temp) then
          call cal_buoyancy_flux_rtp_smp                                &
     &       (np_smp, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1),            &
     &        sph_rtp%istack_inod_rtp_smp, sph_rtp%radius_1d_rtp_r,     &
     &        fl_prop%coef_buo, fls_rtp(1,bs_trns%i_par_temp),          &
     &        fls_rtp(1,bs_trns%i_velo), frs_rtp(1,fs_trns%i_buo_gen))
        else
          call cal_buoyancy_flux_rtp_smp                                &
     &       (np_smp, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1),            &
     &        sph_rtp%istack_inod_rtp_smp, sph_rtp%radius_1d_rtp_r,     &
     &        fl_prop%coef_buo, fls_rtp(1,bs_trns%i_temp),              &
     &        fls_rtp(1,bs_trns%i_velo), frs_rtp(1,fs_trns%i_buo_gen))
        end if
      end if
!
      if(fs_trns%i_c_buo_gen .gt. 0) then
        if    (ref_param_C%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_C%iflag_reference .eq. id_takepiro_temp) then
          call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,      &
     &        sph_rtp%nidx_rtp(1), sph_rtp%istack_inod_rtp_smp,         &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,           &
     &        fls_rtp(1,bs_trns%i_par_light),                           &
     &        fls_rtp(1,bs_trns%i_velo),                                &
     &        frs_rtp(1,fs_trns%i_c_buo_gen))
        else
          call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,      &
     &        sph_rtp%nidx_rtp(1), sph_rtp%istack_inod_rtp_smp,         &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,           &
     &        fls_rtp(1,bs_trns%i_light), fls_rtp(1,bs_trns%i_velo),    &
     &        frs_rtp(1,fs_trns%i_c_buo_gen) )
        end if
      end if
!
      if(fs_trns%i_f_buo_gen .gt. 0) then
        call cal_buoyancy_flux_rtp_smp                                  &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1),              &
     &      sph_rtp%istack_inod_rtp_smp, sph_rtp%radius_1d_rtp_r,       &
     &      fl_prop%coef_buo, fls_rtp(1,bs_trns%i_filter_temp),         &
     &      fls_rtp(1,bs_trns%i_velo), frs_rtp(1,fs_trns%i_f_buo_gen) )
      end if
!
      if(fs_trns%i_velo_scale .gt. 0) then
        call cal_len_scale_by_rot_smp                                   &
     &      (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,     &
     &      fls_rtp(1,bs_trns%i_velo), fls_rtp(1,bs_trns%i_vort),       &
     &      frs_rtp(1,fs_trns%i_velo_scale))
      end if
      if(fs_trns%i_magne_scale .gt. 0) then
        call cal_len_scale_by_rot_smp                                   &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,      &
     &      fls_rtp(1,bs_trns%i_magne), fls_rtp(1,bs_trns%i_current),   &
     &      frs_rtp(1,fs_trns%i_magne_scale))
      end if
      if(fs_trns%i_temp_scale .gt. 0) then
        call cal_len_scale_by_diffuse_smp                               &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,      &
     &      fls_rtp(1,bs_trns%i_temp), fls_rtp(1,bs_trns%i_t_diffuse),  &
     &      frs_rtp(1,fs_trns%i_temp_scale))
      end if
      if(fs_trns%i_comp_scale .gt. 0) then
        call cal_len_scale_by_diffuse_smp                               &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,      &
     &      fls_rtp(1,bs_trns%i_light), fls_rtp(1,bs_trns%i_c_diffuse), &
     &      frs_rtp(1,fs_trns%i_comp_scale))
      end if
!$omp end parallel
!
!
      if(fs_trns%i_mag_stretch .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_rtp_magnetic_streach'
!$omp parallel
        call cal_rtp_magnetic_streach                                   &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,      &
     &      sph_rtp%nidx_rtp(1), sph_rtp%nidx_rtp(2),                   &
     &      sph_rtp%a_r_1d_rtp_r, sph_rtp%cot_theta_1d_rtp,             &
     &      fls_rtp(1,bs_trns%i_magne), fls_rtp(1,bs_trns%i_velo),      &
     &      fls_rtp(1,bs_trns%i_grad_vx), fls_rtp(1,bs_trns%i_grad_vy), &
     &      fls_rtp(1,bs_trns%i_grad_vz),                               &
     &      frs_rtp(1,fs_trns%i_mag_stretch) )
!$omp end parallel
      end if
!
      end subroutine s_cal_energy_flux_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_buoyancy_flux_rtp_smp(np_smp, nnod, nr,            &
     &          inod_smp_stack, radius, coef, scalar, vr, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod, nr
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef, scalar(nnod), vr(nnod)
      real (kind=kreal), intent(in) :: radius(nr)
!
      real (kind=kreal), intent(inout) :: prod(nnod)
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
      end module cal_energy_flux_rtp
