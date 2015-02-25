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
!!      subroutine cal_nonlinear_pole_MHD
!!      subroutine s_cal_energy_flux_rtp
!!@endverbatim
!
      module cal_energy_flux_rtp
!
      use m_precision
      use m_constants
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
      subroutine cal_nonlinear_pole_MHD
!
      use m_machine_parameter
      use m_control_parameter
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use m_physical_property
      use m_work_pole_sph_trans
      use const_wz_coriolis_rtp
      use cal_products_smp
!
!
!$omp parallel
      if( (f_trns%i_m_advect*iflag_t_evo_4_velo) .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (np_smp, nnod_pole, istack_npole_smp, coef_velo,             &
     &      fls_pl(1,bs_trns%i_vort), fls_pl(1,bs_trns%i_velo),         &
     &      frm_pl(1,f_trns%i_m_advect) )
      end if
!
      if( (f_trns%i_lorentz*iflag_4_lorentz) .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (np_smp, nnod_pole, istack_npole_smp, coef_lor,              &
     &      fls_pl(1,bs_trns%i_current), fls_pl(1,bs_trns%i_magne),     &
     &      frm_pl(1,f_trns%i_lorentz) )
      end if
!
!
!
      if( (f_trns%i_vp_induct*iflag_t_evo_4_magne) .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (np_smp, nnod_pole, istack_npole_smp, coef_induct,           &
     &      fls_pl(1,bs_trns%i_velo), fls_pl(1,bs_trns%i_magne),        &
     &      frm_pl(1,f_trns%i_vp_induct) )
      end if
!
!
      if( (f_trns%i_h_flux*iflag_t_evo_4_temp) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (np_smp, nnod_pole, istack_npole_smp, coef_temp,             &
     &      fls_pl(1,bs_trns%i_velo), fls_pl(1,bs_trns%i_temp),         &
     &      frm_pl(1,f_trns%i_h_flux) )
      end if
!
      if( (f_trns%i_c_flux*iflag_t_evo_4_composit) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (np_smp, nnod_pole, istack_npole_smp, coef_light,            &
     &      fls_pl(1,bs_trns%i_velo), fls_pl(1,bs_trns%i_light),        &
     &      frm_pl(1,f_trns%i_c_flux) )
      end if
!
!      if( (f_trns%i_Coriolis*iflag_4_coriolis) .gt. 0) then
!        call cal_wz_coriolis_rtp                                       &
!     &     (nnod_pole, fls_pl(1,bs_trns%i_velo),                       &
!            frm_pl(1,f_trns%i_Coriolis))
!      end if
!$omp end parallel
!
      end subroutine cal_nonlinear_pole_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_cal_energy_flux_rtp
!
      use m_machine_parameter
      use m_control_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_physical_property
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use m_work_4_sph_trans
      use poynting_flux_smp
      use sph_poynting_flux_smp
      use sph_transforms_4_MHD
      use mag_of_field_smp
      use const_wz_coriolis_rtp
      use cal_products_smp
!
!
!$omp parallel
!      if(fs_trns%i_coriolis .gt. 0) then
!        call cal_wz_coriolis_rtp(nnod_rtp, fls_rtp(1,bs_trns%i_velo),  &
!     &      frs_rtp(1,fs_trns%i_Coriolis))
!      end if
!
      if(fs_trns%i_ujb .gt. 0) then
        call cal_dot_prod_no_coef_smp(np_smp, nnod_rtp,                 &
     &      inod_rtp_smp_stack, frm_rtp(1,f_trns%i_lorentz),            &
     &      fls_rtp(1,bs_trns%i_velo), frs_rtp(1,fs_trns%i_ujb) )
      end if
!
      if(fs_trns%i_nega_ujb .gt. 0) then
        call cal_dot_prod_w_coef_smp(np_smp, nnod_rtp,                  &
     &      inod_rtp_smp_stack, dminus,                                 &
     &      frm_rtp(1,f_trns%i_lorentz), fls_rtp(1,bs_trns%i_velo),     &
     &      frs_rtp(1,fs_trns%i_nega_ujb) )
      end if
!
      if(fs_trns%i_me_gen .gt. 0) then
        call cal_dot_prod_no_coef_smp(np_smp, nnod_rtp,                 &
     &      inod_rtp_smp_stack, fls_rtp(1,bs_trns%i_induction),         &
     &      fls_rtp(1,bs_trns%i_magne), frs_rtp(1,fs_trns%i_me_gen))
      end if
!
      if(fs_trns%i_electric .gt. 0) then
        call cal_electric_field_smp                                     &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack, coef_d_magne,         &
     &      fls_rtp(1,bs_trns%i_current),                               &
     &      frm_rtp(1,f_trns%i_vp_induct),                              &
     &      frs_rtp(1,fs_trns%i_electric))
      end if
!
      if(fs_trns%i_poynting .gt. 0) then
        call cal_poynting_flux_smp                                      &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack, coef_d_magne,         &
     &      fls_rtp(1,bs_trns%i_current),                               &
     &      frm_rtp(1,f_trns%i_vp_induct), fls_rtp(1,bs_trns%i_magne),  &
     &      frs_rtp(1,fs_trns%i_poynting))
      end if
!
      if(fs_trns%i_buo_gen .gt. 0) then
        if(iflag_4_ref_temp .eq. id_sphere_ref_temp) then
          call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1), &
     &        inod_rtp_smp_stack, radius_1d_rtp_r, coef_buo,            &
     &        fls_rtp(1,bs_trns%i_par_temp), fls_rtp(1,bs_trns%i_velo), &
     &        frs_rtp(1,fs_trns%i_buo_gen) )
        else
          call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1), &
     &        inod_rtp_smp_stack, radius_1d_rtp_r, coef_buo,            &
     &        fls_rtp(1,bs_trns%i_temp), fls_rtp(1,bs_trns%i_velo),     &
     &        frs_rtp(1,fs_trns%i_buo_gen) )
        end if
      end if
!
      if(fs_trns%i_c_buo_gen .gt. 0) then
        call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1),   &
     &      inod_rtp_smp_stack, radius_1d_rtp_r, coef_comp_buo,         &
     &      fls_rtp(1,bs_trns%i_light), fls_rtp(1,bs_trns%i_velo),      &
     &      frs_rtp(1,fs_trns%i_c_buo_gen) )
      end if
!
      if(fs_trns%i_f_buo_gen .gt. 0) then
        call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1),   &
     &      inod_rtp_smp_stack, radius_1d_rtp_r, coef_buo,              &
     &      fls_rtp(1,bs_trns%i_filter_temp),                           &
     &      fls_rtp(1,bs_trns%i_velo), frs_rtp(1,fs_trns%i_f_buo_gen) )
      end if
!
      if(fs_trns%i_velo_scale .gt. 0) then
        call cal_len_scale_by_rot_smp                                   &
     &      (np_smp, nnod_rtp, inod_rtp_smp_stack,                      &
     &      fls_rtp(1,bs_trns%i_velo), fls_rtp(1,bs_trns%i_vort),       &
     &      frs_rtp(1,fs_trns%i_velo_scale))
      end if
      if(fs_trns%i_magne_scale .gt. 0) then
        call cal_len_scale_by_rot_smp                                   &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack,                       &
     &      fls_rtp(1,bs_trns%i_magne), fls_rtp(1,bs_trns%i_current),   &
     &      frs_rtp(1,fs_trns%i_magne_scale))
      end if
      if(fs_trns%i_temp_scale .gt. 0) then
        call cal_len_scale_by_diffuse_smp                               &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack,                       &
     &      fls_rtp(1,bs_trns%i_temp), fls_rtp(1,bs_trns%i_t_diffuse),  &
     &      frs_rtp(1,fs_trns%i_temp_scale))
      end if
      if(fs_trns%i_comp_scale .gt. 0) then
        call cal_len_scale_by_diffuse_smp                               &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack,                       &
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
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack,                       &
     &      nidx_rtp(1), nidx_rtp(2), a_r_1d_rtp_r, cot_theta_1d_rtp,   &
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
