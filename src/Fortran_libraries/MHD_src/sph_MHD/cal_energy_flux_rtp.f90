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
      subroutine s_cal_energy_flux_rtp
!
      use m_machine_parameter
      use m_control_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_physical_property
      use m_sph_spectr_data
      use m_sph_phys_address
      use sph_poynting_flux_smp
      use sph_transforms_4_MHD
      use products_sph_fields_smp
      use mag_of_field_smp
      use const_wz_coriolis_rtp
!
!
!$omp parallel
!      if((irtp%i_velo*irtp%i_coriolis) .gt. 0) then
!        call cal_wz_coriolis_rtp
!      end if
!
      if((irtp%i_lorentz*irtp%i_ujb) .gt. 0) then
        call cal_rtp_dot_product(irtp%i_lorentz, irtp%i_velo,           &
     &      irtp%i_ujb)
      end if
!
      if((irtp%i_lorentz*irtp%i_nega_ujb) .gt. 0) then
        call cal_rtp_dot_prod_w_coef(dminus,                            &
     &      irtp%i_lorentz, irtp%i_velo, irtp%i_nega_ujb)
      end if
!
      if((irtp%i_induction*irtp%i_me_gen) .gt. 0) then
        call cal_rtp_dot_product(irtp%i_induction, irtp%i_magne,        &
     &      irtp%i_me_gen)
      end if
!
      if((irtp%i_current*irtp%i_vp_induct*irtp%i_electric) .gt. 0) then
        call cal_rtp_electric_field_smp
      end if
!
      if((irtp%i_current*irtp%i_vp_induct*irtp%i_poynting) .gt. 0) then
        call cal_rtp_poynting_flux_smp
      end if
!
      if(irtp%i_buo_gen .gt. 0) then
        if(iflag_4_ref_temp .eq. id_sphere_ref_temp) then
          call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1), &
     &        inod_rtp_smp_stack, radius_1d_rtp_r, coef_buo,            &
     &        d_rtp(1,irtp%i_par_temp), d_rtp(1,irtp%i_velo),           &
     &        d_rtp(1,irtp%i_buo_gen) )
        else
          call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1), &
     &        inod_rtp_smp_stack, radius_1d_rtp_r, coef_buo,            &
     &        d_rtp(1,irtp%i_temp), d_rtp(1,irtp%i_velo),               &
     &        d_rtp(1,irtp%i_buo_gen) )
        end if
      end if
!
      if(irtp%i_c_buo_gen .gt. 0) then
        call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1),   &
     &      inod_rtp_smp_stack, radius_1d_rtp_r, coef_comp_buo,         &
     &      d_rtp(1,irtp%i_light), d_rtp(1,irtp%i_velo),                &
     &      d_rtp(1,irtp%i_c_buo_gen) )
      end if
!
      if(irtp%i_f_buo_gen .gt. 0) then
        call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1),   &
     &      inod_rtp_smp_stack, radius_1d_rtp_r, coef_buo,              &
     &      d_rtp(1,irtp%i_filter_temp), d_rtp(1,irtp%i_velo),          &
     &      d_rtp(1,irtp%i_f_buo_gen) )
      end if
!
      if((irtp%i_vort*irtp%i_velo_scale) .gt. 0) then
        call cal_len_scale_by_rot_smp(np_smp, nnod_rtp,                 &
     &      inod_rtp_smp_stack, d_rtp(1,irtp%i_velo),                   &
     &      d_rtp(1,irtp%i_vort), d_rtp(1,irtp%i_velo_scale))
      end if
      if((irtp%i_current*irtp%i_magne_scale) .gt. 0) then
        call cal_len_scale_by_rot_smp(np_smp, nnod_rtp,                 &
     &      inod_rtp_smp_stack, d_rtp(1,irtp%i_magne),                  &
     &      d_rtp(1,irtp%i_current), d_rtp(1,irtp%i_magne_scale))
      end if
      if((irtp%i_t_diffuse*irtp%i_temp_scale) .gt. 0) then
        call cal_len_scale_by_diffuse_smp(np_smp, nnod_rtp,             &
     &      inod_rtp_smp_stack, d_rtp(1,irtp%i_temp),                   &
     &      d_rtp(1,irtp%i_t_diffuse), d_rtp(1,irtp%i_temp_scale))
      end if
      if((irtp%i_c_diffuse*irtp%i_comp_scale) .gt. 0) then
        call cal_len_scale_by_diffuse_smp(np_smp, nnod_rtp,             &
     &      inod_rtp_smp_stack, d_rtp(1,irtp%i_light),                  &
     &      d_rtp(1,irtp%i_c_diffuse), d_rtp(1,irtp%i_comp_scale))
      end if
!$omp end parallel
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
