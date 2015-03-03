!>@file   cal_nonlinear_sph_MHD.f90
!!@brief  module cal_nonlinear_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine s_cal_nonlinear_sph_MHD
!!      subroutine add_reftemp_advect_sph_MHD(kr_in, kr_out)
!!@endverbatim
!!
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out      Radial ID for outer boundary
!
      module cal_nonlinear_sph_MHD
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_spheric_parameter
      use m_physical_property
      use m_sph_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_nonlinear_sph_MHD
!
      use m_machine_parameter
      use m_spheric_param_smp
      use m_addresses_trans_sph_MHD
      use const_wz_coriolis_rtp
      use cal_products_smp
!
!
!$omp parallel
      if( (f_trns%i_m_advect*iflag_t_evo_4_velo) .gt. 0) then
        call cal_cross_prod_w_coef_smp(np_smp, nnod_rtp,                &
     &      inod_rtp_smp_stack, coef_velo, fld_rtp(1,b_trns%i_vort),    &
     &      fld_rtp(1,b_trns%i_velo), frc_rtp(1,f_trns%i_m_advect) )
      end if
!
      if( (f_trns%i_lorentz*iflag_4_lorentz) .gt. 0) then
        call cal_cross_prod_w_coef_smp(np_smp, nnod_rtp,                &
     &    inod_rtp_smp_stack, coef_lor, fld_rtp(1,b_trns%i_current),    &
     &    fld_rtp(1,b_trns%i_magne), frc_rtp(1,f_trns%i_lorentz) )
      end if
!
!
!
      if( (f_trns%i_vp_induct*iflag_t_evo_4_magne) .gt. 0) then
        call cal_cross_prod_w_coef_smp(np_smp, nnod_rtp,                &
     &    inod_rtp_smp_stack, coef_induct, fld_rtp(1,b_trns%i_velo),    &
     &    fld_rtp(1,b_trns%i_magne), frc_rtp(1,f_trns%i_vp_induct) )
      end if
!
!
      if( (f_trns%i_h_flux*iflag_t_evo_4_temp) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp(np_smp, nnod_rtp,           &
     &    inod_rtp_smp_stack, coef_temp, fld_rtp(1,b_trns%i_velo),      &
     &    fld_rtp(1,b_trns%i_temp), frc_rtp(1,f_trns%i_h_flux) )
      end if
!
      if( (f_trns%i_c_flux*iflag_t_evo_4_composit) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp(np_smp, nnod_rtp,           &
     &    inod_rtp_smp_stack, coef_light, fld_rtp(1,b_trns%i_velo),     &
     &    fld_rtp(1,b_trns%i_light), frc_rtp(1,f_trns%i_c_flux) )
      end if
!
!      if( (f_trns%i_Coriolis*iflag_4_coriolis) .gt. 0) then
!        call cal_wz_coriolis_rtp(nnod_rtp, fld_rtp(1,b_trns%i_velo),   &
!            frc_rtp(1,f_trns%i_Coriolis))
!      end if
!$omp end parallel
!
      end subroutine s_cal_nonlinear_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_reftemp_advect_sph_MHD(kr_in, kr_out)
!
      use m_schmidt_poly_on_rtm
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d_rj(inod,ipol%i_h_advect) = d_rj(inod,ipol%i_h_advect)         &
     &                      + coef_temp * g_sph_rj(j,3) * ar_1d_rj(k,2) &
     &                       * reftemp_rj(k,1) * d_rj(inod,ipol%i_velo)
      end do
!$omp end parallel do
!
      end subroutine add_reftemp_advect_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module cal_nonlinear_sph_MHD
