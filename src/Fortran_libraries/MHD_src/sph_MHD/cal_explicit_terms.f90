!> @file  cal_explicit_terms.f90
!!      module cal_explicit_terms
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate time evolution explicitly
!!
!!@verbatim
!!      subroutine cal_diff_induction_MHD_adams
!!      subroutine cal_diff_induction_wSGS_adams
!!      subroutine cal_diff_induction_MHD_euler
!!
!!      subroutine sel_heat_diff_adv_src_adams(kr_in, kr_out)
!!      subroutine sel_heat_diff_adv_src_euler(kr_in, kr_out)
!!
!!      subroutine sel_light_diff_adv_src_adams(kr_in, kr_out)
!!      subroutine sel_light_diff_adv_src_euler(kr_in, kr_out)
!!
!!      subroutine set_ini_adams_mag_induct
!!      subroutine sel_ini_adams_heat_w_src(kr_in, kr_out)
!!      subroutine sel_ini_adams_light_w_src(kr_in, kr_out)
!!@endverbatim
!!
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out      Radial ID for outer boundary
!
      module cal_explicit_terms
!
      use m_precision
!
      use m_t_int_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_MHD_adams
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_magne) = d_rj(inod,ipol%i_magne)               &
     &              + dt * (coef_exp_b * d_rj(inod,ipol%i_b_diffuse)    &
     &                        + adam_0 * d_rj(inod,ipol%i_induction)    &
     &                        + adam_1 * d_rj(inod,ipol%i_pre_uxb) )
        d_rj(inod,itor%i_magne) = d_rj(inod,itor%i_magne)               &
     &              + dt * (coef_exp_b * d_rj(inod,itor%i_b_diffuse)    &
     &                        + adam_0 * d_rj(inod,itor%i_induction)    &
     &                        + adam_1 * d_rj(inod,itor%i_pre_uxb) )
!
         d_rj(inod,ipol%i_pre_uxb) = d_rj(inod,ipol%i_induction)
         d_rj(inod,itor%i_pre_uxb) = d_rj(inod,itor%i_induction)
       end do
!$omp end do
!
      end subroutine cal_diff_induction_MHD_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_wSGS_adams
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_magne) = d_rj(inod,ipol%i_magne)               &
     &           + dt * (coef_exp_b *  d_rj(inod,ipol%i_b_diffuse)      &
     &                     + adam_0 *  d_rj(inod,ipol%i_induction)      &
     &                     + adam_0 *  d_rj(inod,ipol%i_SGS_induction)  &
     &                     + adam_1 *  d_rj(inod,ipol%i_pre_uxb) )
        d_rj(inod,itor%i_magne) = d_rj(inod,itor%i_magne)               &
     &           + dt * (coef_exp_b *  d_rj(inod,itor%i_b_diffuse)      &
     &                     + adam_0 *  d_rj(inod,itor%i_induction)      &
     &                     + adam_0 *  d_rj(inod,itor%i_SGS_induction)  &
     &                     + adam_1 *  d_rj(inod,itor%i_pre_uxb) )
!
         d_rj(inod,ipol%i_pre_uxb) = d_rj(inod,ipol%i_induction)        &
     &                              + d_rj(inod,ipol%i_SGS_induction)
         d_rj(inod,itor%i_pre_uxb) = d_rj(inod,itor%i_induction)        &
     &                              + d_rj(inod,itor%i_SGS_induction)
       end do
!$omp end do
!
      end subroutine cal_diff_induction_wSGS_adams
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_MHD_euler
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_magne) = d_rj(inod,ipol%i_magne)               &
     &              + dt * (coef_exp_b * d_rj(inod,ipol%i_b_diffuse)    &
     &                                 + d_rj(inod,ipol%i_induction) )
        d_rj(inod,itor%i_magne) = d_rj(inod,itor%i_magne)               &
     &              + dt * (coef_exp_b * d_rj(inod,itor%i_b_diffuse)    &
     &                                 + d_rj(inod,itor%i_induction) )
       end do
!$omp end do
!
      end subroutine cal_diff_induction_MHD_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_wSGS_euler
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_magne) = d_rj(inod,ipol%i_magne)               &
     &          + dt * (coef_exp_b * d_rj(inod,ipol%i_b_diffuse)        &
     &                             + d_rj(inod,ipol%i_induction)        &
     &                             + d_rj(inod,ipol%i_SGS_induction) )
        d_rj(inod,itor%i_magne) = d_rj(inod,itor%i_magne)               &
     &          + dt * (coef_exp_b * d_rj(inod,itor%i_b_diffuse)        &
     &                             + d_rj(inod,itor%i_induction)        &
     &                             + d_rj(inod,itor%i_SGS_induction) )
       end do
!$omp end do
!
      end subroutine cal_diff_induction_wSGS_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_heat_diff_adv_src_adams(kr_in, kr_out)
!
      use m_physical_property
      use cal_diff_adv_src_explicit
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
!
      call sel_scalar_diff_adv_src_adams(kr_in, kr_out,                 &
     &    ipol%i_t_diffuse, ipol%i_h_advect, ipol%i_heat_source,        &
     &    ipol%i_temp, ipol%i_pre_heat, coef_exp_t, coef_h_src)
!
      end subroutine sel_heat_diff_adv_src_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_heat_diff_adv_src_euler(kr_in, kr_out)
!
      use m_physical_property
      use cal_diff_adv_src_explicit
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
!
      call sel_scalar_diff_adv_src_euler(kr_in, kr_out,                 &
     &    ipol%i_t_diffuse, ipol%i_h_advect, ipol%i_heat_source,        &
     &    ipol%i_temp, coef_exp_t, coef_temp, coef_h_src)
!
      end subroutine sel_heat_diff_adv_src_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_light_diff_adv_src_adams(kr_in, kr_out)
!
      use m_physical_property
      use cal_diff_adv_src_explicit
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
!
      call sel_scalar_diff_adv_src_adams(kr_in, kr_out,                 &
     &    ipol%i_c_diffuse, ipol%i_c_advect, ipol%i_light_source,       &
     &    ipol%i_light, ipol%i_pre_composit, coef_exp_c, coef_c_src)
!
      end subroutine sel_light_diff_adv_src_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_light_diff_adv_src_euler(kr_in, kr_out)
!
      use m_physical_property
      use cal_diff_adv_src_explicit
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
!
      call sel_scalar_diff_adv_src_euler(kr_in, kr_out,                 &
     &    ipol%i_c_diffuse, ipol%i_c_advect, ipol%i_light_source,       &
     &    ipol%i_light, coef_exp_c, coef_light, coef_c_src)
!
      end subroutine sel_light_diff_adv_src_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ini_adams_mag_induct
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
         d_rj(inod,ipol%i_pre_uxb) = d_rj(inod,ipol%i_induction)
         d_rj(inod,itor%i_pre_uxb) = d_rj(inod,itor%i_induction)
       end do
!$omp end do
!
      end subroutine set_ini_adams_mag_induct
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_heat_w_src(kr_in, kr_out)
!
      use m_physical_property
      use cal_diff_adv_src_explicit
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
!
      call sel_ini_adams_scalar_w_src(kr_in, kr_out,                    &
     &    ipol%i_h_advect, ipol%i_heat_source, ipol%i_pre_heat,         &
     &    coef_h_src)
!
      end subroutine sel_ini_adams_heat_w_src
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_light_w_src(kr_in, kr_out)
!
      use m_physical_property
      use cal_diff_adv_src_explicit
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
!
      call sel_ini_adams_scalar_w_src(kr_in, kr_out,                    &
     &    ipol%i_c_advect, ipol%i_light_source, ipol%i_pre_composit,    &
     &    coef_c_src)
!
      end subroutine sel_ini_adams_light_w_src
!
! ----------------------------------------------------------------------
!
      end module cal_explicit_terms
