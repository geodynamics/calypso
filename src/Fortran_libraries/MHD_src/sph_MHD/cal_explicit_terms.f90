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
!!      subroutine cal_heat_diff_advect_adams
!!      subroutine cal_heat_diff_advect_euler
!!
!!      subroutine cal_scalar_diff_advect_adams
!!      subroutine cal_scalar_diff_advect_euler
!!
!!      subroutine set_adams_mag_induct_ini
!!      subroutine set_adams_heat_ini
!!      subroutine set_adams_dscalar_ini
!!@endverbatim
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
      subroutine cal_heat_diff_advect_adams
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (nlayer_ICB-1)*nidx_rj(2) + 1
      ied = nlayer_CMB * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol%i_temp) = d_rj(inod,ipol%i_temp)                 &
     &                + dt * (coef_exp_t * d_rj(inod,ipol%i_t_diffuse)  &
     &                          - adam_0 * d_rj(inod,ipol%i_h_advect)   &
     &                          + adam_1 * d_rj(inod,ipol%i_pre_heat))
!
         d_rj(inod,ipol%i_pre_heat) =  -d_rj(inod,ipol%i_h_advect)
       end do
!$omp end do
!
      end subroutine cal_heat_diff_advect_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_heat_diff_advect_euler
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (nlayer_ICB-1)*nidx_rj(2) + 1
      ied = nlayer_CMB * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol%i_temp) = d_rj(inod,ipol%i_temp)                 &
     &       + dt * (coef_exp_t * d_rj(inod,ipol%i_t_diffuse)           &
     &                          - d_rj(inod,ipol%i_h_advect))
       end do
!$omp end do
!
      end subroutine cal_heat_diff_advect_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_scalar_diff_advect_adams
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (nlayer_ICB-1)*nidx_rj(2) + 1
      ied = nlayer_CMB * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol%i_light) = d_rj(inod,ipol%i_light)               &
     &          + dt * (coef_exp_c * d_rj(inod,ipol%i_c_diffuse)        &
     &                    - adam_0 * d_rj(inod,ipol%i_c_advect)         &
     &                    + adam_1 * d_rj(inod,ipol%i_pre_composit) )
!
         d_rj(inod,ipol%i_pre_composit) =  -d_rj(inod,ipol%i_c_advect)
       end do
!$omp end do
!
      end subroutine cal_scalar_diff_advect_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_scalar_diff_advect_euler
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (nlayer_ICB-1)*nidx_rj(2) + 1
      ied = nlayer_CMB * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol%i_light) = d_rj(inod,ipol%i_light)               &
     &         + dt * (coef_exp_c*d_rj(inod,ipol%i_c_diffuse)           &
     &                 - d_rj(inod,ipol%i_c_advect) )
       end do
!$omp end do
!
      end subroutine cal_scalar_diff_advect_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_adams_mag_induct_ini
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
      end subroutine set_adams_mag_induct_ini
!
! ----------------------------------------------------------------------
!
      subroutine set_adams_heat_ini
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (nlayer_ICB-1)*nidx_rj(2) + 1
      ied = nlayer_CMB * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
         d_rj(inod,ipol%i_pre_heat) = -d_rj(inod,ipol%i_h_advect)
      end do
!$omp end do
!
      end subroutine set_adams_heat_ini
!
! ----------------------------------------------------------------------
!
      subroutine set_adams_dscalar_ini
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (nlayer_ICB-1)*nidx_rj(2) + 1
      ied = nlayer_CMB * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
         d_rj(inod,ipol%i_pre_composit) = -d_rj(inod,ipol%i_c_advect)
      end do
!$omp end do
!
      end subroutine set_adams_dscalar_ini
!
! ----------------------------------------------------------------------
!
      end module cal_explicit_terms
