!> @file  cal_explicit_terms.f90
!!      module cal_explicit_terms
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate time evolution explicitly
!!
!!@verbatim
!!      subroutine cal_diff_induction_MHD_adams                         &
!!     &         (dt, coef_exp, ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine cal_diff_induction_wSGS_adams                        &
!!     &         (dt, coef_exp, ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine cal_diff_induction_MHD_euler                         &
!!     &         (dt, coef_exp, ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine cal_diff_induction_wSGS_euler                        &
!!     &         (dt, coef_exp, ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!
!!      subroutine set_ini_adams_mag_induct                             &
!!     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine SGS_ini_adams_mag_induct                             &
!!     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!        type(phys_address), intent(in) :: ipol, itor
!!@endverbatim
!
      module cal_explicit_terms
!
      use m_precision
      use m_t_step_parameter
!
      use t_phys_address
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_MHD_adams                           &
     &         (dt, coef_exp, ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: coef_exp
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_magne) = d_rj(inod,ipol%i_magne)               &
     &         + dt * (coef_exp * d_rj(inod,ipol%i_b_diffuse)           &
     &                 + adam_0 * d_rj(inod,ipol%i_induction)           &
     &                 + adam_1 * d_rj(inod,ipol%i_pre_uxb) )
        d_rj(inod,itor%i_magne) = d_rj(inod,itor%i_magne)               &
     &         + dt * (coef_exp * d_rj(inod,itor%i_b_diffuse)           &
     &                 + adam_0 * d_rj(inod,itor%i_induction)           &
     &                 + adam_1 * d_rj(inod,itor%i_pre_uxb) )
!
        d_rj(inod,ipol%i_pre_uxb) = d_rj(inod,ipol%i_induction)
        d_rj(inod,itor%i_pre_uxb) = d_rj(inod,itor%i_induction)
      end do
!$omp end parallel do
!
      end subroutine cal_diff_induction_MHD_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_wSGS_adams                          &
     &         (dt, coef_exp, ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: coef_exp
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_magne) = d_rj(inod,ipol%i_magne)               &
     &         + dt * (coef_exp *  d_rj(inod,ipol%i_b_diffuse)          &
     &                 + adam_0 *  d_rj(inod,ipol%i_induction)          &
     &                 + adam_0 *  d_rj(inod,ipol%i_SGS_induction)      &
     &                     + adam_1 *  d_rj(inod,ipol%i_pre_uxb) )
        d_rj(inod,itor%i_magne) = d_rj(inod,itor%i_magne)               &
     &         + dt * (coef_exp *  d_rj(inod,itor%i_b_diffuse)          &
     &                 + adam_0 *  d_rj(inod,itor%i_induction)          &
     &                 + adam_0 *  d_rj(inod,itor%i_SGS_induction)      &
     &                 + adam_1 *  d_rj(inod,itor%i_pre_uxb) )
!
        d_rj(inod,ipol%i_pre_uxb) = d_rj(inod,ipol%i_induction) &
     &                           + d_rj(inod,ipol%i_SGS_induction)
        d_rj(inod,itor%i_pre_uxb) = d_rj(inod,itor%i_induction) &
     &                           + d_rj(inod,itor%i_SGS_induction)
      end do
!$omp end parallel do
!
      end subroutine cal_diff_induction_wSGS_adams
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_MHD_euler                           &
     &         (dt, coef_exp, ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: coef_exp
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_magne) = d_rj(inod,ipol%i_magne)               &
     &         + dt * (coef_exp * d_rj(inod,ipol%i_b_diffuse)           &
     &                          + d_rj(inod,ipol%i_induction) )
        d_rj(inod,itor%i_magne) = d_rj(inod,itor%i_magne)               &
     &         + dt * (coef_exp * d_rj(inod,itor%i_b_diffuse)           &
                                + d_rj(inod,itor%i_induction) )
      end do
!$omp end parallel do
!
      end subroutine cal_diff_induction_MHD_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_wSGS_euler                          &
     &         (dt, coef_exp, ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: coef_exp
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_magne) = d_rj(inod,ipol%i_magne)               &
     &         + dt * (coef_exp * d_rj(inod,ipol%i_b_diffuse)           &
     &                          + d_rj(inod,ipol%i_induction)           &
     &                          + d_rj(inod,ipol%i_SGS_induction) )
        d_rj(inod,itor%i_magne) = d_rj(inod,itor%i_magne)               &
     &         + dt * (coef_exp * d_rj(inod,itor%i_b_diffuse)           &
     &                          + d_rj(inod,itor%i_induction)           &
     &                          + d_rj(inod,itor%i_SGS_induction) )
      end do
!$omp end parallel do
!
      end subroutine cal_diff_induction_wSGS_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ini_adams_mag_induct                               &
     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_pre_uxb) = d_rj(inod,ipol%i_induction)
        d_rj(inod,itor%i_pre_uxb) = d_rj(inod,itor%i_induction)
      end do
!$omp end parallel do
!
      end subroutine set_ini_adams_mag_induct
!
! ----------------------------------------------------------------------
!
      subroutine SGS_ini_adams_mag_induct                               &
     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_pre_uxb) = d_rj(inod,ipol%i_induction)         &
     &                            + d_rj(inod,ipol%i_SGS_induction)
        d_rj(inod,itor%i_pre_uxb) = d_rj(inod,itor%i_induction)         &
     &                            + d_rj(inod,itor%i_SGS_induction)
      end do
!$omp end parallel do
!
      end subroutine SGS_ini_adams_mag_induct
!
! ----------------------------------------------------------------------
!
      end module cal_explicit_terms
