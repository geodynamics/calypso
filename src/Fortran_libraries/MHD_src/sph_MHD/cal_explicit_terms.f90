!> @file  cal_explicit_terms.f90
!!      module cal_explicit_terms
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate time evolution of induction by explicit scheme
!!
!!@verbatim
!!      subroutine cal_diff_induction_MHD_adams                         &
!!     &         (cd_prop, ipol_base, ipol_exp, ipol_frc, ipol_dif, dt, &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine cal_diff_induction_MHD_euler                         &
!!     &         (cd_prop, ipol_base, ipol_frc, ipol_dif, dt,           &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine set_ini_adams_mag_induct                             &
!!     &         (ipol_exp, ipol_frc, nnod_rj, ntot_phys_rj, d_rj)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(diffusion_address), intent(in) :: ipol_dif
!!@endverbatim
!
      module cal_explicit_terms
!
      use m_precision
      use m_t_step_parameter
!
      use t_physical_property
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_explicit_term_labels
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
     &         (cd_prop, ipol_base, ipol_exp, ipol_frc, ipol_dif, dt,   &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      type(conductive_property), intent(in) :: cd_prop
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_base%i_magne  ) = d_rj(inod,ipol_base%i_magne  ) &
     &       + dt*(cd_prop%coef_exp * d_rj(inod,ipol_dif%i_b_diffuse  ) &
     &                    + adam_0 * d_rj(inod,ipol_frc%i_induction  )  &
     &                 + adam_1 * d_rj(inod,ipol_exp%i_pre_uxb  ))
        d_rj(inod,ipol_base%i_magne+2) = d_rj(inod,ipol_base%i_magne+2) &
     &       + dt*(cd_prop%coef_exp * d_rj(inod,ipol_dif%i_b_diffuse+2) &
     &                     + adam_0 * d_rj(inod,ipol_frc%i_induction+2) &
     &                     + adam_1 * d_rj(inod,ipol_exp%i_pre_uxb+2))
!
        d_rj(inod,ipol_exp%i_pre_uxb  )                                 &
     &       = d_rj(inod,ipol_frc%i_induction  )
        d_rj(inod,ipol_exp%i_pre_uxb+2)                                 &
     &       = d_rj(inod,ipol_frc%i_induction+2)
      end do
!$omp end parallel do
!
      end subroutine cal_diff_induction_MHD_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_MHD_euler                           &
     &         (cd_prop, ipol_base, ipol_frc, ipol_dif, dt,             &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      type(conductive_property), intent(in) :: cd_prop
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_base%i_magne  ) = d_rj(inod,ipol_base%i_magne  ) &
     &    + dt * (cd_prop%coef_exp * d_rj(inod,ipol_dif%i_b_diffuse  )  &
     &                             + d_rj(inod,ipol_frc%i_induction  ))
        d_rj(inod,ipol_base%i_magne+2) = d_rj(inod,ipol_base%i_magne+2) &
     &    + dt * (cd_prop%coef_exp * d_rj(inod,ipol_dif%i_b_diffuse+2)  &
                                   + d_rj(inod,ipol_frc%i_induction+2))
      end do
!$omp end parallel do
!
      end subroutine cal_diff_induction_MHD_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ini_adams_mag_induct                               &
     &         (ipol_exp, ipol_frc, nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_pre_uxb  )                                 &
     &        = d_rj(inod,ipol_frc%i_induction  )
        d_rj(inod,ipol_exp%i_pre_uxb+2)                                 &
     &        = d_rj(inod,ipol_frc%i_induction+2)
      end do
!$omp end parallel do
!
      end subroutine set_ini_adams_mag_induct
!
! ----------------------------------------------------------------------
!
      end module cal_explicit_terms
