!>@file   cal_tave_sph_ene_spectr.f90
!!        module cal_tave_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  May, 2008
!!
!
!> @brief Evaluate time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine add_average_ene_spectr                               &
!!     &         (time_sph, pre_time, ncomp, spectr_l,                  &
!!     &          ave_spec_l, ave_pre_l, rms_spec_l, rms_pre_l)
!!      subroutine add_deviation_ene_spectr(time_sph, pre_time, ncomp,  &
!!     &          spectr_l, ave_spec_l, sigma_spec_l, spectr_pre_l)
!!
!!      subroutine copy_ene_spectr_2_pre                                &
!!     &         (time_sph, pre_time, ncomp, spectr_l,                  &
!!     &          ave_spec_l, ave_pre_l, rms_spec_l, rms_pre_l)
!!      subroutine copy_deviation_ene_2_pre(time_sph, pre_time,         &
!!     &          ncomp, spectr_l, ave_spec_l, sigma_spec_l, sigma_pre_l)
!!
!!      subroutine divide_average_ene_spectr(time_sph, time_ini,        &
!!     &          ncomp, ave_spec_l, rms_spec_l)
!!      subroutine divide_deviation_ene_spectr(time_sph, time_ini,      &
!!     &                                       ncomp, sigma_spec_l)
!!@endverbatim
!!
!!@n @param istep  time step number
!!@n @param icou   counter for snapshots
!!@n @param ierr   error flag
!
      module cal_tave_sph_ene_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine add_average_ene_spectr                                 &
     &         (time_sph, pre_time, ncomp, spectr_l,                    &
     &          ave_spec_l, ave_pre_l, rms_spec_l, rms_pre_l)
!
      real(kind = kreal), intent(in) :: time_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: spectr_l(ncomp)
!
      real(kind = kreal), intent(inout) :: pre_time
      real(kind = kreal), intent(inout) :: ave_spec_l(ncomp)
      real(kind = kreal), intent(inout) :: ave_pre_l(ncomp)
      real(kind = kreal), intent(inout) :: rms_spec_l(ncomp)
      real(kind = kreal), intent(inout) :: rms_pre_l(ncomp)
!
!
!$omp parallel workshare
        ave_spec_l(1:ncomp) =  ave_spec_l(1:ncomp)                      &
     &             + half * (spectr_l(1:ncomp) + ave_pre_l(1:ncomp))    &
     &                       * (time_sph - pre_time)
        rms_spec_l(1:ncomp) =  rms_spec_l(1:ncomp)                      &
     &             + half * (spectr_l(1:ncomp)**2 + rms_pre_l(1:ncomp)) &
     &                        * (time_sph - pre_time)
        ave_pre_l(1:ncomp) =  spectr_l(1:ncomp)
        rms_pre_l(1:ncomp) =  spectr_l(1:ncomp)**2
!$omp end parallel workshare
      pre_time = time_sph
!
      end subroutine add_average_ene_spectr
!
!   --------------------------------------------------------------------
!
      subroutine add_deviation_ene_spectr(time_sph, pre_time, ncomp,    &
     &          spectr_l, ave_spec_l, sigma_spec_l, spectr_pre_l)
!
      real(kind = kreal), intent(in) :: time_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: spectr_l(ncomp)
      real(kind = kreal), intent(in) :: ave_spec_l(ncomp)
!
      real(kind = kreal), intent(inout) :: pre_time
      real(kind = kreal), intent(inout) :: sigma_spec_l(ncomp)
      real(kind = kreal), intent(inout) :: spectr_pre_l(ncomp)
!
!
!$omp parallel workshare
      sigma_spec_l(1:ncomp) =  sigma_spec_l(1:ncomp)                    &
     &        + half * ((spectr_l(1:ncomp) - ave_spec_l(1:ncomp))**2    &
     &                                      + spectr_pre_l(1:ncomp))    &
     &         * (time_sph - pre_time)
      spectr_pre_l(1:ncomp) = (spectr_l(1:ncomp)                        &
     &                         - ave_spec_l(1:ncomp))**2
!$omp end parallel workshare
      pre_time = time_sph
!
      end subroutine add_deviation_ene_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine  copy_ene_spectr_2_pre                                 &
     &         (time_sph, pre_time, ncomp, spectr_l,                    &
     &          ave_spec_l, ave_pre_l, rms_spec_l, rms_pre_l)
!
      real(kind = kreal), intent(in) :: time_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: spectr_l(ncomp)
!
      real(kind = kreal), intent(inout) :: pre_time
      real(kind = kreal), intent(inout) :: ave_spec_l(ncomp)
      real(kind = kreal), intent(inout) :: ave_pre_l(ncomp)
      real(kind = kreal), intent(inout) :: rms_spec_l(ncomp)
      real(kind = kreal), intent(inout) :: rms_pre_l(ncomp)
!
!
!$omp parallel workshare
      ave_spec_l(1:ncomp) =  0.0d0
      rms_spec_l(1:ncomp) =  0.0d0
      ave_pre_l(1:ncomp) =   spectr_l(1:ncomp)
      rms_pre_l(1:ncomp) =   spectr_l(1:ncomp)**2
!$omp end parallel workshare
      pre_time = time_sph
!
      end subroutine copy_ene_spectr_2_pre
!
!   --------------------------------------------------------------------
!
      subroutine copy_deviation_ene_2_pre(time_sph, pre_time,           &
     &          ncomp, spectr_l, ave_spec_l, sigma_spec_l, sigma_pre_l)
!
      real(kind = kreal), intent(in) :: time_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: spectr_l(ncomp)
      real(kind = kreal), intent(in) :: ave_spec_l(ncomp)
!
      real(kind = kreal), intent(inout) :: pre_time
      real(kind = kreal), intent(inout) :: sigma_spec_l(ncomp)
      real(kind = kreal), intent(inout) :: sigma_pre_l(ncomp)
!
!
!$omp parallel workshare
      sigma_spec_l(1:ncomp) = 0.0d0
      sigma_pre_l(1:ncomp) =  (spectr_l(1:ncomp)                        &
     &                        - ave_spec_l(1:ncomp))**2
!$omp end parallel workshare
      pre_time = time_sph
!
      end subroutine copy_deviation_ene_2_pre
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine divide_average_ene_spectr(time_sph, time_ini,          &
     &          ncomp, ave_spec_l, rms_spec_l)
!
      real(kind = kreal), intent(in) :: time_sph, time_ini
      integer(kind = kint), intent(in) :: ncomp
!
      real(kind = kreal), intent(inout) :: ave_spec_l(1:ncomp)
      real(kind = kreal), intent(inout) :: rms_spec_l(1:ncomp)
!
!
!$omp parallel workshare
      ave_spec_l(1:ncomp)                                               &
     &         = ave_spec_l(1:ncomp) / (time_sph - time_ini)
      rms_spec_l(1:ncomp)                                               &
     &         = sqrt(rms_spec_l(1:ncomp) / (time_sph - time_ini))
!$omp end parallel workshare
!
      end subroutine divide_average_ene_spectr
!
!   --------------------------------------------------------------------
!
      subroutine divide_deviation_ene_spectr(time_sph, time_ini,        &
     &                                       ncomp, sigma_spec_l)
!
      real(kind = kreal), intent(in) :: time_sph, time_ini
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: sigma_spec_l(ncomp)
!
!
!$omp parallel workshare
      sigma_spec_l(1:ncomp)                                             &
     &      = sqrt(sigma_spec_l(1:ncomp) / (time_sph - time_ini))
!$omp end parallel workshare
!
      end subroutine divide_deviation_ene_spectr
!
!   --------------------------------------------------------------------
!
      end module cal_tave_sph_ene_spectr
