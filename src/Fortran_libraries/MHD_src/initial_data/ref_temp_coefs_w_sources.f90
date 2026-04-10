!>@file   ref_temp_coefs_w_sources.f90
!!@brief  module ref_temp_coefs_w_sources
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Obtain homogeneous sources from boundary flux
!!
!!@verbatim
!!     Tempareture at fluid core
!!       T = const_OC + coef_OC / r - (1/6) source_OC r^2
!!     Tempareture at inner core
!!       T = const_IC - (1/6) source_IC r^2
!!
!!      subroutine reftemp_coefs_fix_in_fix_out(r_ICB, r_CMB, source,   &
!!     &          temp_ICB, temp_CMB, const, coef)
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: temp_ICB, temp_CMB
!!        real(kind = kreal), intent(in) :: source
!!        real(kind = kreal), intent(inout) :: const, coef
!!     Coefficients for fixed temperature at ICB and CMB
!!       Input::  Q_OC, T_ICB, T_CMB
!!       Output:: coef_OC, const_OC
!!         coef_OC = (T_ICB - T_CMB - Q_OC * (r_CMB**2 - r_ICB**2)/six)
!!              * r_ICB*r_CMB / (r_CMB - r_ICB)
!!         const_OC = (-T_ICB * r_ICB + T_CMB * r_CMB
!!      &       - Q_OC * (r_CMB**3 - r_ICB**3) / six) / (r_CMB - r_ICB)
!!
!!      subroutine reftemp_coefs_fix_in_flux_out(r_ICB, r_CMB, source,  &
!!     &          temp_ICB, flux_CMB, const, coef)
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: flux_CMB
!!        real(kind = kreal), intent(in) :: source
!!        real(kind = kreal), intent(in) :: temp_ICB
!!        real(kind = kreal), intent(inout) :: const, coef
!!     Coefficients for fixed temperature at ICB and fixed flux at CMB
!!       Input::  Q_OC, T_ICB, f_CMB
!!       Output:: coef_OC, const_OC
!!         coef_OC = -f_CMB * r_CMB**2  - Q_OC * r_CMB**3 / three
!!         const_OC = T_CMB - coef_OC / r_ICB + source_OC * r_ICB^2 / 6
!!
!!      subroutine reftemp_coefs_flux_in_fix_out(r_ICB, r_CMB, source, &
!!     &          flux_ICB, temp_CMB, const, coef)
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: flux_ICB
!!        real(kind = kreal), intent(in) :: source
!!        real(kind = kreal), intent(in) :: temp_CMB
!!        real(kind = kreal), intent(inout) :: const, coef
!!      Coefficients for fixed flux at ICB and fixed temperature at CMB
!!         Input::  Q_OC, f_ICB, T_CMB
!!         Output:: coef_OC, const_OC
!!           coef_OC =  f_ICB * r_ICB**2  - Q_OC * r_ICB**3 / three
!!           const_OC = T_CMB - coef_OC / r_CMB + source_OC * r_CMB^2 / 6
!!
!!      subroutine reftemp_coefs_flux_in_flux_out(r_ICB, r_CMB, source, &
!!     &          flux_ICB, flux_CMB, temp_CMB, const, coef)
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: flux_ICB, flux_CMB
!!        real(kind = kreal), intent(in) :: source
!!        real(kind = kreal), intent(in) :: temp_CMB
!!        real(kind = kreal), intent(inout) :: const, coef
!!       Coefficients for fixed flux at ICB and CMB
!!          Input::  f_ICB, f_CMB, Q_OC, T_CMB
!!          Output:: coef_OC, const_OC
!!         coef_OC =  (f_ICB * r_ICB + f_CMB * r_CMB) * (r_ICB*r_CMB)^2
!!               / (r_CMB^3 - r_ICB^3)
!!         const_OC = T_CMB - coef_OC / r_CMB + source_OC * r_CMB^2 / 6
!!
!!      real(kind = kreal) function reftemp_const_whole_core            &
!!     &                          (r_CMB, source_OC, temp_CMB)
!!        real(kind = kreal), intent(in) :: r_CMB
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(in) :: temp_CMB
!!      Constant for temperature in whole sphere
!!         Input::  Q_OC, T_CMB
!!         Output:: const_OC
!!           const_OC = T_CMB + 3 * Q_OC * r_CMB^2 / 6
!!
!!      subroutine reftemp_coefs_w_IC_source(r_ICB, r_CMB,              &
!!     &          source_OC, source_IC, kappa_IC, temp_CMB,             &
!!     &          const_OC, coef_OC, const_IC)
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(in) :: kappa_IC
!!        real(kind = kreal), intent(in) :: temp_CMB
!!        real(kind = kreal), intent(in) :: source_IC
!!        real(kind = kreal), intent(inout) :: const_OC, coef_OC
!!        real(kind = kreal), intent(inout) :: const_IC
!!     Coefficients for fixed temperature at CMB 
!!                 with sources in inner and outer cores
!!       Input::  Q_IC, Q_OC, kappa_IC, T_CMB
!!       Output:: coef_OC, const_OC, const_IC
!!         coef_OC =  (kappa_IC * Q_IC - Q_OC) * r_ICB^3 / 3
!!         const_OC = T_CMB - coef_OC / r_CMB + source_OC * r_CMB^2 / 6
!!         const_IC = const_OC + coef_OC / r_ICB
!!       &          + (Q_IC - Q_OC) * r_ICB^2 / 6
!!@endverbatim
      module ref_temp_coefs_w_sources
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_coefs_fix_in_fix_out(r_ICB, r_CMB, source,     &
     &          temp_ICB, temp_CMB, const, coef)
!
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: temp_ICB, temp_CMB
      real(kind = kreal), intent(in) :: source
!
      real(kind = kreal), intent(inout) :: const, coef
!
      coef = (temp_ICB - temp_CMB - source * (r_CMB**2 - r_ICB**2)/six) &
     &       * r_ICB*r_CMB / (r_CMB - r_ICB)
      const = (-temp_ICB * r_ICB + temp_CMB * r_CMB                     &
     &       - source * (r_CMB**3 - r_ICB**3) / six) / (r_CMB - r_ICB)
!
      end subroutine reftemp_coefs_fix_in_fix_out
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_coefs_fix_in_flux_out(r_ICB, r_CMB, source,    &
     &          temp_ICB, flux_CMB, const, coef)
!
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: flux_CMB
      real(kind = kreal), intent(in) :: source
      real(kind = kreal), intent(in) :: temp_ICB
!
      real(kind = kreal), intent(inout) :: const, coef
!
      coef = -flux_CMB * r_CMB**2 - source * r_CMB**3 / three
      const = temp_ICB - coef / r_ICB + source * r_ICB**2 / six
!
      end subroutine reftemp_coefs_fix_in_flux_out
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_coefs_flux_in_fix_out(r_ICB, r_CMB, source,   &
     &          flux_ICB, temp_CMB, const, coef)
!
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: flux_ICB
      real(kind = kreal), intent(in) :: source
      real(kind = kreal), intent(in) :: temp_CMB
!
      real(kind = kreal), intent(inout) :: const, coef
!
!
      coef =  flux_ICB * r_ICB**2  - source * r_ICB**3 / three
      const = temp_CMB - coef / r_CMB + source * r_CMB**2 / six
!
      end subroutine reftemp_coefs_flux_in_fix_out
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reftemp_coefs_flux_in_flux_out(r_ICB, r_CMB, source,   &
     &          flux_ICB, flux_CMB, temp_CMB, const, coef)
!
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: flux_ICB, flux_CMB
      real(kind = kreal), intent(in) :: source
      real(kind = kreal), intent(in) :: temp_CMB
!
      real(kind = kreal), intent(inout) :: const, coef
!
!
      coef =  (flux_ICB * r_ICB + flux_CMB * r_CMB) * (r_ICB*r_CMB)**2  &
     &          / (r_CMB**3 - r_ICB**3)
      const = temp_CMB - coef / r_CMB + source * r_CMB**2 / six
!
      end subroutine reftemp_coefs_flux_in_flux_out
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      real(kind = kreal) function reftemp_const_whole_core              &
     &                          (r_CMB, source_OC, temp_CMB)
!
      real(kind = kreal), intent(in) :: r_CMB
      real(kind = kreal), intent(in) :: source_OC
      real(kind = kreal), intent(in) :: temp_CMB
!
      reftemp_const_whole_core = temp_CMB + source_OC * r_CMB**2 / six
!
      end function reftemp_const_whole_core
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_coefs_w_IC_source(r_ICB, r_CMB,                &
     &          source_OC, source_IC, kappa_IC, temp_CMB,               &
     &          const_OC, coef_OC, const_IC)
!
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: source_OC
      real(kind = kreal), intent(in) :: kappa_IC
      real(kind = kreal), intent(in) :: temp_CMB
      real(kind = kreal), intent(in) :: source_IC
!
      real(kind = kreal), intent(inout) :: const_OC, coef_OC
      real(kind = kreal), intent(inout) :: const_IC
!
!
      coef_OC =  (kappa_IC * source_IC - source_OC) * r_ICB**3 / three
      const_OC = temp_CMB - coef_OC / r_CMB                             &
     &          + source_OC * r_CMB**2 / six
      const_IC = const_OC + coef_OC / r_ICB                             &
     &          + (source_IC - source_OC) * r_ICB**2 / six
!
      end subroutine reftemp_coefs_w_IC_source
!
!-----------------------------------------------------------------------
!
      end module ref_temp_coefs_w_sources
