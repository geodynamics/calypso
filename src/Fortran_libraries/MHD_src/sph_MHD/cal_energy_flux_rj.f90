!> @file  cal_energy_flux_rj.f90
!!      module cal_energy_flux_rj
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine s_cal_energy_flux_rj
!!@endverbatim
!
      module cal_energy_flux_rj
!
      use m_precision
      use m_constants
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_energy_flux_rj
!
      use m_sph_phys_address
      use const_sph_radial_grad
!
!
      if(ipol%i_rot_Coriolis .gt. 0) then
        call const_grad_poloidal_moment(ipol%i_rot_Coriolis)
      end if
!
      end subroutine s_cal_energy_flux_rj
!
!-----------------------------------------------------------------------
!
      end module cal_energy_flux_rj
