!>@file   cal_r_buoyancies_on_sph.f90
!!@brief  module cal_r_buoyancies_on_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate buoyancy at specific radius
!!
!!@verbatim
!!      subroutine s_cal_r_buoyancies_on_sph(kr)
!!@endverbatim
!!
!!@param kr  Radial grid ID
!
      module cal_r_buoyancies_on_sph
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_schmidt_poly_on_rtm
!
      implicit  none
!
      private :: cal_r_double_buoyancy_on_sph, cal_r_buoyancy_on_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_r_buoyancies_on_sph(kr)
!
      use m_machine_parameter
      use m_physical_property
!
      integer(kind= kint), intent(in) :: kr
!
!
      if ((iflag_4_gravity*iflag_4_composit_buo) .gt. id_turn_OFF) then
!
        if(iflag_4_ref_temp .ne. id_sphere_ref_temp) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*)'cal_r_double_buoyancy_on_sph', ipol%i_temp
          call cal_r_double_buoyancy_on_sph(kr, ipol%i_temp,            &
     &       ipol%i_div_buoyancy)
        else
          if (iflag_debug.eq.1)                                         &
     &      write(*,*)'cal_r_double_buoyancy_on_sph', ipol%i_par_temp
          call cal_r_double_buoyancy_on_sph(kr, ipol%i_par_temp,        &
     &        ipol%i_div_buoyancy)
        end if
!
      else if ( iflag_4_gravity .gt. id_turn_OFF) then
!
        if(iflag_4_ref_temp .ne. id_sphere_ref_temp) then
          if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sph'
          call cal_r_buoyancy_on_sph(kr, coef_buo, ipol%i_temp,         &
     &        ipol%i_div_buoyancy)
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sph'
          call cal_r_buoyancy_on_sph(kr, coef_buo, ipol%i_par_temp,     &
     &        ipol%i_div_buoyancy)
        end if
!
      else if (iflag_4_composit_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sph'
        call cal_r_buoyancy_on_sph(kr, coef_comp_buo, ipol%i_light,     &
     &      ipol%i_div_comp_buo)
!
      else if (iflag_4_filter_gravity .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sph'
        call cal_r_buoyancy_on_sph(kr, coef_buo, ipol%i_filter_temp,    &
     &      ipol%i_div_filter_buo)
      end if
!
      end subroutine s_cal_r_buoyancies_on_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_r_double_buoyancy_on_sph(kr, is_t, is_fr)
!
      use m_physical_property
!
      integer(kind= kint), intent(in) :: is_t, is_fr, kr
      integer(kind= kint) :: inod, j
!
!
!$omp parallel do private (inod,j)
      do j = 1, nidx_rj(2)
        inod = j + (kr-1) * nidx_rj(2)
        d_rj(inod,is_fr) = (coef_buo*d_rj(inod,is_t)                    &
     &                     + coef_comp_buo*d_rj(inod,ipol%i_light))     &
     &                     * radius_1d_rj_r(kr)
      end do
!$omp end parallel do
!
      end subroutine cal_r_double_buoyancy_on_sph
!
!-----------------------------------------------------------------------
!
      subroutine cal_r_buoyancy_on_sph(kr, coef, is_fld, is_fr)
!
      use m_physical_property
!
      integer(kind= kint), intent(in) :: is_fld, is_fr, kr
      real(kind = kreal), intent(in) :: coef
      integer(kind= kint) :: inod, j
!
!
!$omp parallel do private (inod,j)
      do j = 1, nidx_rj(2)
        inod = j + (kr-1) * nidx_rj(2)
        d_rj(inod,is_fr) = coef*d_rj(inod,is_fld)*radius_1d_rj_r(kr)
      end do
!$omp end parallel do
!
      end subroutine cal_r_buoyancy_on_sph
!
!-----------------------------------------------------------------------
!
      end module cal_r_buoyancies_on_sph
