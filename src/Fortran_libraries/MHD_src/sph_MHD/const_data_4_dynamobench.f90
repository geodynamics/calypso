!>@file   const_data_4_dynamobench.f90
!!@brief  module const_data_4_dynamobench
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June, 2012
!
!>@brief Evaluate dynamo benchmark results
!!
!!@verbatim
!!      subroutine s_const_data_4_dynamobench
!!      subroutine s_const_data_on_circle
!!@endverbatim
!
      module const_data_4_dynamobench
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_data_4_dynamobench
!
      use m_control_params_sph_MHD
      use m_parallel_var_dof
      use cal_rms_fields_by_sph
      use global_field_4_dynamobench
      use m_field_at_mid_equator
!
!
!
      if(iflag_debug.gt.0)  write(*,*) 'mid_eq_transfer_dynamobench'
      call mid_eq_transfer_dynamobench
!
      call cal_rms_sph_outer_core
      call copy_energy_4_dynamobench
!
      if(iflag_icb_velocity .eq. iflag_rotatable_ic) then
        call pick_inner_core_rotation
      end if
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center) then
        call cal_rms_sph_inner_core
        call copy_icore_energy_4_dbench
      end if
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center                     &
     &   .and. iflag_icb_velocity .eq. iflag_rotatable_ic) then
        call pick_mag_torque_inner_core
      end if
!
      end subroutine s_const_data_4_dynamobench
!
! ----------------------------------------------------------------------
!
      end module const_data_4_dynamobench
