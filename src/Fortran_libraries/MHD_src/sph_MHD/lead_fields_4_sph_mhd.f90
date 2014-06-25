!>@file   lead_fields_4_sph_mhd.f90
!!@brief  module lead_fields_4_sph_mhd
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine s_lead_fields_4_sph_mhd
!!      subroutine pressure_4_sph_mhd
!!      subroutine enegy_fluxes_4_sph_mhd
!!@endverbatim
!
      module lead_fields_4_sph_mhd
!
      use m_precision
      use m_machine_parameter
!
!
      implicit none
!
      private :: gradients_of_vectors_sph, enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_lead_fields_4_sph_mhd
!
      use m_control_parameter
      use m_t_step_parameter
      use output_viz_file_control
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
!
      if ( (iflag*mod(istep_max_dt,i_step_output_rst)) .eq.0 ) then
        if(iflag_t_evo_4_velo .gt. id_no_evolution) then
          call pressure_4_sph_mhd
        end if
      end if
!
      if(iflag .eq. 0) then
        call gradients_of_vectors_sph
        call enegy_fluxes_4_sph_mhd
      end if
!
      end subroutine s_lead_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine pressure_4_sph_mhd
!
      use m_sph_phys_address
      use m_boundary_params_sph_MHD
      use cal_sol_sph_fluid_crank
!
      use cal_sph_field_by_rotation
      use const_radial_forces_on_bc
      use cal_div_of_forces
      use const_sph_radial_grad
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_div_of_forces_sph_2'
      call cal_div_of_forces_sph_2
!
      call s_const_radial_forces_on_bc
!
      call sum_div_of_forces
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_pressure_by_div_v'
      call cal_sol_pressure_by_div_v
!
      if(ipol%i_press_grad .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'const_pressure_gradient'
        call const_pressure_gradient                                    &
     &     (sph_bc_U, ipol%i_press, ipol%i_press_grad)
      end if
!
      end subroutine pressure_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine enegy_fluxes_4_sph_mhd
!
      use m_sph_phys_address
      use sph_transforms_4_MHD
      use cal_energy_flux_rtp
      use cal_energy_flux_rj
!
!
!      Evaluate fields for output in spectrum space
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rj'
      call s_cal_energy_flux_rj
!
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_snapshot_MHD'
      call sph_back_trans_snapshot_MHD
!
!
!      Evaluate fields for output in grid space
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rtp'
      call s_cal_energy_flux_rtp
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'sph_forward_trans_snapshot_MHD'
      call sph_forward_trans_snapshot_MHD
!
      end subroutine enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine gradients_of_vectors_sph
!
      use m_sph_phys_address
      use sph_transforms_4_MHD
      use sph_poynting_flux_smp
!
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_velo_to_grad_v_rtp'
      call copy_velo_to_grad_v_rtp
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_tmp_snap_MHD'
      call sph_forward_trans_tmp_snap_MHD
!
      if (iflag_debug.eq.1) write(*,*) 'cal_grad_of_velocities_sph'
      call cal_grad_of_velocities_sph
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'sph_back_trans_tmp_snap_MHD'
      call sph_back_trans_tmp_snap_MHD
!
      end subroutine gradients_of_vectors_sph
!
! ----------------------------------------------------------------------
!
      end module lead_fields_4_sph_mhd
