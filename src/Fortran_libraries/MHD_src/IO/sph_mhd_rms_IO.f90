!>@file   sph_mhd_rms_IO.f90
!!@brief  module sph_mhd_rms_IO
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine open_sph_vol_rms_file_mhd
!!      subroutine output_rms_sph_mhd_control
!!@endverbatim
!
      module sph_mhd_rms_IO
!
      use m_precision
!
      use calypso_mpi
      use m_spheric_parameter
      use m_gauss_coefs_monitor_data
      use m_pickup_sph_spectr_data
      use pickup_sph_coefs
      use pickup_gauss_coefficients
      use output_sph_m_square_file
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file_mhd
!
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
!
!
      call init_gauss_coefs_4_monitor
      call init_sph_spec_4_monitor
!
      end subroutine open_sph_vol_rms_file_mhd
!
!  --------------------------------------------------------------------
!
      subroutine output_rms_sph_mhd_control
!
      use m_t_step_parameter
      use m_boundary_params_sph_MHD
      use set_exit_flag_4_visualizer
      use cal_rms_fields_by_sph
      use m_no_heat_Nusselt_num
!
      integer (kind = kint) :: i_flag
!
!
      call set_output_flag(i_flag, istep_max_dt, i_step_check)
!
      if (i_flag .ne. 0) return
!
      call cal_rms_sph_outer_core
      call cal_gauss_coefficients
      call pickup_sph_spec_4_monitor
      call cal_no_heat_source_Nu(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0) )
!
      call write_total_energy_to_screen(my_rank, i_step_MHD, time)
!
      call write_sph_vol_ave_file(my_rank, i_step_MHD, time)
      call write_sph_vol_ms_file(my_rank, i_step_MHD, time)
      call write_sph_vol_ms_spectr_file(my_rank, i_step_MHD, time)
      call write_sph_layer_ms_file(my_rank, i_step_MHD, time)
!
      call write_gauss_coefs_4_monitor(my_rank, istep_max_dt, time)
      call write_sph_spec_4_monitor(my_rank, istep_max_dt, time)
!
      call write_no_heat_source_Nu(idx_rj_degree_zero,                  &
     &    istep_max_dt, time)
!
      end subroutine output_rms_sph_mhd_control
!
!  --------------------------------------------------------------------
!
      end module sph_mhd_rms_IO
