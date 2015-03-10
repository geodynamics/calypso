!>@file   SPH_analyzer_zm_snap.f90
!!@brief  module SPH_analyzer_zm_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  main routines to evaluate zonal mean field
!!
!!@verbatim
!!      subroutine SPH_analyze_zm_snap(i_step)
!!@endverbatim
!!
!!@param i_step  time step number
!
      module SPH_analyzer_zm_snap
!
      use m_precision
!
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
      subroutine SPH_analyze_zm_snap(i_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_node_id_spherical_IO
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use set_reference_sph_mhd
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
!
      use cal_zonal_mean_sph_spectr
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap(i_step)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd
      call end_eleps_time(9)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_sph_spectr'
      call zonal_mean_all_sph_spectr
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control
      call end_eleps_time(11)
      call end_eleps_time(4)
!
      end subroutine SPH_analyze_zm_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_zm_snap
!
      use output_viz_file_control
      use lead_pole_data_4_sph_mhd
      use nod_phys_send_recv
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use sph_rtp_zonal_rms_data
      use m_sph_spectr_data
!
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
      if(iflag .ne. 0) return
!*
!*  -----------  data transfer to FEM array --------------
!*
      call copy_forces_to_snapshot_rtp
      call copy_snap_vec_fld_from_trans
      call copy_snap_vec_fld_to_trans
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field
!
!*  ----------- transform field at pole and center --------------
!*
      call lead_pole_fields_4_sph_mhd
!
      call phys_send_recv_all
!
      end subroutine SPH_to_FEM_bridge_zm_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_zm_snap
