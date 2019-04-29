!>@file   bcast_4_assemble_sph_ctl.f90
!!@brief  module bcast_4_assemble_sph_ctl
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2007
!
!>@brief  Control data for merge program
!!
!!@verbatim
!!       subroutine bcast_merge_control_data(mgd_ctl)
!!         type(control_data_4_merge), intent(inout) :: mgd_ctl
!!@endverbatim
!
      module bcast_4_assemble_sph_ctl
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine bcast_merge_control_data(mgd_ctl)
!
      use m_machine_parameter
      use t_control_data_4_merge
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
      use bcast_control_arrays
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      call bcast_ctl_data_4_platform(mgd_ctl%source_plt)
      call bcast_ctl_data_4_platform(mgd_ctl%assemble_plt)
!
      call bcast_phys_data_ctl(mgd_ctl%fld_mge_ctl)
      call bcast_ctl_data_4_time_step(mgd_ctl%t_mge_ctl)
      call bcast_ctl_data_4_time_step(mgd_ctl%t2_mge_ctl)
!
      call bcast_ctl_type_r1(mgd_ctl%magnetic_ratio_ctl)
!
      end subroutine bcast_merge_control_data
!
! -----------------------------------------------------------------------
!
      end module bcast_4_assemble_sph_ctl
