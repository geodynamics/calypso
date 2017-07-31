!bcast_4_assemble_sph_ctl.f90
!      module bcast_4_assemble_sph_ctl
!
!      Written by H. Matsui
!
!       subroutine read_control_4_merge
!       subroutine read_control_assemble_sph
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
       subroutine bcast_merge_control_data
!
      use m_machine_parameter
      use m_control_data_4_merge
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
      use bcast_control_arrays
!
!
      call bcast_ctl_data_4_platform(source_plt)
      call bcast_ctl_data_4_platform(assemble_plt)
!
      call bcast_phys_data_ctl(fld_mge_ctl)
      call bcast_ctl_data_4_time_step(t_mge_ctl)
      call bcast_ctl_data_4_time_step(t2_mge_ctl)
!
      call bcast_ctl_type_r1(magnetic_ratio_ctl)
!
      end subroutine bcast_merge_control_data
!
! -----------------------------------------------------------------------
!
      end module bcast_4_assemble_sph_ctl
