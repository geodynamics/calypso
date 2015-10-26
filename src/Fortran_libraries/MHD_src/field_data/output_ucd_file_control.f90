!
!      module output_ucd_file_control
!
!      Programmed by H.Matsui on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!     subroutine s_output_ucd_file_control
!     subroutine output_grd_file_4_snapshot
!
      module output_ucd_file_control
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_output_ucd_file_control
!
      use calypso_mpi
      use m_t_step_parameter
      use m_ucd_data
      use parallel_ucd_IO_select
      use copy_time_steps_4_restart
!      use range_data_IO
!
      integer(kind = kint) :: istep_ucd
!
!
      if(i_step_output_ucd .eq. 0) return
      if(mod(istep_max_dt,i_step_output_ucd) .ne. 0) return
!
      istep_ucd = istep_max_dt / i_step_output_ucd
!
      call copy_time_steps_to_restart
      call sel_write_parallel_ucd_file(istep_ucd, fem_ucd, merged_ucd)
!      call output_range_data(istep_ucd, time)
!
      end subroutine s_output_ucd_file_control
!
! ----------------------------------------------------------------------
!
      subroutine output_grd_file_4_snapshot
!
      use m_geometry_data
      use m_nod_comm_table
      use m_t_step_parameter
      use output_parallel_ucd_file
!
!
      if(i_step_output_ucd .eq. 0) return
      call output_grd_file(node1, ele1, nod_comm)
!
      end subroutine output_grd_file_4_snapshot
!
!-----------------------------------------------------------------------
!
      end module output_ucd_file_control
