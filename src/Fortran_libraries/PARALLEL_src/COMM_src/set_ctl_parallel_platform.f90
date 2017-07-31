!set_ctl_parallel_platform.f90
!      module set_ctl_parallel_platform
!
!        programmed by H.Matsui
!        modified by H.Matsui on Oct., 2008
!        modified by H.Matsui on Nov., 2009
!
!!     subroutine check_control_num_domains(plt)
!!       type(platform_data_control), intent(in) :: plt
!
      module set_ctl_parallel_platform
!
      use m_precision
      use m_error_IDs
!
      use t_ctl_data_4_platforms
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_control_num_domains(plt)
!
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      type(platform_data_control), intent(in) :: plt
!
!
      if (plt%ndomain_ctl%iflag .gt. 0) then
        if( nprocs .ne. plt%ndomain_ctl%intvalue) then
          write(e_message,'(a)')                                        &
     &              'Number of processes should be num. of mesh'
          call calypso_MPI_abort(ierr_P_MPI, e_message)
        end if
      end if
!
      if(my_rank .eq. 0) iflag_debug = i_debug
!
      end subroutine check_control_num_domains
!
! ----------------------------------------------------------------------
!
      end module set_ctl_parallel_platform
