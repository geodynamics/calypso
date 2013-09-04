!set_ctl_parallel_platform.f90
!      module set_ctl_parallel_platform
!
!        programmed by H.Matsui
!        modified by H.Matsui on Oct., 2008
!        modified by H.Matsui on Nov., 2009
!
!     subroutine check_control_num_domains
!
      module set_ctl_parallel_platform
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
      use m_ctl_data_4_platforms
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_control_num_domains
!
!
!
      if (i_num_subdomain .gt. 0) then
        if( nprocs .ne. num_subdomain_ctl) then
          write(e_message,'(a)')                                        &
     &              'Number of processes should be num. of mesh'
          call parallel_abort(ione, e_message)
        end if
      end if
!
      end subroutine check_control_num_domains
!
! ----------------------------------------------------------------------
!
      end module set_ctl_parallel_platform
