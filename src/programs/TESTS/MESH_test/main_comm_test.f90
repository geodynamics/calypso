!main_comm_test.f90
!
!     program  commnication_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program commnication_test
!
      use m_precision
!
      use calypso_mpi
      use analyzer_comm_test

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_communication_test
      call analyze_communication_test

      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program commnication_test
