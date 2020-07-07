!
!     program  kemorin_convert_to_VTK

!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!
!-----------------------------------------------------------------------
!
      program kemorin_convert_to_VTK
!
      use m_precision
!
      use calypso_mpi
      use analyzer_VTK_convert

      implicit none
!
!
      call calypso_MPI_init
!
      call init_analyzer_VTK_convert
      call analyze_VTK_convert
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_convert_to_VTK
