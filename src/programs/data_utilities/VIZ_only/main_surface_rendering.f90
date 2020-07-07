!
!     program  kemorin_surface_rendering

!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!
!-----------------------------------------------------------------------
!
      program kemorin_surface_rendering
!
      use m_precision
!
      use calypso_mpi
      use analyzer_psf

      implicit none
!
!
      call calypso_MPI_init
!
      call init_analyzer_psf
      call analyze_psf
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_surface_rendering
