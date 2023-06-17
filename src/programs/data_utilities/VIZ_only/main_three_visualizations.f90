!>@file   main_three_visualizations.f90
!!@brief  program kemo_three_visualizations
!!
!!@author H. Matsui
!!@date Programmed in Mar., 2000 (ver 1.0)
!!      Modified in May, 2003
!!      Modified in July 2006
!!
!> @brief Main program for three data visualizations
!!
!!@verbatim
!!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!!@endverbatim
      program kemo_three_visualizations
!
      use m_precision
!
      use calypso_mpi
      use analyzer_three_vizs
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_three_vizs
      call analyze_three_vizs
!
      call calypso_MPI_finalize
!
      stop '***** program finished *****'
!
      end program kemo_three_visualizations
