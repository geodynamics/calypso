!>@file   main_sph_MHD.f90
!!@brief  program kemorin_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program for MHD dynamo simulation
!!        without cross sectioning routines
!
     program kemorin_sph_MHD
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_MHD
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sph_MHD
!
      call evolution_sph_MHD
!
      call calypso_MPI_finalize
!
      stop
      end program kemorin_sph_MHD
