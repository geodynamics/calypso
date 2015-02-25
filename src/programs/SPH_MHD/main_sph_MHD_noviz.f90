!>@file   main_sph_MHD_noviz.f90
!!@brief  program sph_MHD_noviz
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program for MHD dynamo simulation
!!        without cross sectioning routines
!
     program sph_MHD_noviz
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_MHD_noviz
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sph_MHD_noviz
!
      call evolution_sph_MHD_noviz
!
      call calypso_MPI_finalize
!
      stop
      end program sph_MHD_noviz
