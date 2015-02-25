!>@file   main_sph_zm_snap.f90
!!@brief  program kemorin_sph_zm_snap
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to evaluate zonal mean field
!
      program kemorin_sph_zm_snap
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_zm_snap
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sph_zm_snap
      call evolution_sph_zm_snap
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_sph_zm_snap
