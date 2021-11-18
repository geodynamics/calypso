!>@file   main_sph_snapshot_w_4vizs.f90
!!@brief  program sph_snapshot_w_4vizs
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to evaluate snapshots from spectr data
!
      program sph_snapshot_w_4vizs
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_snap_w_4vizs
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sph_snap_w_4vizs
!
      call evolution_sph_snap_w_4vizs
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_snapshot_w_4vizs
