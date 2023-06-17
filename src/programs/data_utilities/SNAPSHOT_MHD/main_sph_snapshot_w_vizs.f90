!>@file   main_sph_snapshot_w_vizs.f90
!!@brief  program sph_snapshot_w_vizs
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to evaluate snapshots from spectr data
!!         with visualizers
!!         Input control file: control_snapshot
!
      program sph_snapshot_w_vizs
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_snap_w_vizs
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!
!
      call calypso_MPI_init
!
      call initialize_sph_snap_w_vizs(snap_ctl_name)
      call evolution_sph_snap_w_vizs
!
      call calypso_MPI_finalize
!
      stop '***** program finished *****'
!
      end program sph_snapshot_w_vizs
