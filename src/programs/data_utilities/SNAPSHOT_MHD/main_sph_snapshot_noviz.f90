!>@file   main_sph_snapshot_noviz.f90
!!@brief  program sph_snap_noviz
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to evaluate snapshots from spectr data
!!        without visualization routines
!
      program sph_snap_noviz
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_noviz_sph_snap
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_noviz_sph_snap
!
      call evolution_noviz_sph_snap
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_snap_noviz
