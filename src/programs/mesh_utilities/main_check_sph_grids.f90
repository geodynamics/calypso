!>@file   main_check_sph_grids.f90
!!@brief  program check_sph_grids
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in Feb., 2015 
!
!>@brief  Main program generate spherical harmonics indices
!
      program check_sph_grids
!
      use m_precision
!
      use calypso_mpi
      use analyzer_check_sph_grids
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_check_sph_grids
      call analyze_check_sph_grids
!
      call  calypso_MPI_finalize
!
      stop '***** program finished *****'
!
      end program check_sph_grids
