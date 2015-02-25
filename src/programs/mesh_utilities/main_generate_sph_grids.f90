!>@file   main_generate_sph_grids.f90
!!@brief  program generate_sph_grids
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in Feb., 2015 
!
!>@brief  Main program generate spherical harmonics indices
!
      program generate_sph_grids
!
      use m_precision
!
      use calypso_mpi
      use analyzer_gen_sph_grids
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_gen_sph_grids
      call analyze_gen_sph_grids
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program generate_sph_grids
