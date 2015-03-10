!>@file   main_assemble_sph.f90
!!@brief  program assemble_sph
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble spectr data
!
      program assemble_sph
!
      use m_precision
!
      use calypso_mpi
      use analyzer_assemble_sph
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_assemble_sph
      call analyze_assemble_sph
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program assemble_sph
