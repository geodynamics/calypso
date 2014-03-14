!>@file   main_sph_dynamobench.f90
!!@brief  program sph_dynamobench
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!> @brief Main program for dynamo benchmark check
!
      program sph_dynamobench
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_dynamobench
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sph_dynamobench
!
      call evolution_sph_dynamobench
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_dynamobench
