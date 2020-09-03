!>@file   main_compare_sph_rst.f90
!!@brief  program compare_sph_restart
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble field data
!
      program compare_sph_restart
!
      use m_precision
!
      use calypso_mpi
      use analyzer_compare_sph_rst
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_compare_sph_restart
      call analyze_compare_sph_restart
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program compare_sph_restart
