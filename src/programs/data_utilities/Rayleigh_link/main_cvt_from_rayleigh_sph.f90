!>@file   main_cvt_from_rayleigh_sph.f90
!!@brief  program convert_rayleigh_cvt
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble spectr data
!
      program convert_rayleigh_cvt
!
      use m_precision
!
      use calypso_mpi
      use analyzer_rayleigh_cvt_sph
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_cvt_rayleigh_sph
      call analyze_cvt_rayleigh_sph
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program convert_rayleigh_cvt
