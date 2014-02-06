!>@file   main_sph_add_initial_fld.f90
!!@brief  program sph_add_initial
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in Jan., 2014
!
!>@brief  Main program to add initial field to existing initial field
!!@n       Define initial field at const_sph_initial_spectr.f90
!
     program sph_add_initial
!
      use m_precision
!
      use calypso_mpi
      use SPH_analyzer_add_initial
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_add_sph_initial
!
      call calypso_MPI_finalize
!
      stop
      end program sph_add_initial
