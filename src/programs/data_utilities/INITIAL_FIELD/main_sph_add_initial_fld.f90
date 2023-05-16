!>@file   main_sph_add_initial_fld.f90
!!@brief  program sph_add_initial_field
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in Jan., 2014
!
!>@brief  Main program to add initial field to existing initial field
!!@n       Define initial field at const_sph_initial_spectr.f90
!!@n       Input control file name:: control_MHD
!
     program sph_add_initial_field
!
      use m_precision
!
      use calypso_mpi
      use SPH_analyzer_add_initial
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!
!
      call calypso_MPI_init
!
      call initialize_add_sph_initial(MHD_ctl_name)
!
      call calypso_MPI_finalize
!
      stop
      end program sph_add_initial_field
