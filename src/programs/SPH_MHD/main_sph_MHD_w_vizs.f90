!>@file   main_sph_MHD_w_vizs.f90
!!@brief  program kemorin_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program for MHD dynamo simulation with PVR module
!!         input control file:  control_MHD
!
      program kemorin_sph_MHD
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_MHD_w_vizs
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!
!
      call calypso_MPI_init
!
      call initialize_sph_mhd_w_vizs(MHD_ctl_name)
      call evolution_sph_mhd_w_vizs
!
      call calypso_MPI_finalize
!
      stop
      end program kemorin_sph_MHD
