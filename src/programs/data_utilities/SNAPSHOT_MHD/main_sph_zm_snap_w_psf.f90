!>@file   sph_zm_snap_noviz.f90
!!@brief  program sph_zm_snap_noviz
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  main routines to obtain zonal mean snapshots
!!
!!@verbatim
!!     program  sph_zm_snap_noviz
!!@endverbatim
!
      program sph_zm_snap_noviz
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_zm_snap_w_psf
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sph_zm_snap_w_psf
      call evolution_sph_zm_snap_w_psf
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_zm_snap_noviz
