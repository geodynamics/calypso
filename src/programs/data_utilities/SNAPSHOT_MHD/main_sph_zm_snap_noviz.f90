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
      use analyzer_noviz_sph_zm_snap
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_noviz_sph_zm_snap
      call evolution_voviz_sph_zm_snap
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_zm_snap_noviz
