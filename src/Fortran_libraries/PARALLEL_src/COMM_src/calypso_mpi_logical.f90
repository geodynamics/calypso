!>@file   calypso_mpi_logical.f90
!!@brief  module calypso_mpi_logical
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!> @brief MPI communication routines for 4 byte integer in Calypso
!!
!!@verbatim
!!      subroutine calypso_mpi_bcast_one_logical(buffer, root)
!!        integer, intent(in) :: root
!!        logical, intent(inout) :: buffer
!!      subroutine calypso_mpi_bcast_logical(buffer, count, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint_gl), intent(in) :: count
!!        logical, intent(inout) :: buffer(count)
!!@endverbatim
!!
!!@n @param  icode       error code
!!@n @param  message    message to output
!
      module calypso_mpi_logical
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine calypso_mpi_bcast_one_logical(buffer, root)
!
      integer, intent(in) :: root
      logical, intent(inout) :: buffer
!
      logical :: flag_tmp(1)
!
!
      flag_tmp(1) = buffer
      call MPI_BCAST(flag_tmp, 1, CALYPSO_LOGICAL,                      &
     &               root, CALYPSO_COMM, ierr_MPI)
      buffer = flag_tmp(1)
!
      end subroutine calypso_mpi_bcast_one_logical
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_bcast_logical(buffer, count, root)
!
      integer, intent(in) :: root
      integer(kind = kint_gl), intent(in) :: count
      logical, intent(inout) :: buffer(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_BCAST(buffer(ist+1), ilen_in, CALYPSO_LOGICAL,         &
     &      root, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_bcast_logical
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi_logical
