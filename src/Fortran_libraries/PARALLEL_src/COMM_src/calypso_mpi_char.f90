!>@file   calypso_mpi_char.f90
!!@brief  module calypso_mpi_char
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!
!> @brief MPI communication routines for characters in Calypso
!!
!!@verbatim
!!      subroutine calypso_mpi_bcast_character(buffer, count, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint_gl), intent(in) :: count
!!        character(len = 1), intent(inout) :: buffer(count)
!!@endverbatim
!!
!!@n @param  icode       error code
!!@n @param  message    message to output
!
      module calypso_mpi_char
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
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_bcast_character(buffer, count, root)
!
      integer, intent(in) :: root
      integer(kind = kint_gl), intent(in) :: count
      character(len = 1), intent(inout) :: buffer(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_BCAST(buffer(ist+1), ilen_in, CALYPSO_CHARACTER,       &
     &      root, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_bcast_character
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi_char
