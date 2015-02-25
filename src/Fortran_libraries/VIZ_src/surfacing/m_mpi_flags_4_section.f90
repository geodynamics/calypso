!
!     module   m_mpi_flags_4_section
!.......................................................................
!
!     Writen by H. Matsui  on July., 2006
!
!       subroutine allocate_flags_4_comm_psf
!       subroutine deallocate_flags_4_comm_psf
!
      module   m_mpi_flags_4_section
!
      use m_precision
!
      implicit  none
!
!
      integer, save, allocatable :: sta1_psf(:,:)
!                 work array for communication (wait)
      integer, save, allocatable :: sta2_psf(:,:)
!                 work array for communication (wait)
      integer, save, allocatable :: req1_psf(:  )
!                 work array for communication (wait)
      integer, save, allocatable :: req2_psf(:  )
!                 work array for communication (wait)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_flags_4_comm_psf
!
      use calypso_mpi
!
!
      allocate (sta1_psf(MPI_STATUS_SIZE,nprocs))
      allocate (sta2_psf(MPI_STATUS_SIZE,nprocs))
      allocate (req1_psf(nprocs))
      allocate (req2_psf(nprocs))
!
      end subroutine allocate_flags_4_comm_psf
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_flags_4_comm_psf
!
      deallocate (sta1_psf)
      deallocate (sta2_psf)
      deallocate (req1_psf)
      deallocate (req2_psf)
!
      end subroutine deallocate_flags_4_comm_psf
!
! ----------------------------------------------------------------------
!
      end module   m_mpi_flags_4_section
