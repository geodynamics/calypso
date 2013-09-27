!>@file   m_parallel_var_dof.f90
!!@brief      module m_parallel_var_dof
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n    Modified on Apr., 2008
!!@n    Modified on Dec., 2012
!
!> @brief  Basic parameters for MPI parallelization
!
!      subroutine parallel_cal_init
!
      module   m_parallel_var_dof
!
      use calypso_mpi
      use m_precision
!
      implicit  none
!
!>      MPI communicator for CALYPSO
      integer(kind=kint) :: SOLVER_COMM
!>      total number of processes
      integer(kind=kint) :: nprocs
! 
!>      process ID (start from 0)
      integer(kind=kint) :: my_rank
!>      error flag
      integer(kind=kint) :: ierr
!
      real(kind=kreal) :: START_TIME, END_TIME, COMMtime
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine parallel_cal_init
!
      use m_machine_parameter
!
!
      call  MPI_INIT(ierr)
      call  MPI_COMM_DUP (MPI_COMM_WORLD, SOLVER_COMM, ierr)
      call  MPI_COMM_SIZE(SOLVER_COMM, nprocs, ierr)
      call  MPI_COMM_RANK(SOLVER_COMM, my_rank  , ierr)
!
      end subroutine parallel_cal_init
!
!  ---------------------------------------------------------------------
!
      end module   m_parallel_var_dof
