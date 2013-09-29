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
      call calypso_MPI_init
!
      end subroutine parallel_cal_init
!
!  ---------------------------------------------------------------------
!
      end module   m_parallel_var_dof
