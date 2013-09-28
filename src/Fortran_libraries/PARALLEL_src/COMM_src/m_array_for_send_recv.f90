!>@file   m_array_for_send_recv.f90
!!@brief      module m_array_for_send_recv
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n    Modified on Apr., 2008
!!@n    Modified on Dec., 2012
!!@n    Modified on sep., 2013
!
!>@brief Work array for data communication of FEM data
!!
!!@verbatim
!!      subroutine verify_vector_for_solver(NB, N)
!!      subroutine allocate_vector_for_solver(NB, N)
!!      subroutine deallocate_vector_for_solver
!!
!!      subroutine allocate_iccg_int_matrix(N)
!!      subroutine deallocate_iccg_int_matrix
!!@endverbatim
!!
!!@param  N    length of vector
!!@param  NB   number of vectors to solve
!
      module   m_array_for_send_recv
!
      use m_precision
      use calypso_mpi
!
      implicit  none
!
!>      Vector for solution vector
      real(kind=kreal), allocatable :: x_vec(:)
!>      Vector for right hand side vector
      real(kind=kreal), allocatable :: b_vec(:)
!>      Size of allocated vectors
      integer(kind = kint) :: isize_solver_vect = -1
!
!>      Work area for integer data
      integer(kind=kint), allocatable :: ix_vec(:)
!
      private :: isize_solver_vect
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine verify_vector_for_solver(NB, N)
!
       integer(kind = kint), intent(in) :: NB, N
       integer(kind = kint) :: ncomp
!
!
       ncomp = NB*N
       if (isize_solver_vect .lt. 0) then
         call allocate_vector_for_solver(NB,N)
       else
         if (isize_solver_vect .lt. ncomp) then
           call deallocate_vector_for_solver
           call allocate_vector_for_solver(NB,N)
         end if
       end if
!
       end subroutine verify_vector_for_solver
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_vector_for_solver(NB, N)
!
       integer(kind = kint), intent(in) :: NB, N
!
!
       allocate(x_vec(NB*N))
       allocate(b_vec(NB*N))
       isize_solver_vect = NB*N
!
       if(N*NB .gt. 0) then
         b_vec  = 0.0d00
         x_vec  =0.0d00
       end if
!
       end subroutine allocate_vector_for_solver
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_vector_for_solver
!
!
       deallocate(x_vec, b_vec)
       isize_solver_vect = 0
!
       end subroutine deallocate_vector_for_solver
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine allocate_iccg_int_matrix(N)
!
       integer(kind = kint), intent(in) :: N
!
!
       allocate(ix_vec(N))
       ix_vec  = 0
!
       end subroutine allocate_iccg_int_matrix
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_iccg_int_matrix
!
       deallocate(ix_vec)
!
       end subroutine deallocate_iccg_int_matrix
!
!  ---------------------------------------------------------------------
!
      end module   m_array_for_send_recv
