!>@file   t_vector_for_solver.f90
!!@brief  module t_vector_for_solver
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Dec., 2008
!
!>@brief  Work vector for FEM data communication
!!
!!@verbatim
!! --------------------------------------------------------
!!
!!      subroutine copy_communicator_4_solver(S_COMM)
!!        type(mpi_4_solver), intent(inout) :: S_COMM
!!      subroutine verify_iccgN_vec_type(NB, nnod, v_sol)
!!         integer(kind = kint), intent(in) :: NB, nnod
!!         type(vectors_4_solver), intent(inout) :: v_sol
!!      subroutine alloc_iccgN_vec_type(NB, nnod, v_sol)
!!      subroutine dealloc_iccgN_vec_type(v_sol)
!!         type(vectors_4_solver), intent(inout) :: v_sol
!!
!!      subroutine alloc_iccg_int_vector(nnod, v_sol)
!!      subroutine dealloc_iccg_int_vector(v_sol)
!!         type(vectors_4_solver), intent(inout) :: v_sol
!!
!!       subroutine alloc_iccg_int8_vector(nnod, v_sol)
!!       subroutine dealloc_iccg_int8_vector(v_sol)
!!         type(vectors_4_solver), intent(inout) :: v_sol
!!
!! --------------------------------------------------------
!!@endverbatim
!
      module t_vector_for_solver
!
      use m_precision
!
      implicit  none
!
!
!>      Structure for communicatiors for solver
      type mpi_4_solver
!>        Communicator for each level
        integer :: SOLVER_COMM
!>        MPI rank for each level
        integer :: MG_rank
!>        Total process count (1 to petot)
        integer :: nprocs
!>        Lavel for communicator
        integer(kind=kint) :: icolor_MG
      end type mpi_4_solver
!
!>      Structure for vectors for solver
      type vectors_4_solver
!>        Vector for solution vector
        real(kind=kreal), allocatable :: x_vec(:)
!>        Vector for right hand side vector
        real(kind=kreal), allocatable :: b_vec(:)
!>        Size of allocated vectors
        integer(kind = kint) :: isize_solver_vect = -1
! 
        integer(kind=kint), allocatable :: ix_vec(:)
! 
        integer(kind=kint_gl), allocatable :: i8x_vec(:)
      end type vectors_4_solver
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_communicator_4_solver(S_COMM)
!
      use calypso_mpi
!
      type(mpi_4_solver), intent(inout) :: S_COMM
!
!
      S_COMM%icolor_MG = 0
!
      call MPI_COMM_DUP(CALYPSO_COMM, S_COMM%SOLVER_COMM, ierr_MPI)
      call MPI_COMM_RANK(S_COMM%SOLVER_COMM, S_COMM%MG_rank, ierr_MPI)
      S_COMM%nprocs =  nprocs
!
      end subroutine copy_communicator_4_solver
!
!  ---------------------------------------------------------------------
!
      subroutine verify_iccgN_vec_type(NB, nnod, v_sol)
!
      integer(kind = kint), intent(in) :: NB, nnod
      type(vectors_4_solver), intent(inout) :: v_sol
      integer(kind = kint) :: ncomp
!
!
      ncomp = NB*nnod
      if (v_sol%isize_solver_vect .lt. 0) then
        call alloc_iccgN_vec_type(NB, nnod, v_sol)
      else
        if (v_sol%isize_solver_vect .lt. ncomp) then
          call dealloc_iccgN_vec_type(v_sol)
          call alloc_iccgN_vec_type(NB,nnod, v_sol)
        end if
      end if
!
      end subroutine verify_iccgN_vec_type
!
!  ---------------------------------------------------------------------
!
       subroutine alloc_iccgN_vec_type(NB, nnod, v_sol)
!
       integer(kind = kint), intent(in) :: NB, nnod
       type(vectors_4_solver), intent(inout) :: v_sol
!
!
       if(allocated(v_sol%x_vec)) return
!
       allocate(v_sol%x_vec(NB*nnod))
       allocate(v_sol%b_vec(NB*nnod))
!
       if(nnod .gt. 0) then
!$omp parallel workshare
         v_sol%x_vec(1:NB*nnod) = 0.0d00
         v_sol%b_vec(1:NB*nnod) = 0.0d00
!$omp end parallel workshare
       end if
!
       v_sol%isize_solver_vect = NB*nnod
!
       end subroutine alloc_iccgN_vec_type
!
!  ---------------------------------------------------------------------
!
       subroutine dealloc_iccgN_vec_type(v_sol)
!
       type(vectors_4_solver), intent(inout) :: v_sol
!
!
       if(allocated(v_sol%x_vec) .eqv. .FALSE.) return
       deallocate(v_sol%x_vec, v_sol%b_vec)
       v_sol%isize_solver_vect = 0
!
       end subroutine dealloc_iccgN_vec_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine alloc_iccg_int_vector(nnod, v_sol)
!
       integer(kind = kint), intent(in) :: nnod
       type(vectors_4_solver), intent(inout) :: v_sol
!
!
       if(allocated(v_sol%ix_vec)) return
       allocate(v_sol%ix_vec(nnod))
       if(nnod .gt. 0) then
!$omp parallel workshare
         v_sol%ix_vec(1:nnod) = 0
!$omp end parallel workshare
       end if
!
       end subroutine alloc_iccg_int_vector
!
!  ---------------------------------------------------------------------
!
       subroutine dealloc_iccg_int_vector(v_sol)
!
       type(vectors_4_solver), intent(inout) :: v_sol
!
       if(allocated(v_sol%ix_vec) .eqv. .FALSE.) return
       deallocate(v_sol%ix_vec)
!
       end subroutine dealloc_iccg_int_vector
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine alloc_iccg_int8_vector(nnod, v_sol)
!
       integer(kind = kint), intent(in) :: nnod
       type(vectors_4_solver), intent(inout) :: v_sol
!
!
       if(allocated(v_sol%i8x_vec)) return
       allocate(v_sol%i8x_vec(nnod))
       if(nnod .gt. 0) then
!$omp parallel workshare
         v_sol%i8x_vec(1:nnod)  = 0
!$omp end parallel workshare
       end if
!
       end subroutine alloc_iccg_int8_vector
!
!  ---------------------------------------------------------------------
!
       subroutine dealloc_iccg_int8_vector(v_sol)
!
       type(vectors_4_solver), intent(inout) :: v_sol
!
       if(allocated(v_sol%i8x_vec) .eqv. .FALSE.) return
       deallocate(v_sol%i8x_vec)
!
       end subroutine dealloc_iccg_int8_vector
!
!  ---------------------------------------------------------------------
!
      end module   t_vector_for_solver
