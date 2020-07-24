!>@file   t_calypso_mpi_IO_param.f90
!!@brief  module t_calypso_mpi_IO_param
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!> @brief Base parameter structure for MPI-IO
!!
!!@verbatim
!!      subroutine alloc_istack_merge(id_rank, nprocs_in, IO_param)
!!      subroutine dealloc_istack_merge(IO_param)
!!
!!      integer(kind = kint) function rank_in_multi_domain(iloop)
!!      integer(kind = kint) function num_loop_4_multi_domain(nprocs_in)
!!      subroutine copy_istack_4_parallell_data(istack8, IO_param)
!!      subroutine mul_istack_4_parallell_vect(nvect, IO_param)
!!      subroutine set_numbers_2_head_node(num_local, IO_param)
!!
!!      subroutine istack64_4_parallel_data(num_local, IO_param)
!!      subroutine set_istack_over_subdomains                           &
!!     &         (nprocs_in, nloop, num_local, istack_merged)
!!      subroutine set_istack_4_fixed_num(num_local, IO_param)
!!@endverbatim
!
      module t_calypso_mpi_IO_param
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
!
!>      Structure for parameters of MPI-IO
      type calypso_MPI_IO_params
!>        File ID for MPI-IO
        integer ::  id_file
!>        Byte swap flag for binary data
        integer ::  iflag_bin_swap = -1
!>        process ID for MPI-IO
        integer ::  id_rank
!>        number of subdomains (not equal to number of processes)
        integer ::  nprocs_in
!>        maximum number of loops for subdomains in one process
        integer(kind=kint) ::  nloop = 1
!
!>        global file IO point
        integer(kind = kint_gl) :: ioff_gl
!
!>        Stack of data lengh in each domain
        integer(kind = kint_gl), allocatable :: istack_merged(:)
!>        Local number of data
        integer(kind = kint), allocatable :: num_lc(:)
!>        global number of data
        integer(kind = kint), allocatable :: num_gl(:)
      end type calypso_MPI_IO_params
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_istack_merge(id_rank, nprocs_in, IO_param)
!
      integer, intent(in) :: nprocs_in, id_rank
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      IO_param%id_rank =   id_rank
      IO_param%nprocs_in = nprocs_in
      IO_param%nloop = (IO_param%nprocs_in - 1) / nprocs

      if( (IO_param%nloop*nprocs + my_rank) .lt. nprocs_in) then
        IO_param%nloop = IO_param%nloop + 1
      end if
!      if(i_debug .gt. 0) write(*,*) 'IO_param%nloop',                  &
!     &                                   my_rank, IO_param%nloop
!
      allocate(IO_param%num_lc(IO_param%nprocs_in))
      allocate(IO_param%num_gl(IO_param%nprocs_in))
      allocate(IO_param%istack_merged(0:IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      if(IO_param%nprocs_in .gt. 0) then
!$omp parallel workshare
        IO_param%istack_merged(1:IO_param%nprocs_in) = 0
        IO_param%num_lc(1:IO_param%nprocs_in) = 0
        IO_param%num_gl(1:IO_param%nprocs_in) = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_istack_merge
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_istack_merge(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      deallocate(IO_param%istack_merged)
      deallocate(IO_param%num_lc, IO_param%num_gl)
!
      end subroutine dealloc_istack_merge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function rank_in_multi_domain(iloop)
!
      integer(kind = kint), intent(in) :: iloop
!
      rank_in_multi_domain = my_rank + (iloop - 1) * nprocs
!
      end function rank_in_multi_domain
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_loop_4_multi_domain(nprocs_in)
!
      integer, intent(in) :: nprocs_in
      integer(kind = kint) :: id_rank, nloop
!
      nloop = (nprocs_in - 1) / nprocs
      id_rank = rank_in_multi_domain(nloop+1)
      if(id_rank .lt. nprocs_in) nloop = nloop + 1
!
      num_loop_4_multi_domain = nloop
!
      end function num_loop_4_multi_domain
!
!  ---------------------------------------------------------------------
!
      subroutine copy_istack_4_parallell_data(istack8, IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in)                               &
     &                        :: istack8(0:IO_param%nprocs_in)
!
!
!$omp parallel workshare
      IO_param%istack_merged(0:IO_param%nprocs_in)                      &
     &      = istack8(0:IO_param%nprocs_in)
!$omp end parallel workshare
!
      end subroutine copy_istack_4_parallell_data
!
!  ---------------------------------------------------------------------
!
      subroutine mul_istack_4_parallell_vect(nvect, IO_param)
!
      integer(kind = kint), intent(in)  :: nvect
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
!$omp parallel workshare
      IO_param%istack_merged(0:IO_param%nprocs_in)                      &
     &      = nvect * IO_param%istack_merged(0:IO_param%nprocs_in)
!$omp end parallel workshare
!
      end subroutine mul_istack_4_parallell_vect
!
!  ---------------------------------------------------------------------
!
      subroutine set_numbers_2_head_node(num_local, IO_param)
!
      use calypso_mpi_int
!
      integer(kind = kint), intent(in) :: num_local
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: num_global(nprocs)
!
!
      call MPI_Allgather(num_local, 1, CALYPSO_INTEGER,                 &
     &    num_global, 1, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
      IO_param%istack_merged(1:nprocs) = num_global(1:nprocs)
!
      end subroutine set_numbers_2_head_node
!
!  ---------------------------------------------------------------------
!
      subroutine istack64_4_parallel_data(num_local, IO_param)
!
      use calypso_mpi_int
!
      integer(kind = kint_gl), intent(in) :: num_local
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint_gl) :: num_global(nprocs)
      integer(kind = kint) :: ip
!
!
      call MPI_Allgather(num_local, 1, CALYPSO_GLOBAL_INT,              &
     &    num_global, 1, CALYPSO_GLOBAL_INT, CALYPSO_COMM,              &
     &    ierr_MPI)
!
      IO_param%istack_merged(0) = 0
      do ip = 1, IO_param%nprocs_in
        IO_param%istack_merged(ip) = IO_param%istack_merged(ip-1)       &
     &                              + num_global(ip)
      end do
!
      end subroutine istack64_4_parallel_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_over_subdomains                             &
     &         (nprocs_in, nloop, num_local, istack_merged)
!
      use calypso_mpi_int8
!
      integer, intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: nloop
      integer(kind = kint_gl), intent(in) :: num_local(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint_gl) :: num_lc(0:nprocs_in)
      integer(kind = kint_gl) :: num_gl(0:nprocs_in)
      integer(kind = kint) :: iloop, ip
!
!
!$omp parallel workshare
      num_lc(1:nprocs_in) = 0
      num_gl(1:nprocs_in) = 0
!$omp end parallel workshare
!
      do iloop = 1, nloop
        ip = 1 + my_rank + (iloop - 1) * nprocs
        num_lc(ip) = num_local(iloop)
      end do
!
      num64 = int(nprocs_in,KIND(num64))
      call calypso_mpi_allreduce_int8(num_lc, num_gl, num64, MPI_SUM)
!
      istack_merged(0) = 0
      do ip = 1, nprocs_in
        istack_merged(ip) = istack_merged(ip-1) + num_gl(ip)
      end do
!
      end subroutine set_istack_over_subdomains
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_4_fixed_num(num_local, IO_param)
!
      integer(kind = kint), intent(in) :: num_local
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: ip
!
!
      do ip = 0, IO_param%nprocs_in
        IO_param%istack_merged(ip) = ip * num_local
      end do
!
      end subroutine set_istack_4_fixed_num
!
! -----------------------------------------------------------------------
!
      end module t_calypso_mpi_IO_param
