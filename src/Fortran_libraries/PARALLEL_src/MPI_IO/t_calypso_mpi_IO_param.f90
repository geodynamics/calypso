!>@file   t_calypso_mpi_IO_param.f90
!!@brief  module t_calypso_mpi_IO_param
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!> @brief Base parameter structure for MPI-IO
!!
!!@verbatim
!!      subroutine alloc_istack_merge(id_rank_IO, nprocs_IO, IO_param)
!!      subroutine dealloc_istack_merge(IO_param)
!!
!!      subroutine mpi_write_chara_array_mul(id_file, nprocs_in, nloop, &
!!     &          ioff_gl, istack_merged, c_array)
!!      subroutine mpi_read_chara_array_mul(id_file, nprocs_in, nloop,  &
!!     &          ioff_gl, istack_merged, c_array)
!!      subroutine set_istack_by_chara_length                           &
!!     &         (nprocs_in, nloop, c_array, istack_merged)
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
!>      Structure for real array for MPI-IO
      type realarray_IO
        integer(kind = kint) :: num
        real(kind = kreal), allocatable :: r_IO(:)
      end type realarray_IO
!
!>      Structure for 2D vectr array for MPI-IO
      type vectarray_IO
        integer(kind = kint) :: n1
        integer(kind = kint) :: n2
        real(kind = kreal), allocatable :: v_IO(:,:)
      end type vectarray_IO
!
!>      Structure for integer array for MPI-IO
      type intarray_IO
        integer(kind = kint) :: num
        integer(kind = kint), allocatable :: i_IO(:)
      end type intarray_IO
!
!>      Structure for integer vector array for MPI-IO
      type ivecarray_IO
        integer(kind = kint) :: n1
        integer(kind = kint) :: n2
        integer(kind = kint), allocatable :: iv_IO(:,:)
      end type ivecarray_IO
!
!>      Structure for 8-byte integer array for MPI-IO
      type int8array_IO
        integer(kind = kint) :: num
        integer(kind = kint_gl), allocatable :: i8_IO(:)
      end type int8array_IO
!
!>      Structure for 8-byte integer array for MPI-IO
      type charaarray_IO
        integer(kind = kint) :: num
        character(len = 1), allocatable :: c_IO(:)
      end type charaarray_IO
!
!>      Structure for parameters of MPI-IO
      type calypso_MPI_IO_params
!>        File ID for MPI-IO
        integer ::  id_file
!>        process ID for MPI-IO
        integer(kind=kint) ::  id_rank
!>        number of subdomains (not equal to number of processes)
        integer(kind=kint) ::  nprocs_in
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
!
!>        Structure for real array for MPI-IO
        type(realarray_IO), allocatable ::  r_array(:)
!>        Structure for real array for MPI-IO
        type(vectarray_IO), allocatable ::  v_array(:)
!>        Structure for real array for MPI-IO
        type(intarray_IO), allocatable ::   i_array(:)
!>        Structure for real array for MPI-IO
        type(ivecarray_IO), allocatable ::  iv_array(:)
!>        Structure for real array for MPI-IO
        type(int8array_IO), allocatable ::  i8_array(:)
!>        Structure for real array for MPI-IO
        type(charaarray_IO), allocatable :: c_array(:)
      end type calypso_MPI_IO_params
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_istack_merge(id_rank_IO, nprocs_IO, IO_param)
!
      integer(kind = kint), intent(in) :: nprocs_IO, id_rank_IO
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      IO_param%id_rank =   id_rank_IO
      IO_param%nprocs_in = nprocs_IO
      IO_param%nloop = (IO_param%nprocs_in - 1) / nprocs

      if( (IO_param%nloop*nprocs + my_rank) .lt. nprocs_IO) then
        IO_param%nloop = IO_param%nloop + 1
      end if
      if(i_debug .gt. 0) write(*,*) 'IO_param%nloop',                   &
     &                                   my_rank, IO_param%nloop
!
      allocate(IO_param%r_array(IO_param%nloop))
      allocate(IO_param%v_array(IO_param%nloop))
      allocate(IO_param%i_array(IO_param%nloop))
      allocate(IO_param%iv_array(IO_param%nloop))
      allocate(IO_param%i8_array(IO_param%nloop))
      allocate(IO_param%c_array(IO_param%nloop))
!
      allocate(IO_param%num_lc(IO_param%nprocs_in))
      allocate(IO_param%num_gl(IO_param%nprocs_in))
      allocate(IO_param%istack_merged(0:IO_param%nprocs_in))
!
      IO_param%istack_merged = 0
      if(IO_param%nprocs_in .gt. 0) then
!$omp parallel workshare
        IO_param%num_lc = 0
        IO_param%num_gl = 0
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
      deallocate(IO_param%r_array, IO_param%v_array)
      deallocate(IO_param%c_array, IO_param%iv_array)
      deallocate(IO_param%i_array, IO_param%i8_array)
      deallocate(IO_param%istack_merged)
      deallocate(IO_param%num_lc, IO_param%num_gl)
!
      end subroutine dealloc_istack_merge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_character_buffers(nloop, c_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(charaarray_IO), intent(inout) ::  c_array(nloop)
!
      integer(kind = kint) :: iloop
!
!
      do iloop = 1, nloop
        deallocate(c_array(iloop)%c_IO)
      end do
!
      end subroutine dealloc_character_buffers
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_integer_buffers(nloop, i_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(intarray_IO),  intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: iloop
!
!
      do iloop = 1, nloop
        deallocate(i_array(iloop)%I_IO)
      end do
!
      end subroutine dealloc_integer_buffers
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
      integer(kind = kint), intent(in) :: nprocs_in
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
      integer(kind = kint), intent(in) :: num_local
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: num_global(nprocs)
!
!
      call MPI_Allgather(num_local, ione, CALYPSO_INTEGER,              &
     &    num_global, ione, CALYPSO_INTEGER, CALYPSO_COMM,              &
     &    ierr_MPI)
      IO_param%istack_merged(1:nprocs) = num_global(1:nprocs)
!
      end subroutine set_numbers_2_head_node
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_4_parallell_data(num_local, IO_param)
!
      integer(kind = kint), intent(in) :: num_local
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: num_global(nprocs)
      integer(kind = kint) :: ip
!
!
      call MPI_Allgather(num_local, ione, CALYPSO_INTEGER,              &
     &    num_global, ione, CALYPSO_INTEGER, CALYPSO_COMM,              &
     &    ierr_MPI)
!
      IO_param%istack_merged(0) = 0
      do ip = 1, IO_param%nprocs_in
        IO_param%istack_merged(ip) = IO_param%istack_merged(ip-1)       &
     &                              + num_global(ip)
      end do
!
      end subroutine set_istack_4_parallell_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_over_subdomains                             &
     &         (nprocs_in, nloop, num_local, istack_merged)
!
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint), intent(in) :: num_local(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint) :: num_lc(0:nprocs_in)
      integer(kind = kint) :: num_gl(0:nprocs_in)
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
      call MPI_allREDUCE(num_lc, num_gl, nprocs_in,                     &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
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
! -----------------------------------------------------------------------
!
      subroutine set_istack_by_chara_length                             &
     &         (nprocs_in, nloop, c_array, istack_merged)
!
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      type(charaarray_IO), intent(inout) ::  c_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint) :: num_local(nloop)
!
!
      num_local(1:nloop) = c_array(1:nloop)%num
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_chara_length
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_by_i8_buffer                                &
     &         (nprocs_in, nloop, i8_array, istack_merged)
!
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      type(int8array_IO), intent(inout) ::  i8_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint) :: num_local(nloop)
!
!
      num_local(1:nloop) = i8_array(1:nloop)%num * kint_gl
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_i8_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_by_int_buffer                               &
     &         (nprocs_in, nloop, i_array, istack_merged)
!
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      type(intarray_IO), intent(inout) ::  i_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint) :: num_local(nloop)
!
!
      num_local(1:nloop) = i_array(1:nloop)%num * kint
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_int_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_by_int2d_buffer                             &
     &         (nprocs_in, nloop, iv_array, istack_merged)
!
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      type(ivecarray_IO), intent(inout) ::  iv_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint) :: num_local(nloop)
!
!
      num_local(1:nloop) = iv_array(1:nloop)%n1                         &
     &                    * iv_array(1:nloop)%n2 *kint
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_int2d_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_by_real_buffer                              &
     &         (nprocs_in, nloop, r_array, istack_merged)
!
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      type(realarray_IO), intent(inout) ::  r_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint) :: num_local(nloop)
!
!
      num_local(1:nloop) = r_array(1:nloop)%num * kreal
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_real_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_by_vector_buffer                            &
     &         (nprocs_in, nloop, v_array, istack_merged)
!
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      type(vectarray_IO), intent(inout) ::  v_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint) :: num_local(nloop)
!
!
      num_local(1:nloop) = v_array(1:nloop)%n1                          &
     &                    * v_array(1:nloop)%n2 * kreal
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_vector_buffer
!
!  ---------------------------------------------------------------------
!
      end module t_calypso_mpi_IO_param
