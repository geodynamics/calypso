!>@file  field_file_MPI_IO_b.f90
!!       module field_file_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_field_file_mpi_b                          &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!
!!      subroutine read_step_field_file_mpi_b                           &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!      subroutine read_alloc_stp_fld_file_mpi_b                        &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!      subroutine read_alloc_stp_fld_head_mpi_b                        &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!
!!   Data format for the merged binary field data
!!     1.   Number of process
!!     2.   Time step
!!     3.   Time, Delta t
!!     4.   Stacks of numbe of data points
!!     5.   Number of fields
!!     6.   List of number of components
!!     7.   Field names
!!     8.   List of data size (Byte)
!!     9.   All Field data
!!@endverbatim
!
      module field_file_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use m_phys_constants
      use calypso_mpi
      use m_calypso_mpi_IO
!
      use t_time_data
      use t_field_data_IO
      use t_calypso_mpi_IO_param
!
      implicit none
!
      type(calypso_MPI_IO_params), private, save :: IO_param
!
      private :: read_field_header_mpi_b
      private :: write_field_data_mpi_b
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_step_field_file_mpi_b                            &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
!
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'write binary data by MPI-IO: ', trim(file_name) 
      call open_write_mpi_file_b                                        &
     &   (file_name, num_pe, id_rank, IO_param)
!
      if(id_rank .lt. num_pe) then
        call write_field_data_mpi_b(IO_param,                           &
     &      t_IO%i_time_step, t_IO%time, t_IO%dt,                       &
     &      fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,   &
     &      fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,           &
     &      fld_IO%istack_numnod_IO)
      end if
!
      call close_mpi_file(IO_param)
!
      end subroutine write_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_step_field_file_mpi_b                             &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read binary data by MPI-IO: ', trim(file_name)
!
!
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
      call read_field_header_mpi_b(IO_param, t_IO, fld_IO)
!
      num64 = fld_IO%num_field_IO
      call mpi_read_mul_inthead_b                                       &
     &    (IO_param, num64, fld_IO%num_comp_IO)
!
      call mpi_read_mul_charahead_b                                     &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%fld_name)
!
      num64 = fld_IO%nnod_IO
      call mpi_read_2d_vector_b                                         &
     &   (IO_param, num64, fld_IO%ntot_comp_IO, fld_IO%d_IO)
!
      call dealloc_merged_field_stack(fld_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine read_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_file_mpi_b                          &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read binary data by MPI-IO: ', trim(file_name)
!
!
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
      call read_field_header_mpi_b(IO_param, t_IO, fld_IO)
!
      num64 = fld_IO%num_field_IO
      call alloc_phys_name_IO(fld_IO)
      call mpi_read_mul_inthead_b                                       &
     &    (IO_param, num64, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call mpi_read_mul_charahead_b                                     &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%fld_name)
!
      num64 = fld_IO%nnod_IO
      call mpi_read_2d_vector_b                                         &
     &   (IO_param, num64, fld_IO%ntot_comp_IO, fld_IO%d_IO)
!
      call close_mpi_file(IO_param)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. num_pe) then
        call dealloc_phys_data_IO(fld_IO)
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine read_alloc_stp_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_head_mpi_b                          &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read binary data by MPI-IO: ', trim(file_name)
!
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
      call read_field_header_mpi_b(IO_param, t_IO, fld_IO)
!
      num64 = fld_IO%num_field_IO
      call alloc_phys_name_IO(fld_IO)
      call mpi_read_mul_inthead_b                                       &
     &    (IO_param, num64, fld_IO%num_comp_IO)
!
      call mpi_read_mul_charahead_b                                     &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%fld_name)
!
      call close_mpi_file(IO_param)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. num_pe) then
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine read_alloc_stp_fld_head_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_field_data_mpi_b(IO_param_l,                     &
     &          i_time_step_IO, time_IO, delta_t_IO,                    &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged)
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind=kint), intent(in) :: i_time_step_IO
      real(kind = kreal), intent(in) :: time_IO, delta_t_IO
!
      integer(kind = kint_gl), intent(in)                               &
     &                    :: istack_merged(0:IO_param_l%nprocs_in)
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_write_process_id_b(IO_param_l)
      call mpi_write_one_inthead_b(IO_param_l, i_time_step_IO)
      call mpi_write_one_realhead_b(IO_param_l, time_IO)
      call mpi_write_one_realhead_b(IO_param_l, delta_t_IO)
!
      num64 = IO_param_l%nprocs_in
      call mpi_write_i8stack_head_b(IO_param_l, num64, istack_merged)
!
      call mpi_write_one_inthead_b(IO_param_l, num_field)
      num64 = num_field
      call mpi_write_mul_inthead_b(IO_param_l, num64, ncomp_field)
!
      call mpi_write_mul_charahead_b(IO_param_l, num_field, field_name)
!
      num64 = nnod
      call copy_istack_4_parallell_data(istack_merged, IO_param_l)
      call mpi_write_2d_vector_b(IO_param_l, num64, ntot_comp, d_nod)
!
      end subroutine write_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_header_mpi_b(IO_param_l, t_IO, fld_IO)
!
      use m_phys_constants
      use field_data_MPI_IO
      use MPI_binary_head_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_read_process_id_b(IO_param_l)
!
      call mpi_read_one_inthead_b(IO_param_l, t_IO%i_time_step)
      call mpi_read_one_realhead_b(IO_param_l, t_IO%time)
      call mpi_read_one_realhead_b(IO_param_l, t_IO%dt)
!
      call alloc_merged_field_stack(IO_param_l%nprocs_in, fld_IO)
!
      num64 = IO_param_l%nprocs_in
      call mpi_read_i8stack_head_b                                      &
     &   (IO_param_l, num64, fld_IO%istack_numnod_IO)
      call sync_field_header_mpi                                        &
     &   (IO_param_l%nprocs_in, IO_param_l%id_rank,                     &
     &    fld_IO%nnod_IO, fld_IO%istack_numnod_IO)
!
      call mpi_read_one_inthead_b(IO_param_l, fld_IO%num_field_IO)
!
      end subroutine read_field_header_mpi_b
!
! -----------------------------------------------------------------------
!
      end module field_file_MPI_IO_b
