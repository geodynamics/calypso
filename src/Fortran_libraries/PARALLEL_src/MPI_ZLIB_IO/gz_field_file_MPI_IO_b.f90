!>@file  gz_field_file_MPI_IO_b.f90
!!       module gz_field_file_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_step_fld_file_mpi_b                         &
!!     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!!
!!      subroutine gz_write_field_head_mpi_b                            &
!!     &         (IO_param_l, i_time_step_IO, time_IO, delta_t_IO,      &
!!     &         num_field, ncomp_field, istack_merged)
!!      subroutine gz_write_field_data_mpi_b(IO_param_l,                &
!!     &          nnod, num_field, ntot_comp, field_name, d_nod)
!!
!!      subroutine gz_read_step_field_file_mpi_b                        &
!!     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!!      subroutine gz_rd_alloc_st_fld_file_mpi_b                        &
!!     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!!
!!      subroutine gz_rd_alloc_st_fld_head_mpi_b                        &
!!     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!!@endverbatim
!
      module gz_field_file_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_calypso_mpi_IO
      use t_time_data
      use t_field_data_IO
      use t_calypso_mpi_IO_param
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
!
      private :: gz_write_field_data_mpi_b
      private :: gz_read_step_data_mpi_b, gz_read_field_header_mpi_b
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_step_fld_file_mpi_b                           &
     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      use m_error_IDs
      use MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
!
!
      if(nprocs .ne. nprocs_in)  call calypso_mpi_abort                 &
     &                (ierr_fld, 'gzipped data output does not work')
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'write gzipped binary data by MPI-IO: ', trim(file_name)
!
      call open_write_gz_mpi_file_b                                     &
     &   (file_name, nprocs_in, id_rank, IO_param)
!
      call gz_write_field_head_mpi_b(IO_param,                          &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt,                         &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO,                      &
     &    fld_IO%istack_numnod_IO)
      call gz_write_field_data_mpi_b                                    &
     &   (IO_param, fld_IO%nnod_IO, fld_IO%num_field_IO,                &
     &    fld_IO%ntot_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_write_step_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_field_file_mpi_b                          &
     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      use field_file_MPI_IO
      use MPI_binary_head_IO
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read gzipped binary data by MPI-IO: ', trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, nprocs_in, id_rank, IO_param)
!
      call gz_read_step_data_mpi_b(IO_param,                            &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
      call gz_read_field_header_mpi_b                                   &
     &   (IO_param, fld_IO%nnod_IO, fld_IO%num_field_IO)
!
      call gz_mpi_read_mul_inthead_b                                    &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      call gz_mpi_read_mul_charahead_b                                  &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%fld_name)
      call gz_mpi_read_2d_vector_b                                      &
     &   (IO_param, fld_IO%nnod_IO, fld_IO%ntot_comp_IO, fld_IO%d_IO)
!
      call close_mpi_file(IO_param)
!
      call dealloc_merged_field_stack(fld_IO)
!
      end subroutine gz_read_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_file_mpi_b                          &
     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      use field_file_MPI_IO
      use MPI_binary_head_IO
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &      'read gzipped binary data MPI-IO: ', trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, nprocs_in, id_rank, IO_param)
!
      call gz_read_step_data_mpi_b(IO_param,                            &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
      call gz_read_field_header_mpi_b                                   &
     &   (IO_param, fld_IO%nnod_IO, fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call gz_mpi_read_mul_inthead_b                                    &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call gz_mpi_read_mul_charahead_b                                  &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%fld_name)
      call gz_mpi_read_2d_vector_b                                      &
     &   (IO_param, fld_IO%nnod_IO, fld_IO%ntot_comp_IO, fld_IO%d_IO)
!
      call close_mpi_file(IO_param)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. nprocs_in) then
        call dealloc_phys_data_IO(fld_IO)
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine gz_rd_alloc_st_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_head_mpi_b                          &
     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      use field_file_MPI_IO
      use MPI_binary_head_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read gzipped binary data by MPI-IO: ', trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, nprocs_in, id_rank, IO_param)
!
      call gz_read_step_data_mpi_b(IO_param,                            &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
      call gz_read_field_header_mpi_b                                   &
     &   (IO_param, fld_IO%nnod_IO, fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call gz_mpi_read_mul_inthead_b                                    &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      call gz_mpi_read_mul_charahead_b                                  &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%fld_name)
!
      call close_mpi_file(IO_param)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. nprocs_in) call dealloc_phys_name_IO(fld_IO)
!
      end subroutine gz_rd_alloc_st_fld_head_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_field_head_mpi_b                              &
     &         (IO_param_l, i_time_step_IO, time_IO, delta_t_IO,        &
     &         num_field, ncomp_field, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind=kint), intent(in) :: i_time_step_IO
      real(kind = kreal), intent(in) :: time_IO, delta_t_IO
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
!
!
      call gz_mpi_write_one_inthead_b(IO_param_l, IO_param_l%nprocs_in)
      call gz_mpi_write_one_inthead_b(IO_param_l, i_time_step_IO)
!
      call gz_mpi_write_one_realhead_b(IO_param_l, time_IO)
      call gz_mpi_write_one_realhead_b(IO_param_l, delta_t_IO)
!
!
      call gz_mpi_write_i8stack_head_b                                  &
     &   (IO_param_l, IO_param_l%nprocs_in, istack_merged)
      call gz_mpi_write_one_inthead_b(IO_param_l, num_field)
      call gz_mpi_write_mul_inthead_b                                   &
     &   (IO_param_l, num_field, ncomp_field)
!
      end subroutine gz_write_field_head_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_field_data_mpi_b(IO_param_l,                  &
     &          nnod, num_field, ntot_comp, field_name, d_nod)
!
      use m_phys_constants
      use field_data_IO
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
!
      call gz_mpi_write_mul_charahead_b                                 &
     &   (IO_param_l, num_field, field_name)
      call gz_mpi_write_2d_vector_b(IO_param_l, nnod, ntot_comp, d_nod)
!
      end subroutine gz_write_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_data_mpi_b                                &
     &         (IO_param_l, i_time_step_IO, time_IO, delta_t_IO)
!
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind=kint), intent(inout) :: i_time_step_IO
      real(kind = kreal), intent(inout) :: time_IO, delta_t_IO
!
      integer(kind=kint) :: int_tmp
!
!
      call gz_mpi_read_one_inthead_b(IO_param_l, int_tmp)
      call gz_mpi_read_one_inthead_b(IO_param_l, i_time_step_IO)
!
      call gz_mpi_read_one_realhead_b(IO_param_l, time_IO)
      call gz_mpi_read_one_realhead_b(IO_param_l, delta_t_IO)
!
      end subroutine gz_read_step_data_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_field_header_mpi_b(IO_param_l, nnod, num_field)
!
      use m_phys_constants
      use field_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
      integer(kind=kint), intent(inout) :: nnod, num_field
!
!
      call gz_mpi_read_i8stack_head_b                                   &
     &   (IO_param_l, IO_param_l%nprocs_in, IO_param_l%istack_merged)
      nnod = int(IO_param_l%istack_merged(IO_param_l%id_rank+1)         &
     &         - IO_param_l%istack_merged(IO_param_l%id_rank))
!
      call gz_mpi_read_one_inthead_b(IO_param_l, num_field)
!
      end subroutine gz_read_field_header_mpi_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_file_MPI_IO_b
