!>@file   gz_ucd_field_MPI_IO_b.f90
!!       module gz_ucd_field_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output ucd data into gzipped binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_ucd_field_file_mpi_b(file_name, t_IO, ucd)
!!        type(time_data), intent(in) :: t_IO
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine gz_read_ucd_field_file_mpi_b                         &
!!     &         (file_name, num_pe, id_rank, t_IO, ucd)
!!      subroutine gz_rd_alloc_ucd_fld_file_mpi_b                       &
!!     &         (file_name, num_pe, id_rank, t_IO, ucd)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!
      module gz_ucd_field_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_calypso_mpi_IO
      use t_time_data
      use t_ucd_data
      use t_calypso_mpi_IO_param
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_ucd_field_file_mpi_b(file_name, t_IO, ucd)
!
      use m_error_IDs
      use MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use gz_field_block_MPI_IO_b
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'write gzipped binary data by MPI-IO: ', trim(file_name)
!
      call open_write_gz_mpi_file_b(file_name, IO_param)
!
      call gz_write_field_time_mpi_b(IO_param,                          &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
      call gz_write_field_data_mpi_b(IO_param, ucd%nnod, ucd%num_field, &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name,                   &
     &    ucd%istack_merged_nod, ucd%d_ucd)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_write_ucd_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_ucd_field_file_mpi_b                           &
     &         (file_name, num_pe, id_rank, t_IO, ucd)
!
      use field_file_MPI_IO
      use MPI_binary_head_IO
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use gz_field_block_MPI_IO_b
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read gzipped binary data by MPI-IO: ', trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_read_step_data_mpi_b(IO_param,                            &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
!
      call gz_read_field_header_mpi_b                                   &
     &   (IO_param, ucd%nnod, ucd%num_field)
!
      call gz_mpi_read_mul_inthead_b                                    &
     &   (IO_param, ucd%num_field, ucd%num_comp)
!
      call gz_mpi_read_mul_charahead_b                                  &
     &   (IO_param, ucd%num_field, ucd%phys_name)
      call gz_mpi_read_2d_vector_b                                      &
     &   (IO_param, ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_read_ucd_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_ucd_fld_file_mpi_b                         &
     &         (file_name, num_pe, id_rank, t_IO, ucd)
!
      use field_file_MPI_IO
      use MPI_binary_head_IO
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use gz_field_block_MPI_IO_b
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &      'read gzipped binary data MPI-IO: ', trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_read_step_data_mpi_b(IO_param,                            &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
!
      call gz_read_field_header_mpi_b                                   &
     &   (IO_param, ucd%nnod, ucd%num_field)
!
      call allocate_ucd_phys_name(ucd)
      call gz_mpi_read_mul_inthead_b                                    &
     &   (IO_param, ucd%num_field, ucd%num_comp)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call gz_mpi_read_mul_charahead_b                                  &
     &   (IO_param, ucd%num_field, ucd%phys_name)
      call gz_mpi_read_2d_vector_b                                      &
     &   (IO_param, ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
!
      call close_mpi_file(IO_param)
!
      if(id_rank .ge. num_pe) then
        call deallocate_ucd_phys_data(ucd)
        call deallocate_ucd_phys_name(ucd)
      end if
!
      end subroutine gz_rd_alloc_ucd_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      end module gz_ucd_field_MPI_IO_b
