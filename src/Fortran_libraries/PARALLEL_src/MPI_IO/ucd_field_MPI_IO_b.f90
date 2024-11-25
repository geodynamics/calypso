!>@file  ucd_field_MPI_IO_b.f90
!!       module ucd_field_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_ucd_field_file_mpi_b(file_name, t_IO, ucd)
!!        type(time_data), intent(in) :: t_IO
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine read_ucd_field_file_mpi_b                            &
!!     &         (file_name, num_pe, id_rank, t_IO, ucd)
!!      subroutine read_alloc_ucd_fld_file_mpi_b                        &
!!     &         (file_name, num_pe, id_rank, t_IO, ucd)
!!      subroutine read_alloc_ucd_prm_file_mpi_b                        &
!!     &         (file_name, num_pe, id_rank, t_IO, ucd)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
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
      module ucd_field_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use m_phys_constants
      use calypso_mpi
      use m_calypso_mpi_IO
!
      use t_time_data
      use t_ucd_data
      use t_calypso_mpi_IO_param
!
      implicit none
!
      type(calypso_MPI_IO_params), private, save :: IO_param
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_ucd_field_file_mpi_b(file_name, t_IO, ucd)
!
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
      use field_block_MPI_IO_b
!
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'write binary data by MPI-IO: ', trim(file_name) 
      call open_write_mpi_file_b(file_name, IO_param)
!
      call write_field_time_mpi_b(IO_param,                             &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
      call write_field_data_mpi_b(IO_param,                             &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_nod)
!
      call close_mpi_file(IO_param)
!
      end subroutine write_ucd_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_ucd_field_file_mpi_b                              &
     &         (file_name, num_pe, id_rank, t_IO, ucd)
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
      use field_block_MPI_IO_b
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read binary data by MPI-IO: ', trim(file_name)
!
!
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
      call read_field_time_mpi_b(num_pe, IO_param, t_IO)
      call read_num_field_mpi_b                                         &
     &   (num_pe, IO_param, ucd%nnod, ucd%istack_merged_nod)
      call mpi_read_one_inthead_b(IO_param, ucd%num_field)
!
      call mpi_read_mul_inthead_b                                       &
     &    (IO_param, cast_long(ucd%num_field), ucd%num_comp)
!
      call mpi_read_mul_charahead_b                                     &
     &   (IO_param, ucd%num_field, ucd%phys_name)
!
      call mpi_read_2d_vector_b                                         &
     &   (IO_param, ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
!
      call close_mpi_file(IO_param)
!
      end subroutine read_ucd_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_ucd_fld_file_mpi_b                          &
     &         (file_name, num_pe, id_rank, t_IO, ucd)
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
      use field_block_MPI_IO_b
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read binary data by MPI-IO: ', trim(file_name)
!
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
      call read_field_time_mpi_b(num_pe, IO_param, t_IO)
      call read_num_field_mpi_b                                         &
     &   (num_pe, IO_param, ucd%nnod, ucd%istack_merged_nod)
      call mpi_read_one_inthead_b(IO_param, ucd%num_field)
!
      call allocate_ucd_phys_name(ucd)
      call mpi_read_mul_inthead_b(IO_param, cast_long(ucd%num_field),   &
     &                            ucd%num_comp)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call mpi_read_mul_charahead_b                                     &
     &   (IO_param, ucd%num_field, ucd%phys_name)
!
      call mpi_read_2d_vector_b                                         &
     &   (IO_param, ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
!
      call close_mpi_file(IO_param)
!
      if(id_rank .ge. num_pe) then
        call deallocate_ucd_phys_data(ucd)
        call deallocate_ucd_phys_name(ucd)
      end if
!
      end subroutine read_alloc_ucd_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_ucd_prm_file_mpi_b                          &
     &         (file_name, num_pe, id_rank, t_IO, ucd)
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
      use field_block_MPI_IO_b
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read binary data by MPI-IO: ', trim(file_name)
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
      call read_field_time_mpi_b(num_pe, IO_param, t_IO)
      call read_num_field_mpi_b                                         &
     &   (num_pe, IO_param, ucd%nnod, ucd%istack_merged_nod)
      call mpi_read_one_inthead_b(IO_param, ucd%num_field)
!
      call allocate_ucd_phys_name(ucd)
      call mpi_read_mul_inthead_b(IO_param, cast_long(ucd%num_field),   &
     &                            ucd%num_comp)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call mpi_read_mul_charahead_b                                     &
     &   (IO_param, ucd%num_field, ucd%phys_name)
!
      call close_mpi_file(IO_param)
!
      if(id_rank .ge. num_pe) then
        call deallocate_ucd_phys_data(ucd)
        call deallocate_ucd_phys_name(ucd)
      end if
!
      end subroutine read_alloc_ucd_prm_file_mpi_b
!
! -----------------------------------------------------------------------
!
      end module ucd_field_MPI_IO_b
