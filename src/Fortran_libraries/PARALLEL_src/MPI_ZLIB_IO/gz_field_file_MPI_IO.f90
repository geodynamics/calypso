!>@file  gz_field_file_MPI_IO.f90
!!       module gz_field_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine write_gz_step_field_file_mpi                         &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!        type(time_data), intent(in) :: t_IO
!!        type(field_IO), intent(in) :: fld_IO
!!
!!      subroutine read_step_field_file_gz_mpi                          &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!      subroutine read_alloc_stp_fld_file_gz_mpi                       &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!
!!      subroutine read_alloc_stp_fld_head_gz_mpi                       &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!
!!   Data format for the merged ascii field data
!!     1.   Number of process
!!     2.   Time step
!!     3.   Time, Delta t
!!     4.   Stacks of numbe of data points
!!     5.   Number of fields
!!     6.   List of number of components
!!     7.   Each field data  (Itarate 7.1 - 7.3)
!!      7.1   Field name
!!      7.2   List of data size (Byte, after compressed)
!!      7.3   Field data
!!@endverbatim
!
      module gz_field_file_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_time_data
      use t_field_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_gz_step_field_file_mpi                           &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use gz_field_block_MPI_IO
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
!
      integer, intent(in) :: num_pe, id_rank
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Write gzipped ascii data by MPI-IO: ', trim(file_name)
      call calypso_mpi_write_file_open(file_name, num_pe, id_fld)
!
      if(id_rank .lt. num_pe) then
        ioff_gl = 0
        call write_field_head_gz_mpi                                    &
     &     (id_fld, num_pe, ioff_gl, t_IO, fld_IO%num_field_IO,         &
     &      fld_IO%num_comp_IO,  fld_IO%istack_numnod_IO)
!
        call write_field_data_gz_mpi                                    &
     &     (id_fld, ioff_gl, cast_long(fld_IO%nnod_IO),                 &
     &      fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                   &
     &      fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
      end if
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine write_gz_step_field_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_step_field_file_gz_mpi                            &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use gz_field_data_IO
      use gz_field_block_MPI_IO
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl, num64
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Read gzipped ascii data by MPI-IO: ', trim(file_name)
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_field_step_gz_mpi(id_fld, num_pe, ioff_gl, t_IO)
!
      call read_field_header_gz_mpi                                     &
     &   (id_fld, num_pe, id_rank, ioff_gl, num64,                      &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
      fld_IO%nnod_IO = int(num64,KIND(fld_IO%nnod_IO))
!
      call read_field_num_gz_mpi                                        &
     &   (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_data_gz_mpi                                       &
     &   (id_fld, num_pe, id_rank, ioff_gl, cast_long(fld_IO%nnod_IO),  &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call calypso_close_mpi_file(id_fld)
!
      call dealloc_merged_field_stack(fld_IO)
!
      end subroutine read_step_field_file_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_file_gz_mpi                         &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use gz_field_data_IO
      use gz_field_block_MPI_IO
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer ::  id_fld
      integer(kind = kint_gl) :: ioff_gl, num64
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Read gzipped ascii data by MPI-IO: ', trim(file_name)
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_field_step_gz_mpi(id_fld, num_pe, ioff_gl, t_IO)
!
      call alloc_merged_field_stack(num_pe, fld_IO)
!
      call read_field_header_gz_mpi                                     &
     &   (id_fld, num_pe, id_rank, ioff_gl, num64,                      &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
      fld_IO%nnod_IO = int(num64,KIND(fld_IO%nnod_IO))
!
      call alloc_phys_name_IO(fld_IO)
!
      call read_field_num_gz_mpi                                        &
     &   (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data_gz_mpi                                       &
     &   (id_fld, num_pe, id_rank, ioff_gl, cast_long(fld_IO%nnod_IO),  &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call calypso_close_mpi_file(id_fld)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. num_pe) then
        call dealloc_phys_data_IO(fld_IO)
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine read_alloc_stp_fld_file_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_head_gz_mpi                         &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use gz_field_data_IO
      use gz_field_block_MPI_IO
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer ::  id_fld
      integer(kind = kint_gl) :: ioff_gl, num64
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Read gzipped ascii data by MPI-IO: ', trim(file_name)
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_field_step_gz_mpi(id_fld, num_pe, ioff_gl, t_IO)
!
      call alloc_merged_field_stack(num_pe, fld_IO)
!
      call read_field_header_gz_mpi                                     &
     &   (id_fld, num_pe, id_rank, ioff_gl, num64,                      &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
      fld_IO%nnod_IO = int(num64,KIND(fld_IO%nnod_IO))
!
      call alloc_phys_name_IO(fld_IO)
!
      call read_field_num_gz_mpi                                        &
     &   (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_names_gz_mpi(id_fld, num_pe, id_rank, ioff_gl,    &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
!
      call calypso_close_mpi_file(id_fld)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. num_pe) call dealloc_phys_name_IO(fld_IO)
!
      end subroutine read_alloc_stp_fld_head_gz_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_field_file_MPI_IO
