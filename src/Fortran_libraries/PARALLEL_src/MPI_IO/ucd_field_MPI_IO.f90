!>@file  ucd_field_MPI_IO.f90
!!       module ucd_field_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_ucd_field_file_mpi                             &
!!     &         (file_name, num_pe, id_rank, t_IO, ucd)
!!        type(time_data), intent(in) :: t_IO
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine read_ucd_field_file_mpi                              &
!!     &         (file_name, num_pe, id_rank, t_IO, ucd)
!!      subroutine read_alloc_ucd_fld_file_mpi                          &
!!     &         (file_name, num_pe, id_rank, t_IO, ucd)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
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
!!      7.2   List of data size (Byte)
!!      7.3   Field data
!!@endverbatim
!
      module ucd_field_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_time_data
      use t_ucd_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_ucd_field_file_mpi                               &
     &         (file_name, num_pe, id_rank, t_IO, ucd)
!
      use field_block_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
!
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*) 'Write ascii data by MPI-IO: ',     &
     &                               trim(file_name)
      call calypso_mpi_write_file_open(file_name, num_pe, id_fld)
!
      if(id_rank .lt. num_pe) then
        ioff_gl = 0
        call write_field_data_mpi                                       &
     &     (id_fld, num_pe, id_rank, ioff_gl, t_IO,                     &
     &      ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,       &
     &      ucd%phys_name, ucd%d_ucd, ucd%istack_merged_nod)
      end if
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine write_ucd_field_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_ucd_field_file_mpi                                &
     &         (file_name, num_pe, id_rank, t_IO, ucd)
!
      use field_data_MPI_IO
      use field_block_MPI_IO
!
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*) 'Read ascii data by MPI-IO: ',      &
     &                               trim(file_name)
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_field_time_mpi(id_fld, num_pe, ioff_gl, t_IO)
!
      call read_field_header_mpi(id_fld, num_pe, id_rank,               &
     &    ioff_gl, ucd%nnod, ucd%num_field, ucd%istack_merged_nod)
!
      call read_field_num_mpi                                           &
     &   (id_fld, ioff_gl, ucd%num_field, ucd%num_comp)
!
      call read_field_data_mpi(id_fld, num_pe, id_rank, ioff_gl,        &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_nod)
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine read_ucd_field_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_ucd_fld_file_mpi                            &
     &         (file_name, num_pe, id_rank, t_IO, ucd)
!
      use field_data_MPI_IO
      use field_block_MPI_IO
!
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*) 'Read ascii data by MPI-IO: ',      &
     &                               trim(file_name)
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_field_time_mpi(id_fld, num_pe, ioff_gl, t_IO)
!
      call read_field_header_mpi(id_fld, num_pe, id_rank,               &
     &    ioff_gl, ucd%nnod, ucd%num_field, ucd%istack_merged_nod)
!
      call allocate_ucd_phys_name(ucd)
      call read_field_num_mpi                                           &
     &   (id_fld, ioff_gl, ucd%num_field, ucd%num_comp)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_field_data_mpi(id_fld, num_pe, id_rank, ioff_gl,        &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_nod)
!
      call calypso_close_mpi_file(id_fld)
!
      if(id_rank .ge. num_pe) then
        call deallocate_ucd_phys_data(ucd)
        call deallocate_ucd_phys_name(ucd)
      end if
!
      end subroutine read_alloc_ucd_fld_file_mpi
!
! -----------------------------------------------------------------------
!
      end module ucd_field_MPI_IO
