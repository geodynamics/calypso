!>@file  field_file_MPI_IO.f90
!!       module field_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_field_file_mpi                            &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!
!!      subroutine read_step_field_file_mpi                             &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!      subroutine read_alloc_step_fld_file_mpi                         &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!
!!      subroutine read_alloc_step_fld_head_mpi                         &
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
!!      7.2   List of data size (Byte)
!!      7.3   Field data
!!@endverbatim
!
      module field_file_MPI_IO
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
      private :: read_field_data_mpi, read_field_names_mpi
      private :: write_field_data_mpi
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_step_field_file_mpi                              &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
!
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
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
     &      fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,   &
     &      fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,           &
     &      fld_IO%istack_numnod_IO)
      end if
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine write_step_field_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_step_field_file_mpi                               &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use field_data_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
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
      call alloc_merged_field_stack(num_pe, fld_IO)
!
      call read_field_header_mpi(id_fld, num_pe, id_rank,               &
     &      ioff_gl,  fld_IO%nnod_IO, fld_IO%num_field_IO,              &
     &      fld_IO%istack_numnod_IO)
!
      call read_field_num_mpi                                           &
     &   (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_data_mpi(id_fld, num_pe, id_rank, ioff_gl,        &
     &      fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,   &
     &      fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,           &
     &      fld_IO%istack_numnod_IO)
!
      call dealloc_merged_field_stack(fld_IO)
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine read_step_field_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_step_fld_file_mpi                           &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use field_data_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
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
      call alloc_merged_field_stack(num_pe, fld_IO)
!
      call read_field_header_mpi(id_fld, num_pe, id_rank,               &
     &      ioff_gl,  fld_IO%nnod_IO, fld_IO%num_field_IO,              &
     &      fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call read_field_num_mpi                                           &
     &   (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data_mpi(id_fld, num_pe, id_rank, ioff_gl,        &
     &      fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,   &
     &      fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,           &
     &      fld_IO%istack_numnod_IO)
      call calypso_close_mpi_file(id_fld)
!
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. num_pe) then
        call dealloc_phys_data_IO(fld_IO)
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine read_alloc_step_fld_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_step_fld_head_mpi                           &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use field_data_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
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
      call alloc_merged_field_stack(num_pe, fld_IO)
!
      call read_field_header_mpi(id_fld, num_pe, id_rank,               &
     &      ioff_gl,  fld_IO%nnod_IO, fld_IO%num_field_IO,              &
     &      fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call read_field_num_mpi                                           &
     &   (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_names_mpi(id_fld, num_pe, id_rank, ioff_gl,       &
     &      fld_IO%num_field_IO, fld_IO%num_comp_IO, fld_IO%fld_name,   &
     &      fld_IO%istack_numnod_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      call calypso_close_mpi_file(id_fld)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. num_pe) call dealloc_phys_name_IO(fld_IO)
!
      end subroutine read_alloc_step_fld_head_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_field_data_mpi                                   &
     &         (id_fld, num_pe, id_rank, ioff_gl,                       &
     &          t_IO, nnod, num_field, ntot_comp, ncomp_field,          &
     &          field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use time_data_IO
      use field_data_IO
      use field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(time_data), intent(in) :: t_IO
!
      integer, intent(in) :: id_rank, num_pe
      integer(kind=kint), intent(in) :: nnod
      integer(kind = kint_gl), intent(in) :: istack_merged(0:num_pe)
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: j, icou
!
!
      call calypso_mpi_seek_write_head_c                                &
     &   (id_fld, ioff_gl, step_data_buffer(num_pe, t_IO))
      call calypso_mpi_seek_write_head_c(id_fld, ioff_gl,               &
     &    field_istack_nod_buffer(num_pe, istack_merged))
      call calypso_mpi_seek_write_head_c(id_fld, ioff_gl,               &
     &    field_num_buffer(num_field))
      call calypso_mpi_seek_write_head_c                                &
     &   (id_fld, ioff_gl, field_comp_buffer(num_field, ncomp_field))
!
      icou = 1
      do j = 1, num_field
        call write_field_name_mpi(id_fld, ioff_gl, field_name(j))
        call write_fld_vecotr_mpi(id_fld, num_pe, id_rank, ioff_gl,     &
     &      nnod, ncomp_field(j), d_nod(1,icou), istack_merged)
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine write_field_data_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_mpi(id_fld, num_pe, id_rank,           &
     &          ioff_gl, nnod, num_field, ntot_comp, ncomp_field,       &
     &          field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use field_data_MPI_IO
!
      integer, intent(in) :: id_rank, num_pe
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:num_pe)
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: j, icou
!
!
      icou = 1
      do j = 1, num_field
        call read_field_name_mpi(id_fld, ioff_gl, field_name(j))
        call read_fld_vecotr_mpi(id_fld, num_pe, id_rank, ioff_gl,      &
     &      nnod, ncomp_field(j), d_nod(1,icou), istack_merged)
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine read_field_data_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_names_mpi(id_fld, num_pe, id_rank,          &
     &          ioff_gl, num_field, ncomp_field, field_name,            &
     &          istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use field_data_MPI_IO
!
      integer, intent(in) :: id_rank, num_pe
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:num_pe)
      character(len=kchara), intent(inout) :: field_name(num_field)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: j
!
!
      do j = 1, num_field
        call read_field_name_mpi(id_fld, ioff_gl, field_name(j))
        call skip_fld_vecotr_mpi(num_pe, id_rank, ioff_gl,              &
     &      ncomp_field(j), istack_merged)
      end do
!
      end subroutine read_field_names_mpi
!
! -----------------------------------------------------------------------
!
      end module field_file_MPI_IO
