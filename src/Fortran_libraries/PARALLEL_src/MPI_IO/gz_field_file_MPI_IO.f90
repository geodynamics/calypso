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
!!     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!!        type(time_data), intent(in) :: t_IO
!!        type(field_IO), intent(in) :: fld_IO
!!
!!      subroutine write_field_head_gz_mpi(id_fld, nprocs_in,           &
!!     &          ioff_gl, t_IO, num_field, ncomp_field, istack_merged)
!!
!!      subroutine read_step_field_file_gz_mpi                          &
!!     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!!      subroutine read_alloc_stp_fld_file_gz_mpi                       &
!!     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!!
!!      subroutine read_alloc_stp_fld_head_gz_mpi                       &
!!     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
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
      private :: write_field_data_gz_mpi
      private :: read_field_step_gz_mpi
      private :: read_field_header_gz_mpi, read_field_num_gz_mpi
      private :: read_field_data_gz_mpi, read_field_names_gz_mpi
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_gz_step_field_file_mpi                           &
     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Write gzipped ascii data by MPI-IO: ', trim(file_name)
      call calypso_mpi_write_file_open(file_name, nprocs_in, id_fld)
!
      if(id_rank .lt. nprocs_in) then
        ioff_gl = 0
        call write_field_head_gz_mpi                                    &
     &     (id_fld, nprocs_in, ioff_gl, t_IO, fld_IO%num_field_IO,      &
     &      fld_IO%num_comp_IO,  fld_IO%istack_numnod_IO)
!
        call write_field_data_gz_mpi(id_fld, ioff_gl,                   &
     &      fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,   &
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
     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      use gz_field_data_IO
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Read gzipped ascii data by MPI-IO: ', trim(file_name)
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_field_step_gz_mpi(id_fld, nprocs_in, ioff_gl, t_IO)
!
      call read_field_header_gz_mpi                                     &
     &   (id_fld, nprocs_in, id_rank, ioff_gl, fld_IO%nnod_IO,          &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call read_field_num_gz_mpi                                        &
     &   (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_data_gz_mpi(id_fld, nprocs_in, id_rank, ioff_gl,  &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
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
     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      use gz_field_data_IO
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer ::  id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Read gzipped ascii data by MPI-IO: ', trim(file_name)
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_field_step_gz_mpi(id_fld, nprocs_in, ioff_gl, t_IO)
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_gz_mpi                                     &
     &   (id_fld, nprocs_in, id_rank, ioff_gl, fld_IO%nnod_IO,          &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
!
      call read_field_num_gz_mpi                                        &
     &   (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data_gz_mpi(id_fld, nprocs_in, id_rank, ioff_gl,  &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call calypso_close_mpi_file(id_fld)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. nprocs_in) then
        call dealloc_phys_data_IO(fld_IO)
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine read_alloc_stp_fld_file_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_head_gz_mpi                         &
     &         (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      use gz_field_data_IO
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer ::  id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Read gzipped ascii data by MPI-IO: ', trim(file_name)
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_field_step_gz_mpi(id_fld, nprocs_in, ioff_gl, t_IO)
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_gz_mpi                                     &
     &   (id_fld, nprocs_in, id_rank, ioff_gl, fld_IO%nnod_IO,          &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
!
      call read_field_num_gz_mpi                                        &
     &   (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_names_gz_mpi(id_fld, nprocs_in, id_rank, ioff_gl, &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
!
      call calypso_close_mpi_file(id_fld)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. nprocs_in) call dealloc_phys_name_IO(fld_IO)
!
      end subroutine read_alloc_stp_fld_head_gz_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_field_head_gz_mpi(id_fld, nprocs_in,             &
     &          ioff_gl, t_IO, num_field, ncomp_field, istack_merged)
!
      use m_phys_constants
      use time_data_IO
      use field_data_IO
      use gz_field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind = kint), intent(in) :: nprocs_in
!
      type(time_data), intent(in) :: t_IO
!
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
!
      integer, intent(in) ::  id_fld
!
!
      call gz_write_fld_header_mpi                                      &
     &   (id_fld, ioff_gl, step_data_buffer(nprocs_in, t_IO))
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    field_istack_nod_buffer(nprocs_in, istack_merged))
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    field_num_buffer(num_field))
      call gz_write_fld_header_mpi                                      &
     &   (id_fld, ioff_gl, field_comp_buffer(num_field, ncomp_field))
!
      end subroutine write_field_head_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_field_data_gz_mpi(id_fld, ioff_gl,               &
     &           nnod, num_field, ntot_comp, ncomp_field,               &
     &           field_name, d_nod)
!
      use m_phys_constants
      use field_data_IO
      use gz_field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: nnod
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
      icou = 1
      do j = 1, num_field
        call gz_write_fld_header_mpi                                    &
     &     (id_fld, ioff_gl, each_field_name_buffer(field_name(j)))
        call gz_write_fld_vecotr_mpi(id_fld, ioff_gl,                   &
     &      nnod, ncomp_field(j), d_nod(1,icou))
!        write(*,*) 'gz_write_fld_vecotr_mpi end', j, my_rank, ioff_gl
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine write_field_data_gz_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_step_gz_mpi                                 &
     &         (id_fld, nprocs_in, ioff_gl, t_IO)
!
      use m_error_IDs
      use time_data_IO
      use field_data_MPI_IO
      use gz_field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = kint), intent(in) :: nprocs_in
      type(time_data), intent(inout) :: t_IO
!
      integer(kind=kint) :: iread
      character(len=len_step_data_buf) :: textbuf_c
!
!
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, len_step_data_buf, textbuf_c)
!
      if(my_rank .eq. 0) then
        call read_step_data_buffer(textbuf_c, iread, t_IO)
      end if
!
      if(my_rank.eq.0 .and. nprocs_in .ne. iread) then
        call calypso_mpi_abort(ierr_fld, 'Set correct field data file')
      end if
!
      call sync_field_time_mpi(t_IO)
!
      end subroutine read_field_step_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_header_gz_mpi(id_fld, nprocs_in, id_rank,   &
     &          ioff_gl, nnod, num_field, istack_merged)
!
      use field_data_IO
      use field_data_MPI_IO
      use gz_field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(inout) :: nnod, num_field
      integer(kind = kint_gl), intent(inout)                            &
     &                       :: istack_merged(0:nprocs_in)
!
      integer(kind=kint) :: ilength
      character(len=31+1+16+1) ::           textbuf_c
      character(len=25+1+nprocs_in*16+1) :: textbuf_d
!
!
      ilength = len(textbuf_d)
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, ilength, textbuf_d)
      if(my_rank .eq. 0) call read_field_istack_nod_buffer              &
     &                      (textbuf_d, nprocs_in, istack_merged)
!
      ilength = len(field_num_buffer(izero))
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, ilength, textbuf_c)
      if(my_rank .eq. 0) call read_field_num_buffer                     &
     &                      (textbuf_c, num_field)
!
      call MPI_BCAST(istack_merged, (nprocs_in+1), CALYPSO_GLOBAL_INT,  &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(num_field, ione, CALYPSO_INTEGER, izero,           &
     &    CALYPSO_COMM, ierr_MPI)
!
      call sync_field_header_mpi(nprocs_in, id_rank, nnod,              &
     &    istack_merged)
!
      end subroutine read_field_header_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_gz_mpi                                  &
     &         (id_fld, ioff_gl, num_field, ncomp_field)
!
      use field_data_IO
      use field_data_MPI_IO
      use gz_field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(inout) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
      integer(kind=kint) :: ilength
      character(len=num_field*5+1) :: textbuf
!
!
      ilength = len(textbuf)
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, ilength, textbuf)
      if(my_rank .eq. 0) call read_field_comp_buffer                    &
     &                      (textbuf, num_field, ncomp_field)
!
      call MPI_BCAST(ncomp_field, num_field, CALYPSO_INTEGER, izero,    &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine read_field_num_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_data_gz_mpi(id_fld, nprocs_in, id_rank,     &
     &          ioff_gl, nnod, num_field, ntot_comp, ncomp_field,       &
     &          field_name, d_nod)
!
      use field_data_IO
      use field_data_MPI_IO
      use gz_field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint) :: j, icou
!
!
      icou = 1
      do j = 1, num_field
        call gz_read_fld_1word_mpi(id_fld, ioff_gl, field_name(j))
!         write(*,*) 'gz_read_each_field_mpi start', j, my_rank
        call gz_read_each_field_mpi(id_fld, nprocs_in, id_rank,        &
     &      ioff_gl, nnod, ncomp_field(j), d_nod(1,icou))
!         write(*,*) 'gz_read_each_field_mpi end', j, my_rank
         if(my_rank .eq. 0) write(*,*) 'Read ', j, trim(field_name(j))
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine read_field_data_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_names_gz_mpi(id_fld, nprocs_in, id_rank,    &
     &          ioff_gl, num_field, field_name)
!
      use field_data_IO
      use field_data_MPI_IO
      use gz_field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer(kind=kint), intent(in) :: num_field
      character(len=kchara), intent(inout) :: field_name(num_field)
!
      integer(kind = kint) :: j
!
!
      do j = 1, num_field
        call gz_read_fld_1word_mpi(id_fld, ioff_gl, field_name(j))
        call gz_skip_each_field_mpi(id_fld, nprocs_in, ioff_gl)
      end do
!
      end subroutine read_field_names_gz_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_field_file_MPI_IO
