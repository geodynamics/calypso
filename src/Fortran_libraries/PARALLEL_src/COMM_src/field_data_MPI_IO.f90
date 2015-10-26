!>@file  field_data_MPI_IO.f90
!!       module field_data_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged field file using MPI-IO
!!
!!@verbatim
!!      subroutine sync_field_time_mpi
!!      subroutine sync_field_header_mpi(nprocs_in, id_rank,            &
!!     &           nnod, num_field, istack_merged)
!!      subroutine sync_field_comp_mpi(num_field, ncomp_field)
!!      subroutine sync_field_name_mpi(field_name)
!!      subroutine sync_field_names_mpi(num_field, field_name)
!!
!!      subroutine write_fld_vecotr_mpi(id_fld, nprocs_in, id_rank,     &
!!     &          ioff_gl, field_name, nnod, ncomp, vect, istack_merged)
!!
!!      subroutine read_field_time_mpi(id_fld, nprocs_in, ioff_gl)
!!      subroutine read_field_header_mpi(id_fld, nprocs_in, id_rank,    &
!!     &           ioff_gl, nnod, num_field, istack_merged)
!!      subroutine read_field_num_mpi                                   &
!!     &         (id_fld, ioff_gl, num_field, ncomp_field)
!!
!!      subroutine write_field_vecotr_mpi(id_fld, ioff_gl,              &
!!     &          nnod, ncomp, vect, istack_merged_intnod)
!
!!      subroutine read_field_name_mpi(id_fld, ioff_gl, field_name)
!!      subroutine read_fld_vecotr_mpi(id_fld, nprocs_in, id_rank,      &
!!     &          ioff_gl, nnod, ncomp, vect, istack_merged)
!!      subroutine skip_fld_vecotr_mpi(nprocs_in, ioff_gl, ncomp,       &
!!     &          istack_merged)
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
      module field_data_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_field_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sync_field_time_mpi
!
      use m_time_data_IO
!
!
      call MPI_BCAST(i_time_step_IO, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(time_IO, ione, CALYPSO_REAL, izero,                &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(delta_t_IO, ione, CALYPSO_REAL, izero,             &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine sync_field_time_mpi
!
! -----------------------------------------------------------------------
!
      subroutine sync_field_header_mpi(nprocs_in, id_rank,              &
     &           nnod, num_field, istack_merged)
!
      integer(kind=kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(inout) :: num_field, nnod
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
!
      call MPI_BCAST(istack_merged, (nprocs_in+1), CALYPSO_GLOBAL_INT,  &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_BCAST(num_field, ione, CALYPSO_INTEGER, izero,           &
     &    CALYPSO_COMM, ierr_MPI)
!
      if(id_rank .ge. nprocs_in) then
        nnod = 0
      else
        nnod = int(istack_merged(id_rank+1) - istack_merged(id_rank))
      end if
!
      end subroutine sync_field_header_mpi
!
! -----------------------------------------------------------------------
!
      subroutine sync_field_comp_mpi(num_field, ncomp_field)
!
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
!
      call MPI_BCAST(ncomp_field, num_field, CALYPSO_INTEGER, izero,    &
     &      CALYPSO_COMM, ierr_MPI)
!
      end subroutine sync_field_comp_mpi
!
! -----------------------------------------------------------------------
!
      subroutine sync_field_name_mpi(field_name)
!
      character(len=kchara), intent(inout) :: field_name
!
!
      call MPI_BCAST(field_name, kchara, CALYPSO_CHARACTER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine sync_field_name_mpi
!
! -----------------------------------------------------------------------
!
      subroutine sync_field_names_mpi(num_field, field_name)
!
      integer(kind=kint), intent(in) :: num_field
      character(len=kchara), intent(inout) :: field_name(num_field)
!
!
      call MPI_BCAST(field_name, (num_field*kchara), CALYPSO_CHARACTER, &
     &      izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sync_field_names_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_fld_vecotr_mpi(id_fld, nprocs_in, id_rank,       &
     &          ioff_gl, field_name, nnod, ncomp, vect, istack_merged)
!
      use field_data_IO
!
      integer(kind = kint), intent(in) :: nnod, id_rank, nprocs_in
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      real(kind = kreal), intent(in) :: vect(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      real(kind = kreal) :: v1(ncomp)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: istack_buffer(0:nprocs_in)
      integer(kind = kint_gl) :: inod
!
!
      v1(1:ncomp) = 0.0d0
      ilength = len_each_field_data_buf(ncomp)
      istack_buffer(0:nprocs_in) = ilength * istack_merged(0:nprocs_in)
!
      call calypso_mpi_seek_write_head_c                                &
     &     (id_fld, ioff_gl, each_field_name_buffer(field_name))
      call calypso_mpi_seek_write_head_c(id_fld, ioff_gl,               &
     &    buffer_istack_nod_buffer(nprocs_in, istack_buffer))
!
      ioffset = ioff_gl + ilength * istack_merged(id_rank)
      do inod = 1, nnod
        v1(1:ncomp) = vect(inod,1:ncomp)
        call calypso_mpi_seek_write_chara(id_fld, ioffset, ilength,     &
     &      each_field_data_buffer(ncomp, v1))
      end do
      ioff_gl = ioff_gl + ilength * istack_merged(nprocs_in)
!
      end subroutine write_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_time_mpi(id_fld, nprocs_in, ioff_gl)
!
      use m_time_data_IO
      use field_data_IO
      use m_error_IDs
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: nprocs_in
!
      integer, intent(in) ::  id_fld
!
      character(len=len_step_data_buf) :: textbuf_c
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iread
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_read_lenchara                             &
     &     (id_fld, ioffset, len_step_data_buf, textbuf_c)
        call read_step_data_buffer(textbuf_c, iread)
!
        if(nprocs_in .ne. iread) then
          call calypso_mpi_abort                                        &
     &       (ierr_fld, 'Set correct field data file')
        end if
!
      end if
      ioff_gl = ioff_gl + len_step_data_buf
!
      call sync_field_time_mpi
!
      end subroutine read_field_time_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_header_mpi(id_fld, nprocs_in, id_rank,      &
     &           ioff_gl, nnod, num_field, istack_merged)
!
      use m_phys_constants
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: nprocs_in, id_rank
!
      integer(kind=kint), intent(inout) :: nnod, num_field
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer, intent(in) ::  id_fld
!
      character(len=31+1+16+1) ::           textbuf_c
      character(len=25+1+nprocs_in*16+1) :: textbuf_d
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      ilength = len(textbuf_d)
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_read_lenchara                             &
     &     (id_fld, ioffset, ilength, textbuf_d)
        call read_field_istack_nod_buffer                               &
     &     (textbuf_d, nprocs_in, istack_merged)
      end if
      ioff_gl = ioff_gl + ilength
!
      ilength = len(textbuf_c)
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_read_lenchara                             &
     &       (id_fld, ioffset, ilength, textbuf_c)
        call read_field_num_buffer(textbuf_c, num_field)
      end if
      ioff_gl = ioff_gl + ilength
!
      call sync_field_header_mpi(nprocs_in, id_rank, nnod,              &
     &    num_field, istack_merged)
!
      end subroutine read_field_header_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_mpi                                     &
     &         (id_fld, ioff_gl, num_field, ncomp_field)
!
      use m_phys_constants
      use field_data_IO
      use ucd_data_to_buffer
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
      integer, intent(in) ::  id_fld
!
      character(len=num_field*5+1) :: charabuf_c
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      ilength = len(charabuf_c)
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_read_lenchara                             &
     &       (id_fld, ioffset, ilength, charabuf_c)
        call read_field_comp_buffer(charabuf_c, num_field, ncomp_field)
      end if
      ioff_gl = ioff_gl + ilength
!
      call sync_field_comp_mpi(num_field, ncomp_field)
!
      end subroutine read_field_num_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_name_mpi(id_fld, ioff_gl, field_name)
!
      use m_phys_constants
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=kchara), intent(inout) :: field_name
!
      integer, intent(in) ::  id_fld
!
      character(len=kchara+1) :: textbuf_c
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        ilength = len(textbuf_c)
        call calypso_mpi_seek_read_lenchara                             &
     &     (id_fld, ioffset, ilength, textbuf_c)
        ilength = read_each_field_name_buffer(textbuf_c, field_name)
      end if
!
      call sync_field_name_mpi(field_name)
      ioff_gl = ioff_gl + ilength + 1
!
      end subroutine read_field_name_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_field_vecotr_mpi(id_fld, ioff_gl,                &
     &          nnod, ncomp, vect, istack_merged_intnod)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint), intent(in) :: nnod, ncomp
      real(kind = kreal), intent(in) :: vect(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      character(len=ncomp*25+1), allocatable, target :: textbuf_n(:)
      character(len=ncomp*25+1), pointer :: charatmp
      character(len=kchara) :: fmt_txt
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: i, num
!
!
      write(*,*) 'write_field_vecotr_mpi'
      ilength = ncomp*25 + 1
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
      ioffset = ioff_gl + ilength * istack_merged_intnod(my_rank)
      ioff_gl = ioff_gl + ilength * istack_merged_intnod(nprocs)
!
      if(num .le. 0) return
      write(fmt_txt,'(a1,i5,a16)') '(', ncomp, '(1pE25.15e3),a1)'
!
      allocate(textbuf_n(num))
!
      do i = 1, num
        charatmp => textbuf_n(i)
        write(charatmp,'(1p3E23.12e3,a1)') vect(i,1:ncomp), char(10)
      end do
      call calypso_mpi_seek_wrt_mul_chara(id_fld, ioffset, ilength,     &
     &    num, textbuf_n)
!
      nullify(charatmp)
      deallocate(textbuf_n)
!
      end subroutine write_field_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_fld_vecotr_mpi(id_fld, nprocs_in, id_rank,        &
     &          ioff_gl, nnod, ncomp, vect, istack_merged)
!
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank, nnod
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      character(len=ncomp*25+1) :: textbuf_d
      real(kind = kreal) :: v1(ncomp)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: inod
!
!
      if(id_rank .ge. nprocs_in) return
!   Skip buffer size
      ilength = len(buffer_istack_nod_buffer(nprocs_in, istack_merged))
      ioff_gl = ioff_gl + ilength
!
      v1(1:ncomp) = 0.0d0
      ilength = len(textbuf_d)
      ioffset = ioff_gl + ilength * istack_merged(id_rank)
!
      do inod = 1, nnod
        call calypso_mpi_seek_read_lenchara                             &
     &    (id_fld, ioffset, ilength, textbuf_d)
        call read_each_field_data_buffer(textbuf_d, ncomp, v1)
        vect(inod,1:ncomp) = v1(1:ncomp)
      end do
      ioff_gl = ioff_gl + ilength * istack_merged(nprocs_in)
!
      end subroutine read_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine skip_fld_vecotr_mpi(nprocs_in, ioff_gl, ncomp,         &
     &          istack_merged)
!
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind = kint), intent(in) :: ncomp
!
      real(kind = kreal) :: v1(ncomp)
      integer(kind = kint) :: ilength
!
!
      v1(1:ncomp) = 0.0d0
      ilength = len_each_field_data_buf(ncomp)
      ioff_gl = ioff_gl + ilength * istack_merged(nprocs_in)
!
      end subroutine skip_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      end module field_data_MPI_IO
