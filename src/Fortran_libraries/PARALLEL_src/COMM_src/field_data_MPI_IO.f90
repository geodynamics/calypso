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
!!  External routines in this file
!!      subroutine calypso_mpi_seek_write_ext                           &
!!     &         (id_mpi_file, ioffset, ilength, textbuf)
!!      subroutine read_step_data_buf_ext(textbuf, id_rank)
!!      subroutine read_field_istack_nod_buf_ext                        &
!!     &         (textbuf, nprocs, istack_nod)
!!      subroutine read_bufer_istack_nod_buf_ext                        &
!!     &         (textbuf, nprocs, istack_nod)
!!      subroutine read_field_num_buf_ext(textbuf, num_field)
!!      subroutine read_field_comp_buf_ext                              &
!!     &         (textbuf, num_field, ncomp_field)
!!      subroutine read_each_field_name_buf_ext(textbuf, field_name)
!!      subroutine read_each_field_data_buf_ext(textbuf, ncomp, vect)
!!
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
      ilength = len(each_field_data_buffer(ncomp, v1))
      istack_buffer(0:nprocs_in) = ilength * istack_merged(0:nprocs_in)
!
      call calypso_mpi_seek_write_head_c                                &
     &     (id_fld, ioff_gl, each_field_name_buffer(field_name))
      call calypso_mpi_seek_write_head_c(id_fld, ioff_gl,               &
     &    buffer_istack_nod_buffer(nprocs_in, istack_buffer))
!
      ioffset = int(ioff_gl + ilength * istack_merged(id_rank))
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
      character(len=1), allocatable :: c1buf(:)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength, iread
!
!
      ilength = len(step_data_buffer(izero))
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        allocate(c1buf(ilength))
        call calypso_mpi_seek_read_chara                                &
     &     (id_fld, ioffset, ilength, c1buf(1))
        call read_step_data_buf_ext(c1buf(1), iread)
        deallocate(c1buf)
!
        if(nprocs_in .ne. iread) then
          call calypso_mpi_abort                                        &
     &       (ierr_fld, 'Set correct field data file')
        end if
!
      end if
      ioff_gl = ioff_gl + ilength
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
      character(len=1), allocatable :: c1buf(:)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      ilength = len(field_istack_nod_buffer(nprocs_in, istack_merged))
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        allocate(c1buf(ilength))
        call calypso_mpi_seek_read_chara                                &
     &     (id_fld, ioffset, ilength, c1buf(1))
        call read_field_istack_nod_buf_ext                              &
     &     (c1buf(1), nprocs_in, istack_merged)
        deallocate(c1buf)
      end if
      ioff_gl = ioff_gl + ilength
!
      ilength = len(field_num_buffer(izero))
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        allocate(c1buf(ilength))
        call calypso_mpi_seek_read_chara                              &
     &       (id_fld, ioffset, ilength, c1buf(1))
        call read_field_num_buf_ext(c1buf(1), num_field)
        deallocate(c1buf)
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
      character(len=1), allocatable :: c1buf(:)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      ilength = len(field_comp_buffer(num_field, ncomp_field))
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        allocate(c1buf(ilength))
        call calypso_mpi_seek_read_chara                                &
     &       (id_fld, ioffset, ilength, c1buf(1))
        call read_field_comp_buf_ext(c1buf(1), num_field, ncomp_field)
        deallocate(c1buf)
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
      character(len=1), allocatable :: c1buf(:)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        ilength = kchara + 1
        allocate(c1buf(ilength))
        call calypso_mpi_seek_read_chara                                &
     &     (id_fld, ioffset, ilength, c1buf(1))
        call read_each_field_name_buf_ext(c1buf(1), field_name)
        deallocate(c1buf)
      end if
!
      call sync_field_name_mpi(field_name)
      ioff_gl = ioff_gl + len_trim(field_name) + 1
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
      ioffset = int(ioff_gl + ilength * istack_merged_intnod(my_rank))
      ioff_gl = ioff_gl + ilength * istack_merged_intnod(nprocs)
!
      if(num .le. 0) return
      write(fmt_txt,'(a1,i5,a16)') '(', ncomp, '(1pE25.15e3),a1)'
!
      allocate(textbuf_n(nnod))
!
      do i = 1, num
        charatmp => textbuf_n(i)
        write(charatmp,'(1p3E23.12e3,a1)') vect(i,1:ncomp), char(10)
      end do
      call calypso_mpi_seek_write_ext(id_fld, ioffset, (num*ilength),   &
     &    textbuf_n(1))
!
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
      character(len=1), allocatable :: c1buf(:)
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
      ilength = len(each_field_data_buffer(ncomp, v1))
      ioffset = int(ioff_gl + ilength * istack_merged(id_rank))
      allocate(c1buf(ilength))
!
      do inod = 1, nnod
        call calypso_mpi_seek_read_chara                                &
     &    (id_fld, ioffset, ilength, c1buf(1))
        call read_each_field_data_buf_ext(c1buf(1), ncomp, v1)
        vect(inod,1:ncomp) = v1(1:ncomp)
      end do
      deallocate(c1buf)
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
      ilength = len(each_field_data_buffer(ncomp, v1))
      ioff_gl = ioff_gl + ilength * istack_merged(nprocs_in)
!
      end subroutine skip_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      end module field_data_MPI_IO
!
! -----------------------------------------------------------------------
!
!      External routine to pass character lengh check
!
! -----------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_ext                             &
     &         (id_mpi_file, ioffset, ilength, textbuf)
!
      use m_calypso_mpi_IO
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength), intent(in) :: textbuf
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
!
      call calypso_mpi_seek_write_chara                                 &
     &         (id_mpi_file, ioffset, ilength, textbuf)
!
      end subroutine calypso_mpi_seek_write_ext
!
! -------------------------------------------------------------------
!
      subroutine read_step_data_buf_ext(textbuf, id_rank)
!
      use m_time_data_IO
!
      character(len=len_step_data_buf), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: id_rank
!
!
      call read_step_data_buffer(textbuf, id_rank)
!
      end subroutine read_step_data_buf_ext
!
! -------------------------------------------------------------------
!
      subroutine read_field_istack_nod_buf_ext                          &
     &         (textbuf, nprocs, istack_nod)
!
      use field_data_IO
!
      integer(kind = kint), intent(in) :: nprocs
      character(len=25+1+nprocs*16+1), intent(in) :: textbuf
      integer(kind = kint_gl), intent(inout) :: istack_nod(0:nprocs)
!
      call read_field_istack_nod_buffer(textbuf, nprocs, istack_nod)
!
      end subroutine read_field_istack_nod_buf_ext
!
! -------------------------------------------------------------------
!
      subroutine read_bufer_istack_nod_buf_ext                          &
     &         (textbuf, nprocs, istack_nod)
!
      use field_data_IO
!
      integer(kind = kint), intent(in) :: nprocs
      character(nprocs*16+1), intent(in) :: textbuf
      integer(kind = kint_gl), intent(inout) :: istack_nod(0:nprocs)
!
!
      call read_bufer_istack_nod_buffer(textbuf, nprocs, istack_nod)
!
      end subroutine read_bufer_istack_nod_buf_ext
!
! -------------------------------------------------------------------
!
      subroutine read_field_num_buf_ext(textbuf, num_field)
!
      use field_data_IO
!
      character(len=48), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: num_field
!
!
      call read_field_num_buffer(textbuf, num_field)
!
      end subroutine read_field_num_buf_ext
!
! -------------------------------------------------------------------
!
      subroutine read_field_comp_buf_ext                                &
     &         (textbuf, num_field, ncomp_field)
!
      use field_data_IO
!
      integer(kind = kint), intent(in) :: num_field
      character(len=num_field*5), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: ncomp_field(num_field)
!
!
      call read_field_comp_buffer(textbuf, num_field, ncomp_field)
!
      end subroutine read_field_comp_buf_ext
!
! -------------------------------------------------------------------
!
      subroutine read_each_field_name_buf_ext(textbuf, field_name)
!
      use field_data_IO
!
      character(len=kchara), intent(in) :: textbuf
      character(len=kchara), intent(inout) :: field_name
!
!
      call read_each_field_name_buffer(textbuf, field_name)
!
      end subroutine read_each_field_name_buf_ext
!
! -------------------------------------------------------------------
!
      subroutine read_each_field_data_buf_ext(textbuf, ncomp, vect)
!
      use field_data_IO
!
      integer(kind = kint), intent(in) :: ncomp
      character(len=ncomp*25+1), intent(in) :: textbuf
      real(kind = kreal), intent(inout) :: vect(ncomp)
!
!
      call read_each_field_data_buffer(textbuf, ncomp, vect)
!
      end subroutine read_each_field_data_buf_ext
!
! -------------------------------------------------------------------
!
