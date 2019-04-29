!>@file  gz_field_data_MPI_IO.f90
!!       module gz_field_data_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_fld_vecotr_mpi                              &
!!     &         (id_fld, ioff_gl, nnod, ndir, vector)
!!      subroutine gz_write_fld_header_mpi(id_fld, ioff_gl, header_txt)
!!
!!      subroutine gz_read_fld_charhead_mpi(id_fld,                     &
!!     &         ioff_gl, ilength, chara_dat)
!!      subroutine gz_read_fld_1word_mpi(id_fld, ioff_gl, field_name)
!!      subroutine gz_read_each_field_mpi(id_fld, num_pe, id_rank,      &
!!     &          ioff_gl, nnod, ndir, vector)
!!      subroutine gz_skip_each_field_mpi(id_fld, num_pe, ioff_gl)
!!@endverbatim
!
      module gz_field_data_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_buffer_4_gzip
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_vecotr_mpi                                &
     &         (id_fld, ioff_gl, nnod, ndir, vector)
!
      use field_data_IO
      use data_IO_to_textline
!
      use zlib_convert_ascii_vector
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndir
      real(kind = kreal), intent(in) :: vector(nnod,ndir)
!
      integer, intent(in) ::  id_fld
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = kint) :: ip
      integer(kind = kint_gl) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call defleate_vector_txt(izero, nnod, ndir, vector, zbuf)
!
      call MPI_Allgather(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT,      &
     &    ilen_gzipped_gl, 1, CALYPSO_GLOBAL_INT, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      istack_buffer(0) = 0
      do ip = 1, nprocs
        istack_buffer(ip) = istack_buffer(ip-1) + ilen_gzipped_gl(ip)
      end do
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    buffer_istack_nod_buffer(nprocs, istack_buffer))
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = ioff_gl + istack_buffer(my_rank)
        call calypso_mpi_seek_write_gz(id_fld, ioffset, zbuf)
      end if
      ioff_gl = ioff_gl + istack_buffer(nprocs)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_header_mpi(id_fld, ioff_gl, header_txt)
!
      use zlib_convert_text
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=*), intent(in) :: header_txt
!
      integer, intent(in) ::  id_fld
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        call defleate_characters(len(header_txt), header_txt, zbuf)
!
        ioffset = ioff_gl
        call calypso_mpi_seek_write_gz(id_fld, ioffset, zbuf)
        call dealloc_zip_buffer(zbuf)
      end if
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT,          &
     &    0, CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_write_fld_header_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_charhead_mpi(id_fld,                       &
     &         ioff_gl, ilength, chara_dat)
!
      use zlib_convert_text
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) :: ilength
      character(len=ilength), intent(inout) :: chara_dat
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        zbuf%ilen_gz = int(real(ilength) *1.01 + 24, KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
        call calypso_mpi_seek_read_gz(id_fld, ioffset, zbuf)
!
        call infleate_characters(ilength, chara_dat, zbuf)
      end if
!
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT,          &
     &    0, CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_read_fld_charhead_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_1word_mpi(id_fld, ioff_gl, field_name)
!
      use field_data_IO
      use field_data_MPI_IO
      use zlib_convert_text
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      character(len=kchara), intent(inout) :: field_name
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        zbuf%ilen_gz = int(real(kchara)*1.1 + 24,KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
        call calypso_mpi_seek_read_gz(id_fld, ioffset, zbuf)
!
        call infleate_1word(ilength, field_name, zbuf)
      end if
!
      call sync_field_name_mpi(ilength, field_name)
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT,          &
     &    0, CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_read_fld_1word_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_each_field_mpi(id_fld, num_pe, id_rank,        &
     &          ioff_gl, nnod, ndir, vector)
!
      use field_data_IO
      use field_data_MPI_IO
      use data_IO_to_textline
      use zlib_convert_ascii_vector
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer, intent(in) :: num_pe, id_rank
!
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndir
      real(kind = kreal), intent(inout) :: vector(nnod,ndir)
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len=num_pe*16+1) :: textbuf_p
!
      integer(kind = kint_gl) :: istack_buf(0:num_pe)
!
!
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, len(textbuf_p), textbuf_p)

      if(my_rank .eq. 0) call read_bufer_istack_nod_buffer              &
     &                      (textbuf_p, num_pe, istack_buf)
!
      call MPI_BCAST(istack_buf, int(num_pe+1), CALYPSO_GLOBAL_INT,     &
     &    0, CALYPSO_COMM, ierr_MPI)
!
!
      if(id_rank .ge. num_pe) then
        ioff_gl = ioff_gl + istack_buf(num_pe)
        return
      end if
!
      ioffset = ioff_gl + istack_buf(id_rank)
      ioff_gl = ioff_gl + istack_buf(num_pe)
      zbuf%ilen_gz = istack_buf(id_rank+1) - istack_buf(id_rank)
      call alloc_zip_buffer(zbuf)
      call calypso_mpi_seek_read_gz(id_fld, ioffset, zbuf)
!
      call infleate_vector_txt(izero, nnod, ndir, vector, zbuf)
!
      end subroutine gz_read_each_field_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_skip_each_field_mpi(id_fld, num_pe, ioff_gl)
!
      use field_data_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer, intent(in) :: num_pe
!
      integer :: ilength
      character(num_pe*16+1) :: textbuf_c
!
      integer(kind = kint_gl) :: istack_buf(0:num_pe)
!
!
      ilength = len(buffer_istack_nod_buffer(num_pe,istack_buf))
!
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, ilength, textbuf_c)
      call read_bufer_istack_nod_buffer                                 &
     &   (textbuf_c, num_pe, istack_buf)
!
      ioff_gl = ioff_gl + istack_buf(num_pe)
!
      end subroutine gz_skip_each_field_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_field_data_MPI_IO
