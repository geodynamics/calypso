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
!!      integer(kind = kint) function  gz_defleat_vector_txt            &
!!     &                   (nnod, ndir, vector, ilen_gz, buffer)
!!
!!      subroutine gz_read_fld_charhead_mpi(id_fld,                     &
!!     &         ioff_gl, ilength, chara_dat)
!!      subroutine gz_read_fld_1word_mpi(id_fld, ioff_gl, field_name)
!!      subroutine gz_read_each_field_mpi(id_fld, nprocs_in, id_rank,   &
!!     &          ioff_gl, nnod, ndir, vector)
!!      subroutine gz_skip_each_field_mpi(id_fld, nprocs_in, ioff_gl)
!!@endverbatim
!
      module gz_field_data_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
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
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nnod, ndir
      real(kind = kreal), intent(in) :: vector(nnod,ndir)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength, ip
      integer(kind = kint) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilength = len_each_field_data_buf(ndir)
      ilen_gz = int(real(nnod*ilength) * 1.01) + 24
      allocate(gzip_buf(ilen_gz))
      ilen_gzipped = gz_defleat_vector_txt(nnod, ndir, vector, ilength, &
     &                                     ilen_gz, gzip_buf(1))
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_INTEGER,           &
     &    ilen_gzipped_gl, ione, CALYPSO_INTEGER, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      istack_buffer(0) = 0
      do ip = 1, nprocs
        istack_buffer(ip) = istack_buffer(ip-1) + ilen_gzipped_gl(ip)
      end do
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    buffer_istack_nod_buffer(nprocs, istack_buffer))
!
      if(ilen_gzipped .gt. 0) then
        ioffset = ioff_gl + istack_buffer(my_rank)
        call calypso_mpi_seek_write_chara                               &
     &     (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
      end if
      deallocate(gzip_buf)
      ioff_gl = ioff_gl + istack_buffer(nprocs)
!
      end subroutine gz_write_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_header_mpi(id_fld, ioff_gl, header_txt)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=*), intent(in) :: header_txt
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
!
!
      if(my_rank .eq. 0) then
        ilength = len(header_txt)
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once                                          &
     &     (ilength, header_txt, ilen_gz, ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &         (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_write_fld_header_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function  gz_defleat_vector_txt              &
     &                   (nnod, ndir, vector, ilength, ilen_gz, buffer)
!
      use field_data_IO
!
      integer(kind = kint), intent(in) :: nnod, ndir
      real(kind = kreal), intent(in) :: vector(nnod,ndir)
!
      integer(kind = kint), intent(in) :: ilength, ilen_gz
      character(len=1), intent(inout) :: buffer(ilen_gz)
!
      real(kind = kreal) :: v1(ndir)
      integer(kind = kint) :: ilen_gzipped
      integer(kind = kint_gl) :: inod
!
!
      if(nnod .eq. 1) then
        v1(1:ndir) = vector(1,1:ndir)
        call gzip_defleat_once(ilength,                                 &
     &      each_field_data_buffer(ndir, v1),                           &
     &      ilen_gz, ilen_gzipped, buffer(1))
!
      else if(nnod .gt. 1) then
        v1(1:ndir) = vector(1,1:ndir)
        call gzip_defleat_begin(ilength,                                &
     &      each_field_data_buffer(ndir, v1),                           &
     &      ilen_gz, ilen_gzipped, buffer(1))
        do inod = 2, nnod-1
          v1(1:ndir) = vector(inod,1:ndir)
          call gzip_defleat_cont(ilength,                               &
     &      each_field_data_buffer(ndir, v1), ilen_gz, ilen_gzipped)
        end do
        v1(1:ndir) = vector(nnod,1:ndir)
        call gzip_defleat_last(ilength,                                 &
     &      each_field_data_buffer(ndir, v1), ilen_gz, ilen_gzipped)
      else
        ilen_gzipped = 0
      end if
      gz_defleat_vector_txt = ilen_gzipped
!
      end function gz_defleat_vector_txt
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_charhead_mpi(id_fld,                       &
     &         ioff_gl, ilength, chara_dat)
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: ilength
      character(len=ilength), intent(inout) :: chara_dat
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz                                   &
     &         (id_fld, ioffset, ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once                                          &
     &     (ilen_gz, gzip_buf(1), ilength, chara_dat, ilen_gzipped)
        deallocate(gzip_buf)
      end if
!
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_read_fld_charhead_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_1word_mpi(id_fld, ioff_gl, field_name)
!
      use field_data_IO
      use field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      character(len=kchara), intent(inout) :: field_name
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength, i
!
      character(len=1), allocatable :: gzip_buf(:), textbuf(:)
      character(len=kchara) :: textbuf_c
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        ilen_gz = int(real(kchara) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz                                   &
     &     (id_fld, ioffset, ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once                                          &
     &     (ilen_gz, gzip_buf(1), kchara, textbuf_c, ilen_gzipped)
!
        ilength = read_each_field_name_buffer(textbuf_c, field_name)
        ilength = ilength + 1
!        do i = 1, kchara
!          write(*,*) ilength, i, field_name(i:i),                      &
!     &         iachar(textbuf_c(i:i)), iachar(field_name(i:i))
!        end do
!
!        write(*,*) 'field_name', ilength, trim(field_name)
!
        allocate(textbuf(ilength))
        call gzip_infleat_once                                          &
     &     (ilen_gz, gzip_buf(1), ilength, textbuf(1), ilen_gzipped)
!
        deallocate(gzip_buf, textbuf)
      end if
!
      call sync_field_name_mpi(ilength, field_name)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_read_fld_1word_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_each_field_mpi(id_fld, nprocs_in, id_rank,     &
     &          ioff_gl, nnod, ndir, vector)
!
      use field_data_IO
      use field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer(kind = kint), intent(in) :: nnod, ndir
      real(kind = kreal), intent(inout) :: vector(nnod,ndir)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength, inod
!
      character(len=1), allocatable :: gzip_buf(:)
      character(len=nprocs_in*16+1) :: textbuf_p
      character(len=ndir*25+1) :: textbuf_d
!
      integer(kind = kint_gl) :: istack_buf(0:nprocs_in)
      real(kind = kreal) :: v1(ndir)
!
!
      ilength = len(textbuf_p)
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, ilength, textbuf_p)

      if(my_rank .eq. 0) call read_bufer_istack_nod_buffer              &
     &                      (textbuf_p, nprocs_in, istack_buf)
!
      call MPI_BCAST(istack_buf, (nprocs_in+1), CALYPSO_GLOBAL_INT,     &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
!
      if(id_rank .ge. nprocs_in) then
        ioff_gl = ioff_gl + istack_buf(nprocs_in)
        return
      end if
!
      ilen_gz = int(istack_buf(id_rank+1) - istack_buf(id_rank))
      ilength = len(textbuf_d)
      allocate(gzip_buf(ilen_gz))
!
      ioffset = ioff_gl + istack_buf(id_rank)
      ioff_gl = ioff_gl + istack_buf(nprocs_in)
      call calypso_mpi_seek_read_gz                                     &
     &         (id_fld, ioffset, ilen_gz, gzip_buf(1))
!
      if(nnod .eq. 1) then
        call gzip_infleat_once                                          &
     &   (ilen_gz, gzip_buf(1), ilength, textbuf_d, ilen_gzipped)
        call read_each_field_data_buffer(textbuf_d, ndir, v1)
        vector(1,1:ndir) = v1(1:ndir)
      else if(nnod .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), ilength, textbuf_d, ilen_gzipped)
        call read_each_field_data_buffer(textbuf_d, ndir, v1)
        vector(1,1:ndir) = v1(1:ndir)
        do inod = 2, nnod-1
          call gzip_infleat_cont                                        &
     &        (ilen_gz, ilength, textbuf_d, ilen_gzipped)
          call read_each_field_data_buffer(textbuf_d, ndir, v1)
          vector(inod,1:ndir) = v1(1:ndir)
        end do
        call gzip_infleat_last                                          &
     &     (ilen_gz, ilength, textbuf_d, ilen_gzipped)
        call read_each_field_data_buffer(textbuf_d, ndir, v1)
        vector(nnod,1:ndir) = v1(1:ndir)
      end if
      deallocate(gzip_buf)
!
      end subroutine gz_read_each_field_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_skip_each_field_mpi(id_fld, nprocs_in, ioff_gl)
!
      use field_data_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in
!
      integer(kind = kint) :: ilength
!
      character(nprocs_in*16+1) :: textbuf_c
!
      integer(kind = kint_gl) :: istack_buf(0:nprocs_in)
!
!
      ilength = len(buffer_istack_nod_buffer(nprocs_in,istack_buf))
!
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, ilength, textbuf_c)
      call read_bufer_istack_nod_buffer                                 &
     &   (textbuf_c, nprocs_in, istack_buf)
!
      ioff_gl = ioff_gl + istack_buf(nprocs_in)
!
      end subroutine gz_skip_each_field_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_field_data_MPI_IO
