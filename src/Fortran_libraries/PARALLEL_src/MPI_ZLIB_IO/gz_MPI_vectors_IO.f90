!>@file   gz_MPI_vectors_IO.f90
!!@brief  module gz_MPI_vectors_IO
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_mpi_read_node_position                            &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!      subroutine gz_mpi_read_scalar(IO_param, nnod, scalar)
!!      subroutine gz_mpi_read_vector(IO_param, nnod, numdir, vect)
!!
!!      subroutine gz_mpi_write_node_position                           &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!      subroutine gz_mpi_write_scalar(IO_param, nnod, scalar)
!!      subroutine gz_mpi_write_vector(IO_param, nnod, numdir, vect)
!!@endverbatim
!
      module gz_MPI_vectors_IO
!
      use m_precision
      use m_constants
!
      use t_calypso_mpi_IO_param
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use MPI_ascii_data_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_node_position                              &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(inout) :: id_global(nnod)
      real(kind=kreal), intent(inout) :: xx(nnod, numdir)
!
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = kint) :: i
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:), textbuf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &            - IO_param%istack_merged(IO_param%id_rank))
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      ilen_line = len_int8_and_vector_textline(numdir)
      allocate(textbuf(ilen_line))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &   ilen_gz, gzip_buf(1))
!
      if(nnod .le. 0) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ione, textbuf(1), ilen_gzipped)
      else if(nnod .eq. 1) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_int8_and_vector_textline                              &
     &     (textbuf(1), id_global(1), numdir, xx(1,1))
      else if(nnod .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_int8_and_vector_textline                              &
     &     (textbuf(1), id_global(1), numdir, xx_tmp)
        xx(1,1:numdir) = xx_tmp(1:numdir)
!
        do i = 2, nnod-1
          call gzip_infleat_cont                                        &
     &       (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
          call read_int8_and_vector_textline                            &
     &       (textbuf(1), id_global(i), numdir, xx_tmp)
          xx(i,1:numdir) = xx_tmp(1:numdir)
        end do
!
        call gzip_infleat_last                                          &
     &     (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
        call read_int8_and_vector_textline                              &
     &     (textbuf(1), id_global(nnod), numdir, xx_tmp)
        xx(nnod,1:numdir) = xx_tmp(1:numdir)
      end if
!
      deallocate(gzip_buf, textbuf)
!
      end subroutine gz_mpi_read_node_position
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_scalar(IO_param, nnod, scalar)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod
      real(kind=kreal), intent(inout) :: scalar(nnod)
!
      real(kind = kreal) :: xx_tmp(1)
      integer(kind = kint) :: i
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:), textbuf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &            - IO_param%istack_merged(IO_param%id_rank))
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      ilen_line = len_vector_textline(ione)
      allocate(textbuf(ilen_line))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &   ilen_gz, gzip_buf(1))
!
      if(nnod .le. 0) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ione, textbuf(1), ilen_gzipped)
      else if(nnod .eq. 1) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_vector_textline(textbuf(1), ione, scalar(1))
      else if(nnod .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_vector_textline(textbuf(1), ione, xx_tmp)
        scalar(1) = xx_tmp(1)
!
        do i = 2, nnod-1
          call gzip_infleat_cont                                        &
     &       (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
          call read_vector_textline(textbuf(1), ione, xx_tmp)
          scalar(i) = xx_tmp(1)
        end do
!
        call gzip_infleat_last                                          &
     &     (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
        call read_vector_textline(textbuf(1), ione, xx_tmp)
        scalar(nnod) = xx_tmp(1)
      end if
!
      deallocate(gzip_buf, textbuf)
!
      end subroutine gz_mpi_read_scalar
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_vector(IO_param, nnod, numdir, vect)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      real(kind=kreal), intent(inout) :: vect(nnod, numdir)
!
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = kint) :: i
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:), textbuf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &            - IO_param%istack_merged(IO_param%id_rank))
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      ilen_line = len_vector_textline(numdir)
      allocate(textbuf(ilen_line))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &   ilen_gz, gzip_buf(1))
!
      if(nnod .le. 0) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ione, textbuf(1), ilen_gzipped)
      else if(nnod .eq. 1) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_vector_textline(textbuf(1),  numdir, vect(1,1))
      else if(nnod .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_vector_textline(textbuf(1),  numdir, xx_tmp)
        vect(1,1:numdir) = xx_tmp(1:numdir)
!
        do i = 2, nnod-1
          call gzip_infleat_cont                                        &
     &       (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
          call read_vector_textline(textbuf(1), numdir, xx_tmp)
          vect(i,1:numdir) = xx_tmp(1:numdir)
        end do
!
        call gzip_infleat_last                                          &
     &     (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
        call read_vector_textline(textbuf(1), numdir, xx_tmp)
        vect(nnod,1:numdir) = xx_tmp(1:numdir)
      end if
!
      deallocate(gzip_buf, textbuf)
!
      end subroutine gz_mpi_read_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_node_position                             &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = kint) :: i
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      call gz_mpi_write_num_of_data(IO_param, nnod)
!
      ilen_line = len_int8_and_vector_textline(numdir)
      ilen_gz = int(real(nnod*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nnod .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      int8_and_vector_textline                                    &
     &         (id_global(1), numdir, xx(1,1)),                         &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .gt. 0) then
        xx_tmp(1:numdir) = xx(1,1:numdir)
        call gzip_defleat_begin(ilen_line,                              &
     &     int8_and_vector_textline(id_global(1), numdir, xx_tmp),      &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nnod - 1
          xx_tmp(1:numdir) = xx(i,1:numdir)
          call gzip_defleat_cont(ilen_line,                             &
     &     int8_and_vector_textline(id_global(i), numdir, xx_tmp),      &
     &        ilen_gz, ilen_gzipped)
        end do
        xx_tmp(1:numdir) = xx(nnod,1:numdir)
        call gzip_defleat_last(ilen_line,                               &
     &     int8_and_vector_textline                                     &
     &          (id_global(nnod), numdir, xx_tmp),                      &
     &      ilen_gz, ilen_gzipped)
      end if
!
      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_node_position
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_scalar(IO_param, nnod, scalar)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod
      real(kind=kreal), intent(in) :: scalar(nnod)
!
      integer(kind = kint) :: i
      real(kind = kreal) :: xx_tmp(1)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      call gz_mpi_write_num_of_data(IO_param, nnod)
!
      ilen_line = len_vector_textline(ione)
      ilen_gz = int(real(nnod*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nnod .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      vector_textline(ione, scalar(1)),                           &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .gt. 0) then
        xx_tmp(1) = scalar(1)
        call gzip_defleat_begin(ilen_line,                              &
     &     vector_textline(ione, xx_tmp),                               &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nnod - 1
          xx_tmp(1) = scalar(i)
          call gzip_defleat_cont(ilen_line,                             &
     &     vector_textline(ione, xx_tmp), ilen_gz, ilen_gzipped)
        end do
        xx_tmp(1) = scalar(nnod)
        call gzip_defleat_last(ilen_line,                               &
     &     vector_textline(ione, xx_tmp), ilen_gz, ilen_gzipped)
      end if
!
      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_scalar
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_vector(IO_param, nnod, numdir, vect)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      real(kind=kreal), intent(in) :: vect(nnod, numdir)
!
      integer(kind = kint) :: i
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      call gz_mpi_write_num_of_data(IO_param, nnod)
!
      ilen_line = len_vector_textline(numdir)
      ilen_gz = int(real(nnod*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nnod .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      vector_textline(numdir, vect(1,1)),                         &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .gt. 0) then
        xx_tmp(1:numdir) = vect(1,1:numdir)
        call gzip_defleat_begin(ilen_line,                              &
     &     vector_textline(numdir, xx_tmp),                             &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nnod - 1
          xx_tmp(1:numdir) = vect(i,1:numdir)
          call gzip_defleat_cont(ilen_line,                             &
     &     vector_textline(numdir, xx_tmp), ilen_gz, ilen_gzipped)
        end do
        xx_tmp(1:numdir) = vect(nnod,1:numdir)
        call gzip_defleat_last(ilen_line,                               &
     &     vector_textline(numdir, xx_tmp),ilen_gz, ilen_gzipped)
      end if
!
      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_vector
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_vectors_IO
