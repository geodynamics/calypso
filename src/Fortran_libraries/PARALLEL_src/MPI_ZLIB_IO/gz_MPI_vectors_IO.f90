!>@file   gz_MPI_vectors_IO.f90
!!@brief  module gz_MPI_vectors_IO
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_mpi_read_scalar(IO_param, nnod, scalar)
!!      subroutine gz_mpi_read_vector(IO_param, nnod, numdir, vect)
!!
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
      use t_buffer_4_gzip
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use MPI_ascii_data_IO
!
      implicit none
!
      type(buffer_4_gzip) :: zbuf
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_scalar(IO_param, nnod, scalar)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: nnod
      real(kind=kreal), intent(inout) :: scalar(nnod)
!
!
      call gz_mpi_read_vector(IO_param, nnod, ione, scalar(1))
!
      end subroutine gz_mpi_read_scalar
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_vector(IO_param, nnod, numdir, vect)
!
      use zlib_convert_ascii_vector
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: numdir
      real(kind=kreal), intent(inout) :: vect(nnod, numdir)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call read_byte_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_byte_stack_textline(IO_param%nprocs_in)),               &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      zbuf%ilen_gz = IO_param%istack_merged(IO_param%id_rank+1)         &
     &            - IO_param%istack_merged(IO_param%id_rank)
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(zbuf%ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      call alloc_zip_buffer(zbuf)
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
      call infleate_vector_txt(ione, nnod, numdir, vect, zbuf)
!
      end subroutine gz_mpi_read_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_scalar(IO_param, nnod, scalar)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: nnod
      real(kind=kreal), intent(in) :: scalar(nnod)
!
!
      call gz_mpi_write_vector(IO_param, nnod, ione, scalar(1))
!
      end subroutine gz_mpi_write_scalar
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_vector(IO_param, nnod, numdir, vect)
!
      use zlib_convert_ascii_vector
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: numdir
      real(kind=kreal), intent(in) :: vect(nnod, numdir)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call defleate_vector_txt(ione, nnod, numdir, vect, zbuf)
!
      call gz_mpi_write_stack_over_domain(IO_param, zbuf%ilen_gzipped)
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_gz(IO_param%id_file, ioffset, zbuf)
      end if
!
      call dealloc_zip_buffer(zbuf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_vector
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_vectors_IO
