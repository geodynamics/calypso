!>@file   gz_MPI_position_IO.f90
!!@brief  module gz_MPI_position_IO
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_mpi_read_radial_position                          &
!!     &         (IO_param, nnod64, id_global, rr)
!!      subroutine gz_mpi_read_node_position                            &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!
!!      subroutine gz_mpi_write_radial_position                         &
!!     &         (IO_param, nnod64, id_global, rr)
!!      subroutine gz_mpi_write_node_position                           &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!@endverbatim
!
      module gz_MPI_position_IO
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
      type(buffer_4_gzip),  private :: zbuf
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_radial_position                            &
     &         (IO_param, nnod, id_r_global, rr)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(inout) :: id_r_global(nnod)
      real(kind = kreal), intent(inout) :: rr(nnod)
!
      integer(kind = kint_gl) :: nnod64
      integer(kind = kint_gl), allocatable :: idx_gl_tmp(:)
!
      nnod64 = nnod
      allocate(idx_gl_tmp(nnod64))
      call gz_mpi_read_node_position                                    &
     &   (IO_param, nnod64, ione, idx_gl_tmp, rr)
!
!$omp parallel workshare
      id_r_global(1:nnod64) = int(idx_gl_tmp(1:nnod64))
!$omp end parallel workshare
!
      deallocate(idx_gl_tmp)
!
      end subroutine gz_mpi_read_radial_position
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_node_position                              &
     &         (IO_param, nnod64, numdir, id_global, xx)
!
      use zlib_convert_ascii_vector
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: nnod64
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint_gl), intent(inout) :: id_global(nnod64)
      real(kind=kreal), intent(inout) :: xx(nnod64, numdir)
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
     &              - IO_param%istack_merged(IO_param%id_rank)
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
      call infleate_node_position(nnod64, numdir, id_global, xx, zbuf)
!
      end subroutine gz_mpi_read_node_position
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_radial_position                           &
     &         (IO_param, nnod, id_r_global, rr)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: id_r_global(nnod)
      real(kind = kreal), intent(in) :: rr(nnod)
!
      integer(kind = kint_gl) :: nnod64
      integer(kind = kint_gl), allocatable :: idx_gl_tmp(:)
!
      nnod64 = nnod
      allocate(idx_gl_tmp(nnod64))
!$omp parallel workshare
      idx_gl_tmp(1:nnod64) = id_r_global(1:nnod64)
!$omp end parallel workshare
!
      call gz_mpi_write_node_position                                   &
     &   (IO_param, nnod64, ione, idx_gl_tmp, rr(1))
      deallocate(idx_gl_tmp)
!
      end subroutine gz_mpi_write_radial_position
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_node_position                             &
     &         (IO_param, nnod64, numdir, id_global, xx)
!
      use zlib_convert_ascii_vector
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: nnod64
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint_gl), intent(in) :: id_global(nnod64)
      real(kind = kreal), intent(in) :: xx(nnod64, numdir)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call defleate_node_position(nnod64, numdir, id_global, xx, zbuf)
!
      call gz_mpi_write_stack_over_domain(IO_param, zbuf%ilen_gzipped)
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_gz(IO_param%id_file, ioffset, zbuf)
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_mpi_write_node_position
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_position_IO
