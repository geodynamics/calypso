!>@file   gz_MPI_sph_gl_1d_idx_IO.f90
!!@brief  module gz_MPI_sph_gl_1d_idx_IO
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_mpi_read_rtp_gl_1d_table(IO_param, sph_IO)
!!      subroutine gz_mpi_read_rj_gl_1d_table(IO_param, sph_IO)
!!
!!      subroutine gz_mpi_write_rtp_gl_1d_table(IO_param, sph_IO)
!!      subroutine gz_mpi_write_rj_gl_1d_table(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!@endverbatim
!
      module gz_MPI_sph_gl_1d_idx_IO
!
      use m_precision
      use m_constants
!
      use t_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use m_sph_modes_grid_labels
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use gz_MPI_vectors_IO
      use MPI_ascii_data_IO
!
      implicit none
!
      integer(kind = kint_gl), allocatable :: idx_gl_tmp(:)
      private :: idx_gl_tmp
      private :: gz_mpi_read_1d_gl_address, gz_mpi_write_1d_gl_address
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------!
      subroutine gz_mpi_read_rtp_gl_1d_table(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      sph_IO%numdir_sph = 3
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 1
      sph_IO%ncomp_table_1d(3) = 2
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_rgrid()))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%ist_sph(1))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%ied_sph(1))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%nidx_sph(1))
!
      call alloc_idx_sph_1d1_IO(sph_IO)
!
      allocate(idx_gl_tmp(sph_IO%nidx_sph(1)))
      call gz_mpi_read_node_position(IO_param,                          &
     &   sph_IO%nidx_sph(1), sph_IO%ncomp_table_1d(1),                  &
     &   idx_gl_tmp, sph_IO%r_gl_1)
      sph_IO%idx_gl_1(1:sph_IO%nidx_sph(1))                             &
     &       = int(idx_gl_tmp(1:sph_IO%nidx_sph(1)))
      deallocate(idx_gl_tmp)
!
!
      call gz_mpi_skip_header(IO_param, len(hd_tgrid()))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%ist_sph(2))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%ied_sph(2))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%nidx_sph(2))
!
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      call gz_mpi_read_1d_gl_address(IO_param,                          &
     &   sph_IO%nidx_sph(2), sph_IO%ncomp_table_1d(2), sph_IO%idx_gl_2)
!
!
      call gz_mpi_skip_header(IO_param, len(hd_pgrid()))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%ist_sph(3))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%ied_sph(3))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%nidx_sph(3))
!
      call alloc_idx_sph_1d3_IO(sph_IO)
!
      call gz_mpi_read_1d_gl_address(IO_param,                          &
     &   sph_IO%nidx_sph(3), sph_IO%ncomp_table_1d(3), sph_IO%idx_gl_3)
!
      end subroutine gz_mpi_read_rtp_gl_1d_table
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_rj_gl_1d_table(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      sph_IO%numdir_sph = 2
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 3
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_rgrid()))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%ist_sph(1))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%ied_sph(1))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%nidx_sph(1))
!
      call alloc_idx_sph_1d1_IO(sph_IO)
!
      allocate(idx_gl_tmp(sph_IO%nidx_sph(1)))
      call gz_mpi_read_node_position(IO_param,                          &
     &   sph_IO%nidx_sph(1), sph_IO%ncomp_table_1d(1),                  &
     &   idx_gl_tmp, sph_IO%r_gl_1)
      sph_IO%idx_gl_1(1:sph_IO%nidx_sph(1))                             &
     &       = int(idx_gl_tmp(1:sph_IO%nidx_sph(1)))
      deallocate(idx_gl_tmp)
!
!
      call gz_mpi_skip_header(IO_param, len(hd_jmode()))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%ist_sph(2))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%ied_sph(2))
      call gz_mpi_read_num_of_data(IO_param, sph_IO%nidx_sph(2))
!
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      call gz_mpi_read_1d_gl_address(IO_param,                          &
     &   sph_IO%nidx_sph(2), sph_IO%ncomp_table_1d(2), sph_IO%idx_gl_2)
!
!
      end subroutine gz_mpi_read_rj_gl_1d_table
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_rtp_gl_1d_table(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_rgrid()), hd_rgrid())
!
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ist_sph(1))
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ied_sph(1))
!
      allocate(idx_gl_tmp(sph_IO%nidx_sph(1)))
      idx_gl_tmp(1:sph_IO%nidx_sph(1))                                  &
     &       =  sph_IO%idx_gl_1(1:sph_IO%nidx_sph(1))
      call gz_mpi_write_node_position(IO_param,                         &
     &   sph_IO%nidx_sph(1), ione, idx_gl_tmp, sph_IO%r_gl_1)
      deallocate(idx_gl_tmp)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_tgrid()), hd_tgrid())
!
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ist_sph(2))
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ied_sph(2))
!
      call gz_mpi_write_1d_gl_address(IO_param,                         &
     &    sph_IO%nidx_sph(2), sph_IO%ncomp_table_1d(2),                 &
     &    sph_IO%idx_gl_2)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_pgrid()), hd_pgrid())
!
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ist_sph(3))
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ied_sph(3))
!
      call gz_mpi_write_1d_gl_address(IO_param,                         &
     &    sph_IO%nidx_sph(3), sph_IO%ncomp_table_1d(3),                 &
     &    sph_IO%idx_gl_3)
!
!
      call dealloc_num_idx_sph_IO(sph_IO)
      call dealloc_idx_sph_1d1_IO(sph_IO)
      call dealloc_idx_sph_1d2_IO(sph_IO)
      call dealloc_idx_sph_1d3_IO(sph_IO)
!
      end subroutine gz_mpi_write_rtp_gl_1d_table
!
! ----------------------------------------------------------------------
!
      subroutine gz_mpi_write_rj_gl_1d_table(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_rgrid()), hd_rgrid())
!
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ist_sph(1))
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ied_sph(1))
!
      allocate(idx_gl_tmp(sph_IO%nidx_sph(1)))
      idx_gl_tmp(1:sph_IO%nidx_sph(1))                                  &
     &       =  sph_IO%idx_gl_1(1:sph_IO%nidx_sph(1))
      call gz_mpi_write_node_position(IO_param,                         &
     &    sph_IO%nidx_sph(1), sph_IO%ncomp_table_1d(1),                 &
     &    idx_gl_tmp, sph_IO%r_gl_1)
      deallocate(idx_gl_tmp)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_jmode()), hd_jmode())
!
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ist_sph(2))
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ied_sph(2))
!
      call gz_mpi_write_1d_gl_address(IO_param,                         &
     &    sph_IO%nidx_sph(2), sph_IO%ncomp_table_1d(2),                 &
     &    sph_IO%idx_gl_2)
!
      call dealloc_num_idx_sph_IO(sph_IO)
      call dealloc_idx_sph_1d1_IO(sph_IO)
      call dealloc_idx_sph_1d2_IO(sph_IO)
!
      end subroutine gz_mpi_write_rj_gl_1d_table
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_1d_gl_address                              &
     &         (IO_param, nnod, numdir, idx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint), intent(inout) :: idx(nnod, numdir)
!
      integer(kind = kint) :: idx_tmp(numdir)
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
      ilen_line = len_multi_int_textline(numdir)
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
        call read_multi_int_textline(textbuf(1), numdir, idx(1,1))
      else if(nnod .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_multi_int_textline(textbuf(1), numdir, idx_tmp)
        idx(1,1:numdir) = idx_tmp(1:numdir)
!
        do i = 2, nnod-1
          call gzip_infleat_cont                                        &
     &       (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
          call read_multi_int_textline(textbuf(1),  numdir, idx_tmp)
          idx(i,1:numdir) = idx_tmp(1:numdir)
        end do
!
        call gzip_infleat_last                                          &
     &     (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
        call read_multi_int_textline(textbuf(1), numdir, idx_tmp)
        idx(nnod,1:numdir) = idx_tmp(1:numdir)
      end if
!
      deallocate(gzip_buf, textbuf)
!
      end subroutine gz_mpi_read_1d_gl_address
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_1d_gl_address                             &
     &         (IO_param, nnod, numdir, idx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint), intent(in) :: idx(nnod, numdir)
!
      integer(kind = kint) :: i
      integer(kind = kint) :: idx_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      call gz_mpi_write_num_of_data(IO_param, nnod)
!
      ilen_line = len_multi_int_textline(numdir)
      ilen_gz = int(real(nnod*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nnod .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      multi_int_textline(numdir, idx(1,1)),                       &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .gt. 0) then
        idx_tmp(1:numdir) = idx(1,1:numdir)
        call gzip_defleat_begin(ilen_line,                              &
     &     multi_int_textline(numdir, idx_tmp),                         &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nnod - 1
          idx_tmp(1:numdir) = idx(i,1:numdir)
          call gzip_defleat_cont(ilen_line,                             &
     &     multi_int_textline(numdir, idx_tmp),                         &
     &        ilen_gz, ilen_gzipped)
        end do
        idx_tmp(1:numdir) = idx(nnod,1:numdir)
        call gzip_defleat_last(ilen_line,                               &
     &     multi_int_textline(numdir, idx_tmp),                         &
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
      end subroutine gz_mpi_write_1d_gl_address
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_sph_gl_1d_idx_IO
