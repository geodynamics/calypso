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
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module gz_MPI_sph_gl_1d_idx_IO
!
      use m_precision
      use m_constants
!
      use t_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use t_buffer_4_gzip
      use m_sph_modes_grid_labels
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use gz_MPI_position_IO
      use MPI_ascii_data_IO
!
      implicit none
!
      type(buffer_4_gzip) :: zbuf
!
      private :: gz_mpi_read_1d_gl_address, gz_mpi_write_1d_gl_address
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
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
      call gz_mpi_read_radial_position(IO_param,                        &
     &   sph_IO%nidx_sph(1), sph_IO%idx_gl_1, sph_IO%r_gl_1)
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
      call gz_mpi_read_radial_position(IO_param,                        &
     &   sph_IO%nidx_sph(1), sph_IO%idx_gl_1, sph_IO%r_gl_1)
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
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_rgrid()), hd_rgrid())
!
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ist_sph(1))
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ied_sph(1))
!
      call gz_mpi_write_num_of_data(IO_param, sph_IO%nidx_sph(1))
      call gz_mpi_write_radial_position(IO_param,                       &
     &   sph_IO%nidx_sph(1), sph_IO%idx_gl_1, sph_IO%r_gl_1)
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
      end subroutine gz_mpi_write_rtp_gl_1d_table
!
! ----------------------------------------------------------------------
!
      subroutine gz_mpi_write_rj_gl_1d_table(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_rgrid()), hd_rgrid())
!
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ist_sph(1))
      call gz_mpi_write_num_of_data(IO_param, sph_IO%ied_sph(1))
!
      call gz_mpi_write_num_of_data(IO_param, sph_IO%nidx_sph(1))
      call gz_mpi_write_radial_position(IO_param,                       &
     &    sph_IO%nidx_sph(1), sph_IO%idx_gl_1, sph_IO%r_gl_1)
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
      end subroutine gz_mpi_write_rj_gl_1d_table
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_1d_gl_address                              &
     &         (IO_param, nnod, numdir, idx)
!
      use zlib_cvt_ascii_ele_connect
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(inout) :: idx(nnod, numdir)
!
      integer(kind = kint_gl) :: num64
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
      num64 = nnod
      call infleate_1d_global_address(num64, numdir, idx, zbuf)
!
      end subroutine gz_mpi_read_1d_gl_address
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_1d_gl_address                             &
     &         (IO_param, nnod, numdir, idx)
!
      use zlib_cvt_ascii_ele_connect
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint), intent(in) :: idx(nnod, numdir)
!
      integer(kind = kint_gl) :: num64
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call gz_mpi_write_num_of_data(IO_param, nnod)
!
      num64 = nnod
      call defleate_1d_global_address(num64, numdir, idx, zbuf)
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
      end subroutine gz_mpi_write_1d_gl_address
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_sph_gl_1d_idx_IO
