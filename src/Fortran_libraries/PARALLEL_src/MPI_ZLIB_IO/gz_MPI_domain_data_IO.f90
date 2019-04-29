!>@file   gz_MPI_domain_data_IO.f90
!!@brief  module gz_MPI_domain_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine gz_mpi_read_domain_info(IO_param, comm_IO)
!!      subroutine gz_mpi_read_import_data(IO_param, comm_IO)
!!      subroutine gz_mpi_read_export_data(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine gz_mpi_write_domain_info(IO_param, comm_IO)
!!      subroutine gz_mpi_write_import_data(IO_param, comm_IO)
!!      subroutine gz_mpi_write_export_data(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine gz_mpi_read_int_stack(IO_param, num, istack, ntot)
!!      subroutine gz_mpi_read_comm_table                               &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine gz_mpi_write_int_stack(IO_param, num, istack)
!!      subroutine gz_mpi_write_comm_table                              &
!!     &         (IO_param, ncolumn, num, int_dat)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module gz_MPI_domain_data_IO
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_calypso_mpi_IO_param
      use t_buffer_4_gzip
      use gz_MPI_ascii_data_IO
      use data_IO_to_textline
!
      implicit none
!
      private :: gz_mpi_write_int_vector, gz_mpi_read_int_vector
!
      type(buffer_4_gzip), private :: zbuf
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_domain_info(IO_param, comm_IO)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: nprocs_read
!
!
      call read_integer_textline                                        &
     &   (gz_mpi_read_charahead(IO_param, len_int_txt), nprocs_read)
      if(nprocs_read .ne. IO_param%nprocs_in) then
        call calypso_mpi_abort(ierr_file, '#. of subdmain is wrong')
      end if
!
      call gz_mpi_read_num_of_data(IO_param, comm_IO%num_neib)
      call alloc_neighbouring_id(comm_IO)
!
      call gz_mpi_read_int_vector                                       &
     &   (IO_param, comm_IO%num_neib, comm_IO%id_neib)
!
      end subroutine gz_mpi_read_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_import_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: num_tmp
!
!
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
      call alloc_import_num(comm_IO)
!
      call gz_mpi_read_int_stack(IO_param, comm_IO%num_neib,            &
     &    comm_IO%istack_import, comm_IO%ntot_import)
!
      call gz_mpi_read_num_of_data(IO_param, comm_IO%ntot_import)
      call alloc_import_item(comm_IO)
!
      call gz_mpi_read_comm_table                                       &
     &   (IO_param, ione, comm_IO%ntot_import, comm_IO%item_import)
!
      end subroutine gz_mpi_read_import_data
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_export_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: num_tmp
!
!
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
      call alloc_export_num(comm_IO)
!
      call gz_mpi_read_int_stack(IO_param, comm_IO%num_neib,            &
     &    comm_IO%istack_export, comm_IO%ntot_export)
!
      call gz_mpi_read_num_of_data(IO_param, comm_IO%ntot_import)
      call alloc_export_item(comm_IO)
!
      call gz_mpi_read_comm_table                                       &
     &   (IO_param, ione, comm_IO%ntot_export, comm_IO%item_export)
!
      end subroutine gz_mpi_read_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_domain_info(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
      integer(kind = kint) :: num_tmp
!
!
      num_tmp = int(IO_param%nprocs_in,KIND(num_tmp))
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(num_tmp))
!
      call gz_mpi_write_int_vector                                      &
     &   (IO_param, comm_IO%num_neib, comm_IO%id_neib)
!
      end subroutine gz_mpi_write_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_import_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
!
      call gz_mpi_write_int_stack                                       &
     &   (IO_param, comm_IO%num_neib, comm_IO%istack_import)
!
      call gz_mpi_write_comm_table                                      &
     &   (IO_param, ione, comm_IO%ntot_import, comm_IO%item_import)
!
      end subroutine gz_mpi_write_import_data
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_export_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
!
      call gz_mpi_write_int_stack                                       &
     &   (IO_param, comm_IO%num_neib, comm_IO%istack_export)
!
      call gz_mpi_write_comm_table                                      &
     &   (IO_param, ione, comm_IO%ntot_export, comm_IO%item_export)
!
      end subroutine gz_mpi_write_export_data
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_stack(IO_param, num, istack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(in) :: istack(0:num)
!
!
      if(num .gt. 0) call gz_mpi_write_int_vector                       &
     &                  (IO_param, num, istack(1))
!
      end subroutine gz_mpi_write_int_stack
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_vector(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(in) :: int_dat(num)
!
!
      call gz_mpi_write_num_of_data(IO_param, num)
      call gz_mpi_write_characters                                      &
     &   (IO_param, len_multi_int_textline(num),                        &
     &    multi_int_textline(num, int_dat))
!
      end subroutine gz_mpi_write_int_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_stack(IO_param, num, istack, ntot)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: istack(0:num)
      integer(kind=kint), intent(inout) :: ntot
!
!
      istack(0) = 0
      call gz_mpi_read_int_vector(IO_param, num, istack(1))
      ntot = istack(num)
!
      end subroutine gz_mpi_read_int_stack
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_vector(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer :: ilength
!
!
      call read_byte_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_byte_stack_textline(IO_param%nprocs_in)),               &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
!
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        ilength = len_multi_int_textline(num)
        call read_multi_int_textline                                    &
     &     (gz_mpi_read_characters(IO_param, ilength),                  &
     &      num, int_dat)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_read_int_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_comm_table                                 &
     &         (IO_param, ncolumn, num, int_dat)
!
      use zlib_cvt_ascii_comm_tbl
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: ncolumn
      integer(kind = kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: num64
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
      if(IO_param%id_rank .ge. IO_param%nprocs_in) then
        if(num .gt. 0)  int_dat = 0
        return
      end if
!
      if(zbuf%ilen_gz .le. 0) return
      call alloc_zip_buffer(zbuf)
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
      num64 = num
      call infleate_comm_table(ncolumn, num64, int_dat, zbuf)
!
      end subroutine gz_mpi_read_comm_table
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_comm_table                                &
     &         (IO_param, ncolumn, num, int_dat)
!
      use zlib_cvt_ascii_comm_tbl
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: ncolumn
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: num64
!
!
      call gz_mpi_write_num_of_data(IO_param, num)
!
      num64 = num
      call defleate_comm_table(ncolumn, num64, int_dat, zbuf)
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
      end subroutine gz_mpi_write_comm_table
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_domain_data_IO
