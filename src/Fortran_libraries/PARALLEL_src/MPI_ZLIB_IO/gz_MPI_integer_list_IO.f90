!>@file  gz_MPI_integer_list_IO.f90
!!       module gz_MPI_integer_list_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine gz_mpi_read_ele_connect                              &
!!     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!!      subroutine gz_mpi_read_element_type                             &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine gz_mpi_read_int_list(IO_param, nele, ncomp, ivect)
!!
!!      subroutine gz_mpi_write_ele_connect                             &
!!     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!!      subroutine gz_mpi_write_element_type                            &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine gz_mpi_write_int_list(IO_param, nele, ncomp, ivect)
!!@endverbatim
!
      module gz_MPI_integer_list_IO
!
      use m_precision
!
      use t_calypso_mpi_IO_param
      use t_buffer_4_gzip
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use data_IO_to_textline
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
      subroutine gz_mpi_read_ele_connect                               &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      use zlib_cvt_ascii_ele_connect
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(inout) :: id_global(nele)
      integer(kind=kint), intent(inout) :: ie(nele, nnod_4_ele)
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
      num64 = nele
      call infleate_ele_connect                                         &
     &   (num64, nnod_4_ele, id_global, ie, zbuf)
!
      end subroutine gz_mpi_read_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_element_type                               &
     &         (IO_param, ncolumn, num, int_dat)
!
      use zlib_cvt_ascii_comm_tbl
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: ncolumn
      integer(kind=kint), intent(in) :: num
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
      if(zbuf%ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      call alloc_zip_buffer(zbuf)
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
      num64 = num
      call infleate_element_type(ncolumn, num64, int_dat, zbuf)
!
      end subroutine gz_mpi_read_element_type
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_list(IO_param, nele, ncomp, ivect)
!
      use zlib_cvt_ascii_ele_connect
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, ncomp
      integer(kind=kint), intent(inout) :: ivect(nele, ncomp)
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
      if(zbuf%ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      call alloc_zip_buffer(zbuf)
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
      num64 = nele
      call infleate_ele_int_list(num64, ncomp, ivect, zbuf)
!
      end subroutine gz_mpi_read_int_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_ele_connect                               &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      use zlib_cvt_ascii_ele_connect
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele
      integer(kind=kint), intent(in) :: nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: num64
!
!
      num64 = nele
      call defleate_ele_connect(num64, nnod_4_ele, id_global, ie, zbuf)
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
      end subroutine gz_mpi_write_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_element_type                              &
     &         (IO_param, ncolumn, num, int_dat)
!
      use zlib_cvt_ascii_comm_tbl
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: ncolumn
      integer(kind = kint), intent(in) :: num
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: num64
!
!
      num64 = num
      call defleate_element_type(ncolumn, num64, int_dat, zbuf)
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
      end subroutine gz_mpi_write_element_type
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_list(IO_param, nele, ncomp, ivect)
!
      use zlib_cvt_ascii_ele_connect
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: nele
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: ivect(nele,ncomp)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: num64
!
!
      num64 = nele
      call defleate_ele_int_list(num64, ncomp, ivect, zbuf)
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
      end subroutine gz_mpi_write_int_list
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_integer_list_IO
