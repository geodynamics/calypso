!>@file  gz_assembled_field_MPI_IO.f90
!!       module gz_assembled_field_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief gzipped data IO for 
!!
!!@verbatim
!!      subroutine gz_write_step_asbl_fld_mpi                           &
!!     &         (file_name, nprocs_in, nloop, fld_IO, t_IO)
!!      subroutine gz_write_step_asbl_fld_mpi_b                         &
!!     &         (file_name, nprocs_in, id_rank, nloop, fld_IO, t_IO)
!!        type(field_IO), intent(in) :: fld_IO(nloop)
!!        type(time_data), intent(in) :: t_IO
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
!!
!!   Data format for the merged binary field data
!!     1.   Number of process
!!     2.   Time step
!!     3.   Time, Delta t
!!     4.   Stacks of numbe of data points
!!     5.   Number of fields
!!     6.   List of number of components
!!     7.   Field names
!!     8.   List of data size (Byte)
!!     9.   All Field data
!!@endverbatim
!
      module gz_assembled_field_MPI_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_time_data
      use t_field_data_IO
      use t_calypso_mpi_IO_param
      use m_calypso_mpi_IO
!
      implicit none
!
!>  Structure for zlib data IO
      type mul_zlib_buffers
!>  Original data size
        integer(kind = kint) :: ilen_gz
!>  Data size for compressed data
        integer(kind = kint) :: len_gzipped
!>  Data buffer for zlib IO
        character(len = 1), allocatable :: buffer(:)
      end type mul_zlib_buffers
!
      type(mul_zlib_buffers), allocatable, save :: gz_bufs(:)
!
      private :: gz_bufs
      private :: alloc_assemble_gz_buffer, dealloc_assemble_gz_buffer
      private :: gz_write_asmbl_fld_mpi, gz_write_asmbl_fld_mpi_b
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_assemble_gz_buffer(nloop)
!
      integer(kind = kint), intent(in) :: nloop
!
!
      allocate(gz_bufs(nloop))
!
      end subroutine alloc_assemble_gz_buffer
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_assemble_gz_buffer
!
      deallocate(gz_bufs)
!
      end subroutine dealloc_assemble_gz_buffer
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_step_asbl_fld_mpi                             &
     &         (file_name, nprocs_in, nloop, fld_IO, t_IO)
!
      use field_data_IO
      use gz_field_file_MPI_IO
      use gz_field_data_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nprocs_in
!
      integer(kind = kint), intent(in) :: nloop
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO(nloop)
!
      integer ::  id_fld
!
      integer(kind = kint_gl) :: ioff_gl
      integer(kind = kint) :: icou, j
!
!
      call alloc_assemble_gz_buffer(nloop)
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &      'Write compressed data by MPI-IO: ', trim(file_name)
      call calypso_mpi_write_file_open(file_name, nprocs_in, id_fld)
!
      ioff_gl = 0
      call write_field_head_gz_mpi                                      &
     &   (id_fld, nprocs_in, ioff_gl, t_IO, fld_IO(1)%num_field_IO,     &
     &    fld_IO(1)%num_comp_IO, fld_IO(1)%istack_numnod_IO)
!
      icou = 1
      do j = 1, fld_IO(1)%num_field_IO
        call gz_write_fld_header_mpi(id_fld, ioff_gl,                   &
     &     each_field_name_buffer(fld_IO(1)%fld_name(j)))
        call gz_write_asmbl_fld_mpi(id_fld, nprocs_in, ioff_gl,         &
     &      icou, fld_IO(1)%num_comp_IO(j), nloop, fld_IO, gz_bufs)
        icou = icou + fld_IO(1)%num_comp_IO(j)
      end do
!
      call calypso_close_mpi_file(id_fld)
      call dealloc_assemble_gz_buffer
!
      end subroutine gz_write_step_asbl_fld_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_step_asbl_fld_mpi_b                           &
     &         (file_name, nprocs_in, id_rank, nloop, fld_IO, t_IO)
!
      use gz_field_file_MPI_IO_b
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer(kind = kint), intent(in) :: nloop
      type(field_IO), intent(in) :: fld_IO(nloop)
      type(time_data), intent(in) :: t_IO
!
      type(calypso_MPI_IO_params) :: IO_param
!
!
      call alloc_assemble_gz_buffer(nloop)
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Write compressed binary data by MPI-IO: ', trim(file_name)
      call open_write_gz_mpi_file_b                                     &
     &   (file_name, nprocs_in, id_rank, IO_param)
!
      call gz_write_field_head_mpi_b(IO_param,                          &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt,                         &
     &    fld_IO(1)%num_field_IO, fld_IO(1)%num_comp_IO,                &
     &    fld_IO(1)%istack_numnod_IO)
!
      call gz_mpi_write_mul_charahead_b                                 &
     &   (IO_param, fld_IO(1)%num_field_IO, fld_IO(1)%fld_name)
      call gz_write_asmbl_fld_mpi_b                                     &
     &   (IO_param, nloop, fld_IO, gz_bufs)
!
      call close_mpi_file(IO_param)
      call dealloc_assemble_gz_buffer
!
      end subroutine gz_write_step_asbl_fld_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_asmbl_fld_mpi(id_mpi_file, nprocs_in,         &
     &          ioff_gl, ist_fld, ndir, nloop, fld_IO, gz_bufs)
!
      use field_data_IO
      use m_calypso_mpi_IO
      use gz_field_file_MPI_IO
      use gz_field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: ist_fld, ndir
!
      integer(kind = kint), intent(in) :: nloop
      type(field_IO), intent(in), target :: fld_IO(nloop)
      type(mul_zlib_buffers), intent(inout) :: gz_bufs(nloop)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer, intent(in) ::  id_mpi_file
!
      real(kind = kreal), pointer :: vector(:,:)
      real(kind = kreal) :: v1(ndir)
      integer(kind = kint_gl) :: istack_gz_pe(0:nprocs_in)
      integer(kind = kint) :: len_gz_pe(nprocs_in)
      integer(kind = kint) :: len_gz_lc(nprocs_in)
!
      integer(kind = kint) :: id_rank
      integer(kind = kint) :: iloop, ip
      integer(kind = kint) :: ilength
!
!
      len_gz_lc(1:nprocs_in) = 0
      v1(1:ndir) = 0.0d0
      ilength = len_each_field_data_buf(ndir)
!
!        deflate data
      do iloop = 1, nloop
        id_rank = my_rank + (iloop-1) * nprocs
 !
        if(id_rank .lt. nprocs_in) then
          gz_bufs(iloop)%ilen_gz                                        &
     &       = int(real(fld_IO(iloop)%nnod_IO*ilength) * 1.01) + 24
          allocate(gz_bufs(iloop)%buffer(gz_bufs(iloop)%ilen_gz))
 !
          vector => fld_IO(iloop)%d_IO(:,ist_fld:ist_fld+ndir-1)
          gz_bufs(iloop)%len_gzipped = gz_defleat_vector_txt            &
     &                 (fld_IO(iloop)%nnod_IO, ndir, vector, ilength,   &
     &                  gz_bufs(iloop)%ilen_gz, gz_bufs(iloop)%buffer)
          len_gz_lc(id_rank+1) =       gz_bufs(iloop)%len_gzipped
        else
          gz_bufs(iloop)%ilen_gz = 0
          gz_bufs(iloop)%len_gzipped = 0
          allocate(gz_bufs(iloop)%buffer(gz_bufs(iloop)%ilen_gz))
        end if
      end do
!
!        Count data size
      len_gz_pe(1:nprocs_in) = 0
      call MPI_allREDUCE(len_gz_lc, len_gz_pe, nprocs_in,               &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      istack_gz_pe(0) = 0
      do ip = 1, nprocs_in
        istack_gz_pe(ip) = istack_gz_pe(ip-1) + len_gz_pe(ip)
      end do
!
!       Write buffer size
      call gz_write_fld_header_mpi(id_mpi_file, ioff_gl,                &
     &     buffer_istack_nod_buffer(nprocs_in, istack_gz_pe))
!
!       Write to file
      do iloop = 1, nloop
        id_rank = my_rank + (iloop-1) * nprocs
        if(id_rank .lt. nprocs_in) then
          ioffset = int(ioff_gl) + istack_gz_pe(id_rank)
          call calypso_mpi_seek_write_chara (id_mpi_file, ioffset,      &
     &       gz_bufs(iloop)%len_gzipped, gz_bufs(iloop)%buffer(1))
        end if
        deallocate(gz_bufs(iloop)%buffer)
      end do
      ioff_gl = ioff_gl + istack_gz_pe(nprocs_in)
!
      end subroutine gz_write_asmbl_fld_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_asmbl_fld_mpi_b                               &
     &         (IO_param, nloop, fld_IO, gz_bufs)
!
      use field_data_IO
      use m_calypso_mpi_IO
      use gz_field_file_MPI_IO_b
      use gz_MPI_binary_head_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: nloop
      type(field_IO), intent(in) :: fld_IO(nloop)
      type(mul_zlib_buffers), intent(inout) :: gz_bufs(nloop)
!
      integer(kind = kint_gl) :: istack_gz_pe(0:IO_param%nprocs_in)
      integer(kind = kint) :: len_gz_pe(IO_param%nprocs_in)
      integer(kind = kint) :: len_gz_lc(IO_param%nprocs_in)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer(kind = kint) :: id_rank
      integer(kind = kint) :: iloop, ip, ilength
!
!
      len_gz_lc(1:IO_param%nprocs_in) = 0
!
!        deflate data
      do iloop = 1, nloop
        id_rank = my_rank + (iloop-1) * nprocs
 !
        ilength = fld_IO(iloop)%ntot_comp_IO * fld_IO(iloop)%nnod_IO    &
     &            * kreal
        if(id_rank .lt. IO_param%nprocs_in) then
          gz_bufs(iloop)%ilen_gz                                        &
     &       = int(real(ilength) * 1.01) + 24
          allocate(gz_bufs(iloop)%buffer(gz_bufs(iloop)%ilen_gz))
 !
          call gzip_defleat_once                                        &
     &       (ilength, fld_IO(iloop)%d_IO, gz_bufs(iloop)%ilen_gz,      &
     &        gz_bufs(iloop)%len_gzipped, gz_bufs(iloop)%buffer)
          len_gz_lc(id_rank+1) =       gz_bufs(iloop)%len_gzipped
        else
          gz_bufs(iloop)%ilen_gz = 0
          gz_bufs(iloop)%len_gzipped = 0
          allocate(gz_bufs(iloop)%buffer(gz_bufs(iloop)%ilen_gz))
        end if
      end do
!
!        Count data size
      len_gz_pe(1:IO_param%nprocs_in) = 0
      call MPI_allREDUCE(len_gz_lc, len_gz_pe, IO_param%nprocs_in,      &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      istack_gz_pe(0) = 0
      do ip = 1, IO_param%nprocs_in
        istack_gz_pe(ip) = istack_gz_pe(ip-1) + len_gz_pe(ip)
      end do
!
!       Write data size
      call gz_mpi_write_i8stack_head_b                                  &
     &   (IO_param, IO_param%nprocs_in, istack_gz_pe)
!
!       Write to file
      do iloop = 1, nloop
        id_rank = my_rank + (iloop-1) * nprocs
        if(id_rank .lt. IO_param%nprocs_in) then
          ioffset = int(IO_param%ioff_gl) + istack_gz_pe(id_rank)
          call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,  &
     &       gz_bufs(iloop)%len_gzipped, gz_bufs(iloop)%buffer(1))
        end if
        deallocate(gz_bufs(iloop)%buffer)
      end do
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + istack_gz_pe(IO_param%nprocs_in)
!
      end subroutine gz_write_asmbl_fld_mpi_b
!
! -----------------------------------------------------------------------
!
      end module gz_assembled_field_MPI_IO
