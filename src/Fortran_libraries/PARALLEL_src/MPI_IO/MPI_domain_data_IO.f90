!>@file   MPI_domain_data_IO.f90
!!@brief  module MPI_domain_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine mpi_check_num_of_domains(IO_param)
!!      subroutine mpi_read_domain_info(IO_param, comm_IO)
!!      subroutine mpi_read_import_data(IO_param, comm_IO)
!!      subroutine mpi_read_export_data(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine mpi_write_domain_info(IO_param, comm_IO)
!!      subroutine mpi_write_import_data(IO_param, comm_IO)
!!      subroutine mpi_write_export_data(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine mpi_read_int_stack(IO_param, num, istack, ntot)
!!      subroutine mpi_read_int_items(IO_param, ncolumn, num, int_dat)
!!      subroutine mpi_write_int_stack(IO_param, num, istack)
!!      subroutine mpi_write_int_items(IO_param, ncolumn, num, int_dat)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!
!!      subroutine mpi_write_int_vector(IO_param, num, int_dat)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!      subroutine mpi_read_int_vector(IO_param, num, int_dat)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module MPI_domain_data_IO
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_calypso_mpi_IO_param
      use MPI_binary_head_IO
      use MPI_binary_data_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
!
      implicit none
!
      private :: mpi_read_int_vector, mpi_write_int_vector
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_check_num_of_domains(IO_param)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: nprocs_read
!
!
      call read_integer_textline                                        &
     &   (mpi_read_charahead(IO_param, len_int_txt), nprocs_read)
      if(nprocs_read .ne. IO_param%nprocs_in) then
        call calypso_mpi_abort(ierr_file, '#. of subdmain is wrong')
      end if
!
      end subroutine mpi_check_num_of_domains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_domain_info(IO_param, comm_IO)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call mpi_check_num_of_domains(IO_param)
!
      call mpi_read_num_of_data(IO_param, comm_IO%num_neib)
      call alloc_neighbouring_id(comm_IO)
!
      call mpi_read_int_vector                                          &
     &   (IO_param, comm_IO%num_neib, comm_IO%id_neib)
!
      end subroutine mpi_read_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_import_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: num_tmp
!
!
      call mpi_read_num_of_data(IO_param, num_tmp)
      call alloc_import_num(comm_IO)
!
      call mpi_read_int_stack(IO_param, comm_IO%num_neib,               &
     &    comm_IO%istack_import, comm_IO%ntot_import)
!
      call mpi_read_num_of_data(IO_param, comm_IO%ntot_import)
      call alloc_import_item(comm_IO)
!
      call mpi_read_int_items                                           &
     &   (IO_param, ione, comm_IO%ntot_import, comm_IO%item_import)
!
      end subroutine mpi_read_import_data
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_export_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: num_tmp
!
!
      call mpi_read_num_of_data(IO_param, num_tmp)
      call alloc_export_num(comm_IO)
!
      call mpi_read_int_stack(IO_param, comm_IO%num_neib,               &
     &    comm_IO%istack_export, comm_IO%ntot_export)
!
      call mpi_read_num_of_data(IO_param, comm_IO%ntot_export)
      call alloc_export_item(comm_IO)
!
      call mpi_read_int_items                                           &
     &     (IO_param, ione, comm_IO%ntot_export, comm_IO%item_export)
!
      end subroutine mpi_read_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_domain_info(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
      integer(kind = kint) :: nlength
!
!
      nlength = int(IO_param%nprocs_in,KIND(nlength))
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(nlength))
!
      call mpi_write_int_vector                                         &
     &   (IO_param, comm_IO%num_neib, comm_IO%id_neib)
!
      end subroutine mpi_write_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_import_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
!
      call mpi_write_int_stack                                          &
     &   (IO_param, comm_IO%num_neib, comm_IO%istack_import)
      call mpi_write_int_items                                          &
     &   (IO_param, ione, comm_IO%ntot_import, comm_IO%item_import)
!
      end subroutine mpi_write_import_data
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_export_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
!
      call mpi_write_int_stack                                          &
     &   (IO_param, comm_IO%num_neib, comm_IO%istack_export)
      call mpi_write_int_items                                          &
     &   (IO_param, ione, comm_IO%ntot_export, comm_IO%item_export)
!
      end subroutine mpi_write_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int_stack(IO_param, num, istack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(in) :: istack(0:num)
!
!
      if(num .gt. 0) call mpi_write_int_vector                          &
     &                  (IO_param, num, istack(1))
!
      end subroutine mpi_write_int_stack
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int_vector(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nlength
      integer :: ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call mpi_write_num_of_data(IO_param, num)
!
      ilength = len_multi_int_textline(num)
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        nlength = int(IO_param%istack_merged(i),KIND(nlength))
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &                             + len_multi_int_textline(nlength)
      end do
!
      call mpi_write_charahead(IO_param,                                &
     &    len_byte_stack_textline(IO_param%nprocs_in),                  &
     &    byte_stack_textline(IO_param%nprocs_in,                       &
     &                        IO_param%istack_merged))
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
        call mpi_write_one_chara_b(IO_param%id_file, ioffset,           &
     &      ilength, multi_int_textline(num, int_dat))
!
      end subroutine mpi_write_int_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int_stack(IO_param, num, istack, ntot)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: istack(0:num)
      integer(kind=kint), intent(inout) :: ntot
!
      integer(kind = kint) :: ist
!
!
      istack(0) = 0
      ist = min(1,num)
      call mpi_read_int_vector(IO_param, num, istack(ist))
      ntot = istack(num)
!
      end subroutine mpi_read_int_stack
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int_vector(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: i, nlength
      integer ::  ilength
!
!
      call mpi_skip_read                                               &
     &   (IO_param, len_byte_stack_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        nlength = int(IO_param%istack_merged(i),KIND(nlength))
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &                             + len_multi_int_textline(nlength)
      end do
!
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        ilength = len_multi_int_textline(num)
        call read_multi_int_textline                                    &
     &     (mpi_read_characters(IO_param, ilength), num, int_dat)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_read_int_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int_items(IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, n_item, led, loop
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
!
      call mpi_skip_read                                                &
     &   (IO_param, len_byte_stack_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        n_item = int(IO_param%istack_merged(i))
        if(n_item .le. 0) then
          led = ione
        else if(n_item .gt. 0) then
          nrest = mod((n_item-1),ncolumn) + 1
          loop = (n_item-1)/ncolumn
          led = len_multi_int_textline(nrest)                           &
     &         + len_multi_int_textline(ncolumn) * loop
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1) + led
      end do
!
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        if(num .le. 0) then
          led = ione
        else if(num .gt. 0) then
          ioffset = IO_param%ioff_gl                                    &
     &           + IO_param%istack_merged(IO_param%id_rank)
!
          do i = 0, (num-1)/ncolumn - 1
            ilength = len_multi_int_textline(ncolumn)
            call read_multi_int_textline                                &
     &         (calypso_mpi_seek_read_chara(IO_param%id_file,           &
     &                                      ioffset, ilength),          &
     &          ncolumn, int_dat(ncolumn*i+1))
          end do
          nrest = mod((num-1),ncolumn) + 1
          ilength = len_multi_int_textline(nrest)
          call read_multi_int_textline                                  &
     &       (calypso_mpi_seek_read_chara(IO_param%id_file,             &
     &                                    ioffset, ilength),            &
     &        nrest, int_dat(num-nrest+1))
        end if
      else
        int_dat = 0
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_read_int_items
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int_items(IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint_gl) :: led
      integer(kind = kint) :: i, nrest, loop
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call mpi_write_num_of_data(IO_param, num)
!
      if(num .le. 0) then
        led = ione
      else if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_int_textline(ncolumn) * loop                    &
     &       + len_multi_int_textline(nrest)
      end if
!
      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(num .le. 0) then
        call mpi_write_one_chara_b                                      &
     &     (IO_param%id_file, ioffset, 1, char(10))
      else
        do i = 0, (num-1)/ncolumn - 1
          call mpi_write_one_chara_b(IO_param%id_file, ioffset,         &
     &        len_multi_int_textline(ncolumn),                          &
     &        multi_int_textline(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call mpi_write_one_chara_b(IO_param%id_file, ioffset,           &
     &      len_multi_int_textline(nrest),                              &
     &      multi_int_textline(nrest, int_dat(num-nrest+1)))
      end if
!
      end subroutine mpi_write_int_items
!
! -----------------------------------------------------------------------
!
      end module MPI_domain_data_IO
