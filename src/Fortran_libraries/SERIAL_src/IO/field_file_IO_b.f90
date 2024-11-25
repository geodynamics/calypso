!>@file  field_file_IO_b.f90
!!       module field_file_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_field_file_b                              &
!!     &         (file_name, id_rank, t_IO, fld_IO, ierr)
!!        type(time_data), intent(in) :: t_IO
!!        type(field_IO), intent(in) :: fld_IO
!!
!!      subroutine read_step_field_file_b                               &
!!     &         (file_name, id_rank, t_IO, fld_IO, ierr)
!!      subroutine read_and_allocate_step_field_b                       &
!!     &         (file_name, id_rank, t_IO, fld_IO, ierr)
!!      subroutine read_and_allocate_step_head_b                        &
!!     &         (file_name, id_rank, t_IO, fld_IO, ierr)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module field_file_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_time_data
      use t_field_data_IO
      use t_binary_IO_buffer
      use field_data_IO_b
      use binary_IO
!
      implicit none
!
      integer(kind = kint), parameter :: id_read_fld =  21
      integer(kind = kint), parameter :: id_write_fld = 22
      type(binary_IO_buffer) :: bbuf_fld
      private :: id_read_fld, id_write_fld, bbuf_fld
!
      private :: read_and_allocate_step_b
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_step_field_file_b                                &
     &         (file_name, id_rank, t_IO, fld_IO, ierr)
!
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write binary data file: ', trim(file_name)
!
      bbuf_fld%id_binary = id_write_fld
      call open_write_binary_file(file_name, bbuf_fld)
      if(bbuf_fld%ierr_bin .gt. 0) go to 99
!
      call write_step_data_b(id_rank, t_IO, bbuf_fld)
      if(bbuf_fld%ierr_bin .gt. 0) go to 99
      call write_field_data_b                                           &
     &   (fld_IO%num_field_IO, fld_IO%fld_name, fld_IO%num_comp_IO,     &
     &    cast_long(fld_IO%nnod_IO), fld_IO%ntot_comp_IO, fld_IO%d_IO,  &
     &    bbuf_fld)
      if(bbuf_fld%ierr_bin .gt. 0) go to 99
!
  99  continue
      call close_binary_file(bbuf_fld)
      ierr = bbuf_fld%ierr_bin
!
      end subroutine write_step_field_file_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_step_field_file_b                                 &
     &         (file_name, id_rank, t_IO, fld_IO, ierr)
!
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary data file: ', trim(file_name)
!
      bbuf_fld%id_binary = id_read_fld
      call open_read_binary_file(file_name, id_rank, bbuf_fld)
      if(bbuf_fld%ierr_bin .ne. 0) goto 99
      call read_step_data_b(bbuf_fld, t_IO)
      if(bbuf_fld%ierr_bin .ne. 0) goto 99
      call read_field_num_b                                             &
     &   (bbuf_fld, istack_merged, fld_IO%num_field_IO)
      if(bbuf_fld%ierr_bin .ne. 0) goto 99
!
      call read_mul_integer_b                                           &
     &   (bbuf_fld, cast_long(fld_IO%num_field_IO), fld_IO%num_comp_IO)
      if(bbuf_fld%ierr_bin .gt. 0)  goto 99
!
      call read_field_data_b                                            &
     &   (bbuf_fld, fld_IO%num_field_IO, fld_IO%fld_name,               &
     &    cast_long(fld_IO%nnod_IO), fld_IO%ntot_comp_IO, fld_IO%d_IO)
!
  99  continue
      call close_binary_file(bbuf_fld)
      ierr = bbuf_fld%ierr_bin
!
      end subroutine read_step_field_file_b
!
! -----------------------------------------------------------------------
!
      subroutine read_and_allocate_step_field_b                         &
     &         (file_name, id_rank, t_IO, fld_IO, ierr)
!
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary data file: ', trim(file_name)
!
      bbuf_fld%id_binary = id_read_fld
      call open_read_binary_file(file_name, id_rank, bbuf_fld)
      if(bbuf_fld%ierr_bin .ne. 0) go to 99
      call read_and_allocate_step_b(bbuf_fld, t_IO, fld_IO)
      if(bbuf_fld%ierr_bin .ne. 0) go to 99
!
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data_b                                            &
     &   (bbuf_fld, fld_IO%num_field_IO, fld_IO%fld_name,               &
     &    cast_long(fld_IO%nnod_IO), fld_IO%ntot_comp_IO, fld_IO%d_IO)
!
  99  continue
      call close_binary_file(bbuf_fld)
      ierr = bbuf_fld%ierr_bin
!
      end subroutine read_and_allocate_step_field_b
!
! -----------------------------------------------------------------------
!
      subroutine read_and_allocate_step_head_b                          &
     &         (file_name, id_rank, t_IO, fld_IO, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary data file: ', trim(file_name)
!
      bbuf_fld%id_binary = id_read_fld
      call open_read_binary_file(file_name, id_rank, bbuf_fld)
      if(bbuf_fld%ierr_bin .ne. 0) goto 99
      call read_and_allocate_step_b(bbuf_fld, t_IO, fld_IO)
!
  99  continue
      call close_binary_file(bbuf_fld)
      ierr = bbuf_fld%ierr_bin
!
      end subroutine read_and_allocate_step_head_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_and_allocate_step_b(bbuf, t_IO, fld_IO)
!
      use transfer_to_long_integers
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      call read_step_data_b(bbuf, t_IO)
      if(bbuf%ierr_bin .ne. 0) return
      call read_field_num_b                                             &
     &   (bbuf, istack_merged, fld_IO%num_field_IO)
      fld_IO%nnod_IO = int(istack_merged(1), KIND(fld_IO%nnod_IO))
      if(bbuf%ierr_bin .ne. 0) return
!
      call alloc_phys_name_IO(fld_IO)
!
      call read_mul_integer_b                                           &
     &   (bbuf, cast_long(fld_IO%num_field_IO), fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      end subroutine read_and_allocate_step_b
!
! -----------------------------------------------------------------------
!
      end module field_file_IO_b
