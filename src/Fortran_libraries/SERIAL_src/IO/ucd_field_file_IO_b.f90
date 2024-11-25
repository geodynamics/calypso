!>@file  ucd_field_file_IO_b.f90
!!       module ucd_field_file_IO_b
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief binary format data IO
!!
!!@verbatim
!!      subroutine write_ucd_2_fld_file_b                               &
!!     &         (id_rank, file_name, t_IO, ucd, ierr)
!!
!!      subroutine read_ucd_2_fld_file_b                                &
!!     &         (id_rank, file_name, t_IO, ucd, ierr)
!!      subroutine read_alloc_ucd_2_fld_file_b                          &
!!     &         (id_rank, file_name, t_IO, ucd, ierr)
!!
!!      subroutine read_alloc_ucd_2_fld_header_b                        &
!!     &         (id_rank, file_name, t_IO, ucd, ierr)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param id_rank      process ID
!!@param file_name    file name
!!@param ucd          Structure for FEM field data IO
!
      module ucd_field_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use m_field_file_format
!
      use t_time_data
      use t_ucd_data
      use t_binary_IO_buffer
      use set_ucd_file_names
      use field_data_IO_b
      use binary_IO
      use transfer_to_long_integers
!
      implicit none
!
      integer(kind = kint), parameter :: id_read_fld =  21
      integer(kind = kint), parameter :: id_write_fld = 22
      type(binary_IO_buffer) :: bbuf_ucd
      private :: id_read_fld, id_write_fld, bbuf_ucd
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_ucd_2_fld_file_b                                 &
     &         (id_rank, file_name, t_IO, ucd, ierr)
!
      character(len=kchara), intent(in)  :: file_name
      integer, intent(in) :: id_rank
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
      integer(kind=kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write binary data file: ', trim(file_name)
!
      bbuf_ucd%id_binary = id_write_fld
      call open_write_binary_file(file_name, bbuf_ucd)
      if(bbuf_ucd%ierr_bin .gt. 0) go to 99
!
      call write_step_data_b(id_rank, t_IO, bbuf_ucd)
      if(bbuf_ucd%ierr_bin .gt. 0) go to 99
      call write_field_data_b                                           &
     &   (ucd%num_field, ucd%phys_name, ucd%num_comp,                   &
     &    ucd%nnod, ucd%ntot_comp, ucd%d_ucd, bbuf_ucd)
      if(bbuf_ucd%ierr_bin .gt. 0) go to 99
!
      99  continue
      call close_binary_file(bbuf_ucd)
      ierr = bbuf_ucd%ierr_bin
!
      end subroutine write_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_ucd_2_fld_file_b                                  &
     &         (id_rank, file_name, t_IO, ucd, ierr)
!
      character(len=kchara), intent(in)  :: file_name
      integer, intent(in) :: id_rank
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      integer(kind=kint), intent(inout) :: ierr
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      bbuf_ucd%id_binary = id_read_fld
      call open_read_binary_file(file_name, id_rank, bbuf_ucd)
      if(bbuf_ucd%ierr_bin .ne. 0) goto 99
      call read_step_data_b(bbuf_ucd, t_IO)
      if(bbuf_ucd%ierr_bin .ne. 0) go to 99
      call read_field_num_b                                             &
     &   (bbuf_ucd, istack_merged, ucd%num_field)
      ucd%nnod = istack_merged(1)
      if(bbuf_ucd%ierr_bin .ne. 0) go to 99
!
      call read_mul_integer_b                                           &
     &   (bbuf_ucd, cast_long(ucd%num_field), ucd%num_comp)
      if(bbuf_ucd%ierr_bin .gt. 0) go to 99
      call read_field_data_b                                            &
     &   (bbuf_ucd, ucd%num_field, ucd%phys_name,                       &
     &    ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
      if(bbuf_ucd%ierr_bin .ne. 0) go to 99
!
      call close_binary_file(bbuf_ucd)
      ierr = 0
      return

  99  continue
      ierr = bbuf_ucd%ierr_bin
      return
!
      end subroutine read_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_fld_file_b                            &
     &         (id_rank, file_name, t_IO, ucd, ierr)
!
      character(len=kchara), intent(in)  :: file_name
      integer, intent(in) :: id_rank
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      integer(kind=kint), intent(inout) :: ierr
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      bbuf_ucd%id_binary = id_read_fld
      call open_read_binary_file(file_name, id_rank, bbuf_ucd)
      if(bbuf_ucd%ierr_bin .ne. 0) goto 99
      call read_step_data_b(bbuf_ucd, t_IO)
      ucd%nnod = istack_merged(1)
      if(bbuf_ucd%ierr_bin .ne. 0) goto 99
      call read_field_num_b                                             &
     &   (bbuf_ucd, istack_merged, ucd%num_field)
      ucd%nnod = istack_merged(1)
      if(bbuf_ucd%ierr_bin .ne. 0) goto 99
!
      call allocate_ucd_phys_name(ucd)
!
      call read_mul_integer_b                                           &
     &   (bbuf_ucd, cast_long(ucd%num_field), ucd%num_comp)
      if(bbuf_ucd%ierr_bin .gt. 0) go to 99
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_field_data_b                                            &
     &   (bbuf_ucd, ucd%num_field, ucd%phys_name,                       &
     &    ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
      if(bbuf_ucd%ierr_bin .ne. 0) goto 99
!
      call close_binary_file(bbuf_ucd)
      ierr = 0
      return
!
  99  continue
      ierr = bbuf_ucd%ierr_bin
      return
!
      end subroutine read_alloc_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_fld_header_b                          &
     &         (id_rank, file_name, t_IO, ucd, ierr)
!
      character(len=kchara), intent(in)  :: file_name
      integer, intent(in) :: id_rank
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      integer(kind=kint), intent(inout) :: ierr
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      bbuf_ucd%id_binary = id_read_fld
      call open_read_binary_file(file_name, id_rank, bbuf_ucd)
      if(bbuf_ucd%ierr_bin .ne. 0) goto 99
      call read_step_data_b(bbuf_ucd, t_IO)
      ucd%nnod = istack_merged(1)
      if(bbuf_ucd%ierr_bin .ne. 0) go to 99
      call read_field_num_b                                             &
     &   (bbuf_ucd, istack_merged, ucd%num_field)
      ucd%nnod = istack_merged(1)
      if(bbuf_ucd%ierr_bin .ne. 0) go to 99
!
      call allocate_ucd_phys_name(ucd)
!
      call read_mul_integer_b                                           &
     &   (bbuf_ucd, cast_long(ucd%num_field), ucd%num_comp)
      if(bbuf_ucd%ierr_bin .gt. 0) go to 99
!
      call close_binary_file(bbuf_ucd)
      ierr = 0
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
      return
!
  99  continue
      ierr = bbuf_ucd%ierr_bin
      return
!
      end subroutine read_alloc_ucd_2_fld_header_b
!
!------------------------------------------------------------------
!
      end module ucd_field_file_IO_b
