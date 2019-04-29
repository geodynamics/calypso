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
      use set_ucd_file_names
      use field_data_IO_b
      use binary_IO
      use transfer_to_long_integers
!
      implicit none
!
      type(binary_IO_flags), private :: bin_ucdflags
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
      call open_write_binary_file(file_name, bin_ucdflags)
      if(bin_ucdflags%ierr_IO .ne. 0) ierr = ierr_file
!
      call write_step_data_b(id_rank, t_IO, bin_ucdflags)
      if(bin_ucdflags%ierr_IO .ne. 0) ierr = ierr_file
      call write_field_data_b                                           &
     &   (ucd%num_field, ucd%phys_name, ucd%num_comp,                   &
     &    ucd%nnod, ucd%ntot_comp, ucd%d_ucd, bin_ucdflags)
      if(bin_ucdflags%ierr_IO .ne. 0) ierr = ierr_file
      call close_binary_file
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
      call open_read_binary_file(file_name, id_rank, bin_ucdflags)
      call read_step_data_b(bin_ucdflags, t_IO,                         &
     &    istack_merged, ucd%num_field)
      ucd%nnod = istack_merged(1)
      if(bin_ucdflags%ierr_IO .ne. 0) goto 99
!
      call read_mul_integer_b                                           &
     &   (bin_ucdflags, cast_long(ucd%num_field), ucd%num_comp)
      if(bin_ucdflags%ierr_IO .ne. 0) goto 99
      call read_field_data_b                                            &
     &   (bin_ucdflags, ucd%num_field, ucd%phys_name,                   &
     &    ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
      if(bin_ucdflags%ierr_IO .ne. 0) goto 99
!
      call close_binary_file
      ierr = 0
      return

  99  continue
      ierr = ierr_file
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
      call open_read_binary_file(file_name, id_rank, bin_ucdflags)
      call read_step_data_b(bin_ucdflags, t_IO,                         &
     &    istack_merged, ucd%num_field)
      ucd%nnod = istack_merged(1)
      if(bin_ucdflags%ierr_IO .ne. 0) goto 99
!
      call allocate_ucd_phys_name(ucd)
!
      call read_mul_integer_b                                           &
     &   (bin_ucdflags, cast_long(ucd%num_field), ucd%num_comp)
      if(bin_ucdflags%ierr_IO .ne. 0) goto 99
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_field_data_b                                            &
     &   (bin_ucdflags, ucd%num_field, ucd%phys_name,                   &
     &    ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
      if(bin_ucdflags%ierr_IO .ne. 0) goto 99
!
      call close_binary_file
      ierr = 0
      return
!
  99  continue
      ierr = ierr_file
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
      call open_read_binary_file(file_name, id_rank, bin_ucdflags)
      call read_step_data_b(bin_ucdflags, t_IO,                         &
     &    istack_merged, ucd%num_field)
      ucd%nnod = istack_merged(1)
      if(bin_ucdflags%ierr_IO .ne. 0) goto 99
!
      call allocate_ucd_phys_name(ucd)
!
      call read_mul_integer_b                                           &
     &   (bin_ucdflags, cast_long(ucd%num_field), ucd%num_comp)
!
      call close_binary_file
      ierr = 0
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
      return
!
  99  continue
      ierr = ierr_file
      return
!
      end subroutine read_alloc_ucd_2_fld_header_b
!
!------------------------------------------------------------------
!
      end module ucd_field_file_IO_b
