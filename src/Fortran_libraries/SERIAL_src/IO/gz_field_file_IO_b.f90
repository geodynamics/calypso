!>@file  gz_field_file_IO_b.f90
!!       module gz_field_file_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_step_fld_file_b                             &
!!     &         (gzip_name, id_rank, t_IO, fld_IO)
!!        type(time_data), intent(in) :: t_IO
!!        type(field_IO), intent(in) :: fld_IO
!!
!!      subroutine gz_read_step_field_file_b                            &
!!     &         (gzip_name, id_rank, t_IO, fld_IO)
!!      subroutine gz_rd_alloc_st_fld_file_b                            &
!!     &         (gzip_name, id_rank, t_IO, fld_IO)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!
!!      subroutine gz_rd_alloc_st_fld_head_b                            &
!!     &         (gzip_name, id_rank, t_IO, fld_IO)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module gz_field_file_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_time_data
      use t_field_data_IO
      use gz_field_data_IO_b
      use gz_binary_IO
      use binary_IO
      use skip_gz_comment
      use transfer_to_long_integers
!
      implicit none
!
      type(binary_IO_flags), private :: gz_fldflags
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_step_fld_file_b                               &
     &         (gzip_name, id_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped binary data file: ', trim(gzip_name)
!
      call open_wt_gzfile_b(gzip_name, gz_fldflags)
!
      call gz_write_step_data_b(id_rank,                                &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, gz_fldflags)
      call gz_write_field_data_b                                        &
     &   (cast_long(fld_IO%nnod_IO), fld_IO%num_field_IO,               &
     &    fld_IO%ntot_comp_IO, fld_IO%num_comp_IO, fld_IO%fld_name,     &
     &    fld_IO%d_IO, gz_fldflags)
!
      call close_gzfile_f
!
      end subroutine gz_write_step_fld_file_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_field_file_b                              &
     &         (gzip_name, id_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary data file: ', trim(gzip_name)
!
      call open_rd_gzfile_b(gzip_name, id_rank, gz_fldflags)
      if(gz_fldflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_step_data_b(gz_fldflags,                             &
     &    id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,                &
     &    istack_merged, fld_IO%num_field_IO)
      if(gz_fldflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_mul_integer_b(gz_fldflags,                           &
     &    cast_long(fld_IO%num_field_IO), fld_IO%num_comp_IO)
      if(gz_fldflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_field_data_b(gz_fldflags,                            &
     &    cast_long(fld_IO%nnod_IO), fld_IO%num_field_IO,               &
     &    fld_IO%ntot_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
      if(gz_fldflags%ierr_IO .ne. 0) goto 99
!
      call close_gzfile_f
      return
!
  99  continue
      stop "read error in gzipped field file"
!
      end subroutine gz_read_step_field_file_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_file_b                              &
     &         (gzip_name, id_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary data file: ', trim(gzip_name)
!
      call open_rd_gzfile_b(gzip_name, id_rank, gz_fldflags)
      if(gz_fldflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_step_data_b(gz_fldflags,                             &
     &    id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,                &
     &    istack_merged, fld_IO%num_field_IO)
      if(gz_fldflags%ierr_IO .ne. 0) goto 99
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_mul_integer_b(gz_fldflags,                           &
     &    cast_long(fld_IO%num_field_IO), fld_IO%num_comp_IO)
      if(gz_fldflags%ierr_IO .ne. 0) goto 99
!
      fld_IO%nnod_IO = int(istack_merged(1),KIND(fld_IO%nnod_IO))
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
      call gz_read_field_data_b(gz_fldflags,                            &
     &    cast_long(fld_IO%nnod_IO), fld_IO%num_field_IO,               &
     &    fld_IO%ntot_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile_f
      return
!
  99  continue
      stop "read error in gzipped field file"
!
      end subroutine gz_rd_alloc_st_fld_file_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_head_b                              &
     &         (gzip_name, id_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary data file: ', trim(gzip_name)
!
      call open_rd_gzfile_b(gzip_name, id_rank, gz_fldflags)
      if(gz_fldflags%ierr_IO .ne. 0) goto 99
!
      call gz_read_step_data_b(gz_fldflags,                             &
     &    id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,                &
     &    istack_merged, fld_IO%num_field_IO)
      if(gz_fldflags%ierr_IO .ne. 0) goto 99
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_mul_integer_b(gz_fldflags,                           &
     &    cast_long(fld_IO%num_field_IO), fld_IO%num_comp_IO)
      if(gz_fldflags%ierr_IO .ne. 0) goto 99
!
      call close_gzfile_f
!
      call cal_istack_phys_comp_IO(fld_IO)
      return
!
  99  continue
      stop "read error in gzipped field file"
!
      end subroutine gz_rd_alloc_st_fld_head_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_file_IO_b
