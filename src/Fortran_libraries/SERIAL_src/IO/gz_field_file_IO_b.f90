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
      use t_buffer_4_gzip
      use gz_field_data_IO_b
      use gz_binary_IO
      use binary_IO
      use transfer_to_long_integers
!
      implicit none
!
      type(buffer_4_gzip), private, save :: zbuf_fld
      character, pointer, private, save :: FPz_fld
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
      use gzip_file_access
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
      call open_wt_gzfile_b(FPz_fld, gzip_name, zbuf_fld)
!
      call gz_write_step_data_b(FPz_fld, id_rank,                       &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, zbuf_fld)
      call gz_write_field_data_b                                        &
     &   (FPz_fld,cast_long(fld_IO%nnod_IO), fld_IO%num_field_IO,       &
     &    fld_IO%ntot_comp_IO, fld_IO%num_comp_IO, fld_IO%fld_name,     &
     &    fld_IO%d_IO, zbuf_fld)
!
      call close_gzfile_b(FPz_fld)
!
      end subroutine gz_write_step_fld_file_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_field_file_b                              &
     &         (gzip_name, id_rank, t_IO, fld_IO)
!
      use gzip_file_access
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
      call open_rd_gzfile_b(FPz_fld, gzip_name, id_rank, zbuf_fld)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call gz_read_step_data_b(FPz_fld, zbuf_fld,                       &
     &    id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call gz_read_num_field_b(FPz_fld, zbuf_fld,                       &
     &                         istack_merged, fld_IO%num_field_IO)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call gz_read_mul_integer_b(FPz_fld, zbuf_fld,                     &
     &    cast_long(fld_IO%num_field_IO), fld_IO%num_comp_IO)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call gz_read_field_data_b(FPz_fld, zbuf_fld,                      &
     &    cast_long(fld_IO%nnod_IO), fld_IO%num_field_IO,               &
     &    fld_IO%ntot_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call close_gzfile_b(FPz_fld)
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
      use gzip_file_access
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
      call open_rd_gzfile_b(FPz_fld, gzip_name, id_rank, zbuf_fld)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call gz_read_step_data_b(FPz_fld, zbuf_fld,                       &
     &    id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call gz_read_num_field_b(FPz_fld, zbuf_fld,                       &
     &                         istack_merged, fld_IO%num_field_IO)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_mul_integer_b(FPz_fld, zbuf_fld,                     &
     &    cast_long(fld_IO%num_field_IO), fld_IO%num_comp_IO)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      fld_IO%nnod_IO = int(istack_merged(1),KIND(fld_IO%nnod_IO))
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
      call gz_read_field_data_b(FPz_fld, zbuf_fld,                      &
     &    cast_long(fld_IO%nnod_IO), fld_IO%num_field_IO,               &
     &    fld_IO%ntot_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call close_gzfile_b(FPz_fld)
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
      use gzip_file_access
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
      call open_rd_gzfile_b(FPz_fld, gzip_name, id_rank, zbuf_fld)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call gz_read_step_data_b(FPz_fld, zbuf_fld,                       &
     &    id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call gz_read_num_field_b(FPz_fld, zbuf_fld,                       &
     &                         istack_merged, fld_IO%num_field_IO)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_mul_integer_b(FPz_fld, zbuf_fld,                     &
     &    cast_long(fld_IO%num_field_IO), fld_IO%num_comp_IO)
      if(zbuf_fld%ierr_zlib .ne. 0) go to 99
!
      call close_gzfile_b(FPz_fld)
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
