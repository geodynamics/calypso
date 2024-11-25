!>@file  gz_ucd_field_file_IO_b.f90
!!       module gz_ucd_field_file_IO_b
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief gzipped UCD ascii data IO
!!
!!@verbatim
!!      subroutine gz_write_ucd_2_fld_file_b                            &
!!     &         (id_rank, gzip_name, t_IO, ucd)
!!        type(time_data), intent(in) :: t_IO
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine gz_read_ucd_2_fld_file_b                             &
!!     &         (id_rank, gzip_name, t_IO, ucd)
!!      subroutine gz_read_alloc_ucd_2_fld_file_b                       &
!!     &         (id_rank, gzip_name, t_IO, ucd)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param id_rank  process ID
!!@param gzip_name    File name
!!@param ucd      Structure for FEM field data IO
!
      module gz_ucd_field_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_field_file_format
!
      use t_time_data
      use t_ucd_data
!
      use gz_field_data_IO
      use set_ucd_file_names
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_fu
      character, pointer, private, save :: FPz_udt
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_ucd_2_fld_file_b                              &
     &         (id_rank, gzip_name, t_IO, ucd)
!
      use skip_gz_comment
      use gzip_file_access
      use gz_field_data_IO_b
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped binary step data file: ', trim(gzip_name)
!
      call open_wt_gzfile_b(FPz_udt, gzip_name, zbuf_fu)
!
      call gz_write_step_data_b(FPz_udt, id_rank,                       &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, zbuf_fu)
      call gz_write_field_data_b(FPz_udt, ucd%nnod, ucd%num_field,      &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd,        &
     &    zbuf_fu)
!
      call close_gzfile_b(FPz_udt)
!
      end subroutine gz_write_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_ucd_2_fld_file_b                               &
     &         (id_rank, gzip_name, t_IO, ucd)
!
      use skip_gz_comment
      use gzip_file_access
      use gz_field_data_IO_b
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped binary data file: ', trim(gzip_name)
!
      call open_rd_gzfile_b(FPz_udt, gzip_name, id_rank, zbuf_fu)
      if(zbuf_fu%ierr_zlib .ne. 0) go to 99
!
      call gz_read_step_data_b(FPz_udt, zbuf_fu,                        &
     &    id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt)
      if(zbuf_fu%ierr_zlib .ne. 0) go to 99
!
      call gz_read_num_field_b(FPz_udt, zbuf_fu,                        &
     &                         istack_merged, ucd%num_field)
      if(zbuf_fu%ierr_zlib .ne. 0) go to 99
!
      call gz_read_mul_integer_b                                        &
     &   (FPz_udt, zbuf_fu, cast_long(ucd%num_field), ucd%num_comp)
      if(zbuf_fu%ierr_zlib .ne. 0) go to 99
!
      call gz_read_field_data_b(FPz_udt, zbuf_fu,                       &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%phys_name,        &
     &    ucd%d_ucd)
      if(zbuf_fu%ierr_zlib .ne. 0) go to 99
!
      call close_gzfile_b(FPz_udt)
      return
!
  99  continue
      stop "read error in gzipped field file"
!
      end subroutine gz_read_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_alloc_ucd_2_fld_file_b                         &
     &         (id_rank, gzip_name, t_IO, ucd)
!
      use skip_gz_comment
      use gzip_file_access
      use gz_field_data_IO_b
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped binary data file: ', trim(gzip_name)
!
      call open_rd_gzfile_b(FPz_udt, gzip_name, id_rank, zbuf_fu)
      if(zbuf_fu%ierr_zlib .ne. 0) go to 99
!
      call gz_read_step_data_b(FPz_udt, zbuf_fu,                        &
     &    id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt)
      if(zbuf_fu%ierr_zlib .ne. 0) go to 99
!
      call gz_read_num_field_b(FPz_udt, zbuf_fu,                        &
     &                         istack_merged, ucd%num_field)
      if(zbuf_fu%ierr_zlib .ne. 0) go to 99
!
      call allocate_ucd_phys_name(ucd)
!
      call gz_read_mul_integer_b                                        &
     &   (FPz_udt, zbuf_fu, cast_long(ucd%num_field), ucd%num_comp)
      if(zbuf_fu%ierr_zlib .ne. 0) go to 99
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call gz_read_field_data_b(FPz_udt, zbuf_fu,                       &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%phys_name,        &
     &    ucd%d_ucd)
      if(zbuf_fu%ierr_zlib .ne. 0) go to 99
!
      call close_gzfile_b(FPz_udt)
      return
!
  99  continue
      stop "read error in gzipped field file"
!
      end subroutine gz_read_alloc_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!
      end module gz_ucd_field_file_IO_b
