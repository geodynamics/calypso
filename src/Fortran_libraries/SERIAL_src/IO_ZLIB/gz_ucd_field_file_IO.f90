!>@file  gz_ucd_field_file_IO.f90
!!       module gz_ucd_field_file_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief gzipped UCD ascii data IO
!!
!!@verbatim
!!      subroutine gz_write_ucd_2_fld_file                              &
!!     &         (id_rank, gzip_name, t_IO, ucd)
!!
!!      subroutine gz_read_ucd_2_fld_file                               &
!!     &         (id_rank, gzip_name, t_IO, ucd, ierr_IO)
!!      subroutine gz_read_alloc_ucd_2_ld_file                          &
!!     &         (id_rank, gzip_name, t_IO, ucd, ierr)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param id_rank  process ID
!!@param gzip_name    File name
!!@param ucd      Structure for FEM field data IO
!
      module gz_ucd_field_file_IO
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
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_ucd_2_fld_file                                &
     &         (id_rank, gzip_name, t_IO, ucd)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped step data file: ', trim(gzip_name)
!
      call open_wt_gzfile_a(gzip_name, zbuf_fu)
!
      call write_gz_step_data                                           &
     &   (id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt, zbuf_fu)
      call write_gz_field_data                                          &
     &   (ucd%nnod, ucd%num_field, ucd%ntot_comp,                       &
     &    ucd%num_comp, ucd%phys_name, ucd%d_ucd, zbuf_fu)
!
      call close_gzfile_a(zbuf_fu)
!
      end subroutine gz_write_ucd_2_fld_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_ucd_2_fld_file                                 &
     &         (id_rank, gzip_name, t_IO, ucd, ierr_IO)
!
      use skip_gz_comment
      use gz_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(inout) :: ierr_IO
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile_a(gzip_name, zbuf_fu)
!
      call read_gz_step_data                                            &
     &   (id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,                &
     &    zbuf_fu, ierr_IO)
      if(ierr_IO .gt. 0) return
      call skip_gz_comment_int8_int(ucd%nnod, ucd%num_field, zbuf_fu)
      call read_gz_multi_int(ucd%num_field, ucd%num_comp, zbuf_fu)
!
      call read_gz_field_data(ucd%nnod, ucd%num_field, ucd%ntot_comp,   &
     &    ucd%num_comp, ucd%phys_name, ucd%d_ucd, zbuf_fu)
!
      call close_gzfile_a(zbuf_fu)
!
      end subroutine gz_read_ucd_2_fld_file
!
!------------------------------------------------------------------
!
      subroutine gz_read_alloc_ucd_2_ld_file                            &
     &         (id_rank, gzip_name, t_IO, ucd, ierr_IO)
!
      use skip_gz_comment
      use gz_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(inout) :: ierr_IO
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile_a(gzip_name, zbuf_fu)
!
      call read_gz_step_data                                            &
     &   (id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,                &
     &    zbuf_fu, ierr_IO)
      if(ierr_IO .gt. 0) return
      call skip_gz_comment_int8_int(ucd%nnod, ucd%num_field, zbuf_fu)
!
      call allocate_ucd_phys_name(ucd)
!
      call read_gz_multi_int(ucd%num_field, ucd%num_comp, zbuf_fu)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_gz_field_data                                           &
     &   (ucd%nnod, ucd%num_field, ucd%ntot_comp,                       &
     &    ucd%num_comp, ucd%phys_name, ucd%d_ucd, zbuf_fu)
!
      call close_gzfile_a(zbuf_fu)
!
      end subroutine gz_read_alloc_ucd_2_ld_file
!
!------------------------------------------------------------------
!
      end module gz_ucd_field_file_IO
