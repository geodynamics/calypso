!> @file  gz_field_file_IO.f90
!!      module gz_field_file_IO
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2007
!
!> @brief Field file IO using zlib
!!
!!@verbatim
!!      subroutine write_gz_step_field_file                             &
!!     &         (gzip_name, id_rank, t_IO, fld_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        type(time_data), intent(in) :: t_IO
!!        type(field_IO), intent(in) :: fld_IO
!!
!!      subroutine read_alloc_gz_field_file(gzip_name, id_rank, fld_IO)
!!        character, pointer, intent(in) :: FPz_f
!!
!!      subroutine read_gz_step_field_file                              &
!!     &         (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
!!      subroutine read_alloc_gz_step_field_file                        &
!!     &          (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!
!!      subroutine read_alloc_gz_step_field_head                        &
!!     &         (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module gz_field_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_time_data
      use t_field_data_IO
      use gz_field_data_IO
      use transfer_to_long_integers
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_f
      character, pointer, private, save :: FPz_fld
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_gz_step_field_file                               &
     &         (gzip_name, id_rank, t_IO, fld_IO)
!
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: gzip_name
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write gzipped field file: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile_a(FPz_fld, gzip_name, zbuf_f)
!
      call write_gz_step_data(FPz_fld, id_rank,                         &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt, zbuf_f)
      call write_gz_field_data(FPz_fld, cast_long(fld_IO%nnod_IO),      &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO, zbuf_f)
!
      call close_gzfile_a(FPz_fld, zbuf_f)
!
      end subroutine write_gz_step_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_alloc_gz_field_file(gzip_name, id_rank, fld_IO)
!
      use skip_gz_comment
      use gz_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile_a(FPz_fld, gzip_name, zbuf_f)
!
      call skip_gz_comment_int2                                         &
     &   (FPz_fld, fld_IO%nnod_IO, fld_IO%num_field_IO, zbuf_f)
      call alloc_phys_name_IO(fld_IO)
!
      call read_gz_multi_int                                            &
     &   (FPz_fld, fld_IO%num_field_IO, fld_IO%num_comp_IO, zbuf_f)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_gz_field_data(FPz_fld, cast_long(fld_IO%nnod_IO),       &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO, zbuf_f)
!
      call close_gzfile_a(FPz_fld, zbuf_f)
!
      end subroutine read_alloc_gz_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_gz_step_field_file                                &
     &         (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
!
      use skip_gz_comment
      use gz_data_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: gzip_name
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind=kint), intent(inout) :: ierr_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped field file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile_a(FPz_fld, gzip_name, zbuf_f)
!
      call read_gz_step_data                                            &
     &   (FPz_fld, id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,       &
     &    zbuf_f, ierr_IO)
      if(ierr_IO .gt. 0) return
!
      call skip_gz_comment_int2                                         &
     &   (FPz_fld, fld_IO%nnod_IO, fld_IO%num_field_IO, zbuf_f)
      call read_gz_multi_int                                            &
     &   (FPz_fld, fld_IO%num_field_IO, fld_IO%num_comp_IO, zbuf_f)
      call read_gz_field_data(FPz_fld, cast_long(fld_IO%nnod_IO),       &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO, zbuf_f)
!
      call close_gzfile_a(FPz_fld, zbuf_f)
!
      end subroutine read_gz_step_field_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_gz_step_field_file                          &
     &          (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
!
      use skip_gz_comment
      use gz_data_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: gzip_name
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind=kint), intent(inout) :: ierr_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped field file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile_a(FPz_fld, gzip_name, zbuf_f)
!
      call read_gz_step_data                                            &
     &   (FPz_fld, id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,       &
     &    zbuf_f, ierr_IO)
      if(ierr_IO .gt. 0) return
!
      call skip_gz_comment_int2                                         &
     &   (FPz_fld, fld_IO%nnod_IO, fld_IO%num_field_IO, zbuf_f)
      call alloc_phys_name_IO(fld_IO)
!
      call read_gz_multi_int                                            &
     &   (FPz_fld, fld_IO%num_field_IO, fld_IO%num_comp_IO, zbuf_f)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_gz_field_data(FPz_fld, cast_long(fld_IO%nnod_IO),       &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO, zbuf_f)
!
      call close_gzfile_a(FPz_fld, zbuf_f)
!
      end subroutine read_alloc_gz_step_field_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_gz_step_field_head                          &
     &         (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
!
      use skip_gz_comment
      use gz_data_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: gzip_name
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind=kint), intent(inout) :: ierr_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped field file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile_a(FPz_fld, gzip_name, zbuf_f)
!
      call read_gz_step_data                                            &
     &   (FPz_fld, id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,       &
     &    zbuf_f, ierr_IO)
      if(ierr_IO .gt. 0) return
!
      call skip_gz_comment_int2                                         &
     &   (FPz_fld, fld_IO%nnod_IO, fld_IO%num_field_IO, zbuf_f)
!
      call alloc_phys_name_IO(fld_IO)
      call read_gz_multi_int                                            &
     &   (FPz_fld, fld_IO%num_field_IO, fld_IO%num_comp_IO, zbuf_f)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      call read_gz_field_name(FPz_fld, cast_long(fld_IO%nnod_IO),       &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO, fld_IO%fld_name,     &
     &    zbuf_f)
!
      call close_gzfile_a(FPz_fld, zbuf_f)
!
      end subroutine read_alloc_gz_step_field_head
!
!------------------------------------------------------------------
!
      end module gz_field_file_IO
