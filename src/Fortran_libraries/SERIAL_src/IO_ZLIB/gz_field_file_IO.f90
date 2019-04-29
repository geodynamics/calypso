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
!!        type(time_data), intent(in) :: t_IO
!!        type(field_IO), intent(in) :: fld_IO
!!
!!      subroutine read_alloc_gz_field_file(gzip_name, id_rank, fld_IO)
!!
!!      subroutine read_gz_step_field_file                              &
!!     &         (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
!!      subroutine read_alloc_gz_step_field_file                        &
!!     &          (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!
!!      subroutine read_alloc_gz_step_field_head                        &
!!     &         (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
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
      use skip_gz_comment
      use transfer_to_long_integers
!
      implicit none
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
      call open_wt_gzfile_f(gzip_name)
!
      call write_gz_step_data                                           &
     &   (id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt)
      call write_gz_field_data(cast_long(fld_IO%nnod_IO),               &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile_f
!
      end subroutine write_gz_step_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_alloc_gz_field_file(gzip_name, id_rank, fld_IO)
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile_f(gzip_name)
!
      call skip_gz_comment_int2(fld_IO%nnod_IO, fld_IO%num_field_IO)
      call alloc_phys_name_IO(fld_IO)
!
      call read_gz_multi_int(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_gz_field_data(cast_long(fld_IO%nnod_IO),                &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile_f
!
      end subroutine read_alloc_gz_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_gz_step_field_file                                &
     &         (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
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
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_step_data                                            &
     &   (id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt, ierr_IO)
      if(ierr_IO .gt. 0) return
!
      call skip_gz_comment_int2(fld_IO%nnod_IO, fld_IO%num_field_IO)
      call read_gz_multi_int(fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call read_gz_field_data(cast_long(fld_IO%nnod_IO),                &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile_f
!
      end subroutine read_gz_step_field_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_gz_step_field_file                          &
     &          (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
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
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_step_data                                            &
     &   (id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt, ierr_IO)
      if(ierr_IO .gt. 0) return
!
      call skip_gz_comment_int2(fld_IO%nnod_IO, fld_IO%num_field_IO)
      call alloc_phys_name_IO(fld_IO)
!
      call read_gz_multi_int(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_gz_field_data(cast_long(fld_IO%nnod_IO),                &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile_f
!
      end subroutine read_alloc_gz_step_field_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_gz_step_field_head                          &
     &         (gzip_name, id_rank, t_IO, fld_IO, ierr_IO)
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
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_step_data                                            &
     &   (id_rank, t_IO%i_time_step, t_IO%time, t_IO%dt, ierr_IO)
      if(ierr_IO .gt. 0) return
!
      call skip_gz_comment_int2(fld_IO%nnod_IO, fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call read_gz_multi_int(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call close_gzfile_f
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      end subroutine read_alloc_gz_step_field_head
!
!------------------------------------------------------------------
!
      end module gz_field_file_IO
