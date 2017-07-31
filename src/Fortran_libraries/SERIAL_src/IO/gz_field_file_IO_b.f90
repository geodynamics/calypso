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
!!     &         (gzip_name, my_rank, t_IO, fld_IO)
!!        type(time_data), intent(in) :: t_IO
!!        type(field_IO), intent(in) :: fld_IO
!!
!!      subroutine gz_read_step_field_file_b                            &
!!     &         (gzip_name, my_rank, t_IO, fld_IO)
!!      subroutine gz_rd_alloc_st_fld_file_b                            &
!!     &         (gzip_name, my_rank, t_IO, fld_IO)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!
!!      subroutine gz_rd_alloc_st_fld_head_b                            &
!!     &         (gzip_name, my_rank, t_IO, fld_IO)
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
      use skip_gz_comment
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_step_fld_file_b                               &
     &         (gzip_name, my_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(in) :: my_rank
!
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped binary data file: ', trim(gzip_name)
!
      call open_wt_gzfile_f(gzip_name)
!
      call gz_write_step_data_b(my_rank,                                &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
      call gz_write_field_data_b                                        &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile_f
!
      end subroutine gz_write_step_fld_file_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_field_file_b                              &
     &         (gzip_name, my_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(in) :: my_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary data file: ', trim(gzip_name)
!
      call open_rd_gzfile_f(gzip_name)
      call gz_read_step_data_b                                          &
     &   (my_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,                &
     &    istack_merged, fld_IO%num_field_IO)
!
      call gz_read_mul_integer_b                                        &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call gz_read_field_data_b                                         &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile_f
!
      end subroutine gz_read_step_field_file_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_file_b                              &
     &         (gzip_name, my_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(in) :: my_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary data file: ', trim(gzip_name)
!
      call open_rd_gzfile_f(gzip_name)
      call gz_read_step_data_b                                          &
     &   (my_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,                &
     &    istack_merged, fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_mul_integer_b                                        &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      fld_IO%nnod_IO = int(istack_merged(1))
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
      call gz_read_field_data_b                                         &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile_f
!
      end subroutine gz_rd_alloc_st_fld_file_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_head_b                              &
     &         (gzip_name, my_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(in) :: my_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary data file: ', trim(gzip_name)
!
      call open_rd_gzfile_f(gzip_name)
      call gz_read_step_data_b                                          &
     &   (my_rank, t_IO%i_time_step, t_IO%time, t_IO%dt,                &
     &    istack_merged, fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_mul_integer_b                                        &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call close_gzfile_f
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      end subroutine gz_rd_alloc_st_fld_head_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_file_IO_b
