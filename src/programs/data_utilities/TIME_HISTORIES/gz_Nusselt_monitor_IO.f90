!>@file   gz_Nusselt_monitor_IO.f90
!!@brief  module gz_Nusselt_monitor_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays for gauss coefficients output
!!
!!@verbatim
!!      subroutine alloc_Nusselt_number_series(n_step, Nu_series)
!!      subroutine dealloc_Nusselt_number_series(Nu_series)
!!        integer(kind = kint), intent(in) :: n_step
!!        type(nusselt_number_series), intent(inout) :: Nu_series
!!      subroutine check_Nusselt_time_series(file_name, Nu_type)
!!      subroutine load_Nusselt_time_series                             &
!!     &         (flag_log, file_name, start_time, end_time,            &
!!     &          true_start, true_end, Nu_type, Nu_series)
!!      subroutine read_Nusselt_header(FPz_f, id_stream, flag_gzip,     &
!!     &                               Nu_type, zbuf)
!!        logical, intent(in) :: flag_log
!!        character(len=kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        real(kind = kreal), intent(inout) :: true_start, true_end
!!        type(nusselt_number_data), intent(inout) :: Nu_type
!!        type(nusselt_number_series), intent(inout) :: Nu_series
!!@endverbatim
      module gz_Nusselt_monitor_IO
!
      use m_precision
      use m_constants
      use t_no_heat_Nusselt
      use t_buffer_4_gzip
!
      implicit  none
!
      type nusselt_number_series
!>        Number of time series
        integer(kind = kint) :: n_step = 0
!>        Number of data for each step
        integer(kind = kint), allocatable :: i_step(:)
!>        time
        real(kind = kreal), allocatable :: d_time(:)
!>        Nusselt numbers
        real(kind = kreal), allocatable :: Nu_numbers(:,:)
      end type nusselt_number_series
!
!>      File ID for Gauss coefficients IO
      integer(kind = kint), parameter :: id_Nusselt = 23
!
      private :: read_Nusselt_number_series
      private :: read_Nusselt_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_Nusselt_number_series(n_step, Nu_series)
!
      integer(kind = kint), intent(in) :: n_step
      type(nusselt_number_series), intent(inout) :: Nu_series
!
!
      Nu_series%n_step = n_step
!
      allocate(Nu_series%i_step(Nu_series%n_step))
      allocate(Nu_series%d_time(Nu_series%n_step))
      allocate(Nu_series%Nu_numbers(2,Nu_series%n_step))
!
      if(Nu_series%n_step .gt. 0) then
        Nu_series%i_step = -1
        Nu_series%d_time = 0.0d0
        Nu_series%Nu_numbers = 0.0d0
      end if
!
      end subroutine alloc_Nusselt_number_series
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_Nusselt_number_series(Nu_series)
!
      type(nusselt_number_series), intent(inout) :: Nu_series
!
!
      deallocate(Nu_series%i_step)
      deallocate(Nu_series%d_time)
      deallocate(Nu_series%Nu_numbers)
!
      end subroutine dealloc_Nusselt_number_series
!
! -----------------------------------------------------------------------
!
      subroutine check_Nusselt_time_series(file_name, Nu_type)
!
      use select_gz_stream_file_IO
      use count_monitor_time_series
!
      character(len=kchara), intent(in) :: file_name
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
!
      integer(kind = kint) :: i_start, i_end
      real(kind = kreal) :: start_time, end_time
!
!
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_Nusselt, file_name, flag_gzip1, zbuf1)
      call read_Nusselt_header(FPz_f1, id_Nusselt, flag_gzip1,          &
     &                         Nu_type, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_Nusselt, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*) i_start, start_time
!
      do
        call sel_skip_comment_gz_stream                                 &
     &     (FPz_f1, id_Nusselt, flag_gzip1, zbuf1)
        if(zbuf1%len_used .lt. 0) exit
        read(zbuf1%fixbuf(1),*) i_end, end_time
      end do
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_Nusselt, flag_gzip1, zbuf1)
!
      write(*,*) 'Start step and time: ', i_start, start_time
      write(*,*) 'End step and time: ', i_end, end_time
!
      write(*,*) 'Inner and outer boundary radius: ',                   &
     &          Nu_type%r_ICB_Nu, Nu_type%r_CMB_Nu
!
      end subroutine check_Nusselt_time_series
!
! -----------------------------------------------------------------------
!
      subroutine load_Nusselt_time_series                               &
     &         (flag_log, file_name, start_time, end_time,              &
     &          true_start, true_end, Nu_type, Nu_series)
!
      use gzip_file_access
      use select_gz_stream_file_IO
      use count_monitor_time_series
!
      logical, intent(in) :: flag_log
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time
!
      real(kind = kreal), intent(inout) :: true_start, true_end
      type(nusselt_number_data), intent(inout) :: Nu_type
      type(nusselt_number_series), intent(inout) :: Nu_series
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
!
      integer(kind = kint) :: num_count, ierr
!
!
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_Nusselt, file_name, flag_gzip1, zbuf1)
      call read_Nusselt_header(FPz_f1, id_Nusselt, flag_gzip1,          &
     &                         Nu_type, zbuf1)
!
      call s_count_monitor_time_series                                  &
     &   (flag_log, FPz_f1, id_Nusselt, flag_gzip1, ione,               &
     &    start_time, end_time, true_start, true_end, num_count, zbuf1)
!
      if(flag_gzip1) then
        ierr =  rewind_gzfile(FPz_f1)
      else
        rewind(id_Nusselt)
      end if
!
      call read_Nusselt_header(FPz_f1, id_Nusselt, flag_gzip1,          &
     &                         Nu_type, zbuf1)
!
      call alloc_Nusselt_number_series(num_count, Nu_series)
      call read_Nusselt_number_series(flag_log, FPz_f1, id_Nusselt,     &
     &    flag_gzip1, start_time, end_time, Nu_type, Nu_series, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_Nusselt, flag_gzip1, zbuf1)
!
      write(*,*) 'Start step and time: ',                               &
     &           true_start, true_end, num_count
!
      end subroutine load_Nusselt_time_series
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_Nusselt_number_series                             &
     &         (flag_log, FPz_f, id_stream, flag_gzip,                  &
     &          start_time, end_time, Nu_type, Nu_series, zbuf)
!
      use count_monitor_time_series
!
      logical, intent(in) :: flag_log
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(nusselt_number_data), intent(inout) :: Nu_type
      type(nusselt_number_series), intent(inout) :: Nu_series
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: icou, i_step, ierr, i
      real(kind = kreal) :: time
!
      icou = 0
      do
        call read_Nusselt_4_monitor(FPz_f, id_stream, flag_gzip,        &
     &      i_step, time, Nu_type, zbuf, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          icou = icou + 1
          call copy_Nusselt_to_series                                   &
     &      (icou, i_step, time, Nu_type, Nu_series)
!
          if(flag_log) then
            write(*,'(69a1,a5,i12,a4,1pe16.8e3,a20,i12)',advance="NO")  &
     &          (char(8),i=1,69), 'step ', i_step,                      &
     &          ' at ', time, ' is read. count is  ', icou
          end if
        end if
!
        if(time .ge. end_time) exit
      end do
      if(flag_log) write(*,*)
!
      end subroutine read_Nusselt_number_series
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_Nusselt_header(FPz_f, id_stream, flag_gzip,       &
     &                               Nu_type, zbuf)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
!
      type(nusselt_number_data), intent(inout) :: Nu_type
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call sel_skip_comment_gz_stream(FPz_f, id_stream,                 &
     &                                flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) Nu_type%r_ICB_Nu, Nu_type%r_CMB_Nu
      call sel_skip_comment_gz_stream(FPz_f, id_stream,                 &
     &                                flag_gzip, zbuf)
!
      end subroutine read_Nusselt_header
!
! -----------------------------------------------------------------------
!
      subroutine read_Nusselt_4_monitor                                 &
     &         (FPz_f, id_stream, flag_gzip, i_step, time,              &
     &          Nu_type, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
!
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
      type(nusselt_number_data), intent(inout) :: Nu_type
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      ierr = 1
      call sel_read_line_gz_stream(FPz_f, id_stream,                    &
     &                             flag_gzip, zbuf)
      if(zbuf%len_used .lt. 0) return
!
      read(zbuf%fixbuf(1),*,err=99,end=99) i_step, time,                &
     &                   Nu_type%Nu_ICB, Nu_type%Nu_CMB
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine read_Nusselt_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine copy_Nusselt_to_series                                 &
     &         (icou, i_step, time, Nu_type, Nu_series)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(nusselt_number_data), intent(in) :: Nu_type
      type(nusselt_number_series), intent(inout) :: Nu_series
!
!
      Nu_series%i_step(icou) = i_step
      Nu_series%d_time(icou) = time
      Nu_series%Nu_numbers(1,icou) = Nu_type%Nu_ICB
      Nu_series%Nu_numbers(2,icou) = Nu_type%Nu_CMB
!
      end subroutine copy_Nusselt_to_series
!
! -----------------------------------------------------------------------
!
      end module gz_Nusselt_monitor_IO
