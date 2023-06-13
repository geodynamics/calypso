!>@file   picked_sph_spectr_data_IO.f90
!!@brief  module picked_sph_spectr_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine check_picked_sph_spectr(file_name, picked_IO)
!!      subroutine load_picked_sph_spectr_series                        &
!!     &         (flag_log, file_name, start_time, end_time,            &
!!     &          true_start, true_end, picked_IO)
!!        logical, intent(in) :: flag_log
!!        character(len=kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        real(kind = kreal), intent(inout) :: true_start, true_end
!!        type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!!      subroutine read_pick_series_head(FPz_f, id_stream, flag_gzip,   &
!!     &                                 picked_IO, zbuf)
!!      subroutine read_pick_series_comp_name                           &
!!     &         (FPz_f, id_stream, flag_gzip, picked_IO, zbuf)
!!      subroutine read_sph_spec_monitor(FPz_f, id_stream, flag_gzip,   &
!!     &          i_step, time, picked_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: i_step, ierr
!!        real(kind = kreal), intent(inout) :: time
!!@endverbatim
!!
!!@n @param  id_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module picked_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
      use t_picked_sph_spectr_data_IO
      use t_buffer_4_gzip
!
      implicit  none
!
!>      File ID for spectrum monitor file
      integer(kind = kint), parameter :: id_pick_mode = 23
!
      private :: read_picked_sph_spectr_series
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_picked_sph_spectr(file_name, picked_IO)
!
      use select_gz_stream_file_IO
      use gzip_file_access
!
      character(len=kchara), intent(in) :: file_name
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: i_start, i_end
      real(kind = kreal) :: start_time, end_time
!
      character, pointer :: FPz_f1
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
!
!
      write(*,*) 'Open file: ', trim(file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_pick_mode, file_name, flag_gzip1, zbuf1)
      call read_pick_series_head(FPz_f1, id_pick_mode, flag_gzip1,      &
     &                           picked_IO, zbuf1)
      call alloc_pick_sph_monitor_IO(picked_IO)
      call read_pick_series_comp_name                                   &
     &   (FPz_f1, id_pick_mode, flag_gzip1, picked_IO, zbuf1)
!
      do j = 1, picked_IO%num_mode
        do i = 1, picked_IO%num_layer
          k = i + (j-1) * picked_IO%num_layer
          call sel_skip_comment_gz_stream                               &
     &       (FPz_f1, id_pick_mode, flag_gzip1, zbuf1)
          read(zbuf1%fixbuf(1),*) i_start, start_time,                  &
     &        picked_IO%idx_sph(k,1), picked_IO%radius(k),              &
     &        picked_IO%idx_sph(k,3:4)
          picked_IO%idx_sph(k,2)                                        &
     &       = picked_IO%idx_sph(k,3) * (picked_IO%idx_sph(k,3)+1)      &
     &        + picked_IO%idx_sph(k,4)
        end do
      end do
!
      do
        call sel_skip_comment_gz_stream                                 &
     &     (FPz_f1, id_pick_mode, flag_gzip1, zbuf1)
        if(zbuf1%len_used .le. 0) exit
        if(check_gzfile_eof(FPz_f1) .gt. 0) exit
        read(zbuf1%fixbuf(1),*) i_end, end_time
      end do
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_pick_mode, flag_gzip1, zbuf1)

      write(*,*) 'Start step and time: ', i_start, start_time
      write(*,*) 'End step and time: ', i_end, end_time
!
      write(*,*) 'Saved hermonics mode:'
      do i = 1, picked_IO%num_mode
        j = 1 + (i-1) * picked_IO%num_layer
        write(*,*) i, picked_IO%idx_sph(j,3:4)
      end do
      write(*,*) 'Saved radial points:'
      do i = 1, picked_IO%num_layer
        write(*,*) i, picked_IO%idx_sph(i,1), picked_IO%radius(i)
      end do
      write(*,*) 'Saved field names:'
      do i = 1, picked_IO%ntot_comp
        write(*,*) i, trim(picked_IO%spectr_name(i))
      end do
      call dealloc_pick_sph_monitor_IO(picked_IO)
!
      end subroutine check_picked_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine load_picked_sph_spectr_series                          &
     &         (flag_log, file_name, start_time, end_time,              &
     &          true_start, true_end, picked_IO)
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
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
      character, pointer :: FPz_f1
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
!
      integer(kind = kint) :: ierr, num_count, icou_skip, num
!
!
      write(*,*) 'Open file again: ', trim(file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_pick_mode, file_name, flag_gzip1, zbuf1)
      call read_pick_series_head(FPz_f1, id_pick_mode, flag_gzip1,      &
     &                           picked_IO, zbuf1)
      call alloc_pick_sph_monitor_IO(picked_IO)
      call read_pick_series_comp_name                                   &
     &   (FPz_f1, id_pick_mode, flag_gzip1, picked_IO, zbuf1)
!
      num = picked_IO%num_mode * picked_IO%num_layer
      call s_count_monitor_time_series                                  &
     &   (flag_log, FPz_f1, id_pick_mode, flag_gzip1, num,              &
     &    start_time, end_time, true_start, true_end,                   &
     &    num_count, icou_skip, zbuf1)
!
      if(flag_gzip1) then
        ierr =  rewind_gzfile(FPz_f1)
      else
        rewind(id_pick_mode)
      end if
!
      call read_pick_series_head(FPz_f1, id_pick_mode, flag_gzip1,      &
     &                           picked_IO, zbuf1)
      call read_pick_series_comp_name                                   &
     &   (FPz_f1, id_pick_mode, flag_gzip1, picked_IO, zbuf1)
!
      call alloc_pick_sph_series(num_count, picked_IO)
      call read_picked_sph_spectr_series                                &
     &   (flag_log, FPz_f1, id_pick_mode, flag_gzip1,                   &
     &    start_time, end_time, picked_IO, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_pick_mode, flag_gzip1, zbuf1)
!
      end subroutine load_picked_sph_spectr_series
!
! -----------------------------------------------------------------------
!
      subroutine read_picked_sph_spectr_series                          &
     &         (flag_log, FPz_f, id_stream, flag_gzip,                  &
     &          start_time, end_time, picked_IO, zbuf)
!
      logical, intent(in) :: flag_log
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i_step, ierr, icou, i
      real(kind = kreal) :: time
!
!
      icou = 0
      do
        call read_sph_spec_monitor(FPz_f, id_stream, flag_gzip,         &
     &      i_step, time, picked_IO, zbuf, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          icou = icou + 1
          call copy_to_pick_sph_series(icou, i_step, time, picked_IO)
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
      end subroutine read_picked_sph_spectr_series
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_pick_series_head(FPz_f, id_stream, flag_gzip,     &
     &                                 picked_IO, zbuf)
!
      use sel_gz_read_sph_mtr_header
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      type(sph_spectr_head_labels) :: sph_lbl_IN
      type(read_sph_spectr_data) :: sph_IN
!
!
      call gz_read_sph_pwr_layer_head                                   &
     &   (FPz_f, id_stream, flag_gzip, sph_lbl_IN, sph_IN, zbuf)
      picked_IO%num_layer = sph_IN%nri_sph
      picked_IO%num_mode =  sph_IN%ltr_sph
      picked_IO%num_field = sph_IN%nfield_sph_spec
      picked_IO%ntot_comp = sph_IN%ntot_sph_spec
      write(*,*) 'picked_IO%num_layer', picked_IO%num_layer
      write(*,*) 'picked_IO%num_mode', picked_IO%num_mode
      write(*,*) 'picked_IO%num_field', picked_IO%num_field
      write(*,*) 'picked_IO%ntot_comp', picked_IO%ntot_comp
!
      end subroutine read_pick_series_head
!
! -----------------------------------------------------------------------
!
      subroutine read_pick_series_comp_name                             &
     &         (FPz_f, id_stream, flag_gzip, picked_IO, zbuf)
!
      use sel_gz_read_sph_mtr_header
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      type(read_sph_spectr_data) :: sph_IN
!
!
      sph_IN%num_time_labels = 6
      sph_IN%nfield_sph_spec = picked_IO%num_field
      sph_IN%ntot_sph_spec = picked_IO%ntot_comp
      call alloc_sph_espec_name(sph_IN)
      call sel_read_sph_spectr_name(FPz_f, id_stream, flag_gzip,        &
     &   sph_IN%nfield_sph_spec, sph_IN%num_labels,                     &
     &   sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name, zbuf)
!
      picked_IO%spectr_name(1:picked_IO%ntot_comp) &
     &   = sph_IN%ene_sph_spec_name(7:picked_IO%ntot_comp+6)
!
      end subroutine read_pick_series_comp_name
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_spec_monitor(FPz_f, id_stream, flag_gzip,     &
     &          i_step, time, picked_IO, zbuf, ierr)
!
      use select_gz_stream_file_IO
      use spherical_harmonics
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
!
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: l, m, ipick, ist
!
!
      ierr = 1
      do ipick = 1, picked_IO%num_mode * picked_IO%num_layer
        ist = (ipick-1) * picked_IO%ntot_comp
        call sel_read_line_gz_stream(FPz_f, id_stream,                  &
     &                               flag_gzip, zbuf)
        if(zbuf%len_used .lt. 0) return
!
        read(zbuf%fixbuf(1),*,err=99,end=99) i_step, time,              &
     &     picked_IO%idx_sph(ipick,1), picked_IO%radius(ipick),         &
     &     l, m, picked_IO%d_pk(ist+1:ist+picked_IO%ntot_comp)
        picked_IO%idx_sph(ipick,2) = get_idx_by_full_degree_order(l,m)
        picked_IO%idx_sph(ipick,3) = l
        picked_IO%idx_sph(ipick,4) = m
      end do
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine read_sph_spec_monitor
!
! -----------------------------------------------------------------------
!
      end module picked_sph_spectr_data_IO
