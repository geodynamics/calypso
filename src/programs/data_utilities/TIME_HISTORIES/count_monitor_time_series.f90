!>@file   count_monitor_time_series.f90
!!@brief  module count_monitor_time_series
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2021
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      function c_to_fstring(string)
!!        Character(1,C_char), Intent(In) :: string(*)
!!        Character(:,C_char), Allocatable :: c_to_fstring
!!
!!      subroutine s_count_monitor_time_series                          &
!!     &         (flag_log, FPz_f, id_stream, flag_gzip, nitem_snap,    &
!!     &          start_time, end_time, true_start, true_end,           &
!!     &          num_count, icou_skip, zbuf)
!!      subroutine s_count_monitor_time_start                           &
!!     &         (flag_log, FPz_f, id_stream, flag_gzip, nitem_snap,    &
!!     &          start_time, icou_skip, zbuf)
!!      subroutine s_skip_monitor_time_series                           &
!!     &         (flag_log, FPz_f, id_stream, flag_gzip, nitem_snap,    &
!!     &          icou_skip, zbuf)
!!        logical, intent(in) :: flag_log
!!        integer(kind = kint), intent(in) :: id_file, nitem_snap
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        integer(kind = kint), intent(inout) :: num_count, icou_skip
!!        real(kind = kreal), intent(inout) :: true_start, true_end
!!      subroutine read_ascii_sph_spectr_time(id_file, nitem_snap,      &
!!     &                                      i_step, time, ierr)
!!        integer(kind = kint), intent(in) :: id_file, nitem_snap
!!        integer(kind = kint), intent(inout) :: i_step, ierr
!!        real(kind = kreal), intent(inout) :: time
!!
!!      subroutine cal_time_ave_picked_sph_spectr(num_step, time, imask,&
!!     &          num_comps, d_series, ave_spec, rms_spec, sdev_spec)
!!        integer(kind = kint), intent(in) :: num_step, num_comps
!!        real(kind = kreal), intent(in) :: time(num_step)
!!        real(kind = kreal), intent(in) :: d_series(num_comps,num_step)
!!        integer(kind = kint), intent(in) :: imask(num_step)
!!        real(kind = kreal), intent(inout) :: sdev_spec(num_comps)
!!        real(kind = kreal), intent(inout) :: ave_spec(num_comps)
!!        real(kind = kreal), intent(inout) :: rms_spec(num_comps)
!!@endverbatim
!!
!!@n @param  id_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module count_monitor_time_series
!
      use m_precision
      use m_constants
      use t_buffer_4_gzip
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      function c_to_fstring(string)
!
      use ISO_C_BINDING
!
      Character(1,C_char), Intent(In) :: string(*)
      Character(:,C_char), Allocatable :: c_to_fstring
!
      Integer i,len
      len = 1
      Do While (string(len) .ne. C_null_char)
        len = len + 1
      End Do
      len = len - 1
      Allocate(Character(len,C_char) :: c_to_fstring)
      Do i=1,len
        c_to_fstring(i:i) = string(i)
      End Do
!
      end function c_to_fstring
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine s_count_monitor_time_series                            &
     &         (flag_log, FPz_f, id_stream, flag_gzip, nitem_snap,      &
     &          start_time, end_time, true_start, true_end,             &
     &          num_count, icou_skip, zbuf)
!
      use select_gz_stream_file_IO
!
      logical, intent(in) :: flag_log
      character, pointer, intent(in) :: FPz_f
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_stream, nitem_snap
      real(kind = kreal), intent(in) :: start_time, end_time
!
      integer(kind = kint), intent(inout) :: num_count, icou_skip
      real(kind = kreal), intent(inout) :: true_start, true_end
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i_step, i
      real(kind = kreal) :: time
!
!
      icou_skip = 0
      num_count =  0
      true_start = start_time
      true_end = true_start
      do
        do i = 1, nitem_snap
          call sel_read_line_gz_stream(FPz_f, id_stream,                &
     &                                 flag_gzip, zbuf)
          if(zbuf%len_used .le. 0) go to 99
        end do
        read(zbuf%fixbuf(1),*,err=99) i_step, time
!
        if(time .ge. start_time) then
          if(num_count .eq. 0) true_start = time
          num_count = num_count + 1
        else
          icou_skip = icou_skip + 1
        end if
!
        if(flag_log) then
          write(*,'(65a1,a6,i12,a8,f12.6,a15,i12)',advance="NO")        &
     &       (char(8),i=1,65), 'step= ', i_step, ', time= ', time,      &
     &       ', Read Count:  ', num_count
        end if
!
        if(time .ge. end_time) exit
      end do
  99  continue
      if(flag_log) write(*,*)
      true_end = time
!
      end subroutine s_count_monitor_time_series
!
! -----------------------------------------------------------------------
!
      subroutine s_count_monitor_time_start                             &
     &         (flag_log, FPz_f, id_stream, flag_gzip, nitem_snap,      &
     &          start_time, icou_skip, zbuf)
!
      use select_gz_stream_file_IO
!
      logical, intent(in) :: flag_log
      character, pointer, intent(in) :: FPz_f
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_stream, nitem_snap
      real(kind = kreal), intent(in) :: start_time
!
      integer(kind = kint), intent(inout) :: icou_skip
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i_step, i
      real(kind = kreal) :: time
!
!
      icou_skip = 0
      do
        do i = 1, nitem_snap
          call sel_read_line_gz_stream(FPz_f, id_stream,                &
     &                                 flag_gzip, zbuf)
          if(zbuf%len_used .le. 0) go to 99
        end do
        read(zbuf%fixbuf(1),*,err=99) i_step, time
!
        if(time .ge. start_time) exit
        icou_skip = icou_skip + 1
!
        if(flag_log) then
          write(*,'(65a1,a6,i12,a8,f12.6,a15,i12)',advance="NO")        &
     &       (char(8),i=1,65), 'step= ', i_step, ', time= ', time,      &
     &       ', Read Count:  ', icou_skip
        end if
      end do
  99  continue
      if(flag_log) write(*,*)
!
      end subroutine s_count_monitor_time_start
!
! -----------------------------------------------------------------------
!
      subroutine s_skip_monitor_time_series                             &
     &         (flag_log, FPz_f, id_stream, flag_gzip, nitem_snap,      &
     &          icou_skip, zbuf)
!
      use select_gz_stream_file_IO
!
      logical, intent(in) :: flag_log
      character, pointer, intent(in) :: FPz_f
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_stream, nitem_snap
      integer(kind = kint), intent(in) :: icou_skip
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i_step, i, icou
      real(kind = kreal) :: time
!
!
      do icou = 1, icou_skip
        do i = 1, nitem_snap
          call sel_read_line_gz_stream(FPz_f, id_stream,                &
     &                                 flag_gzip, zbuf)
          if(zbuf%len_used .le. 0) go to 99
        end do
        read(zbuf%fixbuf(1),*,err=99) i_step, time
!
        if(flag_log) then
          write(*,'(65a1,a6,i12,a8,f12.6,a15,i12)',advance="NO")        &
     &       (char(8),i=1,65), 'step= ', i_step, ', time= ', time,      &
     &       ', Skip Count:  ', icou
        end if
      end do
  99  continue
      if(flag_log) write(*,*)
!
      end subroutine s_skip_monitor_time_series
!
! -----------------------------------------------------------------------
!
      subroutine read_ascii_sph_spectr_time(id_file, nitem_snap,        &
     &                                      i_step, time, ierr)
!
      integer(kind = kint), intent(in) :: id_file, nitem_snap
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
      integer(kind = kint) :: ipick
!
!
      ierr = 0
      do ipick = 1, nitem_snap
        read(id_file,*,err=99,end=99) i_step, time
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_ascii_sph_spectr_time
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_time_ave_picked_sph_spectr(num_step, time, imask,  &
     &          num_comps, d_series, ave_spec, rms_spec, sdev_spec)
!
      integer(kind = kint), intent(in) :: num_step, num_comps
      real(kind = kreal), intent(in) :: time(num_step)
      real(kind = kreal), intent(in) :: d_series(num_comps,num_step)
      integer(kind = kint), intent(in) :: imask(num_step)
      real(kind = kreal), intent(inout) :: sdev_spec(num_comps)
      real(kind = kreal), intent(inout) :: ave_spec(num_comps)
      real(kind = kreal), intent(inout) :: rms_spec(num_comps)
!
      integer(kind = kint) :: icou
      real(kind = kreal) :: acou, total_t
!
!
!$omp parallel workshare
      ave_spec(1:num_comps) =   0.0d0
      rms_spec(1:num_comps) =   0.0d0
      sdev_spec(1:num_comps) =  0.0d0
!$omp end parallel workshare
!
      total_t = 1.0d-99
      do icou = 1, num_step-1
        total_t = total_t + (time(icou+1) - time(icou))                 &
     &                     * dble(imask(icou))
!$omp parallel workshare
        ave_spec(1:num_comps) = ave_spec(1:num_comps)                   &
     &                         + half * (d_series(1:num_comps,icou)     &
     &                                 + d_series(1:num_comps,icou+1))  &
     &                         * (time(icou+1) - time(icou))            &
     &                         * dble(imask(icou))
!$omp end parallel workshare
      end do
      acou = one / total_t
!
!$omp parallel workshare
      ave_spec(1:num_comps) = ave_spec(1:num_comps) * acou
!$omp end parallel workshare

      do icou = 1, num_step-1
!$omp parallel workshare
        rms_spec(1:num_comps) = rms_spec(1:num_comps)                   &
     &                       + half * (d_series(1:num_comps,icou)**2    &
     &                               + d_series(1:num_comps,icou+1)**2) &
     &                         * (time(icou+1) - time(icou))            &
     &                         * dble(imask(icou))
        sdev_spec(1:num_comps) = sdev_spec(1:num_comps)                 &
     &                       + half * ((d_series(1:num_comps,icou)      &
     &                                 - ave_spec(1:num_comps))**2      &
     &                               + (d_series(1:num_comps,icou+1)    &
     &                                 - ave_spec(1:num_comps))**2)     &
     &                         * (time(icou+1) - time(icou))            &
     &                         * dble(imask(icou))
!$omp end parallel workshare
      end do
!$omp parallel workshare
      rms_spec(1:num_comps) =   sqrt(rms_spec(1:num_comps) * acou)
      sdev_spec(1:num_comps) =  sqrt(sdev_spec(1:num_comps) * acou)
!$omp end parallel workshare
!
      write(*,*) 'Integration time: ', total_t
!
      end subroutine cal_time_ave_picked_sph_spectr
!
! -------------------------------------------------------------------
!
      end module count_monitor_time_series
