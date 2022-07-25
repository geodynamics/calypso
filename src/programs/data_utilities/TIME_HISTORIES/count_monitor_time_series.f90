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
!!      subroutine read_write_line_text(id_read, id_write,              &
!!     &                                nchara_line, line_text, ierr)
!!        integer(kind = kint), intent(in) :: id_read, id_write
!!        integer(kind = kint), intent(in) :: nchara_line
!!        character(len = 1), intent(inout) :: line_text(nchara_line)
!!        integer(kind = kint), intent(inout) :: ierr
!!
!!      subroutine s_count_monitor_time_series                          &
!!     &         (flag_log, id_file, nitem_snap, start_time, end_time,  &
!!     &          true_start, true_end, num_count)
!!        logical, intent(in) :: flag_log
!!        integer(kind = kint), intent(in) :: id_file, nitem_snap
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        integer(kind = kint), intent(inout) :: num_count
!!        real(kind = kreal), intent(inout) :: true_start, true_end
!!      subroutine read_sph_spectr_time(id_file, nitem_snap,            &
!!     &                              i_step, time, ierr)
!!        integer(kind = kint), intent(in) :: id_file, nitem_snap
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
      module count_monitor_time_series
!
      use m_precision
      use m_constants
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
! -------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_write_line_text(id_read, id_write,                &
     &                                nchara_line, line_text, ierr)
!
      integer(kind = kint), intent(in) :: id_read, id_write
      integer(kind = kint), intent(in) :: nchara_line
!
      character(len = 1), intent(inout) :: line_text(nchara_line)
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a1,i5,a5)') '(', nchara_line, '(a1))'
      ierr = 0
      read(id_read,fmt_txt,err=99,end=99) line_text(1:nchara_line)
      write(id_write,fmt_txt) line_text(1:nchara_line)
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_write_line_text
!
!   --------------------------------------------------------------------
!
      subroutine s_count_monitor_time_series                            &
     &         (flag_log, id_file, nitem_snap, start_time, end_time,    &
     &          true_start, true_end, num_count)
!
      logical, intent(in) :: flag_log
      integer(kind = kint), intent(in) :: id_file, nitem_snap
      real(kind = kreal), intent(in) :: start_time, end_time
!
      integer(kind = kint), intent(inout) :: num_count
      real(kind = kreal), intent(inout) :: true_start, true_end
!
      integer(kind = kint) :: i_step, ierr, i
      real(kind = kreal) :: time
!
!
      num_count = 0
      true_start = start_time
      true_end = true_start
      do
        call read_sph_spectr_time(id_file, nitem_snap,                  &
     &                            i_step, time, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          if(num_count .eq. 0) true_start = time
!
          num_count = num_count + 1
          if(flag_log) then
            write(*,'(69a1,a5,i12,a4,1pe16.8e3,a20,i12)',advance="NO")  &
     &        (char(8),i=1,69), 'step ', i_step,                        &
     &        ' at ', time, ' is read. count is  ', num_count
          end if
        end if
!
        if(time .ge. end_time) exit
      end do
      if(flag_log) write(*,*)
      true_end = time
!
      end subroutine s_count_monitor_time_series
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_spectr_time(id_file, nitem_snap,              &
     &                                i_step, time, ierr)
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
      end subroutine read_sph_spectr_time
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_time_ave_picked_sph_spectr(num_step, time,         &
     &          num_comps, d_series, ave_spec, rms_spec, sdev_spec)
!
      integer(kind = kint), intent(in) :: num_step, num_comps
      real(kind = kreal), intent(in) :: time(num_step)
      real(kind = kreal), intent(in) :: d_series(num_comps,num_step)
      real(kind = kreal), intent(inout) :: sdev_spec(num_comps)
      real(kind = kreal), intent(inout) :: ave_spec(num_comps)
      real(kind = kreal), intent(inout) :: rms_spec(num_comps)
!
      integer(kind = kint) :: icou
      real(kind = kreal) :: acou
!
!
      acou = one / (time(num_step) - time(1))
!$omp parallel workshare
      ave_spec(1:num_comps) =   0.0d0
      rms_spec(1:num_comps) =   0.0d0
      sdev_spec(1:num_comps) =  0.0d0
!$omp end parallel workshare
!
      do icou = 1, num_step-1
!$omp parallel workshare
        ave_spec(1:num_comps) = ave_spec(1:num_comps)                   &
     &                         + half * (d_series(1:num_comps,icou)     &
     &                                 + d_series(1:num_comps,icou+1))  &
     &                         * (time(icou+1) - time(icou))
!$omp end parallel workshare
      end do
!$omp parallel workshare
      ave_spec(1:num_comps) = ave_spec(1:num_comps) * acou
!$omp end parallel workshare

      do icou = 1, num_step-1
!$omp parallel workshare
        rms_spec(1:num_comps) = rms_spec(1:num_comps)                   &
     &                       + half * (d_series(1:num_comps,icou)**2    &
     &                               + d_series(1:num_comps,icou+1)**2) &
     &                         * (time(icou+1) - time(icou))
        sdev_spec(1:num_comps) = sdev_spec(1:num_comps)                 &
     &                       + half * ((d_series(1:num_comps,icou)      &
     &                                 - ave_spec(1:num_comps))**2      &
     &                               + (d_series(1:num_comps,icou+1)    &
     &                                 - ave_spec(1:num_comps))**2)     &
     &                         * (time(icou+1) - time(icou))
!$omp end parallel workshare
      end do
!$omp parallel workshare
      rms_spec(1:num_comps) =   sqrt(rms_spec(1:num_comps) * acou)
      sdev_spec(1:num_comps) =  sqrt(sdev_spec(1:num_comps) * acou)
!$omp end parallel workshare
!
      end subroutine cal_time_ave_picked_sph_spectr
!
! -------------------------------------------------------------------
!
      end module count_monitor_time_series
