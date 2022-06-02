!t_average_nusselt.f90
!      program t_average_nusselt
!
!        programmed by H.Matsui on Apr., 2014
!
!
!! -----------------------------------------------------------------
!!    Input control file:  control_sph_time_average
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    nusselt_number_prefix        'Nusselt'
!!  end time_averaging_sph_monitor
!! -----------------------------------------------------------------
!

      program t_average_nusselt
!
      use m_precision
      use m_constants
!
      use t_no_heat_Nusselt
      use t_ctl_data_tave_sph_monitor
!
      implicit  none
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(nusselt_number_data), save :: Nu_t
!
      real(kind = kreal) :: prev_Nu(2) = 0.0d0
      real(kind = kreal) :: ave_Nu(2)
      real(kind = kreal) :: sdev_Nu(2)
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: i_step, ierr, icou, i
      real(kind = kreal) :: acou, time, prev_time
      real(kind = kreal) :: start_time, end_time, true_start
!
!
      write(*,*) '-----------------------------------------------'
      write(*,*) '                  CAUTION!!'
      write(*,*) 'This program can evaluate Nusselt number'
      write(*,*) 'only when there is NO internal heat source'
      write(*,*) 'in the outer core!!'
      write(*,*) '-----------------------------------------------'
      write(*,*) ''
      write(*,*) 'Input picked harmonics coefficients file prefix'
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
!
      if(tave_sph_ctl1%Nusselt_file_prefix%iflag .eq. 0) then
        write(*,*) 'Set File prefix for Nusselt number'
        stop
      end if
      Nu_t%Nusselt_file_head                                            &
     &      = tave_sph_ctl1%Nusselt_file_prefix%charavalue
!
      if(tave_sph_ctl1%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set start time'
        stop
      end if
      start_time = tave_sph_ctl1%start_time_ctl%realvalue
!
      if(tave_sph_ctl1%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set end time'
        stop
      end if
      end_time = tave_sph_ctl1%end_time_ctl%realvalue
!
!       Open Nusselt data file
!
      call open_read_no_heat_source_Nu(id_pick, Nu_t)
!
!       Evaluate time average
!
      icou = 0
      time = start_time
      prev_time = start_time
      ave_Nu(1:2) = 0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' averaging finished. Count=  ', icou
      do
        call read_no_heat_source_Nu                                     &
     &     (id_pick, i_step, time, Nu_t, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          if(icou .eq. 0) then
            true_start = time
            prev_time = time
            prev_Nu(1) = Nu_t%Nu_ICB
            prev_Nu(2) = Nu_t%Nu_CMB
          else
            ave_Nu(1) = ave_Nu(1) + half*(Nu_t%Nu_ICB + prev_Nu(1))     &
     &                 * (time - prev_time)
            ave_Nu(2) = ave_Nu(2) + half*(Nu_t%Nu_CMB + prev_Nu(2))     &
     &                 * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
        prev_Nu(1) = Nu_t%Nu_ICB
        prev_Nu(2) = Nu_t%Nu_CMB
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', i_step,  ' averaging finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
      write(*,*)
      close(id_pick)
!
      acou = one / (time - true_start)
      ave_Nu(1:2) = ave_Nu(1:2) * acou
!
!       Evaluate standard deviation
!
      call open_read_no_heat_source_Nu(id_pick, Nu_t)
!
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' deviation finished. Count=  ', icou
      icou = 0
      time = start_time
      prev_time = start_time
      sdev_Nu(1:2) = 0.0d0
      do
        call read_no_heat_source_Nu                                     &
     &     (id_pick, i_step, time, Nu_t, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          if(icou .eq. 0) then
            true_start = time
            prev_time = time
            prev_Nu(1) = (Nu_t%Nu_ICB - ave_Nu(1))**2
            prev_Nu(2) = (Nu_t%Nu_CMB - ave_Nu(2))**2
          else
            sdev_Nu(1) = sdev_Nu(1)                                     &
     &           + half*( (Nu_t%Nu_ICB - ave_Nu(1))**2 + prev_Nu(1))    &
     &                   * (time - prev_time)
            sdev_Nu(2) = sdev_Nu(2)                                     &
     &           + half*( (Nu_t%Nu_CMB - ave_Nu(2))**2 + prev_Nu(2))    &
     &                   * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
        prev_Nu(1) = (Nu_t%Nu_ICB - ave_Nu(1))**2
        prev_Nu(2) = (Nu_t%Nu_CMB - ave_Nu(2))**2
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', i_step,  ' deviation finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
      write(*,*)
      close(id_pick)
!
      acou = one / (time - true_start)
      sdev_Nu(1:2) = sqrt(sdev_Nu(1:2) * acou)
!
!    output Results
!
      write(*,'(a,1p2e25.15e3)') 'Inner and outer radius: ',            &
     &                          Nu_t%r_ICB_Nu, Nu_t%r_CMB_Nu
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, end_time
      write(*,'(a)') 'Average and Std. Dev. of Nu at ICB:'
      write(*,'(1p2e25.15e3)')  ave_Nu(1), sdev_Nu(1)
      write(*,'(a)') 'Average and Std. Dev. of Nu at CMB:'
      write(*,'(1p2e25.15e3)')  ave_Nu(2), sdev_Nu(2)
!
      write(*,*) '***** program finished *****'
      stop
!
      end program t_average_nusselt
