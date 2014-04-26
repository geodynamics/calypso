!t_average_nusselt.f90
!      program t_average_nusselt
!
!        programmed by H.Matsui on Apr., 2014
!
      program t_average_nusselt
!
      use m_precision
      use m_constants
!
      use m_no_heat_Nusselt_num
!
      implicit  none
!
      real(kind = kreal) :: prev_Nu(2)
      real(kind = kreal) :: ave_Nu(2)
      real(kind = kreal) :: sdev_Nu(2)
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: i_step, ierr, icou, ipick, nd, i
      integer(kind = kint) :: istep_start
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
      read(5,*) Nusselt_file_head
!
      write(*,*) 'Input start and end time'
      read(5,*) start_time, end_time
!
      call open_read_no_heat_source_Nu(id_pick)
!
!       Evaluate time average
!
      icou = 0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' averaging finished. Count=  ', icou
      do
        call read_no_heat_source_Nu(id_pick, i_step, time, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          prev_Nu(1) = Nu_ICB
          prev_Nu(2) = Nu_CMB
!
          if(icou .eq. 0) then
            true_start = time
          else
            ave_Nu(1) = ave_Nu(1)                                       &
     &             + half*(Nu_ICB + prev_Nu(1)) * (time - prev_time)
            ave_Nu(2) = ave_Nu(2)                                       &
     &             + half*(Nu_CMB + prev_Nu(2)) * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', i_step,  ' averaging finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
      write(*,*)
      close(id_pick)
!
      acou = one / (end_time - true_start)
      ave_Nu(1:2) = ave_Nu(1:2) * acou
!
!       Evaluate standard deviation
!
      call open_read_no_heat_source_Nu(id_pick)
!
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' deviation finished. Count=  ', icou
      icou = 0
      do
        call read_no_heat_source_Nu(id_pick, i_step, time, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          prev_Nu(1) = (Nu_ICB - ave_Nu(1))**2
          prev_Nu(2) = (Nu_CMB - ave_Nu(2))**2
!
          if(icou .eq. 0) then
            true_start = time
          else
            sdev_Nu(1) = sdev_Nu(1)                                     &
     &             + half*( (Nu_ICB - ave_Nu(1))**2 + prev_Nu(1))       &
     &                   * (time - prev_time)
            sdev_Nu(2) = sdev_Nu(2)                                     &
     &             + half*( (Nu_CMB - ave_Nu(2))**2 + prev_Nu(2))       &
     &                   * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', i_step,  ' deviation finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
      write(*,*)
      close(id_pick)
!
      acou = one / (end_time - true_start)
      sdev_Nu(1:2) = sqrt(sdev_Nu(1:2)) * acou
!
!    output Results
!
      write(*,'(a,1p2e25.15e3)') 'Inner and outer radius: ',            &
     &                          r_ICB_Nu, r_CMB_Nu
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
