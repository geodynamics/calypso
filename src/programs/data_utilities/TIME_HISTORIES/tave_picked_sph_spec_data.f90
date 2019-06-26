!tave_picked_sph_spec_data.f90
!      program tave_picked_sph_spec_data
!
!        programmed by H.Matsui on Dec., 2012
!
      program tave_picked_sph_spec_data
!
      use m_precision
      use m_constants
      use t_picked_sph_spectr_data_IO
!
      implicit  none
!
      type(picked_spectrum_data_IO), save :: pick_IO
!
      real(kind = kreal), allocatable :: prev_spec(:,:)
      real(kind = kreal), allocatable :: ave_spec(:,:)
      real(kind = kreal), allocatable :: rms_spec(:,:)
      real(kind = kreal), allocatable :: sdev_spec(:,:)
!
      character(len=kchara) :: evo_header
      character(len=kchara) :: tave_header
      character(len=kchara) :: trms_header
      character(len=kchara) :: sdev_header
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: i_step, ierr, icou, i, nd
      real(kind = kreal) :: acou, time, prev_time
      real(kind = kreal) :: start_time, end_time, true_start
!
!
      write(*,*) 'Input picked spectr evolution file header'
      read(5,*) evo_header
!
      write(tave_header,'(a6,a)') 't_ave_', trim(evo_header)
      write(trms_header,'(a8,a)') 't_rms_', trim(evo_header)
      write(sdev_header,'(a8,a)') 't_sigma_', trim(evo_header)
!
      write(*,*) 'Input start and end time'
      read(5,*) start_time, end_time
!
      call open_sph_spec_read(id_pick, evo_header, pick_IO)
!
      allocate(prev_spec(pick_IO%ntot_comp,pick_IO%ntot_pick_spectr))
      allocate(ave_spec(pick_IO%ntot_comp,pick_IO%ntot_pick_spectr))
      allocate(rms_spec(pick_IO%ntot_comp,pick_IO%ntot_pick_spectr))
      allocate(sdev_spec(pick_IO%ntot_comp,pick_IO%ntot_pick_spectr))
!
!$omp parallel workshare
      prev_spec =  0.0d0
      ave_spec =   0.0d0
      rms_spec =   0.0d0
      sdev_spec =  0.0d0
!$omp end parallel workshare
!
!       Evaluate time average
!
      icou = 0
      true_start = start_time
      prev_time = true_start
      do
        call read_sph_spec_monitor                                      &
     &     (id_pick, i_step, time, pick_IO, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          if(icou .eq. 0) then
            true_start = time
          else
!
!$omp parallel
            do i = 1, pick_IO%ntot_pick_spectr
!$omp do
              do nd = 1, pick_IO%ntot_comp
                ave_spec(nd,i) = ave_spec(nd,i) + half                  &
     &           * (pick_IO%d_pk(nd,i) + prev_spec(nd,i))               &
     &           * (time - prev_time)
                rms_spec(nd,i) = rms_spec(nd,i) + half                  &
     &           * (pick_IO%d_pk(nd,i)**2 + prev_spec(nd,i)**2)         &
     &           * (time - prev_time)
              end do
!$omp end do nowait
            end do
!$omp end parallel
          end if
!
!$omp parallel
          do i = 1, pick_IO%ntot_pick_spectr
!$omp do
            do nd = 1, pick_IO%ntot_comp
              prev_spec(nd,i) = pick_IO%d_pk(nd,i)
            end do
!$omp end do nowait
          end do
!$omp end parallel
!
          icou = icou + 1
          write(*,*) 'step ', i_step,                                   &
     &        ' is added for time average: count is  ', icou, time
        end if
        prev_time = time
!
        if(time .ge. end_time) exit
      end do
      close(id_pick)
!
      acou = one / (time - true_start)
!$omp parallel
      do i = 1, pick_IO%ntot_pick_spectr
!$omp do
        do nd = 1, pick_IO%ntot_comp
          sdev_spec(nd,i) = rms_spec(nd,i) - ave_spec(nd,i)**2
!
          ave_spec(nd,i) =   ave_spec(nd,i) * acou
          rms_spec(nd,i) =   sqrt(rms_spec(nd,i) * acou)
          sdev_spec(nd,i) =  sqrt(sdev_spec(nd,i) * acou)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!    output time average
!
      do i = 1, pick_IO%ntot_pick_spectr
        do nd = 1, pick_IO%ntot_comp
          pick_IO%d_pk(nd,i) = ave_spec(nd,i)
        end do
      end do
!
      call write_tave_sph_spec_monitor                                  &
     &   (tave_header, i_step, time, true_start, pick_IO)
!
!    output RMS deviation
!
      do i = 1, pick_IO%ntot_pick_spectr
        do nd = 1, pick_IO%ntot_comp
          pick_IO%d_pk(nd,i) = rms_spec(nd,i)
        end do
      end do
!
      call write_tave_sph_spec_monitor                                  &
     &   (trms_header, i_step, time, true_start, pick_IO)
!
!    output standard deviation
!
      do i = 1, pick_IO%ntot_pick_spectr
        do nd = 1, pick_IO%ntot_comp
          pick_IO%d_pk(nd,i) = sdev_spec(nd,i)
        end do
      end do
!
      call write_tave_sph_spec_monitor                                  &
     &   (sdev_header, i_step, time, true_start, pick_IO)
!
      call dealloc_pick_sph_monitor_IO(pick_IO)
      deallocate(prev_spec, ave_spec, sdev_spec)
!
      write(*,*) '***** program finished *****'
      stop
!
      end program tave_picked_sph_spec_data
