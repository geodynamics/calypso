!>@file   t_average_sph_ene_spec.f90
!!        program t_average_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Evaluate time average and standard deviation 
!!        from spherical harmonics spectrum data
!
      program t_average_sph_ene_spec
!
      use m_precision
!
      use m_tave_sph_ene_spectr
      use cal_tave_sph_ene_spectr
!
      implicit none
!
!
      integer(kind = kint) :: ierr
      integer(kind = kint) :: icou, istep
      real(kind = kreal) :: start_time, end_time
!
!
      call select_sph_ene_spec_data_file
      call set_org_ene_spec_file_name
!
      write(*,*) 'Input start and end time'
      read(*,*) start_time, end_time
!
      if(iflag_sph_ene_file .eq. 1) then
        call count_degree_on_volume_data
      else  if(iflag_sph_ene_file .eq. 2) then
        call count_degree_on_layer_data
      else
        call count_degree_one_layer_data
      end if
      call allocate_sph_espec_data
      call allocate_tave_sph_espec_data
!
!    Evaluate time average
!
      call open_org_ene_spec_data
!
      ist_true = -1
      icou = 0
      do
        if(iflag_sph_ene_file .eq. 1) then
          call read_org_volume_ene_data(istep, ierr)
        else
          call read_org_layer_ene_data(istep, ierr)
        end if
        if(ierr.gt.0) go to 99
!
        if (time_sph .ge. start_time) then
          if (ist_true .eq. -1) then
            ist_true = istep
          end if
          icou = icou + 1
          ied_true = istep
!
          call sum_average_ene_sph
          if (icou .eq. 1) then
            time_ini = time_sph
            call reset_tave_sph_espec_data
          end if
        end if
!
        if (time_sph .ge. end_time) exit
!
        write(*,*) 'step', istep, 'averaging finished. Count: ', icou
      end do
   99 continue
      call close_ene_spec_data
!
      call divide_average_ene_sph
      call output_tave_ene_sph_data
!
      call close_ene_spec_data
!
!  Evaluate standard deviation
!
      call open_org_ene_spec_data
!
      ist_true = -1
      icou = 0
      do
        if(iflag_sph_ene_file .eq. 1) then
          call read_org_volume_ene_data(istep, ierr)
        else
          call read_org_layer_ene_data(istep, ierr)
        end if
        if(ierr.gt.0) go to 98
!
        if (time_sph .ge. start_time) then
          if (ist_true .eq. -1) then
            ist_true = istep
          end if
          icou = icou + 1
          ied_true = istep
!
          call sum_deviation_ene_sph
          if (icou .eq. 1) then
            time_ini = time_sph
            call reset_tsigma_sph_espec_data
          end if
        end if
!
        if (time_sph .ge. end_time) exit
!
        write(*,*) 'step', istep, 'deviation finished. Count: ', icou
!
      end do
   98 continue
      call close_ene_spec_data
!
      call divide_deviation_ene_sph
      call output_tsigma_ene_sph_data

!
      stop
      end program t_average_sph_ene_spec
