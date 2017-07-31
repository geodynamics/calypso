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
      use t_read_sph_spectra
      use set_parallel_file_name
!
      implicit none
!
!
      integer(kind = kint) :: iflag_data_mode
      character(len = kchara) :: input_header
      character(len = kchara) :: fname_org_rms, fname_org_rms_l
      real(kind = kreal) :: start_time, end_time
      type(read_sph_spectr_data), save :: sph_IN_t
!
!
!
      write(*,*) ' Choose data to take average'
      write(*,*)  ' 0: Spectrum data over l, m, and l-m '
      write(*,*)  ' 1: Average over volume or sphere'
      write(*,*)  ' 2: One spectrum data '
      read(*,*) iflag_data_mode
!
      call select_sph_ene_spec_data_file(sph_IN_t, input_header)
!
      if(iflag_data_mode .eq. 0) then
        call add_dat_extension(input_header, fname_org_rms)
        write(fname_org_rms_l, '(a,a6)')                                &
     &                        trim(input_header), '_l.dat'
      else if(iflag_data_mode .eq. 1) then
        call add_dat_extension(input_header, fname_org_rms)
      else if(iflag_data_mode .eq. 2) then
        call add_dat_extension(input_header, fname_org_rms_l)
      end if
!
      write(*,*) 'Input start and end time'
      read(*,*) start_time, end_time
!
!    Evaluate time average
!
      sph_IN_t%iflag_spectr = 0
      if(iflag_data_mode .eq. 1) then
        call sph_spectr_average                                         &
     &     (fname_org_rms, start_time, end_time, sph_IN_t)
        call sph_spectr_std_deviation                                   &
     &     (fname_org_rms, start_time, end_time, sph_IN_t)
      end if
!
      sph_IN_t%iflag_spectr = 1
      if(iflag_data_mode .eq. 0 .or. iflag_data_mode .eq. 2) then
        call sph_spectr_average                                         &
     &     (fname_org_rms_l, start_time, end_time, sph_IN_t)
        call sph_spectr_std_deviation                                   &
     &     (fname_org_rms_l, start_time, end_time, sph_IN_t)
      end if
!
      if(iflag_data_mode .eq. 0) then
        write(fname_org_rms, '(a,a6)')                                  &
     &                        trim(input_header), '_m.dat'
        call sph_spectr_average                                         &
     &     (fname_org_rms, start_time, end_time, sph_IN_t)
        call sph_spectr_std_deviation                                   &
     &     (fname_org_rms, start_time, end_time, sph_IN_t)
!
        write(fname_org_rms,'(a,a7)')                                   &
     &                        trim(input_header), '_lm.dat'
        call sph_spectr_average                                         &
     &     (fname_org_rms, start_time, end_time, sph_IN_t)
        call sph_spectr_std_deviation                                   &
     &     (fname_org_rms, start_time, end_time, sph_IN_t)
      end if
!
      stop
!
      end program t_average_sph_ene_spec
