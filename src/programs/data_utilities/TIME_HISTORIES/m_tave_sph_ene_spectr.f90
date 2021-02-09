!>@file   m_tave_sph_ene_spectr.f90
!!        module m_tave_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine sph_spectr_average                                   &
!!     &         (fname_org, iflag_spectr, iflag_vol_ave,               &
!!     &          spec_evo_p, sph_IN)
!!      subroutine sph_spectr_std_deviation                             &
!!     &         (fname_org, iflag_spectr, iflag_vol_ave,               &
!!     &          spec_evo_p, sph_IN)
!!        type(sph_spectr_file_param), intent(in) :: spec_evo_p
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!@endverbatim
!
      module m_tave_sph_ene_spectr
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
      use t_ctl_param_sph_series_util
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_file_rms =      34
!
      real(kind = kreal), allocatable :: ave_spec_l(:,:,:)
      real(kind = kreal), allocatable :: sigma_spec_l(:,:,:)
      real(kind = kreal), allocatable :: spectr_pre_l(:,:,:)
!
      private :: id_file_rms
      private :: ave_spec_l, sigma_spec_l, spectr_pre_l
      private :: allocate_tave_sph_data, deallocate_tave_sph_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sph_spectr_average                                     &
     &         (fname_org, iflag_spectr, iflag_vol_ave,                 &
     &          spec_evo_p, sph_IN)
!
      use sph_mean_square_IO_select
      use cal_tave_sph_ene_spectr
!
      character(len = kchara), intent(in) :: fname_org
      integer(kind = kint), intent(in) :: iflag_spectr, iflag_vol_ave
      type(sph_spectr_file_param), intent(in) :: spec_evo_p
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len = kchara) :: file_name
      real(kind = kreal) :: time_ini, pre_time
      integer(kind = kint) :: i, icou, ltr, ierr, ist_true
!
!
      open(id_file_rms, file=fname_org)
!
      if(iflag_spectr .gt. 0) then
        call select_input_sph_pwr_head(id_file_rms,                     &
     &      spec_evo_p%iflag_old_fmt, iflag_vol_ave, sph_IN)
        ltr = sph_IN%ltr_sph
      else
        call select_input_sph_series_head(id_file_rms,                  &
     &      spec_evo_p%iflag_old_fmt, iflag_vol_ave, sph_IN)
        ltr = 0
      end if
!
      call allocate_tave_sph_data(ltr, sph_IN)
!
      icou = 0
      ist_true = -1
      pre_time = sph_IN%time
      write(*,'(a6,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=  ', icou
      do
        if(iflag_spectr .gt. 0) then
          call select_input_sph_pwr_data(id_file_rms,                   &
     &        spec_evo_p%iflag_old_fmt, iflag_vol_ave, sph_IN, ierr)
        else
          call select_input_sph_series_data(id_file_rms,                &
     &        spec_evo_p%iflag_old_fmt, iflag_vol_ave, sph_IN, ierr)
        end if
!
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. spec_evo_p%start_time) then
          if (ist_true .eq. -1) then
            ist_true = sph_IN%i_step
            time_ini = sph_IN%time
!
            call copy_ene_spectr_2_pre(sph_IN%time, pre_time,          &
     &          sph_IN%nri_sph, ltr, sph_IN%ntot_sph_spec,             &
     &          sph_IN%spectr_IO, ave_spec_l, spectr_pre_l)
          else
!
            call sum_average_ene_spectr(sph_IN%time, pre_time,         &
     &          sph_IN%nri_sph, ltr, sph_IN%ntot_sph_spec,             &
     &          sph_IN%spectr_IO, ave_spec_l, spectr_pre_l)

            icou = icou + 1
          end if
        end if
!
        write(*,'(60a1,a6,i12,a30,i12)',advance="NO") (char(8),i=1,60), &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=   ', icou
        if (sph_IN%time .ge. spec_evo_p%end_time) exit
      end do
!
   99 continue
      write(*,*)
      close(id_file_rms)
!
!
      call divide_average_ene_spectr(sph_IN%time, time_ini,             &
     &    sph_IN%nri_sph, ltr, sph_IN%ntot_sph_spec, ave_spec_l,        &
     &    sph_IN%spectr_IO)
!
!  Output average
      write(file_name, '(a6,a)') 't_ave_', trim(fname_org)
      open(id_file_rms, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (id_file_rms, iflag_vol_ave, sph_IN)
!
      if(iflag_spectr .gt. 0) then
        call select_output_sph_pwr_data                                 &
     &     (id_file_rms, iflag_vol_ave, sph_IN)
      else
        call select_output_sph_series_data                              &
     &     (id_file_rms, iflag_vol_ave, sph_IN)
      end if
!
      close(id_file_rms)
!
      call dealloc_sph_espec_data(sph_IN)
!
      end subroutine sph_spectr_average
!
!   --------------------------------------------------------------------
!
      subroutine sph_spectr_std_deviation                               &
     &         (fname_org, iflag_spectr, iflag_vol_ave,                 &
     &          spec_evo_p, sph_IN)
!
      use sph_mean_square_IO_select
      use cal_tave_sph_ene_spectr
!
      character(len = kchara), intent(in) :: fname_org
      integer(kind = kint), intent(in) :: iflag_spectr, iflag_vol_ave
      type(sph_spectr_file_param), intent(in) :: spec_evo_p
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len = kchara) :: file_name
      real(kind = kreal) :: time_ini, pre_time
      integer(kind = kint) :: i, icou, ltr, ierr, ist_true
!
!  Evaluate standard deviation
!
      write(*,*) 'Open file ', trim(fname_org)
      open(id_file_rms, file=fname_org)
!
      if(iflag_spectr .gt. 0) then
        call select_input_sph_pwr_head(id_file_rms,                     &
     &      spec_evo_p%iflag_old_fmt, iflag_vol_ave, sph_IN)
        ltr = sph_IN%ltr_sph
      else
        call select_input_sph_series_head(id_file_rms,                  &
     &      spec_evo_p%iflag_old_fmt, iflag_vol_ave, sph_IN)
        ltr = 0
      end if
!
      icou = 0
      ist_true = -1
      pre_time = sph_IN%time
      sigma_spec_l = 0.0d0
      write(*,'(a6,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' deviation finished. Count=  ', icou
      do
        if(iflag_spectr .gt. 0) then
          call select_input_sph_pwr_data(id_file_rms,                   &
     &        spec_evo_p%iflag_old_fmt, iflag_vol_ave, sph_IN, ierr)
        else
          call select_input_sph_series_data(id_file_rms,                &
     &        spec_evo_p%iflag_old_fmt, iflag_vol_ave, sph_IN, ierr)
        end if
!
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. spec_evo_p%start_time) then
          if (ist_true .eq. -1) then
            ist_true = sph_IN%i_step
            time_ini = sph_IN%time
            call copy_deviation_ene_2_pre(sph_IN%time, pre_time,        &
     &          sph_IN%nri_sph, ltr, sph_IN%ntot_sph_spec,              &
     &          sph_IN%spectr_IO, ave_spec_l, sigma_spec_l,             &
     &          spectr_pre_l)
!
          else
            call sum_deviation_ene_spectr(sph_IN%time, pre_time,        &
     &          sph_IN%nri_sph, ltr, sph_IN%ntot_sph_spec,              &
     &          sph_IN%spectr_IO, ave_spec_l, sigma_spec_l,             &
     &          spectr_pre_l)
!
            icou = icou + 1
          end if
        end if
!
        write(*,'(60a1,a6,i12,a30,i12)',advance="NO") (char(8),i=1,60), &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' deviation finished. Count=   ', icou
        if (sph_IN%time .ge. spec_evo_p%end_time) exit
      end do
   99 continue
      write(*,*)
      close(id_file_rms)
!
      call divide_deviation_ene_spectr(sph_IN%time, time_ini,           &
     &    sph_IN%nri_sph, ltr, sph_IN%ntot_sph_spec,                    &
     &    sigma_spec_l, sph_IN%spectr_IO)
!
      write(file_name, '(a8,a)') 't_sigma_', trim(fname_org)
      open(id_file_rms, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (id_file_rms, iflag_vol_ave,sph_IN)
!
      if(iflag_spectr .gt. 0) then
        call select_output_sph_pwr_data                                 &
     &     (id_file_rms, iflag_vol_ave, sph_IN)
      else
        call select_output_sph_series_data                              &
     &     (id_file_rms, iflag_vol_ave, sph_IN)
      end if
!
      close(id_file_rms)
!
      call dealloc_sph_espec_data(sph_IN)
      call deallocate_tave_sph_data
!
      end subroutine sph_spectr_std_deviation
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine allocate_tave_sph_data(ltr, sph_IN)
!
      integer(kind = kint), intent(in) :: ltr
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      integer(kind = kint) :: ncomp
!
!
      ncomp = sph_IN%ntot_sph_spec
      allocate( ave_spec_l(ncomp,0:ltr,sph_IN%nri_sph) )
      allocate( sigma_spec_l(ncomp,0:ltr,sph_IN%nri_sph) )
      allocate( spectr_pre_l(ncomp,0:ltr,sph_IN%nri_sph) )
!
      if(ncomp .le. 0) return
      ave_spec_l =  0.0d0
      sigma_spec_l =  0.0d0
      spectr_pre_l = 0.0d0
!
      end subroutine allocate_tave_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_tave_sph_data
!
      deallocate(ave_spec_l, sigma_spec_l, spectr_pre_l)
!
      end subroutine deallocate_tave_sph_data
!
!   --------------------------------------------------------------------
!
      end module m_tave_sph_ene_spectr
