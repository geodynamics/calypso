!>@file   sph_mean_square_IO_select.f90
!!@brief  module sph_mean_square_IO_select
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine select_input_sph_series_head(id_file, flag_old_fmt,  &
!!     &          flag_spectr, flag_vol_ave, sph_IN)
!!      subroutine select_input_sph_series_data(id_file, flag_old_fmt,  &
!!     &         flag_spectr, flag_vol_ave, sph_IN, ierr)
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_old_fmt, flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!
!!      subroutine select_output_sph_pwr_head                           &
!!     &         (id_file, flag_vol_ave, sph_IN)
!!      subroutine select_output_sph_series_data                        &
!!     &         (id_file, flag_vol_ave, sph_IN)
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!
!!      integer(kind = kint) function lengh_spectr_data_line            &
!!     &                   (flag_spectr, flag_vol_ave, sph_IN)
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!@endverbatim
!
      module sph_mean_square_IO_select
!
      use m_precision
      use t_read_sph_spectra
      use simple_sph_spectr_head_IO
      use simple_sph_spectr_data_IO
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine select_input_sph_series_head(id_file, flag_old_fmt,    &
     &          flag_spectr, flag_vol_ave, sph_IN)
!
      use simple_sph_spectr_head_IO
!
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_old_fmt, flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      if(flag_vol_ave) then
        call read_sph_pwr_vol_head(id_file, sph_IN)
        sph_IN%nri_sph = 1
        if(flag_spectr) then
          sph_IN%num_time_labels = 3
        else
          sph_IN%num_time_labels = 2
        end if
      else
        call read_sph_pwr_layer_head(id_file, sph_IN)
        if(flag_spectr) then
          if(flag_old_fmt) then
            sph_IN%num_time_labels = 4
          else
            sph_IN%num_time_labels = 5
          end if
        else
          if(flag_old_fmt) then
            sph_IN%num_time_labels = 3
          else
            sph_IN%num_time_labels = 4
          end if
        end if
      end if
!
      call alloc_sph_espec_name(sph_IN)
      call read_sph_spectr_name                                         &
     &   (id_file, sph_IN%nfield_sph_spec, sph_IN%num_labels,           &
     &    sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name)
!
      if(flag_spectr .eqv. .FALSE.) sph_IN%ltr_sph = 0
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
!
      end subroutine select_input_sph_series_head
!
!   --------------------------------------------------------------------
!
      subroutine select_input_sph_series_data(id_file, flag_old_fmt,    &
     &         flag_spectr, flag_vol_ave, sph_IN, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_old_fmt, flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(flag_vol_ave) then
        if(flag_spectr) then
          call read_volume_spectr_sph(id_file, sph_IN, ierr)
        else
          call read_volume_pwr_sph(id_file, sph_IN, ierr)
        end if
      else
        if(flag_spectr) then
          if(flag_old_fmt) then
            call read_layer_spectr_sph_old(id_file, sph_IN, ierr)
          else
            call read_layer_spectr_sph(id_file, sph_IN, ierr)
          end if
        else
          if(flag_old_fmt) then
            call read_layer_pwr_sph_old(id_file, sph_IN, ierr)
          else
            call read_layer_pwr_sph(id_file, sph_IN, ierr)
          end if
        end if
      end if
!
      end subroutine select_input_sph_series_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_output_sph_pwr_head                             &
     &         (id_file, flag_vol_ave, sph_IN)
!
      use simple_sph_spectr_data_IO
!
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      if(flag_vol_ave) then
        call write_sph_pwr_vol_head(id_file, sph_IN)
      else
        call write_sph_pwr_layer_head(id_file, sph_IN)
      end if
!
      end subroutine select_output_sph_pwr_head
!
!   --------------------------------------------------------------------
!
      subroutine select_output_sph_series_data                          &
     &         (id_file, flag_spectr, flag_vol_ave, sph_IN)
!
      use simple_sph_spectr_data_IO
!
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
!
      if(flag_spectr) then
        if(flag_vol_ave) then
          call write_vol_spectr_data(id_file, sph_IN)
        else
          call write_layer_spectr_data(id_file, sph_IN)
        end if
      else
        if(flag_vol_ave) then
          call write_vol_sph_data(id_file, sph_IN)
        else
          call write_layer_sph_data(id_file, sph_IN)
        end if
      end if
!
      end subroutine select_output_sph_series_data
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function lengh_spectr_data_line              &
     &                   (flag_spectr, flag_vol_ave, sph_IN)
!
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: nchara_line
!
      if(flag_spectr) then
        if(flag_vol_ave) then
          nchara_line = sph_IN%ntot_sph_spec * 25 + 16+25+16
        else
          nchara_line = sph_IN%ntot_sph_spec * 25 + 16+25+16+25+16
        end if
      else
        if(flag_vol_ave) then
          nchara_line = sph_IN%ntot_sph_spec * 25 + 16+25
        else
          nchara_line = sph_IN%ntot_sph_spec * 25 + 16+25+16+25
        end if
      end if
      lengh_spectr_data_line = nchara_line
!
      end function lengh_spectr_data_line
!
!   --------------------------------------------------------------------
!
      end module sph_mean_square_IO_select
