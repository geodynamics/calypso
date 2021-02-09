!>@file   sph_mean_square_IO_select.f90
!!@brief  module sph_mean_square_IO_select
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine select_input_sph_series_head                         &
!!     &         (id_file, iflag_old_fmt, iflag_vol_ave, sph_IN)
!!      subroutine select_input_sph_series_data                         &
!!     &         (id_file, iflag_old_fmt, iflag_vol_ave, sph_IN, ierr)
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!
!!      subroutine select_input_sph_pwr_head                            &
!!     &         (id_file, iflag_old_fmt, iflag_vol_ave, sph_IN)
!!      subroutine select_input_sph_pwr_data                            &
!!     &         (id_file, iflag_old_fmt, iflag_vol_ave, sph_IN, ierr)
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!
!!      subroutine select_output_sph_pwr_head                           &
!!     &         (id_file, iflag_vol_ave, sph_IN)
!!      subroutine select_output_sph_series_data                        &
!!     &         (id_file, iflag_vol_ave, sph_IN)
!!      subroutine select_output_sph_pwr_data                           &
!!     &         (id_file, iflag_vol_ave, sph_IN)
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
      subroutine select_input_sph_series_head                           &
     &         (id_file, iflag_old_fmt, iflag_vol_ave, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_old_fmt, iflag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      if(iflag_old_fmt .gt. 0) then
        if(iflag_vol_ave .eq. 1) then
          call input_sph_pwr_vol_head_old(id_file, sph_IN)
        else
          call input_sph_pwr_layer_head_old(id_file, sph_IN)
        end if
!
      else
        if(iflag_vol_ave .eq. 1) then
          call input_sph_pwr_vol_head(id_file, sph_IN)
        else
          call input_sph_pwr_layer_head(id_file, sph_IN)
        end if
      end if
!
      end subroutine select_input_sph_series_head
!
!   --------------------------------------------------------------------
!
      subroutine select_input_sph_series_data                           &
     &         (id_file, iflag_old_fmt, iflag_vol_ave, sph_IN, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_old_fmt, iflag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(iflag_vol_ave .eq. 1) then
        call read_volume_pwr_sph(id_file, sph_IN, ierr)
      else
        if(iflag_old_fmt .gt. 0) then
          call read_layer_pwr_sph_old(id_file, sph_IN, ierr)
        else
          call read_layer_pwr_sph(id_file, sph_IN, ierr)
        end if
      end if
!
      end subroutine select_input_sph_series_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_input_sph_pwr_head                              &
     &         (id_file, iflag_old_fmt, iflag_vol_ave, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_old_fmt, iflag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      if(iflag_old_fmt .gt. 0) then
        if(iflag_vol_ave .eq. 1) then
          call input_sph_spectr_vol_head_old(id_file, sph_IN)
        else
          call input_sph_spectr_layer_head_old(id_file, sph_IN)
        end if
!
      else
        if(iflag_vol_ave .eq. 1) then
          call input_sph_spectr_vol_head(id_file, sph_IN)
        else
          call input_sph_spectr_layer_head(id_file, sph_IN)
        end if
      end if
!
      end subroutine select_input_sph_pwr_head
!
!   --------------------------------------------------------------------
!
      subroutine select_input_sph_pwr_data                              &
     &         (id_file, iflag_old_fmt, iflag_vol_ave, sph_IN, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_old_fmt, iflag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(iflag_vol_ave .eq. 1) then
        call read_volume_spectr_sph(id_file, sph_IN, ierr)
      else
        if(iflag_old_fmt .gt. 0) then
          call read_layer_spectr_sph_old(id_file, sph_IN, ierr)
        else
          call read_layer_spectr_sph(id_file, sph_IN, ierr)
        end if
      end if
!
      end subroutine select_input_sph_pwr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_output_sph_pwr_head                             &
     &         (id_file, iflag_vol_ave, sph_IN)
!
      use simple_sph_spectr_data_IO
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      if(iflag_vol_ave .eq. 1) then
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
     &         (id_file, iflag_vol_ave, sph_IN)
!
      use simple_sph_spectr_data_IO
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_vol_ave
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
!
      if(iflag_vol_ave .eq. 1) then
        call write_vol_sph_data(id_file, sph_IN)
      else
        call write_layer_sph_data(id_file, sph_IN)
      end if
!
      end subroutine select_output_sph_series_data
!
!   --------------------------------------------------------------------
!
      subroutine select_output_sph_pwr_data                             &
     &         (id_file, iflag_vol_ave, sph_IN)
!
      use simple_sph_spectr_data_IO
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_vol_ave
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
!
      if(iflag_vol_ave .eq. 1) then
        call write_vol_spectr_data(id_file, sph_IN)
      else
        call write_layer_spectr_data(id_file, sph_IN)
      end if
!
      end subroutine select_output_sph_pwr_data
!
!   --------------------------------------------------------------------
!
      end module sph_mean_square_IO_select
