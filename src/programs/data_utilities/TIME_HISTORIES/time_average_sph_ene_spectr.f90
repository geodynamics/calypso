!>@file   time_average_sph_ene_spectr.f90
!!@brief  module time_average_sph_ene_spectr
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of spectrum data
!!
!!@verbatim
!!      integer(c_int) function                                         &
!!    &     time_ave_sdev_sph_volume_pwr_f(cname, cstart, cend) Bind(C)
!!      integer(c_int) function                                         &
!!    &     time_ave_sdev_sph_vol_spectr_f(cname, cstart, cend) Bind(C)
!!      integer(c_int) function                                         &
!!    &     time_ave_sdev_sph_layer_pwr_f(cname, cstart, cend) Bind(C)
!!      integer(c_int) function                                         &
!!    &     time_ave_sdev_sph_layer_spec_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine load_field_labels_f                                  &
!!     &         (l_truncation, yname, tave_spectr, sdev_spectr)        &
!!     &          bind(c, name="load_field_labels_f")
!!        character(1,C_char), intent(in) :: yname(*)
!!        integer(C_int), Value :: l_truncation
!!        real(c_double), intent(inout) ::  tave_spectr(*)
!!        real(c_double), intent(inout) ::  sdev_spectr(*)
!!@endverbatim
!
      module time_average_sph_ene_spectr
!
      use ISO_C_BINDING
!
      use m_precision
      use m_constants
!
      use t_read_sph_spectra
      use t_tave_sph_volume_spectr
      use count_monitor_time_series
!
      implicit  none
!
      type(read_sph_spectr_data), save, private :: tave_sph_IN
      type(vol_spectr_ave_sigma_work), save, private :: WK_tave_s
      type(sph_spectr_head_labels), save, private :: sph_lbl_IN1
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_sdev_sph_volume_pwr_f(cname, cstart, cend) Bind(C)
!
      use time_ave_sph_volume_mean
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: fname_org
!
      write(fname_org,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call time_ave_sdev_sph_volume_mean(fname_org,                     &
     &                                   start_time, end_time)
!
      time_ave_sdev_sph_volume_pwr_f = 0
      end function time_ave_sdev_sph_volume_pwr_f
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_sdev_sph_vol_spectr_f(cname, cstart, cend) Bind(C)
!
      use time_ave_sph_monitor_data
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: fname_org
!
      write(fname_org,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call time_ave_sdev_sph_volume_spec(fname_org,                     &
     &                                   start_time, end_time)
!
      time_ave_sdev_sph_vol_spectr_f = 0
      end function time_ave_sdev_sph_vol_spectr_f
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_sdev_sph_layer_pwr_f(cname, cstart, cend) Bind(C)
!
      use t_tave_sph_layer_mean
      use tave_sdev_sph_layer_mean
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: fname_org
!
      write(fname_org,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call time_ave_sdev_sph_layer_mean(fname_org,                      &
     &                                  start_time, end_time)
!
      time_ave_sdev_sph_layer_pwr_f = 0
      end function time_ave_sdev_sph_layer_pwr_f
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_sdev_sph_layer_spec_f(cname, cstart, cend) Bind(C)
!
      use t_tave_sph_layer_spectr
      use tave_sdev_sph_layer_spec
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: fname_org
!
      write(fname_org,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call time_ave_sdev_sph_layer_spec(fname_org,                      &
     &                                  start_time, end_time)
!
      time_ave_sdev_sph_layer_spec_f = 0
      end function time_ave_sdev_sph_layer_spec_f
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &          read_tave_sdev_sph_vol_spec_f(monitor_prefix_c) Bind(C)
!
      use t_tave_sph_volume_spectr
      use tave_sdev_sph_volume_spec
!
      character(1,C_char), intent(in) :: monitor_prefix_c(*)
!
      character(len=kchara) :: monitor_prefix
      integer(kind = kint) :: l_truncation
!
      write(monitor_prefix,'(a)') trim(c_to_fstring(monitor_prefix_c))
      call read_time_ave_sph_vol_spec(monitor_prefix, sph_lbl_IN1,      &
     &                                tave_sph_IN, WK_tave_s)
      read_tave_sdev_sph_vol_spec_f = tave_sph_IN%ltr_sph
!
      end function read_tave_sdev_sph_vol_spec_f
!
! -------------------------------------------------------------------
!
      subroutine load_field_labels_f                                    &
     &         (l_truncation, yname, tave_spectr, sdev_spectr)          &
     &          bind(c, name="load_field_labels_f")
!
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: yname(*)
      integer(C_int), Value :: l_truncation
      real(c_double), intent(inout) ::  tave_spectr(*)
      real(c_double), intent(inout) ::  sdev_spectr(*)
!
      integer(kind = kint) :: i, id_pick
      character(len=kchara) :: draw_name
!
      write(draw_name,'(a)') trim(c_to_fstring(yname))
!      write(*,*) 'Field: ', draw_name
!
!      do i = 1, tave_sph_IN%num_time_labels
!        write(*,*) 'In Fortran: ', i,                                  &
!     &            trim(tave_sph_IN%ene_sph_spec_name(i))
!      end do
      id_pick = 0
      do i = 1, tave_sph_IN%num_labels
        if(trim(draw_name) .eq. tave_sph_IN%ene_sph_spec_name(i)) then
          id_pick = i - tave_sph_IN%num_time_labels
          exit
        end if
      end do
      if(id_pick .le. 0) then
        write(*,*) 'Input field cannot be found.'
        return
      end if
!
      tave_spectr(1:l_truncation+1)                                     &
     &           = WK_tave_s%ave_spec_l(id_pick,0:l_truncation)
      sdev_spectr(1:l_truncation+1)                                     &
     &           = WK_tave_s%sigma_spec_l(id_pick,0:l_truncation)
!
!      write(*,*) 'In Fortran: ', trim(draw_name), id_pick
!      do i = 0, l_truncation
!        write(*,*) i, tave_spectr(i+1)
!      end do
!
      end subroutine load_field_labels_f
!
! -------------------------------------------------------------------
!
      end module time_average_sph_ene_spectr
