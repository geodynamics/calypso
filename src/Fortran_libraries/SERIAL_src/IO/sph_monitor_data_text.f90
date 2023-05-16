!>@file   sph_monitor_data_text.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!> @brief Time spectrum data output routines for utilities
!!
!!@verbatim
!!      function volume_pwr_data_text(i_step, time,                     &
!!     &                              ntot_sph_spec, spectr_IO)
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        integer(kind = kint), intent(in) :: ntot_sph_spec
!!        real(kind = kreal), intent(in) :: spectr_IO(ntot_sph_spec)
!!        character(len=(16 + (ntot_sph_spec+1)*25 + 1))                &
!!     &                                      :: volume_pwr_data_text
!!      function volume_spectr_data_text(i_step, time, i_mode,          &
!!     &                                 ntot_sph_spec, spectr_IO)
!!        integer(kind = kint), intent(in) :: i_step, i_mode
!!        real(kind = kreal), intent(in) :: time
!!        integer(kind = kint), intent(in) :: ntot_sph_spec
!!        real(kind = kreal), intent(in) :: spectr_IO(ntot_sph_spec)
!!        character(len=(2*16 + (ntot_sph_spec+1)*25 + 1))              &
!!     &                                      :: volume_spectr_data_text
!!      function layer_pwr_data_text(i_step, time, kr_sph, r_sph,       &
!!     &                             ntot_sph_spec, spectr_IO)
!!        integer(kind = kint), intent(in) :: i_step, kr_sph
!!        real(kind = kreal), intent(in) :: r_sph, time
!!        integer(kind = kint), intent(in) :: ntot_sph_spec
!!        real(kind = kreal), intent(in) :: spectr_IO(ntot_sph_spec)
!!        character(len=(2*16 + (ntot_sph_spec+2)*25 + 1))              &
!!     &                                      :: layer_pwr_data_text
!!      function layer_spectr_data_text(i_step, time, kr_sph, r_sph,    &
!!     &        i_mode, ntot_sph_spec, spectr_IO)
!!        integer(kind = kint), intent(in) :: i_step, kr_sph, i_mode
!!        real(kind = kreal), intent(in) :: r_sph, time
!!        integer(kind = kint), intent(in) :: ntot_sph_spec
!!        real(kind = kreal), intent(in) :: spectr_IO(ntot_sph_spec)
!!        character(len=(3*16 + (ntot_sph_spec+2)*25 + 1))              &
!!     &                                    :: layer_spectr_data_text
!!
!!      function picked_each_mode_data_text                             &
!!     &       (i_step, time, radius, kr, l, m, ntot_comp_rj, d_rj_out)
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time, radius
!!        integer(kind = kint), intent(in) :: kr, l, m
!!        integer(kind = kint), intent(in) :: ntot_comp_rj
!!        real(kind = kreal), intent(in) :: d_rj_out(ntot_comp_rj)
!!      character(len = 4*16+2*25+ntot_comp_rj*25+1)                    &
!!     &                          :: picked_each_mode_data_text
!!@endverbatim
!
      module sph_monitor_data_text
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      function volume_pwr_data_text(i_step, time,                       &
     &                              ntot_sph_spec, spectr_IO)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: ntot_sph_spec
      real(kind = kreal), intent(in) :: spectr_IO(ntot_sph_spec)
!
      character(len=(16 + (ntot_sph_spec+1)*25 + 1))                    &
     &                                      :: volume_pwr_data_text
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a16,i5,a16)') '(i16,1pE25.15e3,',                 &
     &                             ntot_sph_spec, '(1pE25.15e3),a1)'
      write(volume_pwr_data_text,fmt_txt) i_step, time,                 &
     &                             spectr_IO(1:ntot_sph_spec), char(10)
!
      end function volume_pwr_data_text
!
!   --------------------------------------------------------------------
!
      function layer_pwr_data_text(i_step, time, kr_sph, r_sph,         &
     &                             ntot_sph_spec, spectr_IO)
!
      integer(kind = kint), intent(in) :: i_step, kr_sph
      real(kind = kreal), intent(in) :: r_sph, time
      integer(kind = kint), intent(in) :: ntot_sph_spec
      real(kind = kreal), intent(in) :: spectr_IO(ntot_sph_spec)
!
      character(len=(2*16 + (ntot_sph_spec+2)*25 + 1))                  &
     &                                      :: layer_pwr_data_text
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a31,i5,a16)') '(i16,1pE25.15e3,i16,1pE25.15e3,',  &
     &                             ntot_sph_spec, '(1pE25.15e3),a1)'
      write(layer_pwr_data_text,fmt_txt) i_step, time,                  &
     &      kr_sph, r_sph, spectr_IO(1:ntot_sph_spec), char(10)
!
      end function layer_pwr_data_text
!
!   --------------------------------------------------------------------
!
      function layer_spectr_data_text(i_step, time, kr_sph, r_sph,      &
     &        i_mode, ntot_sph_spec, spectr_IO)
!
      integer(kind = kint), intent(in) :: i_step, kr_sph, i_mode
      real(kind = kreal), intent(in) :: r_sph, time
      integer(kind = kint), intent(in) :: ntot_sph_spec
      real(kind = kreal), intent(in) :: spectr_IO(ntot_sph_spec)
!
      character(len=(3*16 + (ntot_sph_spec+2)*25 + 1))                  &
     &                                      :: layer_spectr_data_text
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a35,i5,a16)')                                     &
     &            '(i16,1pE25.15e3,i16,1pE25.15e3,i16,',                &
     &              ntot_sph_spec, '(1pE25.15e3),a1)'
      write(layer_spectr_data_text,fmt_txt) i_step, time,               &
     &      kr_sph, r_sph, i_mode, spectr_IO(1:ntot_sph_spec), char(10)
!
      end function layer_spectr_data_text
!
!   --------------------------------------------------------------------
!
      function volume_spectr_data_text(i_step, time, i_mode,            &
     &                                 ntot_sph_spec, spectr_IO)
!
      integer(kind = kint), intent(in) :: i_step, i_mode
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: ntot_sph_spec
      real(kind = kreal), intent(in) :: spectr_IO(ntot_sph_spec)
!
      character(len=(2*16 + (ntot_sph_spec+1)*25 + 1))                  &
     &                                      :: volume_spectr_data_text
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a20,i5,a16)') '(i16,1pE25.15e3,i16,',             &
     &                             ntot_sph_spec, '(1pE25.15e3),a1)'
      write(volume_spectr_data_text,fmt_txt) i_step, time,              &
     &            i_mode, spectr_IO(1:ntot_sph_spec), char(10)
!
      end function volume_spectr_data_text
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      function picked_each_mode_data_text                               &
     &       (i_step, time, radius, kr, l, m, ntot_comp_rj, d_rj_out)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time, radius
      integer(kind = kint), intent(in) :: kr, l, m
      integer(kind = kint), intent(in) :: ntot_comp_rj
      real(kind = kreal), intent(in) :: d_rj_out(ntot_comp_rj)
!
      character(len = 4*16+2*25+ntot_comp_rj*25+1)                      &
     &                          :: picked_each_mode_data_text
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a37,i4,a17)')                                     &
     &         '(i16,1pe25.15e3, i16,1pe25.15e3,2i16,',                 &
     &           ntot_comp_rj, '(1pE25.15e3), a1)'
      write(picked_each_mode_data_text,fmt_txt) i_step, time,           &
     &          kr, radius, l, m, d_rj_out(1:ntot_comp_rj), char(10)
!
      end function  picked_each_mode_data_text
!
! ----------------------------------------------------------------------
!
      end module sph_monitor_data_text
