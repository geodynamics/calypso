!>@file   sph_power_spectr_data_text.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!> @brief Time spectrum data output routines for utilities
!!
!!@verbatim
!!      subroutine len_sph_vol_spectr_header(lbl_OUT, sph_OUT,          &
!!     &                                     len_each, len_tot)
!!        type(sph_spectr_head_labels), intent(in) :: lbl_OUT
!!        type(read_sph_spectr_params), intent(in) :: sph_OUT
!!        integer(kind = kint), intent(inout)  :: len_each(6)
!!        integer(kind = kint), intent(inout)  :: len_tot
!!      function sph_vol_spectr_header_text(len_header, len_each,       &
!!     &                                    lbl_OUT, sph_OUT)
!!        integer(kind = kint), intent(in) :: len_header
!!        integer(kind = kint), intent(in) :: len_each(6)
!!        type(sph_spectr_head_labels), intent(in) :: lbl_OUT
!!        type(read_sph_spectr_params), intent(in) :: sph_OUT
!!        character(len = len_header) :: sph_vol_spectr_header_text
!!
!!      subroutine len_sph_layer_spectr_header(lbl_OUT, sph_OUT,        &
!!     &                                       len_each, len_tot)
!!        type(sph_spectr_head_labels), intent(in) :: lbl_OUT
!!        type(read_sph_spectr_params), intent(in) :: sph_OUT
!!        integer(kind = kint), intent(inout)  :: len_each(6)
!!        integer(kind = kint), intent(inout)  :: len_tot
!!      function sph_layer_spectr_header_text(len_header, len_each,     &
!!     &                                      lbl_OUT, sph_OUT)
!!        integer(kind = kint), intent(in) :: len_header
!!        integer(kind = kint), intent(in) :: len_each(6)
!!        type(sph_spectr_head_labels), intent(in) :: lbl_OUT
!!        type(read_sph_spectr_params), intent(in) :: sph_OUT
!!        character(len = len_header) :: sph_layer_spectr_header_text
!!@endverbatim
!
      module sph_power_spectr_data_text
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine len_sph_vol_spectr_header(lbl_OUT, sph_OUT,            &
     &                                     len_each, len_tot)
!
      use sph_monitor_header_text
!
      type(sph_spectr_head_labels), intent(in) :: lbl_OUT
      type(read_sph_spectr_params), intent(in) :: sph_OUT
      integer(kind = kint), intent(inout)  :: len_each(6)
      integer(kind = kint), intent(inout)  :: len_tot
!
!
      len_each(1) = len_moniter_i2_head_text(lbl_OUT%hdr_nri,           &
     &                                       lbl_OUT%hdr_ltr)
      len_each(2) = len_moniter_i2_head_text(lbl_OUT%hdr_ICB_id,        &
     &                                       lbl_OUT%hdr_CMB_id)
      len_each(3) = len_moniter_ir_head_text(lbl_OUT%hdr_kr_in,         &
     &                                       lbl_OUT%hdr_r_in)
      len_each(4) = len_moniter_ir_head_text(lbl_OUT%hdr_kr_out,        &
     &                                       lbl_OUT%hdr_r_out)
!
      len_each(5) = len_monitor_data_ncomps_text(lbl_OUT%hdr_num_field, &
     &                                       lbl_OUT%hdr_num_comp,      &
     &                                       sph_OUT%nfield_sph_spec)
      len_each(6) = len_data_names_text(sph_OUT%num_labels,             &
     &                                  sph_OUT%ene_sph_spec_name)
      len_tot = sum(len_each(1:6))
!
      end subroutine len_sph_vol_spectr_header
!
!   --------------------------------------------------------------------
!
      function sph_vol_spectr_header_text(len_header, len_each,         &
     &                                    lbl_OUT, sph_OUT)
!
      use sph_monitor_header_text
!
      integer(kind = kint), intent(in) :: len_header
      integer(kind = kint), intent(in) :: len_each(6)
      type(sph_spectr_head_labels), intent(in) :: lbl_OUT
      type(read_sph_spectr_params), intent(in) :: sph_OUT
!
      character(len = len_header) :: sph_vol_spectr_header_text
!
      integer(kind = kint) :: ist
      character(len = len_header) :: textbuf
!
!
      ist = 0
      textbuf(ist+1:ist+len_each(1))                                    &
     &     = monitor_int2_head_text(len_each(1),                        &
     &                              lbl_OUT%hdr_nri, lbl_OUT%hdr_ltr,   &
     &                              sph_OUT%nri_sph, sph_OUT%ltr_sph)
      ist = ist + len_each(1)
!
      textbuf(ist+1:ist+len_each(2))                                    &
     &     = monitor_int2_head_text(len_each(2),                        &
     &                          lbl_OUT%hdr_ICB_id, lbl_OUT%hdr_CMB_id, &
     &                          sph_OUT%kr_ICB, sph_OUT%kr_CMB)
      ist = ist + len_each(2)
!
      textbuf(ist+1:ist+len_each(3))                                    &
     &     = monitor_ir_head_text(len_each(3),                          &
     &                          lbl_OUT%hdr_kr_in, lbl_OUT%hdr_r_in,    &
     &                          sph_OUT%kr_inner, sph_OUT%r_inner)
      ist = ist + len_each(3)
!
      textbuf(ist+1:ist+len_each(4))                                    &
     &     = monitor_ir_head_text(len_each(4),                          &
     &                          lbl_OUT%hdr_kr_out, lbl_OUT%hdr_r_out,  &
     &                          sph_OUT%kr_outer, sph_OUT%r_outer)
      ist = ist + len_each(4)
!
      textbuf(ist+1:ist+len_each(5))                                    &
     &     = monitor_data_ncomps_text(len_each(5),                      &
     &                  lbl_OUT%hdr_num_field, lbl_OUT%hdr_num_comp,    &
     &                  sph_OUT%nfield_sph_spec, sph_OUT%ntot_sph_spec, &
     &                  sph_OUT%ncomp_sph_spec)
      ist = ist + len_each(5)
!
      textbuf(ist+1:ist+len_each(6))                                    &
     &     = monitor_data_names_text(len_each(6), sph_OUT%num_labels,   &
     &                               sph_OUT%ene_sph_spec_name)
      sph_vol_spectr_header_text = textbuf
!
      end function sph_vol_spectr_header_text
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine len_sph_layer_spectr_header(lbl_OUT, sph_OUT,          &
     &                                       len_each, len_tot)
!
      use sph_monitor_header_text
!
      type(sph_spectr_head_labels), intent(in) :: lbl_OUT
      type(read_sph_spectr_params), intent(in) :: sph_OUT
      integer(kind = kint), intent(inout)  :: len_each(6)
      integer(kind = kint), intent(inout)  :: len_tot
!
!
      len_each(1) = len_moniter_i2_head_text(lbl_OUT%hdr_nri,           &
     &                                       lbl_OUT%hdr_ltr)
      len_each(2) = len_moniter_i2_head_text(lbl_OUT%hdr_ICB_id,        &
     &                                       lbl_OUT%hdr_CMB_id)
      len_each(3) = 0
      len_each(4) = 0
!
      len_each(5) = len_monitor_data_ncomps_text(lbl_OUT%hdr_num_field, &
     &                                         lbl_OUT%hdr_num_comp,    &
     &                                         sph_OUT%nfield_sph_spec)
      len_each(6) = len_data_names_text(sph_OUT%num_labels,             &
     &                                  sph_OUT%ene_sph_spec_name)
      len_tot = sum(len_each(1:6))
!
      end subroutine len_sph_layer_spectr_header
!
!   --------------------------------------------------------------------
!
      function sph_layer_spectr_header_text(len_header, len_each,       &
     &                                      lbl_OUT, sph_OUT)
!
      use sph_monitor_header_text
!
      integer(kind = kint), intent(in) :: len_header
      integer(kind = kint), intent(in) :: len_each(6)
      type(sph_spectr_head_labels), intent(in) :: lbl_OUT
      type(read_sph_spectr_params), intent(in) :: sph_OUT
!
      character(len = len_header) :: sph_layer_spectr_header_text
!
      integer(kind = kint) :: ist
      character(len = len_header) :: textbuf
!
!
      ist = 0
      textbuf(ist+1:ist+len_each(1))                                    &
     &     = monitor_int2_head_text(len_each(1),                        &
     &                              lbl_OUT%hdr_nri, lbl_OUT%hdr_ltr,   &
     &                              sph_OUT%nri_sph, sph_OUT%ltr_sph)
      ist = ist + len_each(1)
!
      textbuf(ist+1:ist+len_each(2))                                    &
     &     = monitor_int2_head_text(len_each(2),                        &
     &                          lbl_OUT%hdr_ICB_id, lbl_OUT%hdr_CMB_id, &
     &                          sph_OUT%kr_ICB, sph_OUT%kr_CMB)
      ist = ist + len_each(2)
!
      textbuf(ist+1:ist+len_each(5))                                    &
     &     = monitor_data_ncomps_text(len_each(5),                      &
     &                  lbl_OUT%hdr_num_field, lbl_OUT%hdr_num_comp,    &
     &                  sph_OUT%nfield_sph_spec, sph_OUT%ntot_sph_spec, &
     &                  sph_OUT%ncomp_sph_spec)
      ist = ist + len_each(5)
!
      textbuf(ist+1:ist+len_each(6))                                    &
     &     = monitor_data_names_text(len_each(6), sph_OUT%num_labels,   &
     &                               sph_OUT%ene_sph_spec_name)
      sph_layer_spectr_header_text = textbuf
!
      end function sph_layer_spectr_header_text
!
!   --------------------------------------------------------------------
!
      end module sph_power_spectr_data_text
