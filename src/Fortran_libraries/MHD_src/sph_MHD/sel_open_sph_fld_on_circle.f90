!>@file   sel_open_sph_fld_on_circle.f90
!!@brief  module sel_open_sph_fld_on_circle
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine sel_open_sph_fld_on_circle_file(id_file, base_name,  &
!!     &          monitor_labels, circle, sph_OUT, zbuf, flag_gzip_lc)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len = kchara), intent(in) :: base_name
!!        type(sph_spectr_head_labels), intent(in) :: monitor_labels
!!        type(circle_parameters), intent(in) :: circle
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        logical, intent(inout) :: flag_gzip_lc
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module sel_open_sph_fld_on_circle
!
      use m_precision
      use m_constants
!
      use t_sph_circle_parameters
      use t_sph_spectr_head_labels
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
      private :: write_sph_field_on_circle_head
      private :: len_sph_field_on_circle_header
      private :: sph_field_on_circle_text
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_open_sph_fld_on_circle_file(id_file, base_name,    &
     &          monitor_labels, circle, sph_OUT, zbuf, flag_gzip_lc)
!
      use select_gz_stream_file_IO
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: base_name
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
      type(circle_parameters), intent(in) :: circle
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc
!
      character(len = kchara) :: fname
      logical :: flag_miss
!
!
      fname = base_name
      call check_gzip_or_ascii_file(base_name, fname,                   &
     &                              flag_gzip_lc, flag_miss)
!
      if(flag_miss) then
        write(*,*) 'Make file to write: ', trim(fname)
        open(id_file, file=fname, status='replace',                     &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
        call write_sph_field_on_circle_head(flag_gzip_lc, id_file,      &
     &      monitor_labels, sph_OUT, zbuf, circle)
      else
        open(id_file, file=fname, status='old', position='append',      &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      end if
!
      end subroutine sel_open_sph_fld_on_circle_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_field_on_circle_head(flag_gzip, id_file,     &
     &          monitor_labels, sph_OUT, zbuf, circle)
!
      use select_gz_stream_file_IO
      use sph_power_spectr_data_text
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(circle_parameters), intent(in) :: circle
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot, len_line
!
!
      call len_sph_field_on_circle_header                               &
     &   (monitor_labels, sph_OUT, len_line, len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip, id_file,                 &
     &    sph_field_on_circle_text(len_tot, len_each, len_line,         &
     &                             monitor_labels, sph_OUT, circle),    &
     &    zbuf)
!
      end subroutine write_sph_field_on_circle_head
!
!   --------------------------------------------------------------------
!
      subroutine len_sph_field_on_circle_header                         &
     &         (lbl_OUT, sph_OUT, len_line, len_each, len_tot)
!
      use sph_monitor_header_text
!
      type(sph_spectr_head_labels), intent(in) :: lbl_OUT
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      integer(kind = kint), intent(inout)  :: len_each(6)
      integer(kind = kint), intent(inout)  :: len_line, len_tot
!
!
      len_line = 27 + 16 + 1
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
      len_tot = 4 + 4*len_line + sum(len_each(1:6))
!
      end subroutine len_sph_field_on_circle_header
!
!   --------------------------------------------------------------------
!
      function sph_field_on_circle_text(len_header, len_each, len_line, &
     &                                  lbl_OUT, sph_OUT, circle)
!
      use sph_monitor_header_text
!
      integer(kind = kint), intent(in) :: len_header, len_line
      integer(kind = kint), intent(in) :: len_each(6)
      type(sph_spectr_head_labels), intent(in) :: lbl_OUT
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(circle_parameters), intent(in) :: circle
!
      character(len = len_header) :: sph_field_on_circle_text
!
      integer(kind = kint) :: ist
      character(len = len_header) :: textbuf
!
!
      write(textbuf(1:2),'(2a1)') "#", char(10)
      ist = 2
      write(textbuf(ist+1:ist+len_line),'(a27,1pE16.6e3,a1)')           &
     &     "#  Radius:             r = ", circle%r_circle, char(10)
      ist = ist + len_line
      write(textbuf(ist+1:ist+len_line),'(a27,1pE16.6e3,a1)')           &
     &     "#  Cylindrical Radius: s = ", circle%s_circle, char(10)
      ist = ist + len_line
      write(textbuf(ist+1:ist+len_line),'(a27,1pE16.6e3,a1)')           &
     &     "#  Vertical position:  z = ", circle%z_circle, char(10)
      ist = ist + len_line
      write(textbuf(ist+1:ist+len_line),'(a27,1pE16.6e3,a1)')           &
     &     "#  Colatitude:         r = ", circle%colat_circle, char(10)
      ist = ist + len_line
      write(textbuf(ist+1:ist+len_line),'(2a1)') "#", char(10)
      ist = ist + 2
!
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
      sph_field_on_circle_text = textbuf
!
      end function sph_field_on_circle_text
!
!   --------------------------------------------------------------------
!
      end module sel_open_sph_fld_on_circle
