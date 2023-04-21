!>@file   sel_gz_read_sph_mtr_header.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief gzipped spectr monitor data reading routines
!!
!!@verbatim
!!      subroutine gz_read_sph_pwr_vol_head                             &
!!     &         (FPz_f, id_stream, flag_gzip, sph_lbl_IN, sph_IN, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine gz_read_sph_pwr_layer_head                           &
!!     &         (FPz_f, id_stream, flag_gzip, sph_lbl_IN, sph_IN, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine sel_read_sph_spectr_name                             &
!!     &         (FPz_f, id_stream, flag_gzip, nfield_sph_spec,         &
!!     &          num_labels, ncomp_sph_spec, ene_sph_spec_name, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: nfield_sph_spec, num_labels
!!        integer(kind = kint), intent(inout)                           &
!!     &                     :: ncomp_sph_spec(nfield_sph_spec)
!!        character(len = kchara), intent(inout)                        &
!!     &                     :: ene_sph_spec_name(num_labels)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
      module sel_gz_read_sph_mtr_header
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_sph_pwr_vol_head                               &
     &         (FPz_f, id_stream, flag_gzip, sph_lbl_IN, sph_IN, zbuf)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
!
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_lbl_IN%hdr_nri, sph_lbl_IN%hdr_ltr
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nri_sph, sph_IN%ltr_sph
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_lbl_IN%hdr_ICB_id,                     &
     &                      sph_lbl_IN%hdr_CMB_id
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_ICB, sph_IN%kr_CMB
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_lbl_IN%hdr_kr_in,                      &
     &                      sph_lbl_IN%hdr_r_in
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_inner, sph_IN%r_inner
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_lbl_IN%hdr_kr_out,                     &
     &                      sph_lbl_IN%hdr_r_out
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_outer, sph_IN%r_outer
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_lbl_IN%hdr_num_field,                  &
     &                      sph_lbl_IN%hdr_num_comp
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nfield_sph_spec,                    &
     &                      sph_IN%ntot_sph_spec
!
      end subroutine gz_read_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_sph_pwr_layer_head                             &
     &         (FPz_f, id_stream, flag_gzip, sph_lbl_IN, sph_IN, zbuf)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
!
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_lbl_IN%hdr_nri, sph_lbl_IN%hdr_ltr
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nri_sph, sph_IN%ltr_sph
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_lbl_IN%hdr_ICB_id,                     &
     &                      sph_lbl_IN%hdr_CMB_id
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_ICB, sph_IN%kr_CMB
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_lbl_IN%hdr_num_field,                  &
     &                      sph_lbl_IN%hdr_num_comp
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nfield_sph_spec,                    &
     &    sph_IN%ntot_sph_spec
!
      end subroutine gz_read_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      subroutine sel_read_sph_spectr_name                               &
     &         (FPz_f, id_stream, flag_gzip, nfield_sph_spec,           &
     &          num_labels, ncomp_sph_spec, ene_sph_spec_name, zbuf)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: nfield_sph_spec, num_labels
!
      integer(kind = kint), intent(inout)                               &
     &                     :: ncomp_sph_spec(nfield_sph_spec)
      character(len = kchara), intent(inout)                            &
     &                     :: ene_sph_spec_name(num_labels)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      do
        call sel_skip_comment_gz_stream                                 &
     &     (FPz_f, id_stream, flag_gzip, zbuf)
        read(zbuf%fixbuf(1),*) ncomp_sph_spec(ist+1:ist+zbuf%num_word)
        if(ist+zbuf%num_word .ge. nfield_sph_spec) exit
        ist = ist+zbuf%num_word
      end do
        call sel_skip_comment_gz_stream                                 &
     &     (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) ene_sph_spec_name(1:num_labels)

      end subroutine sel_read_sph_spectr_name
!
!   --------------------------------------------------------------------
!
      end module sel_gz_read_sph_mtr_header
