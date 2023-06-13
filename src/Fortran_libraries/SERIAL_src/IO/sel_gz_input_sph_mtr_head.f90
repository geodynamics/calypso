!>@file   sel_gz_input_sph_mtr_head.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief gzipped spectr monitor data reading routines
!!
!!@verbatim
!!      subroutine read_sph_volume_mean_head(FPz_f, id_stream,          &
!!     &          flag_gzip, sph_lbl_IN, sph_IN, zbuf)
!!      subroutine read_sph_volume_spectr_head(FPz_f, id_stream,        &
!!     &          flag_gzip, sph_lbl_IN, sph_IN, zbuf)
!!      subroutine read_sph_layer_mean_head(FPz_f, id_stream,           &
!!     &          flag_gzip, flag_old_fmt, sph_lbl_IN, sph_IN, zbuf)
!!      subroutine read_sph_layer_spectr_head(FPz_f, id_stream,         &
!!     &          flag_gzip, flag_old_fmt, sph_lbl_IN, sph_IN, zbuf)
!!      subroutine read_picked_sph_head(FPz_f, id_stream,               &
!!     &          flag_gzip, sph_lbl_IN, sph_IN, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip, flag_old_fmt
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
      module sel_gz_input_sph_mtr_head
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
      subroutine read_sph_volume_mean_head(FPz_f, id_stream,            &
     &          flag_gzip, sph_lbl_IN, sph_IN, zbuf)
!
      use sel_gz_read_sph_mtr_header
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
      sph_IN%nri_sph =         1
      sph_IN%num_time_labels = 2
      call gz_read_sph_pwr_vol_head(FPz_f, id_stream, flag_gzip,        &
     &                              sph_lbl_IN, sph_IN, zbuf)
!
      call alloc_sph_espec_name(sph_IN)
      call sel_read_sph_spectr_name(FPz_f, id_stream, flag_gzip,        &
     &   sph_IN%nfield_sph_spec, sph_IN%num_labels,                     &
     &   sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name, zbuf)
      sph_IN%nri_dat = 1
!
      end subroutine read_sph_volume_mean_head
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_volume_spectr_head(FPz_f, id_stream,          &
     &          flag_gzip, sph_lbl_IN, sph_IN, zbuf)
!
      use sel_gz_read_sph_mtr_header
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
      sph_IN%nri_sph =         1
      sph_IN%num_time_labels = 3
      sph_IN%nri_dat = 1
      call gz_read_sph_pwr_vol_head(FPz_f, id_stream, flag_gzip,        &
     &                              sph_lbl_IN, sph_IN, zbuf)
!
      call alloc_sph_espec_name(sph_IN)
      call sel_read_sph_spectr_name(FPz_f, id_stream, flag_gzip,        &
     &   sph_IN%nfield_sph_spec, sph_IN%num_labels,                     &
     &   sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name, zbuf)
!
      end subroutine read_sph_volume_spectr_head
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_layer_mean_head(FPz_f, id_stream,             &
     &          flag_gzip, flag_old_fmt, sph_lbl_IN, sph_IN, zbuf)
!
      use sel_gz_read_sph_mtr_header
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip, flag_old_fmt
!
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      sph_IN%num_time_labels = 4
      if(flag_old_fmt) sph_IN%num_time_labels = 3
      call gz_read_sph_pwr_layer_head(FPz_f, id_stream, flag_gzip,      &
     &                                sph_lbl_IN, sph_IN, zbuf)
      sph_IN%nri_dat = sph_IN%nri_sph
!
      call alloc_sph_espec_name(sph_IN)
      call sel_read_sph_spectr_name(FPz_f, id_stream, flag_gzip,        &
     &   sph_IN%nfield_sph_spec, sph_IN%num_labels,                     &
     &   sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name, zbuf)
!
      end subroutine read_sph_layer_mean_head
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_layer_spectr_head(FPz_f, id_stream,           &
     &          flag_gzip, flag_old_fmt, sph_lbl_IN, sph_IN, zbuf)
!
      use sel_gz_read_sph_mtr_header
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip, flag_old_fmt
!
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      sph_IN%num_time_labels = 5
      if(flag_old_fmt) sph_IN%num_time_labels = 4
      call gz_read_sph_pwr_layer_head(FPz_f, id_stream, flag_gzip,      &
     &                                sph_lbl_IN, sph_IN, zbuf)
      sph_IN%nri_dat = sph_IN%nri_sph
!
      call alloc_sph_espec_name(sph_IN)
      call sel_read_sph_spectr_name(FPz_f, id_stream, flag_gzip,        &
     &   sph_IN%nfield_sph_spec, sph_IN%num_labels,                     &
     &   sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name, zbuf)
!
      end subroutine read_sph_layer_spectr_head
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_picked_sph_head(FPz_f, id_stream,                 &
     &          flag_gzip, sph_lbl_IN, sph_IN, zbuf)
!
      use sel_gz_read_sph_mtr_header
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
      sph_IN%num_time_labels = 6
      call gz_read_sph_pwr_layer_head(FPz_f, id_stream, flag_gzip,      &
     &                                sph_lbl_IN, sph_IN, zbuf)
!
      call alloc_sph_espec_name(sph_IN)
      call sel_read_sph_spectr_name(FPz_f, id_stream, flag_gzip,        &
     &   sph_IN%nfield_sph_spec, sph_IN%num_labels,                     &
     &   sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name, zbuf)
!
      sph_IN%nri_dat = sph_IN%nri_sph
!
      end subroutine read_picked_sph_head
!
!   --------------------------------------------------------------------
!
      end module sel_gz_input_sph_mtr_head
