!>@file   sph_volume_monitor_snap_IO.f90
!!        module sph_volume_monitor_snap_IO
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine write_sph_vol_spectr_snapshot(flag_gzip, file_name,  &
!!     &          comment, sph_IN, spectr_IO)
!!        logical, intent(in) :: flag_gzip
!!        character(len = kchara), intent(in) :: file_name
!!        character(len = *), intent(in) :: comment
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!        real(kind = kreal), intent(in)                                &
!!     &            :: spectr_IO(sph_IN%ntot_sph_spec,0:sph_IN%ltr_sph)
!!      subroutine read_alloc_sph_vol_spec_head(fname_org,              &
!!     &                                        sph_lbl_IN, sph_IN)
!!      subroutine read_sph_vol_spec_snapshot(fname_org, sph_lbl_IN,    &
!!     &                                      sph_IN, spectr_IO)
!!        character(len = kchara), intent(in) :: fname_org
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!      real(kind = kreal), intent(inout)                               &
!!     &             :: spectr_IO(sph_IN%ntot_sph_spec, 0:sph_IN%ltr_sph)
!!
!!      subroutine write_sph_vol_mean_tave_sdev                         &
!!     &         (flag_gzip, ave_fname, comment, sph_lbl_IN, sph_IN,    &
!!     &          ntot_comp, ave_mean, rms_mean, sdev_mean)
!!        logical, intent(in) :: flag_gzip
!!        character(len = kchara), intent(in) :: ave_fname
!!        character(len = *), intent(in) :: comment
!!        type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(in) :: ave_mean(ntot_comp)
!!        real(kind = kreal), intent(in) :: rms_mean(ntot_comp)
!!        real(kind = kreal), intent(in) :: sdev_mean(ntot_comp)
!!@endverbatim
!
      module sph_volume_monitor_snap_IO
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_stream = 46
      integer(kind = kint), parameter, private :: id_read_rms = 45
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_vol_spectr_snapshot(flag_gzip, file_name,    &
     &          comment, sph_IN, spectr_IO)
!
      use set_parallel_file_name
      use select_gz_stream_file_IO
      use gz_open_sph_vol_mntr_file
      use gz_volume_spectr_monitor_IO
!
      logical, intent(in) :: flag_gzip
      character(len = kchara), intent(in) :: file_name
      character(len = *), intent(in) :: comment
      type(read_sph_spectr_data), intent(in) :: sph_IN
      real(kind = kreal), intent(in)                                    &
     &            :: spectr_IO(sph_IN%ntot_sph_spec,0:sph_IN%ltr_sph)
!
      type(buffer_4_gzip) :: zbuf_wt
!
!
      write(*,*) 'Write file: ', trim(file_name)
      open(id_stream, file=file_name, status='replace',                 &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      call sel_gz_write_text_stream(flag_gzip, id_stream,               &
     &                              comment, zbuf_wt)
      call write_sph_pwr_vol_head(flag_gzip, id_stream,                 &
     &                            sph_pwr_labels, sph_IN, zbuf_wt)
      call sel_gz_write_volume_spectr_mtr(flag_gzip, id_stream,         &
     &    sph_IN%i_step, sph_IN%time, sph_IN%ltr_sph,                   &
     &    sph_IN%ntot_sph_spec, spectr_IO(1,0), zbuf_wt)
      close(id_stream)
!
      end subroutine write_sph_vol_spectr_snapshot
!
!   --------------------------------------------------------------------
!
      subroutine read_alloc_sph_vol_spec_head(fname_org,                &
     &                                        sph_lbl_IN, sph_IN)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use gz_volume_spectr_monitor_IO
!
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      logical :: flag_gzip1
      character, pointer :: FPz_f1
      type(buffer_4_gzip) :: zbuf_rd
!
!  Read spectr data file
      write(*,*) 'Open file ', trim(fname_org)
      call sel_open_read_gz_stream_file(FPz_f1, id_read_rms,            &
     &                                  fname_org, flag_gzip1, zbuf_rd)
      call read_sph_volume_spectr_head(FPz_f1, id_read_rms,             &
     &    flag_gzip1, sph_lbl_IN, sph_IN, zbuf_rd)
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_rms, flag_gzip1, zbuf_rd)
!
      end subroutine read_alloc_sph_vol_spec_head
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_vol_spec_snapshot(fname_org, sph_lbl_IN,      &
     &                                      sph_IN, spectr_IO)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use gz_volume_spectr_monitor_IO
!
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      real(kind = kreal), intent(inout)                                 &
     &             :: spectr_IO(sph_IN%ntot_sph_spec, 0:sph_IN%ltr_sph)
!
      integer(kind = kint) :: ierr
      logical :: flag_gzip1
      character, pointer :: FPz_f1
      type(buffer_4_gzip) :: zbuf_rd
!
!  Read spectr data file
      write(*,*) 'Open file ', trim(fname_org)
      call sel_open_read_gz_stream_file(FPz_f1, id_read_rms,            &
     &                                  fname_org, flag_gzip1, zbuf_rd)
      call read_sph_volume_spectr_head(FPz_f1, id_read_rms,             &
     &    flag_gzip1, sph_lbl_IN, sph_IN, zbuf_rd)
!
      call sel_gz_read_volume_spectr_mtr(FPz_f1, id_read_rms,           &
     &    flag_gzip1, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,             &
     &    sph_IN%i_step, sph_IN%time, sph_IN%i_mode,                    &
     &    spectr_IO(1,0), zbuf_rd, ierr)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_rms, flag_gzip1, zbuf_rd)
!
      end subroutine read_sph_vol_spec_snapshot
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_sph_vol_mean_tave_sdev                           &
     &         (flag_gzip, ave_fname, comment, sph_lbl_IN, sph_IN,      &
     &          ntot_comp, ave_mean, rms_mean, sdev_mean)
!
      use select_gz_stream_file_IO
      use gz_open_sph_vol_mntr_file
      use sph_monitor_data_text
!
      logical, intent(in) :: flag_gzip
      character(len = kchara), intent(in) :: ave_fname
      character(len = *), intent(in) :: comment
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(in) :: sph_IN
      integer(kind = kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: ave_mean(ntot_comp)
      real(kind = kreal), intent(in) :: rms_mean(ntot_comp)
      real(kind = kreal), intent(in) :: sdev_mean(ntot_comp)
!
      type(buffer_4_gzip) :: zbuf_wt
!
      character(len=24+28+30+1+4), parameter :: comment_2               &
     &          =  '# 1st data: Time average' // char(10)               &
     &          // '# 2nd data: R.M.S. over time' // char(10)           &
     &          // '# 3rd data: Standard Deviation' // char(10)         &
     &          // '#' // char(10)
!
!
      write(*,*) 'Write file: ', trim(ave_fname)
      open(id_stream, file=ave_fname, status='replace',                 &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      call sel_gz_write_text_stream(flag_gzip, id_stream,               &
     &                              comment, zbuf_wt)
      call sel_gz_write_text_stream(flag_gzip, id_stream,               &
     &                              comment_2, zbuf_wt)
      call write_sph_pwr_vol_head(flag_gzip, id_stream,                 &
     &                            sph_lbl_IN, sph_IN, zbuf_wt)
      call sel_gz_write_text_stream(flag_gzip, id_stream,               &
     &    volume_pwr_data_text(sph_IN%i_step, sph_IN%time,              &
     &    ntot_comp, ave_mean), zbuf_wt)
      call sel_gz_write_text_stream(flag_gzip, id_stream,               &
     &    volume_pwr_data_text(sph_IN%i_step, sph_IN%time,              &
     &    ntot_comp, rms_mean), zbuf_wt)
      call sel_gz_write_text_stream(flag_gzip, id_stream,               &
     &    volume_pwr_data_text(sph_IN%i_step, sph_IN%time,              &
     &    ntot_comp, sdev_mean), zbuf_wt)
      close(id_stream)
!
      end subroutine write_sph_vol_mean_tave_sdev
!
!   --------------------------------------------------------------------
!
      end module sph_volume_monitor_snap_IO
