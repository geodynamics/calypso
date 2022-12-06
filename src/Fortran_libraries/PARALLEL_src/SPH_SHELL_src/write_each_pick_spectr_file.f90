!>@file   write_each_pick_spectr_file.f90
!!@brief  module write_each_pick_spectr_file
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine open_write_each_picked_spectr(inum, id_file,         &
!!     &          nlayer_ICB, nlayer_CMB, picked, flag_gzip_lc, zbuf)
!!      logical function error_each_picked_spectr(inum, id_file,        &
!!     &                nlayer_ICB, nlayer_CMB, picked, zbuf)
!!        integer(kind = kint), intent(in) :: inum
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(picked_spectrum_data), intent(in) :: picked
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        logical, intent(inout) :: flag_gzip_lc
!!@endverbatim
!!
      module write_each_pick_spectr_file
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_data
      use t_time_data
      use t_buffer_4_gzip
!
      implicit  none
!
      private :: each_picked_mode_file_name
      private :: write_each_pick_sph_file_head
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine open_write_each_picked_spectr(inum, id_file,           &
     &          nlayer_ICB, nlayer_CMB, picked, flag_gzip_lc, zbuf)
!
      use write_field_labels
      use select_gz_stream_file_IO
      use gz_open_sph_monitor_file
!
      integer(kind = kint), intent(in) :: inum
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc
!
      logical :: flag_miss
      character(len = kchara) :: file_name, base_name
!
!
      flag_gzip_lc = picked%flag_gzip
      base_name = each_picked_mode_file_name(picked%file_prefix,        &
     &                                       picked%idx_out(inum,1),    &
     &                                       picked%idx_out(inum,2))
      call check_gzip_or_ascii_file(base_name, file_name,               &
     &                              flag_gzip_lc, flag_miss)
!
      if(flag_miss) then
        open(id_file, file=file_name, status='replace',                 &
     &       form='unformatted', ACCESS='stream')
        call write_each_pick_sph_file_head                              &
     &      (id_file, nlayer_ICB, nlayer_CMB, picked, zbuf)
      else
        open(id_file, file=file_name, status='old', position='append',  &
     &       form='unformatted', ACCESS='stream')
      end if
!
      end subroutine open_write_each_picked_spectr
!
! -----------------------------------------------------------------------
!
      logical function error_each_picked_spectr(inum, id_file,          &
     &                nlayer_ICB, nlayer_CMB, picked, zbuf)
!
      use set_parallel_file_name
      use write_pick_sph_spectr_data
      use write_field_labels
      use delete_data_files
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use compare_sph_monitor_header
      use gz_open_sph_monitor_file
!
      integer(kind = kint), intent(in) :: inum
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      logical :: flag_gzip_lc, flag_miss
      character, pointer :: FPz_fp
      character(len = kchara) :: file_name, base_name
      type(read_sph_spectr_data) :: sph_IN_p, sph_OUT_p
      type(sph_spectr_head_labels) :: sph_lbl_IN_p
!
!
      error_each_picked_spectr = .FALSE.
      flag_gzip_lc = picked%flag_gzip
      base_name = each_picked_mode_file_name(picked%file_prefix,        &
     &                                       picked%idx_out(inum,1),    &
     &                                       picked%idx_out(inum,2))
      call sel_open_check_gz_stream_file(FPz_fp, id_file, base_name,    &
     &                                   flag_gzip_lc, flag_miss, zbuf)
      if(flag_miss) go to 99

      call s_select_input_picked_sph_head(FPz_fp, id_file,              &
     &    flag_gzip_lc, sph_lbl_IN_p, sph_IN_p, zbuf)
      call sel_close_read_gz_stream_file(FPz_fp, id_file,               &
     &                                   flag_gzip_lc, zbuf)
!
      call dup_each_pick_sph_file_header(nlayer_ICB, nlayer_CMB,        &
     &                                   picked, sph_OUT_p)
!
      error_each_picked_spectr                                          &
     &    = .not. cmp_sph_layer_monitor_heads(sph_lbl_IN_p, sph_IN_p,   &
     &                                        pick_spectr_labels,       &
     &                                        sph_OUT_p)
      if(error_each_picked_spectr) go to 98
      error_each_picked_spectr                                          &
     &    = .not. cmp_sph_monitor_field_labels(pick_spectr_labels,      &
     &                                        sph_IN_p, sph_OUT_p)
!
   98 continue
      call dealloc_sph_espec_name(sph_IN_p)
      call dealloc_sph_espec_name(sph_OUT_p)
!
      return
!
!       Make new file with header
   99 continue
      open(id_file, file=file_name, status='replace',                   &
     &     form='unformatted', ACCESS='stream')
!
      call write_each_pick_sph_file_head                                &
     &   (id_file, nlayer_ICB, nlayer_CMB, picked, zbuf)
      close(id_file)
      error_each_picked_spectr = .FALSE.
!
      end function error_each_picked_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_each_pick_sph_file_head                          &
     &         (id_file, nlayer_ICB, nlayer_CMB, picked, zbuf)
!
      use sph_power_spectr_data_text
      use write_field_labels
      use write_pick_sph_spectr_data
      use t_buffer_4_gzip
      use data_convert_by_zlib
      use select_gz_stream_file_IO
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      type(read_sph_spectr_data) :: sph_OUT
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      call dup_each_pick_sph_file_header(nlayer_ICB, nlayer_CMB,        &
     &                                   picked, sph_OUT)
      call len_sph_layer_spectr_header(pick_spectr_labels, sph_OUT,     &
     &                                 len_each, len_tot)
!
      call sel_gz_write_text_stream(picked%flag_gzip, id_file,          &
     &    sph_layer_spectr_header_text(len_tot, len_each,               &
     &                                 pick_spectr_labels, sph_OUT),    &
     &    zbuf)
      call dealloc_sph_espec_name(sph_OUT)
!
      end subroutine write_each_pick_sph_file_head
!
! -----------------------------------------------------------------------
!
      character(len=kchara) function each_picked_mode_file_name         &
     &                             (file_prefix, l, m)
!
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: l, m
!
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: mm
!
      mm = abs(m)
      write(fname_tmp,'(a,a2)') trim(file_prefix), '_l'
      call add_index_after_name(l, fname_tmp, file_name)
      write(fname_tmp,'(a,a2)') trim(file_name), '_m'
      call add_index_after_name(mm, fname_tmp, file_name)
      if(m .lt. 0) then
        write(fname_tmp,'(a,a1)') trim(file_name), 's'
      else
        write(fname_tmp,'(a,a1)') trim(file_name), 'c'
      end if
!
      each_picked_mode_file_name = add_dat_extension(fname_tmp)
!
      end function each_picked_mode_file_name
!
! -----------------------------------------------------------------------
!
      end module write_each_pick_spectr_file
