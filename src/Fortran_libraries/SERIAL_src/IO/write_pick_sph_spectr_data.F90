!>@file   write_pick_sph_spectr_data.F90
!!@brief  module write_pick_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Weite picked monitor spectrum data
!!
!!@verbatim
!!      subroutine sel_gz_write_picked_spec_data(glag_zlib, id_file,    &
!!     &          time_d, picked, inum, d_rj_out, zbuf)
!!        logical, intent(in) :: flag_zlib
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: inum
!!        type(time_data), intent(in) :: time_d
!!        type(picked_spectrum_data), intent(in) :: picked
!!
!!      subroutine dup_each_pick_sph_file_header(nlayer_ICB, nlayer_CMB,&
!!     &                                    picked, sph_OUT)
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(picked_spectrum_data), intent(in) :: picked
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!@endverbatim
!!
      module write_pick_sph_spectr_data
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
      private :: gz_write_picked_spec_data
      private :: dup_pick_sph_file_header_base
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_gz_write_picked_spec_data(flag_zlib, id_file,      &
     &          time_d, picked, inum, d_rj_out, zbuf)
!
      use sph_monitor_data_text
      use gzip_defleate
!
      logical, intent(in) :: flag_zlib
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: inum
      type(time_data), intent(in) :: time_d
      type(picked_spectrum_data), intent(in) :: picked
!
      real(kind = kreal), intent(in)                                    &
     &              :: d_rj_out(picked%ntot_comp_rj,picked%num_layer)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: knum
!
!
#ifdef ZLIB_IO
      if(flag_zlib) then
        call gz_write_picked_spec_data(id_file, time_d, picked,         &
     &                                 inum, d_rj_out, zbuf)
        return
      end if
#endif
!
      do knum = 1, picked%num_layer
        write(id_file) picked_each_mode_data_text                       &
     &             (time_d%i_time_step, time_d%time,                    &
     &              picked%radius_gl(knum,1), picked%id_radius(knum,1), &
     &              picked%idx_out(inum,1), picked%idx_out(inum,2),     &
     &              picked%ntot_comp_rj, d_rj_out(1,knum))
      end do
!
      end subroutine sel_gz_write_picked_spec_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dup_each_pick_sph_file_header(nlayer_ICB, nlayer_CMB,  &
     &                                    picked, sph_OUT)
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      sph_OUT%ltr_sph = 1
      call dup_pick_sph_file_header_base(nlayer_ICB, nlayer_CMB,        &
     &                                   picked, sph_OUT)
!
      end subroutine dup_each_pick_sph_file_header
!
! -----------------------------------------------------------------------
#ifdef ZLIB_IO
! -----------------------------------------------------------------------
!
      subroutine gz_write_picked_spec_data(id_file, time_d, picked,     &
     &                                     inum, d_rj_out, zbuf)
!
      use sph_monitor_data_text
      use gzip_defleate
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: inum
      type(time_data), intent(in) :: time_d
      type(picked_spectrum_data), intent(in) :: picked
!
      real(kind = kreal), intent(in)                                    &
     &              :: d_rj_out(picked%ntot_comp_rj,picked%num_layer)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: knum, line_len
!
!
        line_len = len(picked_each_mode_data_text                       &
     &                           (time_d%i_time_step, time_d%time,      &
     &                            picked%radius_gl(1,1),                &
     &                            picked%id_radius(1,1),                &
     &                            picked%idx_out(1,1),                  &
     &                            picked%idx_out(1,2),                  &
     &                            picked%ntot_comp_rj, d_rj_out(1,1)))
        zbuf%ilen_gz = int(dble(picked%num_layer*line_len)*1.01 + 24,   &
     &                     KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
        zbuf%ilen_gzipped = 0
        if(picked%num_layer .eq. 1) then
          knum = 1
          call gzip_defleat_char_once(line_len,                         &
     &        picked_each_mode_data_text                                &
     &             (time_d%i_time_step, time_d%time,                    &
     &              picked%radius_gl(knum,1), picked%id_radius(knum,1), &
     &              picked%idx_out(inum,1), picked%idx_out(inum,2),     &
     &              picked%ntot_comp_rj, d_rj_out(1,knum)),             &
     &        int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
        else
          knum = 1
          call gzip_defleat_char_begin(line_len,                        &
     &        picked_each_mode_data_text                                &
     &             (time_d%i_time_step, time_d%time,                    &
     &              picked%radius_gl(knum,1), picked%id_radius(knum,1), &
     &              picked%idx_out(inum,1), picked%idx_out(inum,2),     &
     &              picked%ntot_comp_rj, d_rj_out(1,knum)),             &
     &        int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
          do knum = 2, picked%num_layer - 1
            call gzip_defleat_char_cont(line_len,                       &
     &          picked_each_mode_data_text                              &
     &             (time_d%i_time_step, time_d%time,                    &
     &              picked%radius_gl(knum,1), picked%id_radius(knum,1), &
     &              picked%idx_out(inum,1), picked%idx_out(inum,2),     &
     &              picked%ntot_comp_rj, d_rj_out(1,knum)),             &
     &          zbuf)
          end do
          knum = picked%num_layer
          call gzip_defleat_char_last(line_len,                         &
     &        picked_each_mode_data_text                                &
     &                             (time_d%i_time_step, time_d%time,    &
     &                              picked%radius_gl(knum,1),           &
     &                              picked%id_radius(knum,1),           &
     &                              picked%idx_out(inum,1),             &
     &                              picked%idx_out(inum,2),             &
     &                              picked%ntot_comp_rj,                &
     &                              d_rj_out(1,knum)),                  &
     &        zbuf)
        end if
!
      write(id_file) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_picked_spec_data
!
! -----------------------------------------------------------------------
#endif
! -----------------------------------------------------------------------
!
      subroutine dup_pick_sph_file_header_base(nlayer_ICB, nlayer_CMB,  &
     &                                         picked, sph_OUT)
!
      use m_time_labels
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: i, icou
!
      sph_OUT%nri_sph = picked%num_layer
      sph_OUT%nri_dat = picked%num_layer
      sph_OUT%kr_ICB =  nlayer_ICB
      sph_OUT%kr_CMB =  nlayer_CMB
!
      sph_OUT%nfield_sph_spec = picked%num_field_rj
      sph_OUT%ntot_sph_spec =   picked%ntot_comp_rj
      sph_OUT%num_time_labels = 6
      call alloc_sph_espec_name(sph_OUT)
      call alloc_sph_spectr_data(picked%num_sph_mode, sph_OUT)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      sph_OUT%ene_sph_spec_name(3) = 'Radius_ID'
      sph_OUT%ene_sph_spec_name(4) = 'Radius'
      sph_OUT%ene_sph_spec_name(5) = 'Degree'
      sph_OUT%ene_sph_spec_name(6) = 'Order'
!
      icou = sph_OUT%num_time_labels
      do i = 1, picked%num_field_rj
        sph_OUT%ncomp_sph_spec(i) = picked%istack_comp_rj(i)            &
     &                             - picked%istack_comp_rj(i-1)
      end do
      do i = 1, picked%ntot_comp_rj
        sph_OUT%ene_sph_spec_name(icou+i) = picked%spectr_name(i)
      end do
!
      end subroutine dup_pick_sph_file_header_base
!
! -----------------------------------------------------------------------
!
      end module write_pick_sph_spectr_data
