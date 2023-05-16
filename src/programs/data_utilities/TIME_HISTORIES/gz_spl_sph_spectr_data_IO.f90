!>@file   gz_spl_sph_spectr_data_IO.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief gzipped spectr monitor data reading routines
!!
!!@verbatim
!!      subroutine sel_gz_input_sph_layer_mean                          &
!!     &         (FPz_f, id_stream, flag_gzip, flag_old_fmt,            &
!!     &          nri_sph, ntot_comp, i_step, time, kr_sph, r_sph,      &
!!     &          spectr_IO, zbuf, ierr)
!!      subroutine sel_gz_read_layer_mean_mtr                           &
!!     &         (FPz_f, id_stream, flag_gzip, nri_sph, ntot_comp,      &
!!     &          i_step, time, kr_sph, r_sph, spectr_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: nri_sph, ntot_comp
!!        integer(kind = kint), intent(inout) :: i_step
!!        real(kind = kreal), intent(inout) :: time
!!        integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
!!        real(kind = kreal), intent(inout) :: r_sph(nri_sph)
!!        real(kind = kreal), intent(inout)                             &
!!     &                     :: spectr_IO(ntot_comp,nri_sph)
!!        integer(kind = kint), intent(inout) :: ierr
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine sel_gz_input_sph_layer_spec                          &
!!     &         (FPz_f, id_stream, flag_gzip, flag_old_fmt,            &
!!     &          nri_sph, ltr, ntot_comp, i_step, time, kr_sph, r_sph, &
!!     &          i_mode, spectr_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip, flag_old_fmt
!!        integer(kind = kint), intent(in) :: nri_sph, ltr
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        integer(kind = kint), intent(inout) :: i_step
!!        real(kind = kreal), intent(inout) :: time
!!        integer(kind = kint), intent(inout) :: i_mode(0:ltr)
!!        integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
!!        real(kind = kreal), intent(inout) :: r_sph(nri_sph)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: spectr_IO(ntot_comp,0:ltr,nri_sph)
!!        integer(kind = kint), intent(inout) :: ierr
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine copy_sph_monitor_line_to_text                        &
!!     &         (FPz_f, id_read, id_write, flag_gzip, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_read, id_write
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!
!!      subroutine gz_read_volume_pwr_sph(FPz_f, id_stream, flag_gzip,  &
!!     &          ntot_comp, i_step, time, spectr_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        integer(kind = kint), intent(inout) :: i_step
!!        real(kind = kreal), intent(inout) :: time
!!        real(kind = kreal), intent(inout) :: spectr_IO(ntot_comp)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
      module gz_spl_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
      use t_buffer_4_gzip
      use gzip_file_access
      use skip_gz_comment
!
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sel_gz_input_sph_layer_mean                            &
     &         (FPz_f, id_stream, flag_gzip, flag_old_fmt,              &
     &          nri_sph, ntot_comp, i_step, time, kr_sph, r_sph,        &
     &          spectr_IO, zbuf, ierr)
!
      use old_sph_spectr_data_IO
      use gz_layer_mean_monitor_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip, flag_old_fmt
      integer(kind = kint), intent(in) :: nri_sph, ntot_comp
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) :: time
      integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
      real(kind = kreal), intent(inout) :: r_sph(nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ntot_comp,nri_sph)
      integer(kind = kint), intent(inout) :: ierr
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if(flag_old_fmt) then
        call gz_read_layer_mean_sph_old(FPz_f, id_stream, flag_gzip,    &
     &      nri_sph, ntot_comp, i_step, time, kr_sph,                   &
     &      spectr_IO, zbuf, ierr)
      else
        call sel_gz_read_layer_mean_mtr(FPz_f, id_stream, flag_gzip,    &
     &      nri_sph, ntot_comp, i_step, time, kr_sph, r_sph,            &
     &      spectr_IO, zbuf, ierr)
      end if
!
      end subroutine sel_gz_input_sph_layer_mean
!
!   --------------------------------------------------------------------
!
      subroutine sel_gz_input_sph_layer_spec                            &
     &         (FPz_f, id_stream, flag_gzip, flag_old_fmt,              &
     &          nri_sph, ltr, ntot_comp, i_step, time, kr_sph, r_sph,   &
     &          i_mode, spectr_IO, zbuf, ierr)
!
      use old_sph_spectr_data_IO
      use gz_layer_spectr_monitor_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip, flag_old_fmt
      integer(kind = kint), intent(in) :: nri_sph, ltr
      integer(kind = kint), intent(in) :: ntot_comp
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) :: time
      integer(kind = kint), intent(inout) :: i_mode(0:ltr)
      integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
      real(kind = kreal), intent(inout) :: r_sph(nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ntot_comp,0:ltr,nri_sph)
      integer(kind = kint), intent(inout) :: ierr
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if(flag_old_fmt) then
        call gz_read_layer_spectr_sph_old                               &
     &         (FPz_f, id_stream, flag_gzip,  nri_sph, ltr, ntot_comp,  &
     &          i_step, time, kr_sph, i_mode, spectr_IO, zbuf, ierr)
      else
        call sel_gz_read_layer_spectr_mtr                               &
     &         (FPz_f, id_stream, flag_gzip, nri_sph, ltr, ntot_comp,   &
     &          i_step, time, kr_sph, r_sph, i_mode,                    &
     &          spectr_IO, zbuf, ierr)
      end if
!
      end subroutine sel_gz_input_sph_layer_spec
!
!   --------------------------------------------------------------------
!
      subroutine copy_sph_monitor_line_to_text                          &
     &         (FPz_f, id_read, id_write, flag_gzip, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_read, id_write
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 1
      call sel_read_line_gz_stream(FPz_f, id_read, flag_gzip, zbuf)
      if(zbuf%len_used .lt. 0) return
!
      write(id_write,'(a)') zbuf%fixbuf(1)(1:zbuf%len_used-1)
      ierr = 0
!
      end subroutine copy_sph_monitor_line_to_text
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine gz_read_volume_pwr_sph(FPz_f, id_stream, flag_gzip,    &
     &          ntot_comp, i_step, time, spectr_IO, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: ntot_comp
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) :: time
      real(kind = kreal), intent(inout) :: spectr_IO(ntot_comp)
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 1
      call sel_read_line_gz_stream(FPz_f, id_stream, flag_gzip, zbuf)
      if(zbuf%len_used .lt. 0) return
!
      read(zbuf%fixbuf(1),*,err=99) i_step, time,                       &
     &                             spectr_IO(1:ntot_comp)
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine gz_read_volume_pwr_sph
!
!   --------------------------------------------------------------------
!
      end module gz_spl_sph_spectr_data_IO
