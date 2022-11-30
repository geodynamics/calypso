!>@file   old_sph_spectr_data_IO.F90
!!
!! @author H. Matsui
!! @date   Programmed in  Sep., 2020
!!
!
!> @brief Old spectrum monitor data IO for utilities
!!
!!@verbatim
!!      subroutine sel_read_layer_pwr_sph_old(FPz_f, id_stream,         &
!!     &         flag_gzip, flag_spectr, sph_IN, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip, flag_spectr
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
      module old_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
      private gz_read_layer_spectr_sph_old, gz_read_layer_pwr_sph_old
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sel_read_layer_pwr_sph_old(FPz_f, id_stream,           &
     &         flag_gzip, flag_spectr, sph_IN, zbuf, ierr)
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip, flag_spectr
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
      if(flag_spectr) then
        call gz_read_layer_spectr_sph_old                               &
     &     (FPz_f, id_stream, flag_gzip, sph_IN, zbuf, ierr)
      else
        call gz_read_layer_pwr_sph_old                                  &
     &     (FPz_f, id_stream, flag_gzip, sph_IN, zbuf, ierr)
      end if
!
      end subroutine sel_read_layer_pwr_sph_old
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine  gz_read_layer_pwr_sph_old                             &
     &         (FPz_f, id_stream, flag_gzip, sph_IN, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: kr
!
!
      ierr = 1
      do kr = 1, sph_IN%nri_sph
        call sel_read_line_gz_stream(FPz_f, id_stream,                  &
     &                                 flag_gzip, zbuf)
        if(zbuf%len_used .lt. 0) return
!
        read(zbuf%fixbuf(1),*,err=99)                                   &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph(kr),              &
     &      sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,kr)
      end do
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine gz_read_layer_pwr_sph_old
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_layer_spectr_sph_old                           &
     &         (FPz_f, id_stream, flag_gzip, sph_IN, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: kr, lth
!
!
      ierr = 0
      do kr = 1, sph_IN%nri_sph
        do lth = 0, sph_IN%ltr_sph
          call sel_read_line_gz_stream(FPz_f, id_stream,                &
     &                                 flag_gzip, zbuf)
          if(zbuf%len_used .lt. 0) return
!
          read(zbuf%fixbuf(1),*,err=99) sph_IN%i_step, sph_IN%time,     &
     &        sph_IN%kr_sph(kr), sph_IN%i_mode(lth),                    &
     &        sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,kr)
        end do
      end do
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine gz_read_layer_spectr_sph_old
!
!   --------------------------------------------------------------------
!
      end module old_sph_spectr_data_IO
