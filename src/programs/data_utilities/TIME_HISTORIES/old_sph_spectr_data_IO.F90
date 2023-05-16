!>@file   old_sph_spectr_data_IO.F90
!!
!! @author H. Matsui
!! @date   Programmed in  Sep., 2020
!!
!
!> @brief Old spectrum monitor data IO for utilities
!!
!!@verbatim
!!      subroutine gz_read_layer_mean_sph_old                           &
!!     &         (FPz_f, id_stream, flag_gzip, nri_sph, ntot_comp,      &
!!     &          i_step, time, kr_sph, spectr_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: nri_sph, ntot_comp
!!        integer(kind = kint), intent(inout) :: i_step
!!        real(kind = kreal), intent(inout) :: time
!!        integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: spectr_IO(ntot_comp,nri_sph)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine gz_read_layer_spectr_sph_old                         &
!!     &         (FPz_f, id_stream, flag_gzip,  nri_sph, ltr, ntot_comp,&
!!     &          i_step, time, kr_sph, i_mode, spectr_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: nri_sph, ltr
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        integer(kind = kint), intent(inout) :: i_step
!!        real(kind = kreal), intent(inout) :: time
!!        integer(kind = kint), intent(inout) :: i_mode(0:ltr)
!!        integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: spectr_IO(ntot_comp,0:ltr,nri_sph)
!!        integer(kind = kint), intent(inout) :: ierr
!!        type(buffer_4_gzip), intent(inout) :: zbuf
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
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_layer_mean_sph_old                             &
     &         (FPz_f, id_stream, flag_gzip, nri_sph, ntot_comp,        &
     &          i_step, time, kr_sph, spectr_IO, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: nri_sph, ntot_comp
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) :: time
      integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ntot_comp,nri_sph)
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: kr
!
!
      ierr = 1
      do kr = 1, nri_sph
        call sel_read_line_gz_stream(FPz_f, id_stream,                  &
     &                                 flag_gzip, zbuf)
        if(zbuf%len_used .lt. 0) return
!
        read(zbuf%fixbuf(1),*,err=99)                                   &
     &      i_step, time, kr_sph(kr), spectr_IO(1:ntot_comp,kr)
      end do
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine gz_read_layer_mean_sph_old
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_layer_spectr_sph_old                           &
     &         (FPz_f, id_stream, flag_gzip,  nri_sph, ltr, ntot_comp,  &
     &          i_step, time, kr_sph, i_mode, spectr_IO, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: nri_sph, ltr
      integer(kind = kint), intent(in) :: ntot_comp
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) :: time
      integer(kind = kint), intent(inout) :: i_mode(0:ltr)
      integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ntot_comp,0:ltr,nri_sph)
      integer(kind = kint), intent(inout) :: ierr
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: kr, lth
!
!
      ierr = 0
      do kr = 1, nri_sph
        do lth = 0, ltr
          call sel_read_line_gz_stream(FPz_f, id_stream,                &
     &                                 flag_gzip, zbuf)
          if(zbuf%len_used .lt. 0) return
!
          read(zbuf%fixbuf(1),*,err=99) i_step, time,                   &
     &        kr_sph(kr), i_mode(lth), spectr_IO(1:ntot_comp,lth,kr)
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
