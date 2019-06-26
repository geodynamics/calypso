!>@file   simple_sph_spectr_data_IO.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time spectrum data IO for utilities
!!
!!@verbatim
!!      integer(kind = kint) function  read_volume_pwr_sph              &
!!     &                            (id_file, sph_IN)
!!      integer(kind = kint) function  read_volume_spectr_sph           &
!!     &                            (id_file, sph_IN)
!!      integer(kind = kint) function  read_layer_pwr_sph               &
!!     &                            (id_file, sph_IN)
!!      integer(kind = kint) function  read_layer_spectr_sph            &
!!     &                            (id_file, sph_IN)
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!
!!      subroutine write_vol_sph_data(id_file, sph_IN)
!!      subroutine write_vol_spectr_data(id_file, sph_IN)
!!      subroutine write_layer_sph_data(id_file, sph_IN)
!!      subroutine write_layer_spectr_data(id_file, sph_IN)
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!
!!      integer(kind = kint) function  read_layer_pwr_sph_old           &
!!     &                            (id_file, sph_IN)
!!      integer(kind = kint) function  read_layer_spectr_sph_old        &
!!     &                            (id_file, sph_IN)
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!@endverbatim
!
      module simple_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function  read_volume_pwr_sph                &
     &                            (id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      read_volume_pwr_sph = 0
      read(id_file,*,err=99,end=99) sph_IN%i_step, sph_IN%time,         &
     &             sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,1)
      return
!
   99 continue
      read_volume_pwr_sph = 1
      return
!
      end function read_volume_pwr_sph
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function  read_volume_spectr_sph             &
     &                            (id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      integer(kind = kint) :: itmp, lth
!
!
      read_volume_spectr_sph = 0
      do lth = 0, sph_IN%ltr_sph
        read(id_file,*,err=99,end=99) sph_IN%i_step, sph_IN%time, itmp, &
     &               sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,1)
      end do
      return
!
   99 continue
      read_volume_spectr_sph = 1
      return
!
      end function read_volume_spectr_sph
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function  read_layer_pwr_sph                 &
     &                            (id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      integer(kind = kint) :: kr
!
!
      read_layer_pwr_sph = 0
      do kr = 1, sph_IN%nri_sph
        read(id_file,*,err=99,end=99) sph_IN%i_step, sph_IN%time,       &
     &      sph_IN%kr_sph(kr), sph_IN%r_sph(kr),                        &
     &      sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,kr)
      end do
      return
!
   99 continue
      read_layer_pwr_sph = 1
      return
!
      end function read_layer_pwr_sph
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function  read_layer_spectr_sph              &
     &                            (id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      integer(kind = kint) :: kr, itmp, lth
!
!
      read_layer_spectr_sph = 0
      do kr = 1, sph_IN%nri_sph
        do lth = 0, sph_IN%ltr_sph
          read(id_file,*,err=99,end=99) sph_IN%i_step, sph_IN%time,     &
     &        sph_IN%kr_sph(kr), sph_IN%r_sph(kr), itmp,                &
     &        sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,kr)
          end do
        end do
      return
!
   99 continue
      read_layer_spectr_sph = 1
      return
!
      end function read_layer_spectr_sph
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_vol_sph_data(id_file, sph_IN)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
!
      write(id_file,1000) sph_IN%i_step, sph_IN%time,                   &
     &                  sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,1)
!
 1000 format(i16,1pE25.15e3,1p255E25.15e3)
!
      end subroutine write_vol_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_vol_spectr_data(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: lth
!
!
      do lth = 0, sph_IN%ltr_sph
        write(id_file,1000) sph_IN%i_step, sph_IN%time, lth,            &
     &               sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,1)
      end do
!
 1000 format(i16,1pE25.15e3,i16,1p255E25.15e3)
!
      end subroutine write_vol_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_layer_sph_data(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: kr
!
!
      do kr = 1, sph_IN%nri_sph
        write(id_file,1000) sph_IN%i_step, sph_IN%time,                 &
     &         sph_IN%kr_sph(kr), sph_IN%r_sph(kr),                     &
     &         sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,kr)
      end do
!
 1000 format(i16,1pE25.15e3,i16,1pE25.15e3,1p255E25.15e3)
!
      end subroutine write_layer_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_layer_spectr_data(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: kr, lth
!
!
      do kr = 1, sph_IN%nri_sph
        do lth = 0, sph_IN%ltr_sph
          write(id_file,1000) sph_IN%i_step, sph_IN%time,               &
     &         sph_IN%kr_sph(kr), sph_IN%r_sph(kr), lth,                &
     &         sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,kr)
        end do
      end do
!
 1000 format(i16,1pE25.15e3,i16,1pE25.15e3,i16,1p255E25.15e3)
!
      end subroutine write_layer_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function  read_layer_pwr_sph_old             &
     &                            (id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      integer(kind = kint) :: kr
!
!
      read_layer_pwr_sph_old = 0
      do kr = 1, sph_IN%nri_sph
        read(id_file,*,err=99,end=99)                                   &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph(kr),              &
     &      sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,kr)
      end do
      return
!
   99 continue
      read_layer_pwr_sph_old = 1
      return
!
      end function read_layer_pwr_sph_old
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function  read_layer_spectr_sph_old          &
     &                            (id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      integer(kind = kint) :: kr, itmp, lth
!
!
      read_layer_spectr_sph_old = 0
      do kr = 1, sph_IN%nri_sph
        do lth = 0, sph_IN%ltr_sph
          read(id_file,*,err=99,end=99) sph_IN%i_step, sph_IN%time,     &
     &        sph_IN%kr_sph(kr), itmp,                                  &
     &        sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,kr)
        end do
      end do
      return
!
   99 continue
      read_layer_spectr_sph_old = 1
      return
!
      end function read_layer_spectr_sph_old
!
!   --------------------------------------------------------------------
!
      end module simple_sph_spectr_data_IO
