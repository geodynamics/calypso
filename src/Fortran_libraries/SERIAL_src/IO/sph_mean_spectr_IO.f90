!>@file   sph_mean_spectr_IO.f90
!!@brief  module sph_mean_spectr_IO
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      integer(kind = kint) function check_sph_vol_mean_sq_file        &
!!     &         (id_file, fname_rms, mode_label,                       &
!!     &          ltr, num_rms_rj, ntot_rms_rj, num_rms_comp_rj,        &
!!     &          rms_name_rj, nri, nlayer_ICB, nlayer_CMB,             &
!!     &          kr_inner, kr_outer)
!!
!!      subroutine open_sph_vol_mean_sq_file                            &
!!     &         (id_file, fname_rms, mode_label,                       &
!!     &          ltr, num_rms_rj, ntot_rms_rj, num_rms_comp_rj,        &
!!     &          rms_name_rj, nri, nlayer_ICB, nlayer_CMB,             &
!!     &          kr_inner, kr_outer, r_inner,  r_outer)
!!      subroutine open_sph_mean_sq_file(id_file, fname_rms, mode_label,&
!!     &          ltr, num_rms_rj, ntot_rms_rj, num_rms_comp_rj,        &
!!     &          rms_name_rj, nri_rms, nlayer_ICB, nlayer_CMB)
!!      subroutine write_sph_volume_data(id_file, istep, time,          &
!!     &          ltr, ntot_rms_rj, rms_sph_x)
!!      subroutine write_sph_layerd_power(id_file, istep, time,         &
!!     &          ntot_rms_rj, nri_rms, kr_rms, r_rms, rms_sph)
!!      subroutine write_sph_layer_data(id_file, istep, time,           &
!!     &          ltr, ntot_rms_rj, nri_rms, kr_rms, r_rms, rms_sph_x)
!!@endverbatim
!!
!!@n @param my_rank       Process ID
!!@n @param istep         time step number
!!@n @param time          time
!!
!!@n @param id_file       file ID for output
!!@n @param fname_rms     file name for output
!!@n @param mode_label    data label for degree or order of harmonics
!
      module sph_mean_spectr_IO
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function check_sph_vol_mean_sq_file          &
     &         (id_file, fname_rms, mode_label,                         &
     &          ltr, num_rms_rj, ntot_rms_rj, num_rms_comp_rj,          &
     &          rms_name_rj, nri, nlayer_ICB, nlayer_CMB,               &
     &          kr_inner, kr_outer)
!
      use sph_mean_spectr_header_IO
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
      integer(kind = kint), intent(in) :: nri, ltr
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_inner, kr_outer
      integer(kind = kint), intent(in) :: num_rms_comp_rj(num_rms_rj)
      character (len=kchara), intent(in) :: rms_name_rj(num_rms_rj)
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', err = 99)
!
      check_sph_vol_mean_sq_file                                        &
     &         = check_sph_vol_mean_sq_header(id_file, mode_label, ltr, &
     &          num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,  &
     &          nri, nlayer_ICB, nlayer_CMB, kr_inner, kr_outer)
      close(id_file)
!
      return
!
   99 continue
      write(*,*) 'No mean suare file'
      check_sph_vol_mean_sq_file = 0
!
      end function check_sph_vol_mean_sq_file
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_mean_sq_file                              &
     &         (id_file, fname_rms, mode_label,                         &
     &          ltr, num_rms_rj, ntot_rms_rj, num_rms_comp_rj,          &
     &          rms_name_rj, nri, nlayer_ICB, nlayer_CMB,               &
     &          kr_inner, kr_outer, r_inner,  r_outer)
!
      use sph_mean_spectr_header_IO
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
      integer(kind = kint), intent(in) :: nri, ltr
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_inner, kr_outer
      real(kind = kreal), intent(in) ::   r_inner,  r_outer
      integer(kind = kint), intent(in) :: num_rms_comp_rj(num_rms_rj)
      character (len=kchara), intent(in) :: rms_name_rj(num_rms_rj)
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='replace')
      call write_sph_vol_mean_sq_header(id_file, mode_label, ltr,       &
     &    num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,        &
     &    nri, nlayer_ICB, nlayer_CMB, kr_inner, kr_outer,              &
     &    r_inner, r_outer)
!
      end subroutine open_sph_vol_mean_sq_file
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_mean_sq_file(id_file, fname_rms, mode_label,  &
     &          ltr, num_rms_rj, ntot_rms_rj, num_rms_comp_rj,          &
     &          rms_name_rj, nri_rms, nlayer_ICB, nlayer_CMB)
!
      use sph_mean_spectr_header_IO
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
      integer(kind = kint), intent(in) :: nri_rms, ltr
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: num_rms_comp_rj(num_rms_rj)
      character (len=kchara), intent(in) :: rms_name_rj(num_rms_rj)
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='replace')
      call write_sph_mean_sq_header(id_file, mode_label, ltr,           &
     &    num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,        &
     &    nri_rms, nlayer_ICB, nlayer_CMB)
!
      end subroutine open_sph_mean_sq_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_volume_data(id_file, istep, time,            &
     &          ltr, ntot_rms_rj, rms_sph_x)
!
      integer(kind = kint), intent(in) :: id_file, istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint), intent(in) :: ltr, ntot_rms_rj
      real(kind = kreal), intent(in) :: rms_sph_x(0:ltr, ntot_rms_rj)
!
      integer(kind = kint) :: lm
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a20,i3,a16)')                                     &
     &     '(i16,1pe23.14e3,i16,', ntot_rms_rj, '(1pe23.14e3),a1)'
!
      do lm = 0, ltr
        write(id_file,fmt_txt) istep, time, lm,                         &
     &                         rms_sph_x(lm,1:ntot_rms_rj)
      end do
!
      end subroutine write_sph_volume_data
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layerd_power(id_file, istep, time,           &
     &          ntot_rms_rj, nri_rms, kr_rms, r_rms, rms_sph)
!
      integer(kind = kint), intent(in) :: id_file, istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint), intent(in) :: nri_rms, ntot_rms_rj
      integer(kind=kint), intent(in) :: kr_rms(nri_rms)
      real(kind = kreal), intent(in) :: r_rms(nri_rms)
      real(kind = kreal), intent(in) :: rms_sph(nri_rms,ntot_rms_rj)
!
      integer(kind = kint) :: k
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a20,i3,a16)')                                     &
     &     '(i16,1pe23.14e3,i16,', (ntot_rms_rj+1), '(1pe23.14e3),a1)'
      do k = 1, nri_rms
        write(id_file,fmt_txt) istep, time, kr_rms(k), r_rms(k),        &
     &                         rms_sph(k,1:ntot_rms_rj)
      end do
!
      end subroutine write_sph_layerd_power
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_data(id_file, istep, time,             &
     &          ltr, ntot_rms_rj, nri_rms, kr_rms, r_rms, rms_sph_x)
!
      integer(kind = kint), intent(in) :: id_file, istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint), intent(in) :: nri_rms, ltr, ntot_rms_rj
      integer(kind=kint), intent(in) :: kr_rms(nri_rms)
      real(kind = kreal), intent(in) :: r_rms(nri_rms)
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(nri_rms,0:ltr, ntot_rms_rj)
!
      integer(kind = kint) :: k, lm
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a35,i3,a16)')                                     &
     &     '(i16,1pe23.14e3,i16,1pe23.14e3,i16,',                       &
     &       ntot_rms_rj, '(1pe23.14e3),a1)'
!
      do k = 1, nri_rms
        do lm = 0, ltr
          write(id_file,fmt_txt) istep, time, kr_rms(k), r_rms(k),      &
     &                           lm, rms_sph_x(k,lm,1:ntot_rms_rj)
        end do
      end do
!
      end subroutine write_sph_layer_data
!
! -----------------------------------------------------------------------
!
      end module sph_mean_spectr_IO
