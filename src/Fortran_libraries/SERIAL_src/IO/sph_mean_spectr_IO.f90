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
!!     &                   (id_file, fname_rms, mode_label,             &
!!     &                    ene_labels, sph_params, sph_rj, v_pwr)
!!      subroutine open_sph_vol_mean_sq_file(id_file, fname_rms,        &
!!     &          mode_label, ene_labels, sph_params, sph_rj, v_pwr)
!!      subroutine open_sph_mean_sq_file(id_file, fname_rms, mode_label,&
!!     &          ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_vol_mean_squares), intent(in) :: v_pwr
!!        type(sph_mean_squares), intent(in) :: pwr
!!
!!      subroutine write_sph_volume_data                                &
!!     &         (id_file, time_d, ltr, ntot_rms_rj, rms_sph_x)
!!      subroutine write_sph_layerd_power(id_file, time_d, pwr, rms_sph)
!!      subroutine write_sph_layer_data                                 &
!!     &         (id_file, time_d, ltr, pwr, rms_sph_x)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_mean_squares), intent(in) :: pwr
!!@endverbatim
!!
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
      use t_time_data
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
      use t_energy_label_parameters
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
     &                   (id_file, fname_rms, mode_label,               &
     &                    ene_labels, sph_params, sph_rj, v_pwr)
!
      use sph_mean_spectr_header_IO
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', err = 99)
!
      check_sph_vol_mean_sq_file                                        &
     &         = check_sph_vol_mean_sq_header(id_file, mode_label,      &
     &          ene_labels, sph_params, sph_rj, v_pwr)
      close(id_file)
!      write(*,*) 'Checked ', trim(fname_rms),                          &
!     &     check_sph_vol_mean_sq_file
!
      return
!
   99 continue
      write(*,*) 'No mean suare file: ', trim(fname_rms)
      check_sph_vol_mean_sq_file = 0
!
      end function check_sph_vol_mean_sq_file
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_mean_sq_file(id_file, fname_rms,          &
     &          mode_label, ene_labels, sph_params, sph_rj, v_pwr)
!
      use sph_mean_spectr_header_IO
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='replace')
      call write_sph_vol_mean_sq_header                                 &
     &   (id_file, mode_label, ene_labels, sph_params, sph_rj, v_pwr)
!
      end subroutine open_sph_vol_mean_sq_file
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_mean_sq_file(id_file, fname_rms, mode_label,  &
     &          ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr)
!
      use sph_mean_spectr_header_IO
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_mean_squares), intent(in) :: pwr
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='replace')
      call write_sph_mean_sq_header(id_file, mode_label,                &
     &    ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr)
!
      end subroutine open_sph_mean_sq_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_volume_data                                  &
     &         (id_file, time_d, ltr, ntot_rms_rj, rms_sph_x)
!
      integer(kind = kint), intent(in) :: id_file
      type(time_data), intent(in) :: time_d
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
        write(id_file,fmt_txt) time_d%i_time_step, time_d%time, lm,     &
     &                         rms_sph_x(lm,1:ntot_rms_rj)
      end do
!
      end subroutine write_sph_volume_data
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layerd_power(id_file, time_d, pwr, rms_sph)
!
      integer(kind = kint), intent(in) :: id_file
      type(time_data), intent(in) :: time_d
      type(sph_mean_squares), intent(in) :: pwr
!
      real(kind = kreal), intent(in)                                    &
     &           :: rms_sph(pwr%nri_rms,pwr%ntot_comp_sq)
!
      integer(kind = kint) :: k
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a20,i3,a16)') '(i16,1pe23.14e3,i16,',             &
     &                        (pwr%ntot_comp_sq+1), '(1pe23.14e3),a1)'
      do k = 1, pwr%nri_rms
        write(id_file,fmt_txt) time_d%i_time_step, time_d%time,         &
     &                         pwr%kr_4_rms(k), pwr%r_4_rms(k),         &
     &                         rms_sph(k,1:pwr%ntot_comp_sq)
      end do
!
      end subroutine write_sph_layerd_power
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_data                                   &
     &         (id_file, time_d, ltr, pwr, rms_sph_x)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) ::  ltr
      type(time_data), intent(in) :: time_d
      type(sph_mean_squares), intent(in) :: pwr
!
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq)
!
      integer(kind = kint) :: k, lm
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a35,i3,a16)')                                     &
     &     '(i16,1pe23.14e3,i16,1pe23.14e3,i16,',                       &
     &       pwr%ntot_comp_sq, '(1pe23.14e3),a1)'
!
      do k = 1, pwr%nri_rms
        do lm = 0, ltr
          write(id_file,fmt_txt) time_d%i_time_step, time_d%time,       &
     &                          pwr%kr_4_rms(k), pwr%r_4_rms(k), lm,    &
     &                          rms_sph_x(k,lm,1:pwr%ntot_comp_sq)
        end do
      end do
!
      end subroutine write_sph_layer_data
!
! -----------------------------------------------------------------------
!
      end module sph_mean_spectr_IO
