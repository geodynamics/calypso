!> @file  output_sph_m_square_file.f90
!!      module output_sph_m_square_file
!!
!! @author  H. Matsui
!! @date Programmed in Feb. 2008
!
!> @brief Output mean square of spectr data
!!
!!@verbatim
!!      subroutine write_total_energy_to_screen(my_rank, istep, time)
!!
!!      subroutine write_sph_vol_ms_file(my_rank, istep, time)
!!      subroutine write_sph_vol_ms_spectr_file(my_rank, istep, time)
!!      subroutine write_sph_layer_ms_file(my_rank, istep, time)
!!
!!      subroutine write_sph_1layer_ms_spec_file(my_rank, istep, time)
!!@endverbatim
!!
!!@n @param my_rank       Process ID
!!@n @param istep         time step number
!!@n @param time          time
!
      module output_sph_m_square_file
!
      use m_precision
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine write_total_energy_to_screen(my_rank, istep, time)
!
      use m_phys_labels
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: i, icomp
!
!
      if(my_rank .gt. 0) return
      write(*,'(a10,i16,a10,1pe15.8)',advance='no')                     &
     &            'time step=',istep,'time=',time
!
      do i = 1, num_rms_rj
        if (rms_name_rj(i) .eq. fhd_velo) then
          icomp = istack_rms_comp_rj(i)
          write(*,'(a,1pe15.8)',advance='no')                           &
     &              '  E_kin = ', rms_sph_vol(icomp)
          exit
        end if
      end do
!
      do i = 1, num_rms_rj
        if (rms_name_rj(i) .eq. fhd_magne) then
          icomp = istack_rms_comp_rj(i)
          write(*,'(a,1pe15.8)',advance='no')                           &
     &              '  E_mag = ', rms_sph_vol(icomp)
          exit
        end if
      end do
      write(*,*)
!
      end subroutine write_total_energy_to_screen
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ms_file(my_rank, istep, time)
!
      use set_parallel_file_name
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(ntot_rms_rj .eq. 0)  return
!
      call add_dat_extension(fhead_rms_vol, fname_rms)
      write(mode_label,'(a)') 'EMPTY'
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label)
!
      write(id_file_rms,'(i16,1pe23.14e3,1p200e23.14e3)')               &
     &                 istep, time, rms_sph_vol(1:ntot_rms_rj)
      close(id_file_rms)
!
      end subroutine write_sph_vol_ms_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ms_spectr_file(my_rank, istep, time)
!
      use set_parallel_file_name
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(iflag_volume_rms_spec .eq. 0)  return
      if(ntot_rms_rj .eq. 0)  return
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_vol), '_l.dat'
      write(mode_label,'(a)') 'degree'
      call open_sph_mean_sq_file                                        &
     &      (id_file_rms_l, fname_rms, mode_label)
!
      write(fname_rms,'(a,a7)') trim(fhead_rms_vol), '_lm.dat'
      write(mode_label,'(a)') 'diff_deg_order'
      call open_sph_mean_sq_file                                        &
     &      (id_file_rms_lm, fname_rms, mode_label)
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_vol), '_m.dat'
      write(mode_label,'(a)') 'order'
      call open_sph_mean_sq_file                                        &
     &      (id_file_rms_m, fname_rms, mode_label)
!
      call write_sph_vol_pwr(istep, time)
!
      close(id_file_rms_l)
      close(id_file_rms_m)
      close(id_file_rms_lm)
!
      end subroutine write_sph_vol_ms_spectr_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_layer_ms_file(my_rank, istep, time)
!
      use set_parallel_file_name
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(iflag_layer_rms_spec .eq. 0)  return
      if(ntot_rms_rj .eq. 0)  return
!
      write(fname_rms,   '(a,a4)') trim(fhead_rms_layer), '.dat'
      write(mode_label,'(a)') 'radial_id'
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label)
!
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_l.dat'
      write(mode_label,'(a)') 'radial_id    degree'
      call open_sph_mean_sq_file(id_file_rms_l, fname_rms, mode_label)
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_m.dat'
      write(mode_label,'(a)') 'radial_id    order'
      call open_sph_mean_sq_file(id_file_rms_m, fname_rms, mode_label)
!
      write(fname_rms,'(a,a7)') trim(fhead_rms_layer), '_lm.dat'
      write(mode_label,'(a)') 'radial_id    diff_deg_order'
      call open_sph_mean_sq_file(id_file_rms_lm, fname_rms, mode_label)
!
      call write_sph_selected_layer_pwr(istep, time)
!
      close(id_file_rms)
      close(id_file_rms_l)
      close(id_file_rms_m)
      close(id_file_rms_lm)
!
      end subroutine write_sph_layer_ms_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_1layer_ms_spec_file(my_rank, istep, time)
!
      use m_rms_4_sph_spectr
      use m_pickup_sph_spectr_data
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      character(len = kchara) :: fname_tmp1, fname_tmp2
      character(len = kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0) return
      if(ntot_rms_rj .eq. 0) return
!
      call add_int_suffix(id_pick_layer(1), fhead_rms_layer,            &
     &   fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms)
      write(mode_label,'(a)') 'radial_id, '
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label)
!
!
      write(fname_rms, '(a,a2)') trim(fhead_rms_layer), '_l'
      call add_int_suffix(id_pick_layer(1), fname_rms, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms)
      write(mode_label,'(a)') 'radial_id    degree'
      call open_sph_mean_sq_file                                        &
     &    (id_file_rms_l, fname_rms, mode_label)
!
      write(fname_rms, '(a,a2)') trim(fhead_rms_layer), '_m'
      call add_int_suffix(id_pick_layer(1), fname_rms, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms)
      write(mode_label,'(a)') 'radial_id    order'
      call open_sph_mean_sq_file                                        &
     &    (id_file_rms_m, fname_rms, mode_label)
!
      write(fname_rms, '(a,a3)') trim(fhead_rms_layer), '_lm'
      call add_int_suffix(id_pick_layer(1), fname_rms, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms)
      write(mode_label,'(a)') 'radial_id    diff_deg_order'
      call open_sph_mean_sq_file                                        &
     &    (id_file_rms_lm, fname_rms, mode_label)
!
!
      call write_sph_selected_layer_pwr(istep, time)
!
      close(id_file_rms)
!
      close(id_file_rms_l)
      close(id_file_rms_m)
      close(id_file_rms_lm)
!
      end subroutine write_sph_1layer_ms_spec_file
!
!  --------------------------------------------------------------------
!
      end module output_sph_m_square_file
