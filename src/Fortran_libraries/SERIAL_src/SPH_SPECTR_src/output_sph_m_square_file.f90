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
!!@endverbatim
!!
!!@n @param my_rank       Process ID
!!@n @param istep         time step number
!!@n @param time          time
!
      module output_sph_m_square_file
!
      use m_precision
      use m_constants
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      implicit none
!
!>      File ID for mean square data
      integer(kind = kint), parameter, private :: id_file_rms = 34
!
!    output flag
!
!>      Output flag for layerd mean square data
      integer(kind = kint) :: iflag_layer_rms_spec =  0
!>      Output flag for volume mean square data
      integer(kind = kint) :: iflag_volume_rms_spec = 0
!>      Output flag for volume average data
      integer(kind = kint) :: iflag_volume_ave_sph =  0
!
!>      Output flag for spectrum with respect to degree
      integer(kind = kint) :: iflag_spectr_l =  1
!>      Output flag for spectrum with respect to order
      integer(kind = kint) :: iflag_spectr_m =  1
!>      Output flag for spectrum with respect to l-m
      integer(kind = kint) :: iflag_spectr_lm = 1
!
!
!>      File prefix for volume mean square file
      character(len = kchara) :: fhead_rms_vol =    'sph_pwr_volume'
!>      File prefix for layered mean square file
      character(len = kchara) :: fhead_rms_layer =  'sph_pwr_layer'
!>      File prefix for volume average file
      character(len = kchara) :: fhead_ave_vol =    'sph_ave_volume'
!
      private :: write_sph_volume_spec_file
      private :: write_sph_layer_pwr_file, write_sph_layer_spec_file
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
      subroutine write_sph_vol_ave_file(istep, time)
!
      use set_parallel_file_name
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(idx_rj_degree_zero .eq. 0)  return
      if(ntot_rms_rj .eq. 0)  return
!
      write(fname_rms, '(a,a4)') trim(fhead_ave_vol), '.dat'
      write(mode_label,'(a)') 'EMPTY'
      call open_sph_mean_sq_file                                        &
     &      (id_file_rms, fname_rms, mode_label)
!
      write(id_file_rms,'(i15,1pe23.14e3,1p200e23.14e3)')               &
     &                 istep, time, ave_sph_vol(1:ntot_rms_rj)
      close(id_file_rms)
!
      end subroutine write_sph_vol_ave_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
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
!
      if(iflag_spectr_l .gt. izero) then
        write(fname_rms, '(a,a6)') trim(fhead_rms_vol), '_l.dat'
        write(mode_label,'(a)') 'degree'
        call write_sph_volume_spec_file(fname_rms, mode_label,          &
     &      istep, time, rms_sph_vol_l)
      end if
!
      if(iflag_spectr_m .gt. izero) then
        write(fname_rms,'(a,a7)') trim(fhead_rms_vol), '_lm.dat'
        write(mode_label,'(a)') 'diff_deg_order'
        call write_sph_volume_spec_file(fname_rms, mode_label,          &
     &      istep, time, rms_sph_vol_m)
      end if
!
      if(iflag_spectr_lm .gt. izero) then
        write(fname_rms, '(a,a6)') trim(fhead_rms_vol), '_m.dat'
        write(mode_label,'(a)') 'order'
        call write_sph_volume_spec_file(fname_rms, mode_label,          &
     &      istep, time, rms_sph_vol_lm)
      end if
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
      if(iflag_layer_rms_spec .eq. izero)  return
      if(ntot_rms_rj .eq. 0)  return
!
!
      write(fname_rms,   '(a,a4)') trim(fhead_rms_layer), '.dat'
      write(mode_label,'(a)') 'radial_id'
      call write_sph_layer_pwr_file(fname_rms, mode_label, istep, time)
!
      if(iflag_spectr_l .gt. izero) then
        write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_l.dat'
        write(mode_label,'(a)') 'radial_id    degree'
        call write_sph_layer_spec_file(fname_rms, mode_label,           &
     &      istep, time, rms_sph_l)
      end if
!
      if(iflag_spectr_m .gt. izero) then
        write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_m.dat'
        write(mode_label,'(a)') 'radial_id    order'
        call write_sph_layer_spec_file(fname_rms, mode_label,           &
     &      istep, time, rms_sph_m)
      end if
!
      if(iflag_spectr_lm .gt. izero) then
        write(fname_rms,'(a,a7)') trim(fhead_rms_layer), '_lm.dat'
        write(mode_label,'(a)') 'radial_id    diff_deg_order'
        call write_sph_layer_spec_file(fname_rms, mode_label,           &
     &      istep, time, rms_sph_lm)
      end if
!
      end subroutine write_sph_layer_ms_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_volume_spec_file(fname_rms, mode_label,      &
     &          istep, time, rms_sph_x)
!
      use m_spheric_parameter
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(0:l_truncation, ntot_rms_rj)
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label)
      call write_sph_volume_data(id_file_rms, istep, time, rms_sph_x)
      close(id_file_rms)
!
      end subroutine write_sph_volume_spec_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_pwr_file(fname_rms, mode_label,        &
     &          istep, time)
!
      use m_spheric_parameter
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label)
      call write_sph_layerd_power(id_file_rms, istep, time)
      close(id_file_rms)
!
      end subroutine write_sph_layer_pwr_file
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_spec_file(fname_rms, mode_label,       &
     &          istep, time, rms_sph_x)
!
      use m_spheric_parameter
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(nri_rms,0:l_truncation, ntot_rms_rj)
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label)
      call write_sph_layer_data(id_file_rms, istep, time, rms_sph_x)
      close(id_file_rms)
!
      end subroutine write_sph_layer_spec_file
!
! -----------------------------------------------------------------------
!
      end module output_sph_m_square_file
