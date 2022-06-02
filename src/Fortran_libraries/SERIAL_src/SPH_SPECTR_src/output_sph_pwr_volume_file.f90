!> @file  output_sph_pwr_volume_file.f90
!!      module output_sph_pwr_volume_file
!!
!! @author  H. Matsui
!! @date Programmed in Feb. 2008
!
!> @brief Output mean square of spectr data
!!
!!@verbatim
!!      subroutine write_total_energy_to_screen(id_rank, time_d, pwr)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_mean_squares), intent(in) :: pwr
!!
!!      integer(kind = kint) function check_sph_vol_ms_file             &
!!     &             (id_rank, ene_labels, sph_params, sph_rj, pwr)
!!      subroutine write_sph_vol_ms_file                                &
!!     &         (id_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
!!      subroutine write_sph_vol_ms_spectr_file                         &
!!     &         (id_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_mean_squares), intent(in) :: pwr
!!@endverbatim
!!
!!@n @param id_rank       Process ID
!!@n @param istep         time step number
!!@n @param time          time
!
      module output_sph_pwr_volume_file
!
      use m_precision
      use m_constants
      use t_time_data
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
      use t_energy_label_parameters
      use sph_mean_spectr_header_IO
!
      implicit none
!
!>      File ID for mean square data
      integer(kind = kint), parameter, private :: id_file_rms = 34
!
      private :: write_sph_volume_spec_file, write_sph_volume_pwr_file
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine write_total_energy_to_screen(id_rank, time_d, pwr)
!
      use m_base_field_labels
      use sph_mean_spectr_IO
!
      integer, intent(in) :: id_rank
      type(time_data), intent(in) :: time_d
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: i, icomp
!
!
      if(id_rank .ne. pwr%v_spectr(1)%irank_m) return
      write(*,'(a10,i16,a10,1pe15.8)',advance='no')                     &
     &     'time step=', time_d%i_time_step, 'time=', time_d%time
!
      do i = 1, pwr%num_fld_sq
        if (pwr%pwr_name(i) .eq. velocity%name) then
          icomp = pwr%istack_comp_sq(i)
          write(*,'(a,1pe15.8)',advance='no')                           &
     &              '  E_kin = ', pwr%v_spectr(1)%v_sq(icomp)
          exit
        end if
      end do
!
      do i = 1, pwr%num_fld_sq
        if (pwr%pwr_name(i) .eq. magnetic_field%name) then
          icomp = pwr%istack_comp_sq(i)
          write(*,'(a,1pe15.8)',advance='no')                           &
     &              '  E_mag = ', pwr%v_spectr(1)%v_sq(icomp)
          exit
        end if
      end do
      write(*,*)
!
      end subroutine write_total_energy_to_screen
!
!  --------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function check_sph_vol_ms_file               &
     &             (id_rank, ene_labels, sph_params, sph_rj, pwr)
!
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer, intent(in) :: id_rank
!
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_mean_squares), intent(in) :: pwr
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      check_sph_vol_ms_file = 0
      if(pwr%ntot_comp_sq .eq. 0)  return
!
      if(id_rank .ne. pwr%v_spectr(1)%irank_m) return
!
      write(fname_rms,   '(a,a6)')                                      &
     &      trim(pwr%v_spectr(1)%fhead_rms_v), '_s.dat'
      write(mode_label,'(a)') 'EMPTY'
      check_sph_vol_ms_file = check_sph_vol_mean_sq_file(id_file_rms,   &
     &                       fname_rms, mode_label, ene_labels,         &
     &                       sph_params, sph_rj, pwr%v_spectr(1))
!
      end function check_sph_vol_ms_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ms_file                                  &
     &         (id_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
!
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer, intent(in) :: id_rank
!
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_mean_squares), intent(in) :: pwr
!
      character(len=kchara) :: fname_rms, mode_label
      integer(kind = kint) :: i
!
!
      if(pwr%ntot_comp_sq .eq. 0)  return
!
      do i = 1, pwr%num_vol_spectr
        if(id_rank .eq. pwr%v_spectr(i)%irank_m)  then
!          write(*,*) 'write_sph_volume_pwr_file', id_rank, i
          write(fname_rms,   '(a,a6)')                                  &
     &      trim(pwr%v_spectr(i)%fhead_rms_v), '_s.dat'
          write(mode_label,'(a)') 'EMPTY'
          call write_sph_volume_pwr_file                                &
     &      (fname_rms, mode_label, ene_labels, time_d,                 &
     &       sph_params, sph_rj, pwr%v_spectr(i), pwr%v_spectr(i)%v_sq)
        end if
      end do
!
      end subroutine write_sph_vol_ms_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ms_spectr_file                           &
     &         (id_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
!
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer, intent(in) :: id_rank
!
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_mean_squares), intent(in) :: pwr
!
      character(len=kchara) :: fname_rms, mode_label
      integer(kind = kint) :: i
!
!
      if(pwr%ntot_comp_sq .eq. 0)  return
!
!
      do i = 1, pwr%num_vol_spectr
        if(pwr%v_spectr(i)%iflag_volume_rms_spec .eq. 0)  cycle
!
        if(id_rank .eq. pwr%v_spectr(i)%irank_l) then
!          write(*,*) 'write_sph_vol_ms_spectr_file l', id_rank, i
          write(fname_rms, '(a,a6)')                                    &
     &         trim(pwr%v_spectr(i)%fhead_rms_v), '_l.dat'
          write(mode_label,'(a)') 'degree'
          call write_sph_volume_spec_file                               &
     &       (fname_rms, mode_label, ene_labels, time_d,                &
     &        sph_params, sph_rj, pwr%v_spectr(i), pwr%v_spectr(i)%v_l)
        end if
!
        if(id_rank .eq. pwr%v_spectr(i)%irank_lm) then
!          write(*,*) 'write_sph_vol_ms_spectr_file lm', id_rank, i
          write(fname_rms, '(a,a7)')                                    &
     &       trim(pwr%v_spectr(i)%fhead_rms_v), '_lm.dat'
          write(mode_label,'(a)') 'diff_deg_order'
          call write_sph_volume_spec_file                               &
     &      (fname_rms, mode_label, ene_labels, time_d,                 &
     &       sph_params, sph_rj, pwr%v_spectr(i), pwr%v_spectr(i)%v_lm)
        end if
!
        if(id_rank .eq. pwr%v_spectr(i)%irank_m) then
!          write(*,*) 'write_sph_vol_ms_spectr_file m', id_rank, i
           write(fname_rms,'(a,a6)')                                    &
     &       trim(pwr%v_spectr(i)%fhead_rms_v), '_m.dat'
          write(mode_label,'(a)') 'order'
          call write_sph_volume_spec_file                               &
     &      (fname_rms, mode_label, ene_labels, time_d,                 &
     &       sph_params, sph_rj, pwr%v_spectr(i), pwr%v_spectr(i)%v_m)
!
          write(fname_rms, '(a,a7)')                                    &
     &       trim(pwr%v_spectr(i)%fhead_rms_v), '_m0.dat'
          write(mode_label,'(a)') 'EMPTY'
          call write_sph_volume_pwr_file                                &
     &      (fname_rms, mode_label, ene_labels, time_d,                 &
     &       sph_params, sph_rj, pwr%v_spectr(i), pwr%v_spectr(i)%v_m0)
        end if
      end do
!
      end subroutine write_sph_vol_ms_spectr_file
!
!  --------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_volume_spec_file                             &
     &         (fname_rms, mode_label, ene_labels, time_d,              &
     &          sph_params, sph_rj, v_pwr, rms_sph_x)
!
      use sph_mean_spectr_IO
!
      character(len=kchara), intent(in) :: fname_rms, mode_label
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(0:sph_params%l_truncation, v_pwr%ntot_comp_sq)
!
!
      call open_sph_vol_mean_sq_file(id_file_rms, fname_rms,            &
     &    mode_label, ene_labels, sph_params, sph_rj, v_pwr)
      call write_sph_volume_data(id_file_rms, time_d,                   &
     &    sph_params%l_truncation, v_pwr%ntot_comp_sq, rms_sph_x)
      close(id_file_rms)
!
      end subroutine write_sph_volume_spec_file
!
! -----------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_volume_pwr_file                              &
     &         (fname_rms, mode_label, ene_labels, time_d,              &
     &          sph_params, sph_rj, v_pwr, rms_sph_v)
!
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      character(len=kchara), intent(in) :: fname_rms, mode_label
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
      real(kind = kreal), intent(in) :: rms_sph_v(v_pwr%ntot_comp_sq)
!
!
      call open_sph_vol_mean_sq_file(id_file_rms, fname_rms,            &
     &    mode_label, ene_labels, sph_params, sph_rj, v_pwr)
!
      write(id_file_rms,'(i16,1pe23.14e3,1p200e23.14e3)')               &
     &                 time_d%i_time_step,                              &
     &                 time_d%time, rms_sph_v(1:v_pwr%ntot_comp_sq)
      close(id_file_rms)
!
      end subroutine write_sph_volume_pwr_file
!
!  --------------------------------------------------------------------
!
      end module output_sph_pwr_volume_file
