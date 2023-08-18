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
!!      subroutine write_sph_vol_ms_file                                &
!!     &         (id_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
!!      subroutine write_sph_vol_ms_spectr_file                         &
!!     &         (id_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_mean_squares), intent(in) :: pwr
!!
!!      subroutine write_sph_volume_pwr_file                            &
!!     &         (fname_rms, mode_label, ene_labels, time_d,            &
!!     &          sph_params, sph_rj, v_pwr, rms_sph_v)
!!        character(len=kchara), intent(in) :: fname_rms, mode_label
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_vol_mean_squares), intent(in) :: v_pwr
!!        real(kind = kreal), intent(in) :: rms_sph_v(v_pwr%ntot_comp_sq)
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
      private :: write_sph_volume_spec_file
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
      subroutine write_sph_vol_ms_file                                  &
     &         (id_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
!
      use set_parallel_file_name
      use skip_comment_f
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
          if(no_file_flag(pwr%v_spectr(i)%fhead_rms_v)) cycle
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
      use skip_comment_f
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
        if(pwr%v_spectr(i)%flag_skip_v_spec_l) cycle
!
        if(id_rank .eq. pwr%v_spectr(i)%irank_l) then
          if(no_file_flag(pwr%v_spectr(i)%fhead_rms_v)) cycle
!          write(*,*) 'write_sph_vol_ms_spectr_file l', id_rank, i
          write(fname_rms, '(a,a6)')                                    &
     &         trim(pwr%v_spectr(i)%fhead_rms_v), '_l.dat'
          write(mode_label,'(a)') 'degree'
          call write_sph_volume_spec_file                               &
     &       (fname_rms, mode_label, ene_labels, time_d,                &
     &        sph_params, sph_rj, pwr%v_spectr(i), pwr%v_spectr(i)%v_l)
        end if
      end do
!
      do i = 1, pwr%num_vol_spectr
        if(pwr%v_spectr(i)%iflag_volume_rms_spec .eq. 0)  cycle
        if(pwr%v_spectr(i)%flag_skip_v_spec_lm) cycle
!
        if(id_rank .eq. pwr%v_spectr(i)%irank_lm) then
          if(no_file_flag(pwr%v_spectr(i)%fhead_rms_v)) cycle
!          write(*,*) 'write_sph_vol_ms_spectr_file lm', id_rank, i
          write(fname_rms, '(a,a7)')                                    &
     &       trim(pwr%v_spectr(i)%fhead_rms_v), '_lm.dat'
          write(mode_label,'(a)') 'diff_deg_order'
          call write_sph_volume_spec_file                               &
     &      (fname_rms, mode_label, ene_labels, time_d,                 &
     &       sph_params, sph_rj, pwr%v_spectr(i), pwr%v_spectr(i)%v_lm)
        end if
      end do
!
      do i = 1, pwr%num_vol_spectr
        if(pwr%v_spectr(i)%iflag_volume_rms_spec .eq. 0)  cycle
        if(pwr%v_spectr(i)%flag_skip_v_spec_m) cycle
!
        if(id_rank .eq. pwr%v_spectr(i)%irank_m) then
          if(no_file_flag(pwr%v_spectr(i)%fhead_rms_v)) cycle
!          write(*,*) 'write_sph_vol_ms_spectr_file m', id_rank, i
          write(fname_rms,'(a,a6)')                                     &
     &       trim(pwr%v_spectr(i)%fhead_rms_v), '_m.dat'
          write(mode_label,'(a)') 'order'
          call write_sph_volume_spec_file                               &
     &      (fname_rms, mode_label, ene_labels, time_d,                 &
     &       sph_params, sph_rj, pwr%v_spectr(i), pwr%v_spectr(i)%v_m)
        end if
      end do
!
      do i = 1, pwr%num_vol_spectr
        if(pwr%v_spectr(i)%iflag_volume_rms_spec .eq. 0)  cycle
        if(pwr%v_spectr(i)%flag_skip_v_spec_m0) cycle
!
        if(id_rank .eq. pwr%v_spectr(i)%irank_m) then
          if(no_file_flag(pwr%v_spectr(i)%fhead_rms_v)) cycle
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
      use t_read_sph_spectra
      use t_buffer_4_gzip
      use gz_open_sph_vol_mntr_file
      use sph_mean_spectr_header_IO
      use gz_volume_spectr_monitor_IO
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
      real(kind = kreal), allocatable :: spectr_IO(:,:)
      type(read_sph_spectr_data), save :: sph_OUT
      type(buffer_4_gzip), save :: zbuf_m
      logical :: flag_gzip_lc
!
!
      call dup_sph_vol_spectr_header                                    &
     &   (mode_label, sph_params%l_truncation,                          &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    ene_labels, sph_rj, v_pwr, sph_OUT)
      call alloc_sph_spectr_data(sph_OUT%ltr_sph, sph_OUT)
      allocate(spectr_IO(v_pwr%ntot_comp_sq,0:sph_params%l_truncation))
!
      flag_gzip_lc = v_pwr%gzip_flag_vol_spec
      call sel_open_sph_vol_monitor_file(id_file_rms, fname_rms,        &
     &    sph_pwr_labels, sph_OUT, zbuf_m, flag_gzip_lc)
      call swap_volume_spectr_to_IO(sph_params%l_truncation,            &
     &    v_pwr%ntot_comp_sq, rms_sph_x, spectr_IO(1,0))
      call sel_gz_write_volume_spectr_mtr                               &
     &   (flag_gzip_lc, id_file_rms, time_d%i_time_step, time_d%time,   &
     &    sph_params%l_truncation, v_pwr%ntot_comp_sq,                  &
     &    spectr_IO(1,0), zbuf_m)
      deallocate(spectr_IO)
      call dealloc_sph_espec_data(sph_OUT)
      call dealloc_sph_espec_name(sph_OUT)
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
      use t_read_sph_spectra
      use t_buffer_4_gzip
      use set_parallel_file_name
      use gz_open_sph_vol_mntr_file
      use sph_monitor_data_text
      use select_gz_stream_file_IO
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
      type(read_sph_spectr_data), save :: sph_OUT
      type(buffer_4_gzip), save :: zbuf_m
      logical :: flag_gzip_lc
!
!
      call dup_sph_vol_spectr_header                                    &
     &   (mode_label, sph_params%l_truncation,                          &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    ene_labels, sph_rj, v_pwr, sph_OUT)
!
      flag_gzip_lc = v_pwr%gzip_flag_vol_spec
      call sel_open_sph_vol_monitor_file(id_file_rms, fname_rms,        &
     &    sph_pwr_labels, sph_OUT, zbuf_m, flag_gzip_lc)
      call dealloc_sph_espec_name(sph_OUT)
!
      call sel_gz_write_text_stream(flag_gzip_lc, id_file_rms,          &
     &    volume_pwr_data_text(time_d%i_time_step, time_d%time,         &
     &    v_pwr%ntot_comp_sq, rms_sph_v), zbuf_m)
      close(id_file_rms)
!
      end subroutine write_sph_volume_pwr_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      logical function error_sph_vol_ms_file                            &
     &               (id_rank, ene_labels, sph_params, sph_rj, v_pwr)
!
      use t_read_sph_spectra
      use t_buffer_4_gzip
      use gz_open_sph_vol_mntr_file
      use sph_monitor_data_text
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
!
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
      character(len=kchara) :: fname_rms, mode_label
!
      type(read_sph_spectr_data), save :: sph_OUT
      logical :: flag_gzip_lc, error
!
!
      error_sph_vol_ms_file = .FALSE.
      if(v_pwr%ntot_comp_sq .eq. 0)  return
!
      if(id_rank .ne. v_pwr%irank_m) return
!
      flag_gzip_lc = v_pwr%gzip_flag_vol_spec
      write(fname_rms,   '(a,a6)')                                      &
     &      trim(v_pwr%fhead_rms_v), '_s.dat'
      write(mode_label,'(a)') 'EMPTY'
      call dup_sph_vol_spectr_header                                    &
     &   (mode_label, sph_params%l_truncation,                          &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    ene_labels, sph_rj, v_pwr, sph_OUT)
      call check_sph_vol_monitor_file(fname_rms, sph_pwr_labels,        &
     &    sph_OUT, flag_gzip_lc, error)
      call dealloc_sph_espec_name(sph_OUT)
      error_sph_vol_ms_file =  error
!
      end function error_sph_vol_ms_file
!
!  --------------------------------------------------------------------
!
      end module output_sph_pwr_volume_file
