!> @file  output_sph_pwr_layer_file.f90
!!      module output_sph_pwr_layer_file
!!
!! @author  H. Matsui
!! @date Programmed in Feb. 2008
!
!> @brief Output mean square of spectr data
!!
!!@verbatim
!!      subroutine write_sph_layer_ms_file                              &
!!     &         (id_rank, ene_labels, time_d, sph_params, pwr)
!!      subroutine write_sph_layer_spectr_file                          &
!!     &         (id_rank, ene_labels, time_d, sph_params, pwr)
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
      module output_sph_pwr_layer_file
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
      private :: write_sph_layer_pwr_file, write_sph_layer_spec_file
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_layer_ms_file                                &
     &         (id_rank, ene_labels, time_d, sph_params, pwr)
!
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
!
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_mean_squares), intent(in) :: pwr
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(pwr%iflag_layer_rms_spec .eq. izero)  return
      if(pwr%ntot_comp_sq .eq. 0)  return
!
      if(id_rank .ne. pwr%irank_m) return
!
!       write(*,*) 'write_sph_layer_ms_file m', id_rank
      write(fname_rms,   '(a,a6)') trim(pwr%fhead_rms_layer), '_s.dat'
      write(mode_label,'(a)') 'EMPTY'
      call write_sph_layer_pwr_file(fname_rms, mode_label,              &
     &    ene_labels, time_d, sph_params%l_truncation,                  &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    pwr, pwr%shl_sq)
!
      end subroutine write_sph_layer_ms_file
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_spectr_file                            &
     &         (id_rank, ene_labels, time_d, sph_params, pwr)
!
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
!
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_mean_squares), intent(in) :: pwr
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(pwr%iflag_layer_rms_spec .eq. izero)  return
      if(pwr%ntot_comp_sq .eq. 0)  return
!
      if(id_rank .eq. pwr%irank_m) then
!        write(*,*) 'write_sph_layer_spec_file m', id_rank
        write(fname_rms, '(a,a6)') trim(pwr%fhead_rms_layer), '_m.dat'
        write(mode_label,'(a)') 'order'
        call write_sph_layer_spec_file(fname_rms, mode_label,           &
     &      ene_labels, time_d, sph_params%l_truncation,                &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      pwr, pwr%shl_m)
!
        write(fname_rms,'(a,a7)') trim(pwr%fhead_rms_layer), '_m0.dat'
        write(mode_label,'(a)') 'EMPTY'
        call write_sph_layer_pwr_file(fname_rms, mode_label,            &
     &      ene_labels, time_d, sph_params%l_truncation,                &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      pwr, pwr%shl_m0)
      end if
!
      if(id_rank .eq. pwr%irank_l) then
!        write(*,*) 'write_sph_layer_spec_file l', id_rank
        write(fname_rms, '(a,a6)') trim(pwr%fhead_rms_layer), '_l.dat'
        write(mode_label,'(a)') 'degree'
        call write_sph_layer_spec_file(fname_rms, mode_label,           &
     &      ene_labels, time_d, sph_params%l_truncation,                &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      pwr, pwr%shl_l)
      end if
!
      if(id_rank .eq. pwr%irank_lm) then
!        write(*,*) 'write_sph_layer_spec_file lm', id_rank
        write(fname_rms,'(a,a7)') trim(pwr%fhead_rms_layer), '_lm.dat'
        write(mode_label,'(a)') 'diff_deg_order'
        call write_sph_layer_spec_file(fname_rms, mode_label,           &
     &      ene_labels, time_d, sph_params%l_truncation,                &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      pwr, pwr%shl_lm)
      end if
!
      end subroutine write_sph_layer_spectr_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_pwr_file                               &
     &         (fname_rms, mode_label, ene_labels, time_d,              &
     &          ltr, nlayer_ICB, nlayer_CMB, pwr, rms_sph)
!
      use t_read_sph_spectra
      use t_buffer_4_gzip
      use gz_open_sph_layer_mntr_file
      use gz_layer_mean_monitor_IO
      use sph_mean_spectr_header_IO
      use set_parallel_file_name
      use sph_power_spectr_data_text
!
      character(len=kchara), intent(in) :: fname_rms, mode_label
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_mean_squares), intent(in) :: pwr
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in)                                    &
     &           :: rms_sph(pwr%nri_rms,pwr%ntot_comp_sq)
!
      real(kind = kreal), allocatable :: spectr_IO(:,:)
      type(read_sph_spectr_data), save :: sph_OUT
      type(buffer_4_gzip), save :: zbuf_m
      logical :: flag_gzip_lc
!
!
      call dup_sph_layer_spectr_header(mode_label,                      &
     &    ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr, sph_OUT)
      call alloc_sph_spectr_data(izero, sph_OUT)
      allocate(spectr_IO(pwr%ntot_comp_sq,pwr%nri_rms))
!
      flag_gzip_lc = pwr%gzip_flag_rms_layer
      call sel_open_sph_layer_mean_file(id_file_rms, fname_rms,         &
     &    sph_pwr_labels, sph_OUT, zbuf_m, flag_gzip_lc)
      call swap_layer_mean_to_IO(pwr%nri_rms, pwr%ntot_comp_sq,         &
     &                           rms_sph, spectr_IO(1,1))
      call sel_gz_write_layer_mean_mtr                                  &
     &   (flag_gzip_lc, id_file_rms, time_d%i_time_step, time_d%time,   &
     &    pwr%nri_rms, pwr%kr_4_rms, pwr%r_4_rms,                       &
     &    pwr%ntot_comp_sq, spectr_IO(1,1), zbuf_m)
      deallocate(spectr_IO)
      call dealloc_sph_espec_data(sph_OUT)
      call dealloc_sph_espec_name(sph_OUT)
      close(id_file_rms)
!
      end subroutine write_sph_layer_pwr_file
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_spec_file                              &
     &         (fname_rms, mode_label, ene_labels, time_d,              &
     &          ltr, nlayer_ICB, nlayer_CMB, pwr, rms_sph_x)
!
      use t_read_sph_spectra
      use t_buffer_4_gzip
      use gz_open_sph_layer_mntr_file
      use gz_layer_spectr_monitor_IO
      use sph_mean_spectr_header_IO
      use set_parallel_file_name
      use sph_power_spectr_data_text
!
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_mean_squares), intent(in) :: pwr
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq)
!
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
      real(kind = kreal), allocatable :: spectr_IO(:,:,:)
      type(read_sph_spectr_data), save :: sph_OUT
      type(buffer_4_gzip), save :: zbuf_m
      logical :: flag_gzip_lc
!
!
      call dup_sph_layer_spectr_header(mode_label,                      &
     &    ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr, sph_OUT)
      call alloc_sph_spectr_data(sph_OUT%ltr_sph, sph_OUT)
      allocate(spectr_IO(pwr%ntot_comp_sq,0:ltr,pwr%nri_rms))
!
      flag_gzip_lc = pwr%gzip_flag_rms_layer
      call sel_open_sph_layer_mean_file(id_file_rms, fname_rms,         &
     &    sph_pwr_labels, sph_OUT, zbuf_m, flag_gzip_lc)
      call swap_layer_spectr_to_IO(pwr%nri_rms, ltr, pwr%ntot_comp_sq,  &
     &                             rms_sph_x, spectr_IO(1,0,1))
      call sel_gz_write_layer_spectr_mtr                                &
     &   (flag_gzip_lc, id_file_rms, time_d%i_time_step, time_d%time,   &
     &    pwr%nri_rms, pwr%kr_4_rms, pwr%r_4_rms, ltr,                  &
     &    pwr%ntot_comp_sq, spectr_IO(1,0,1), zbuf_m)
      deallocate(spectr_IO)
      call dealloc_sph_espec_data(sph_OUT)
      call dealloc_sph_espec_name(sph_OUT)
      close(id_file_rms)
!
      end subroutine write_sph_layer_spec_file
!
! -----------------------------------------------------------------------
!
      end module output_sph_pwr_layer_file
