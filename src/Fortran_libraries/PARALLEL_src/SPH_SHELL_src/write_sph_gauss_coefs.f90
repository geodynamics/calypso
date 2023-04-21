!>@file   write_sph_gauss_coefs.f90
!!@brief  module write_sph_gauss_coefs
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine append_sph_gauss_coefs_file                          &
!!     &         (time_d, sph_params, sph_rj, ipol, rj_fld,             &
!!     &          gauss, SR_sig)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: gauss
!!        type(send_recv_status), intent(inout) :: SR_sig
!!
!!     logical function error_gauss_coefs_header(sph_params, sph_rj,    &
!!    &                                          gauss)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(picked_spectrum_data), intent(in) :: gauss
!!@endverbatim
!!
      module write_sph_gauss_coefs
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_address
      use t_phys_data
      use t_time_data
      use t_read_sph_spectra
      use t_buffer_4_gzip
      use m_monitor_file_labels
!
      implicit  none
!
      integer(kind = kint), parameter, private :: id_gauss_coef = 23
!
      type(sph_spectr_head_labels), parameter, private                  &
     &            :: gauss_coefs_labels = sph_spectr_head_labels(       &
     &                         hdr_nri = 'radial_layers',               &
     &                         hdr_ltr = 'truncation',                  &
     &                         hdr_ICB_id = 'ICB_id',                   &
     &                         hdr_CMB_id = 'CMB_id',                   &
     &                         hdr_kr_in =  'Not_used',                 &
     &                         hdr_r_in =   'Not_used',                 &
     &                         hdr_kr_out = 'Not_used',                 &
     &                         hdr_r_out =  'Reference_radius',         &
     &                         hdr_num_field = 'Number_of_gauss_coefs', &
     &                         hdr_num_comp = 'Number_of_gauss_coefs')
!
      private :: s_set_sph_gauss_coefs
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine append_sph_gauss_coefs_file                            &
     &         (time_d, sph_params, sph_rj, ipol, rj_fld,               &
     &          gauss, SR_sig)
!
      use t_solver_SR
      use t_read_sph_spectra
      use set_parallel_file_name
      use delete_data_files
      use select_gz_stream_file_IO
      use sph_monitor_data_text
      use gz_open_sph_vol_mntr_file
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: gauss
!
      type(send_recv_status), intent(inout) :: SR_sig
!
      real(kind=kreal), allocatable :: d_rj_out(:)
!
      type(read_sph_spectr_data) :: sph_OUT
      character(len=kchara) :: file_name
      logical :: flag_gzip_lc
      type(buffer_4_gzip) :: zbuf_m
!
!
      if(gauss%num_sph_mode .le. 0) return
!
      if(my_rank .eq. 0) then
        allocate(d_rj_out(gauss%istack_picked_spec_lc(nprocs)))
!$omp parallel workshare
        d_rj_out(1:gauss%istack_picked_spec_lc(nprocs)) = 0.0d0
!$omp end parallel workshare
      else
        allocate(d_rj_out(0))
      end if
!
      call s_set_sph_gauss_coefs(sph_params, sph_rj, ipol, rj_fld,      &
     &    gauss, gauss%istack_picked_spec_lc(nprocs), d_rj_out, SR_sig)
!
      if(my_rank .eq. 0) then
        file_name = add_dat_extension(gauss%file_prefix)
!
        flag_gzip_lc = gauss%flag_gzip
        call dup_gauss_coefs_header_to_IO                               &
     &     (sph_params%l_truncation, sph_rj%nidx_rj(1),                 &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      gauss, sph_OUT)
        call sel_open_sph_vol_monitor_file(id_gauss_coef, file_name,    &
     &      gauss_coefs_labels, sph_OUT, zbuf_m, flag_gzip_lc)
!
        call sel_gz_write_text_stream(flag_gzip_lc, id_gauss_coef,      &
     &      volume_pwr_data_text(time_d%i_time_step, time_d%time,       &
     &      gauss%istack_picked_spec_lc(nprocs), d_rj_out), zbuf_m)
        call dealloc_sph_espec_name(sph_OUT)
        close(id_gauss_coef)
      end if
      deallocate(d_rj_out)
!
      end subroutine append_sph_gauss_coefs_file
!
!  ---------------------------------------------------------------------
!
      subroutine dup_gauss_coefs_header_to_IO                           &
     &         (ltr, nri, nlayer_ICB, nlayer_CMB, gauss, sph_OUT)
!
      use m_time_labels
!
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: gauss
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: icou, ntot
!
!
      sph_OUT%ltr_sph = ltr
      sph_OUT%nri_sph = nri
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  nlayer_ICB
      sph_OUT%kr_CMB =  nlayer_CMB
      sph_OUT%kr_inner = izero
      sph_OUT%kr_outer = izero
      sph_OUT%r_inner =  zero
      sph_OUT%r_outer =  gauss%radius_gl(1)
!
      ntot = gauss%istack_picked_spec_lc(nprocs)
      sph_OUT%nfield_sph_spec = ntot
      sph_OUT%ntot_sph_spec =   ntot
      sph_OUT%num_time_labels = 2
      call alloc_sph_espec_name(sph_OUT)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      icou = sph_OUT%num_time_labels
      sph_OUT%ene_sph_spec_name(icou+1:icou+ntot)                       &
     &                             = gauss%gauss_mode_name_out(1:ntot)
      sph_OUT%ncomp_sph_spec(1:ntot) = 1
!
      end subroutine dup_gauss_coefs_header_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine s_set_sph_gauss_coefs(sph_params, sph_rj,              &
     &          ipol, rj_fld, gauss, ntot_gauss, d_rj_out, SR_sig)
!
      use t_solver_SR
      use pickup_gauss_coefficients
      use collect_SR_N
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: gauss
      integer(kind = kint), intent(in) :: ntot_gauss
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_gauss)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint_gl) :: num
      real(kind=kreal), allocatable :: d_rj_lc(:)
!
!
      num = gauss%istack_picked_spec_lc(my_rank+1)                      &
     &     - gauss%istack_picked_spec_lc(my_rank)
      allocate(d_rj_lc(gauss%num_sph_mode_lc))
      if(num .gt. 0) then
        call gauss_coefficients_4_write                                 &
     &     (sph_params, sph_rj, ipol, rj_fld, gauss, d_rj_lc)
      end if
!
      call collect_small_send_recv(gauss%istack_picked_spec_lc,         &
     &    gauss%num_sph_mode_lc, d_rj_lc,                               &
     &    gauss%istack_picked_spec_lc(nprocs), d_rj_out, SR_sig)
      deallocate(d_rj_lc)
!
      end subroutine s_set_sph_gauss_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
     logical function error_gauss_coefs_header(sph_params, sph_rj,      &
    &                                          gauss)
!
      use set_parallel_file_name
      use check_sph_monitor_header
      use gz_open_sph_vol_mntr_file
      use compare_sph_monitor_header
      use sph_power_spectr_data_text
      use sel_gz_input_sph_mtr_head
      use select_gz_stream_file_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(picked_spectrum_data), intent(in) :: gauss
!!
      type(read_sph_spectr_data) :: sph_OUT_g
      character(len = kchara) :: base_name
      logical :: flag_gzip_lc, error
!
!
      error_gauss_coefs_header = .FALSE.
      if(gauss%num_sph_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      call dup_gauss_coefs_header_to_IO                                 &
     &   (sph_params%l_truncation, sph_rj%nidx_rj(1),                   &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    gauss, sph_OUT_g)
!
      flag_gzip_lc = gauss%flag_gzip
      base_name = add_dat_extension(gauss%file_prefix)
      call check_sph_vol_monitor_file(base_name, gauss_coefs_labels,    &
    &     sph_OUT_g, flag_gzip_lc, error)
      call dealloc_sph_espec_name(sph_OUT_g)
      error_gauss_coefs_header = error
!
      end function error_gauss_coefs_header
!
! -----------------------------------------------------------------------
!
      end module write_sph_gauss_coefs
