!>@file   write_typical_scale.f90
!!@brief      module write_typical_scale
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate lengh scale data
!!
!!@verbatim
!!      subroutine write_typical_scales                                 &
!!     &         (i_step, time, sph_params, sph_rj, sph_bc_U, pwr, tsl)
!!      logical function error_typical_scale_header(sph_params, sph_rj, &
!!    &                                             sph_bc_U, pwr, tsl)
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(typical_scale_data), intent(in) :: tsl
!!@endverbatim
!
      module write_typical_scale
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_sph_typical_scales
      use t_field_labels
      use t_phys_data
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_boundary_params_sph_MHD
      use t_buffer_4_gzip
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_scale = 36
!
      private :: dup_typ_scale_head_params
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_typical_scales                                   &
     &         (i_step, time, sph_params, sph_rj, sph_bc_U, pwr, tsl)
!
      use t_rms_4_sph_spectr
      use t_read_sph_spectra
      use t_sph_volume_mean_square
      use sph_monitor_data_text
      use select_gz_stream_file_IO
      use gz_open_sph_monitor_file
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_mean_squares), intent(in) :: pwr
      type(typical_scale_data), intent(in) :: tsl
!
      character(len = kchara) :: base_name
      logical :: flag_gzip_lc
      type(buffer_4_gzip) :: zbuf_t
      type(read_sph_spectr_params) :: sph_OUT_s
      real(kind=kreal), allocatable :: d_rj_out(:)
      integer(kind = kint) :: icou
!
!
      if(tsl%iflag_ub_scales .le. izero) return
      if((tsl%icomp_kene + tsl%icomp_mene) .le. 0) return
      if(my_rank .ne. pwr%v_spectr(1)%irank_m) return
!
      allocate(d_rj_out(tsl%num_lscale))
      icou = 0
      if(tsl%icomp_kene .gt. 0) then
        d_rj_out(icou+1) = tsl%dl_kin
        d_rj_out(icou+2) = tsl%dm_kin
        d_rj_out(icou+3) = tsl%dlm_kin
        icou = icou + 3
      end if
      if(tsl%icomp_mene .gt. 0) then
        d_rj_out(icou+1) = tsl%dl_mag
        d_rj_out(icou+2) = tsl%dm_mag
        d_rj_out(icou+3) = tsl%dlm_mag
      end if
!
      call dup_typ_scale_head_params                                    &
     &   (sph_params%l_truncation, sph_rj%nidx_rj(1),                   &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out,                              &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0), tsl, sph_OUT_s)
!
      flag_gzip_lc = tsl%flag_gzip_scale
      base_name = add_dat_extension(tsl%scale_prefix)
      call sel_open_sph_vol_monitor_file(id_scale, base_name,           &
     &    sph_pwr_labels, sph_OUT_s, zbuf_t, flag_gzip_lc)
      call dealloc_sph_espec_name(sph_OUT_s)

      call sel_gz_write_text_stream(flag_gzip_lc, id_scale,             &
     &    volume_pwr_data_text(i_step, time, tsl%num_lscale, d_rj_out), &
     &    zbuf_t)
      close(id_scale)
!
      deallocate(d_rj_out)
!
      end subroutine write_typical_scales
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function error_typical_scale_header(sph_params, sph_rj,   &
     &                                            sph_bc_U, pwr, tsl)
!
      use t_rms_4_sph_spectr
      use t_read_sph_spectra
      use set_parallel_file_name
      use gz_open_sph_monitor_file
      use check_sph_monitor_header
      use compare_sph_monitor_header
      use sph_power_spectr_data_text
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_mean_squares), intent(in) :: pwr
      type(typical_scale_data), intent(in) :: tsl
!
      character(len = kchara) :: file_name, base_name
!
      character, pointer:: FPz_fp
      logical :: flag_gzip_lc, flag_miss, error
      type(read_sph_spectr_params) :: sph_IN_t, sph_OUT_t
      type(sph_spectr_head_labels) :: sph_lbl_IN_t
      type(buffer_4_gzip) :: zbuf_t
!
!
      error_typical_scale_header = .FALSE.
      if(tsl%iflag_ub_scales .le. izero) return
      if(tsl%num_lscale .le. 0) return
      if(my_rank .ne. pwr%v_spectr(1)%irank_m) return
!
      call dup_typ_scale_head_params                                    &
     &   (sph_params%l_truncation, sph_rj%nidx_rj(1),                   &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out,                              &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0), tsl, sph_OUT_t)
!
      flag_gzip_lc = tsl%flag_gzip_scale
      base_name = add_dat_extension(tsl%scale_prefix)
      call check_sph_vol_monitor_file(base_name, sph_pwr_labels,        &
    &     sph_OUT_t, flag_gzip_lc, error)
      call dealloc_sph_espec_name(sph_OUT_t)
      error_typical_scale_header = error
!
      end function error_typical_scale_header
!
! -----------------------------------------------------------------------
!
      subroutine dup_typ_scale_head_params                              &
     &         (ltr, nri, nlayer_ICB, nlayer_CMB,                       &
     &          kr_in, kr_out, r_in, r_out, tsl, sph_OUT)
!
      use t_read_sph_spectra
!
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: r_in, r_out
      type(typical_scale_data), intent(in) :: tsl
!
      type(read_sph_spectr_params), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: i
!
!
      sph_OUT%ltr_sph = ltr
      sph_OUT%nri_sph = nri
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB = nlayer_ICB
      sph_OUT%kr_CMB = nlayer_CMB
      sph_OUT%kr_inner = kr_in
      sph_OUT%kr_outer = kr_out
      sph_OUT%r_inner = r_in
      sph_OUT%r_outer = r_out
      sph_OUT%nfield_sph_spec = tsl%num_lscale
      sph_OUT%ntot_sph_spec =   tsl%num_lscale
      sph_OUT%num_time_labels = 2
!
      call alloc_sph_espec_name(sph_OUT)
!
      sph_OUT%ncomp_sph_spec(1:tsl%num_lscale)                          &
     &               = tsl%ncomp_lscale(1:tsl%num_lscale)
!
      sph_OUT%ene_sph_spec_name(1) = 't_step'
      sph_OUT%ene_sph_spec_name(2) = 'time'
      do i = 1, tsl%num_lscale
        sph_OUT%ene_sph_spec_name(i+sph_OUT%num_time_labels)            &
     &                                        = tsl%lscale_name(i)
      end do
!
      end subroutine dup_typ_scale_head_params
!
!   --------------------------------------------------------------------
!
      end module write_typical_scale
