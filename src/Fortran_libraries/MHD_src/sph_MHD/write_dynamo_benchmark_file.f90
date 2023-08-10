!>@file   write_dynamo_benchmark_file.f90
!!@brief  module write_dynamo_benchmark_file
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine write_dynamobench_files                              &
!!     &         (my_rank, sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, &
!!     &          time_d, gzip_flag, circ_mid_eq, bench)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_vol_mean_squares), intent(in) :: v_pwr
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!        logical, intent(in) :: gzip_flag
!!        type(dynamobench_monitor), intent(in) :: bench
!!        type(circle_fld_maker), intent(in) :: circ_mid_eq
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module write_dynamo_benchmark_file
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_phys_address
      use t_base_field_labels
      use t_sph_volume_mean_square
      use t_field_4_dynamobench
      use t_field_on_circle
      use t_read_sph_spectra
      use t_time_data
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_dbench = 36
!
      type(sph_spectr_head_labels), parameter                           &
     &            :: sph_dnamobench_labels = sph_spectr_head_labels(    &
     &                           hdr_nri = 'radial_layers',             &
     &                           hdr_ltr = 'truncation',                &
     &                           hdr_ICB_id = 'ICB_id',                 &
     &                           hdr_CMB_id = 'CMB_id',                 &
     &                           hdr_kr_in =  'Not_used',               &
     &                           hdr_r_in =   'Not_used',               &
     &                           hdr_kr_out = 'Upper_boundary_ID',      &
     &                           hdr_r_out =  'Upper_boundary_radius',  &
     &                           hdr_num_field = 'Number_of_field',     &
     &                           hdr_num_comp = 'Number_of_components')
!
      private :: dup_dynamobench_header_to_IO
      private :: dup_detail_dbench_header_to_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_dynamobench_files                                &
     &         (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr,            &
     &          time_d, gzip_flag, circ_mid_eq, bench)
!
      use write_monitors_circle_file
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_vol_mean_squares), intent(in) :: v_pwr
      type(time_data), intent(in) :: time_d
!
      logical, intent(in) :: gzip_flag
      type(dynamobench_monitor), intent(in) :: bench
      type(circle_fld_maker), intent(in) :: circ_mid_eq
!
!
      if(bench%iflag_dynamobench .le. izero) return
      if(my_rank .ne. 0) return
      call write_dynamobench_file(sph_params, sph_rj, ipol,             &
     &   sph_MHD_bc, v_pwr, time_d, gzip_flag, bench)
      call write_detailed_dbench_file(sph_params, sph_rj, ipol,         &
     &   sph_MHD_bc, v_pwr, time_d, gzip_flag, bench)
      call write_mtr_on_circle_file(my_rank, sph_params, time_d,        &
     &                              circ_mid_eq)
!
      end subroutine write_dynamobench_files
!
! ----------------------------------------------------------------------
!
      subroutine write_dynamobench_file                                 &
     &         (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr,            &
     &          time_d, gzip_flag, bench)
!
      use t_buffer_4_gzip
      use set_parallel_file_name
      use sph_monitor_data_text
      use gz_open_sph_vol_mntr_file
      use select_gz_stream_file_IO
      use dup_dynamobench_data_to_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_vol_mean_squares), intent(in) :: v_pwr
      type(time_data), intent(in) :: time_d
!
      logical, intent(in) :: gzip_flag
      type(dynamobench_monitor), intent(in) :: bench
!
!      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      logical :: flag_gzip_lc
      character(len = kchara) :: file_name
      type(buffer_4_gzip) :: zbuf_d
      type(read_sph_spectr_data) :: sph_OUT_d
!
      real(kind = kreal), allocatable :: data_out(:)
!
!
      if(my_rank .ne. 0) return
!
      call dup_dynamobench_header_to_IO                                 &
     &   (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, sph_OUT_d)
!
      allocate(data_out(sph_OUT_d%ntot_sph_spec))
      call dup_dynamobench_monitor_data                                 &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base, bench,   &
     &    sph_OUT_d%ntot_sph_spec, data_out)
!
      flag_gzip_lc = gzip_flag
      file_name = add_dat_extension(bench%benchmark_file_prefix)
      call sel_open_sph_vol_monitor_file(id_dbench, file_name,          &
     &    sph_dnamobench_labels, sph_OUT_d, zbuf_d, flag_gzip_lc)
      call dealloc_sph_espec_name(sph_OUT_d)
!
      call sel_gz_write_text_stream(flag_gzip_lc, id_dbench,            &
     &    volume_pwr_data_text(time_d%i_time_step, time_d%time,         &
     &                         sph_OUT_d%ntot_sph_spec, data_out),      &
     &    zbuf_d)
      close(id_dbench)
      deallocate(data_out)
!
      end subroutine write_dynamobench_file
!
! ----------------------------------------------------------------------
!
      subroutine write_detailed_dbench_file                             &
     &         (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr,            &
     &          time_d, gzip_flag, bench)
!
      use t_buffer_4_gzip
      use set_parallel_file_name
      use sph_monitor_data_text
      use gz_open_sph_vol_mntr_file
      use select_gz_stream_file_IO
      use dup_detailed_dbench_to_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_vol_mean_squares), intent(in) :: v_pwr
      type(time_data), intent(in) :: time_d
!
      logical, intent(in) :: gzip_flag
      type(dynamobench_monitor), intent(in) :: bench
!
!      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      logical :: flag_gzip_lc
      character(len = kchara) :: file_name
      type(buffer_4_gzip) :: zbuf_d
      type(read_sph_spectr_data) :: sph_OUT_d
!
      real(kind = kreal), allocatable :: detail_out(:)
!
!
      if(no_file_flag(bench%detail_bench_file_prefix)) return
      if(my_rank .ne. 0) return
!
      call dup_detail_dbench_header_to_IO                               &
     &   (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, sph_OUT_d)
!
      allocate(detail_out(sph_OUT_d%ntot_sph_spec))
      call dup_detail_dbench_monitor_data                               &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base, bench,   &
     &    sph_OUT_d%ntot_sph_spec, detail_out)
!
      flag_gzip_lc = gzip_flag
      file_name = add_dat_extension(bench%detail_bench_file_prefix)
      call sel_open_sph_vol_monitor_file(id_dbench, file_name,          &
     &    sph_dnamobench_labels, sph_OUT_d, zbuf_d, flag_gzip_lc)
      call dealloc_sph_espec_name(sph_OUT_d)
!
      call sel_gz_write_text_stream(flag_gzip_lc, id_dbench,            &
     &    volume_pwr_data_text(time_d%i_time_step, time_d%time,         &
     &                         sph_OUT_d%ntot_sph_spec, detail_out),    &
     &                         zbuf_d)
      close(id_dbench)
      deallocate(detail_out)
!
      end subroutine write_detailed_dbench_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dup_dynamobench_header_to_IO                           &
     &         (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, sph_OUT)
!
      use dup_dynamobench_data_to_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      if(allocated(sph_OUT%ene_sph_spec_name)) return
!
      sph_OUT%ltr_sph = sph_params%l_truncation
      sph_OUT%nri_sph = sph_rj%nidx_rj(1)
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  sph_params%nlayer_ICB
      sph_OUT%kr_CMB =  sph_params%nlayer_CMB
      sph_OUT%kr_inner = v_pwr%kr_inside(1)
      sph_OUT%kr_outer = v_pwr%kr_outside(1)
      sph_OUT%r_inner =  v_pwr%r_inside
      sph_OUT%r_outer =  v_pwr%r_outside
!
      call count_dynamobench_monitor_name                               &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base,          &
     &    sph_OUT%nfield_sph_spec, sph_OUT%ntot_sph_spec)
!
      sph_OUT%num_time_labels = 2
      call alloc_sph_espec_name(sph_OUT)
      call copy_dynamobench_monitor_name                                &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base,          &
     &    sph_OUT%nfield_sph_spec, sph_OUT%num_labels,                  &
     &    sph_OUT%ncomp_sph_spec, sph_OUT%ene_sph_spec_name)
!
      end subroutine dup_dynamobench_header_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine dup_detail_dbench_header_to_IO                         &
     &         (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, sph_OUT)
!
      use dup_detailed_dbench_to_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      if(allocated(sph_OUT%ene_sph_spec_name)) return
!
      sph_OUT%ltr_sph = sph_params%l_truncation
      sph_OUT%nri_sph = sph_rj%nidx_rj(1)
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  sph_params%nlayer_ICB
      sph_OUT%kr_CMB =  sph_params%nlayer_CMB
      sph_OUT%kr_inner = v_pwr%kr_inside(1)
      sph_OUT%kr_outer = v_pwr%kr_outside(1)
      sph_OUT%r_inner =  v_pwr%r_inside
      sph_OUT%r_outer =  v_pwr%r_outside
!
      call cnt_detail_dbench_monitor_name                               &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base,          &
     &    sph_OUT%nfield_sph_spec, sph_OUT%ntot_sph_spec)
!
      sph_OUT%num_time_labels = 2
      call alloc_sph_espec_name(sph_OUT)
      call copy_detail_dbench_monitor_name                              &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base,          &
     &    sph_OUT%nfield_sph_spec, sph_OUT%num_labels,                  &
     &    sph_OUT%ncomp_sph_spec, sph_OUT%ene_sph_spec_name)
!
      end subroutine dup_detail_dbench_header_to_IO
!
! ----------------------------------------------------------------------
!
      end module write_dynamo_benchmark_file
