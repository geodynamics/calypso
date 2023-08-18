!>@file   write_monitors_circle_file.f90
!!@brief  module write_monitors_circle_file
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine write_mtr_on_circle_file(my_rank, sph_params,        &
!!     &                                    time_d, cdat)
!!        integer, intent(in) :: my_rank
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(time_data), intent(in) :: time_d
!!        type(circle_fld_maker), intent(in) :: cdat
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module write_monitors_circle_file
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_time_data
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_read_sph_spectra
      use t_field_on_circle
      use t_circle_transform
      use t_sph_circle_parameters
      use t_FFT_selector
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_circle = 36
!
      type(sph_spectr_head_labels), parameter                           &
     &            :: circle_field_labels = sph_spectr_head_labels(      &
     &                           hdr_nri = 'Number_of_grids',           &
     &                           hdr_ltr = 'truncation',                &
     &                           hdr_ICB_id = 'ICB_id',                 &
     &                           hdr_CMB_id = 'CMB_id',                 &
     &                           hdr_kr_in =  'Not_used',               &
     &                           hdr_r_in =   'Cylindrical_radius',     &
     &                           hdr_kr_out = 'Not_used',               &
     &                           hdr_r_out =  'Height_z',               &
     &                           hdr_num_field = 'Number_of_field',     &
     &                           hdr_num_comp = 'Number_of_components')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_mtr_on_circle_file(my_rank, sph_params,          &
     &                                    time_d, cdat)
!
      integer, intent(in) :: my_rank
      type(sph_shell_parameters), intent(in) :: sph_params
      type(time_data), intent(in) :: time_d
      type(circle_fld_maker), intent(in) :: cdat
!
!
      if(my_rank .ne. 0) return
!
      call write_fields_on_circle_file(my_rank, sph_params, time_d,     &
     &    cdat%leg_circ, cdat%circle, cdat%d_circle,                    &
     &    cdat%mphi_list, cdat%phi_list)
      call write_spectr_on_circle_file(my_rank, sph_params, time_d,     &
     &    cdat%leg_circ, cdat%circle, cdat%d_circle)
      call write_phase_on_circle_file(my_rank, sph_params, time_d,      &
     &    cdat%leg_circ, cdat%circle, cdat%d_circle)
!
      end subroutine write_mtr_on_circle_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_fields_on_circle_file                            &
     &         (my_rank, sph_params, time_d,                            &
     &          leg_circ, circle, d_circle, mphi_list, phi_list)
!
      use t_buffer_4_gzip
      use set_parallel_file_name
      use sph_monitor_data_text
      use gz_open_sph_layer_mntr_file
      use gz_layer_mean_monitor_IO
      use dup_fields_on_circle_to_IO
      use sel_open_sph_fld_on_circle
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(sph_shell_parameters), intent(in) :: sph_params
      type(time_data), intent(in) :: time_d
      type(circle_transform_spectr), intent(in) :: leg_circ
      type(circle_parameters), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
      integer(kind = kint), intent(in) :: mphi_list(circle%mphi_circle)
      real(kind = kreal), intent(in) :: phi_list(circle%mphi_circle)
!
      logical :: flag_gzip_lc
      character(len = kchara) :: file_name
      type(buffer_4_gzip) :: zbuf_d
      type(read_sph_spectr_data) :: sph_OUT_d
!
      real(kind = kreal), allocatable :: spectr_IO(:,:)
!
!
      if(no_file_flag(circle%circle_field_file_prefix)) return
      if(my_rank .ne. 0) return
!
      call dup_field_on_circ_header_to_IO(sph_params,                   &
     &    leg_circ, circle, d_circle, sph_OUT_d)
!
      allocate(spectr_IO(d_circle%ntot_phys_viz,circle%mphi_circle))
!
      flag_gzip_lc = circle%gzip_flag_circle
      file_name = add_dat_extension(circle%circle_field_file_prefix)
      call sel_open_sph_fld_on_circle_file                              &
     &   (id_circle, file_name, circle_field_labels, circle,            &
     &    sph_OUT_d, zbuf_d, flag_gzip_lc)
      call swap_layer_mean_to_IO(circle%mphi_circle,                    &
     &    d_circle%ntot_phys_viz, d_circle%d_fld, spectr_IO(1,1))
      call sel_gz_write_layer_mean_mtr                                  &
     &   (flag_gzip_lc, id_circle, time_d%i_time_step, time_d%time,     &
     &    circle%mphi_circle, mphi_list, phi_list,                      &
     &    d_circle%ntot_phys_viz, spectr_IO(1,1), zbuf_d)
      close(id_circle)
!
      call dealloc_sph_espec_name(sph_OUT_d)
      deallocate(spectr_IO)
!
      end subroutine write_fields_on_circle_file
!
! ----------------------------------------------------------------------
!
      subroutine write_spectr_on_circle_file(my_rank, sph_params,       &
     &          time_d, leg_circ, circle, d_circle)
!
      use t_buffer_4_gzip
      use set_parallel_file_name
      use sph_monitor_data_text
      use gz_open_sph_layer_mntr_file
      use gz_volume_spectr_monitor_IO
      use dup_fields_on_circle_to_IO
      use sel_open_sph_fld_on_circle
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(sph_shell_parameters), intent(in) :: sph_params
      type(time_data), intent(in) :: time_d
      type(circle_transform_spectr), intent(in) :: leg_circ
      type(circle_parameters), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
!
      logical :: flag_gzip_lc
      character(len = kchara) :: file_name, fname_tmp
      type(buffer_4_gzip) :: zbuf_d
      type(read_sph_spectr_data) :: sph_OUT_d
!
      real(kind = kreal), allocatable :: spectr_IO(:,:)
!
!
      if(no_file_flag(circle%circle_spectr_file_prefix)) return
      if(my_rank .ne. 0) return
!
      call dup_spectr_on_circ_header_to_IO(sph_params,                  &
     &    leg_circ, circle, d_circle, sph_OUT_d)
!
      allocate(spectr_IO(d_circle%ntot_phys_viz,0:leg_circ%ltr_circle))
!
      flag_gzip_lc = circle%gzip_flag_circle
      write(fname_tmp,'(a,a4)')                                         &
     &           trim(circle%circle_spectr_file_prefix), "_pwr"
      file_name = add_dat_extension(fname_tmp)
      call sel_open_sph_fld_on_circle_file                              &
     &   (id_circle, file_name, circle_field_labels, circle,            &
     &    sph_OUT_d, zbuf_d, flag_gzip_lc)
      call swap_volume_spectr_to_IO(leg_circ%ltr_circle,                &
     &    d_circle%ntot_phys_viz, leg_circ%vrtm_mag, spectr_IO(1,0))
      call sel_gz_write_volume_spectr_mtr                               &
     &   (flag_gzip_lc, id_circle, time_d%i_time_step, time_d%time,     &
     &    leg_circ%ltr_circle, d_circle%ntot_phys_viz,                  &
     &    spectr_IO(1,0), zbuf_d)
      close(id_circle)
!
      call dealloc_sph_espec_name(sph_OUT_d)
      deallocate(spectr_IO)
!
      end subroutine write_spectr_on_circle_file
!
! ----------------------------------------------------------------------
!
      subroutine write_phase_on_circle_file(my_rank, sph_params,        &
     &          time_d, leg_circ, circle, d_circle)
!
      use t_buffer_4_gzip
      use set_parallel_file_name
      use sph_monitor_data_text
      use gz_open_sph_layer_mntr_file
      use gz_volume_spectr_monitor_IO
      use dup_fields_on_circle_to_IO
      use sel_open_sph_fld_on_circle
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(sph_shell_parameters), intent(in) :: sph_params
      type(time_data), intent(in) :: time_d
      type(circle_transform_spectr), intent(in) :: leg_circ
      type(circle_parameters), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
!
      logical :: flag_gzip_lc
      character(len = kchara) :: file_name, fname_tmp
      type(buffer_4_gzip) :: zbuf_d
      type(read_sph_spectr_data) :: sph_OUT_d
!
      real(kind = kreal), allocatable :: spectr_IO(:,:)
!
!
      if(no_file_flag(circle%circle_spectr_file_prefix)) return
      if(my_rank .ne. 0) return
!
      call dup_spectr_on_circ_header_to_IO(sph_params,                  &
     &    leg_circ, circle, d_circle, sph_OUT_d)
!
      allocate(spectr_IO(d_circle%ntot_phys_viz,0:leg_circ%ltr_circle))
!
      flag_gzip_lc = circle%gzip_flag_circle
      write(fname_tmp,'(a,a6)')                                         &
     &        trim(circle%circle_spectr_file_prefix), "_phase"
      file_name = add_dat_extension(fname_tmp)
      call sel_open_sph_fld_on_circle_file                              &
     &   (id_circle, file_name, circle_field_labels, circle,            &
     &    sph_OUT_d, zbuf_d, flag_gzip_lc)
      call swap_volume_spectr_to_IO(leg_circ%ltr_circle,                &
     &    d_circle%ntot_phys_viz, leg_circ%vrtm_phase, spectr_IO(1,0))
      call sel_gz_write_volume_spectr_mtr                               &
     &   (flag_gzip_lc, id_circle, time_d%i_time_step, time_d%time,     &
     &    leg_circ%ltr_circle, d_circle%ntot_phys_viz,                  &
     &    spectr_IO(1,0), zbuf_d)
      close(id_circle)
!
      call dealloc_sph_espec_name(sph_OUT_d)
      deallocate(spectr_IO)
!
      end subroutine write_phase_on_circle_file
!
! ----------------------------------------------------------------------
!
      end module write_monitors_circle_file
